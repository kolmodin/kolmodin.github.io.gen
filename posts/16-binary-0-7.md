---
title: binary 0.7
date: March 3, 2013
tags: binary, haskell
---

`binary 0.7` has just been released and compared to the current version included
in the Haskell Platform, `binary 0.5.1.0`, it brings a few new things. Most of
this has also been available in the `0.6.*` versions which already have been
available on hackage for some time.

## Incremental interface and improved error reporting

This is probably the most requested feature, and was introduced already in
`binary 0.6` In older versions of `binary`, you could run the Get monad with the
following function:

```haskell
runGet :: Get a -> Lazy.ByteString -> a
```

It takes the monad you want to execute, together a with a lazy `ByteString`
containing all the input it will need to finish. While this is straight
forward and easy to use, it does not meet all needs:
1. There is no error reporting (compare with a function returning `Maybe a`, or
   `Either ErrorMessage a`). If a decoder error happens, it'll just call
   [`error :: String -> a`](
   http://hackage.haskell.org/package/base/docs/Prelude.html#v:error).
2. What if you don't have all input when you start to decode, like for
   example if you're reading from a large file or a socket? Previously,
   lazy IO has been used to address this, but we will look into why
   this is not always suitable.

To address (1), we introduce the data type `Decoder` as
the return type for our new run function.

```haskell
-- | Incremental interface to run a decoder.
runGetIncremental :: Get a -> Decoder a
```

A `Decoder` can have successfully finished in which case it'll be
the `Done` constructor and have the final value, or have encountered a problem,
in which case it'll be the `Fail` constructor together with the error message.
In both cases, you will also get the remaining unused input, and the number of
bytes consumed when the decoder succeeded or failed. In the next section, we
will see why the this new run function does not need input as a second argument.

For (2), the traditional answer has been to use lazy I/O to get a lazy stream of
data, which will hide that I/O is going on, and we can pretend that we already
have all the input in memory. While this is fine for some scenarios, we have
given up control over error handling and file handle resources. [For some
applications, this is simply not good enough.
](http://stackoverflow.com/questions/5892653/whats-so-bad-about-lazy-i-o)

Here's where the incremental interface can help. It works in the same way in
`binary` as seen in other libraries for parsing. Instead of taking any input, it
immediately produces a `Decoder`.  In addition to `Done` and `Fail` mentioned
above, it also has the `Partial` constructor, meaning that the `Decoder` needs
more input in order to finish.

```haskell
-- | A decoder produced by running a 'Get' monad.
data Decoder a
  = Done !B.ByteString !ByteOffset a
    -- ^ The decoder has successfully finished. Except for the
    -- output value you also get any unused input as well as the
    -- number of bytes consumed.
  | Fail !B.ByteString !ByteOffset String
    -- ^ The decoder ran into an error. The decoder either used
    -- 'fail' or was not provided enough input. Contains any
    -- unconsumed input and the number of bytes consumed.
  | Partial (Maybe B.ByteString -> Decoder a)
    -- ^ The decoder has consumed the available input and needs
    -- more to continue. Provide 'Just' if more input is available
    -- and 'Nothing' otherwise, and you will get a new 'Decoder'.
```

## GHC Generics

`binary` has also got support for automatically generating instances of the
Binary class for your data types, using GHC's generics.

```haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Binary
import GHC.Generics (Generic)

data Foo = Foo deriving (Generic)

-- GHC will automatically fill out the instance
instance Binary Foo
```

The was committed to binary by Bryan O'Sullivan, though pieces copied
from the cereal package where it was written by Bas van Dijk.

## Control.Alternative

`binary` now implements the [Alternative class](
http://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Alternative),
and you can use the `<|>` combinator to try a decoder and fallback to another if
the first failed.  Although many binary formats don't strictly require
backtracking in the decoder, it can be used for convenience or if it leads to
code which is easier to read. In the example below, we parse frames in some
binary format, trying first one kind of frames, then the other. If it's neither
kind, we fail altogether.

```haskell
data Frame = FrameA {- some fields specific to A frames -}
           | FrameB {- some fields specific to B frames -}

decodeFrame :: Get Frame
decodeFrame = decodeFrameA <|> decodeFrameB <|> fail "not a frame!"

decodeFrameA :: Get Frame
decodeFrameA = do
  word <- getWord8
  unless (isFrameAMagicWord word) $ fail "not an A-frame!"
  {- continue decoding... -}
  return (FrameA {..})

decodeFrameB :: Get Frame
decodeFrameB = do
  word <- getWord8
  unless (isFrameBMagicWord word) $ fail "not an B-frame!"
  {- continue decoding... -}
  return (FrameB {..})
```

In this particular example, though, it's not required to have backtracking, as
we could have first decoded the first `Word8`, and then chosen to continue with
either `FrameA` or `FrameB`.  This continues to be the better choice for
performance and should be preferred when possible, but one can imagine a more
complicated binary format where using `<|>` is required or simply leads to
cleaner code.

## Control.Applicative

For each use of a primitive decoder, such as `getWord8`, `getWord32le`, and so
on, `binary` needs to check whether there is sufficient input left, or if it
needs to demand more input from the caller (by returning a `Partial`, as
discussed above).

```haskell
decodeWordTuple :: Get (Word32, Word16)
decodeWordTuple = do
  a <- getWord32le
  b <- getWord16le
  return (a,b)
```

Here, `binary` will check twice whether there is enough input. First for the 4
bytes of the `Word32`, and a second time whether there is 2 bytes left for the
`Word16`.  Once could think this is wasteful, and that we should only check once
at the beginning for the 6 bytes that we know will be required.  Unfortunately,
with a monadic API this is not possible (maybe possible with some new fancy kind
of rewrite rules?).

When using the [applicative class
](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Applicative.html#t:Applicative) though,
`binary` tries to be clever and join these checks.

```haskell
decodeWordTuple :: Get (Word32, Word16)
decodeWordTuple = (,) <$> getWord32le <*> getWord16le
```

It does this by using GHC's rewrite rules to rewrite the expression into
a single decoder. Note that for this to make a worthwhile difference to
the performance of your application, your application needs to already
be quite tuned. The rewriting also relies heavily on that all decoder
functions gets inlined, and can therefore be a bit hard to trigger.

## Backwards compatibility

I'm pleased to say that while the internals of binary has undergone
major changes, it still largely provides the same API to the developer.
A few convenience functions have been added, and a few functions that
doesn't make sense any more have been removed. Read the haddocks for the
[full API here](http://hackage.haskell.org/package/binary-0.7.0.1).


### Additions

#### Data.Binary
```haskell
decodeFileOrFail :: Binary a => FilePath
                 -> IO (Either (ByteOffset, String) a)
decodeOrFail :: Binary a =>
  Lazy.ByteString -> Either (Lazy.ByteString, ByteOffset, String)
                            (Lazy.ByteString, ByteOffset, a)
```

#### Data.Binary.Get
```haskell
runGetIncremental :: Get a -> Decoder a
runGetOrFail :: Get a -> Lazy.ByteString
             -> Either (Lazy.ByteString, ByteOffset, String)
                       (Lazy.ByteString, ByteOffset, a)
pushChunk :: Decoder a -> ByteString -> Decoder a
pushChunks :: Decoder a -> Lazy.ByteString -> Decoder a
pushEndOfInput :: Decoder a -> Decoder a
```

### Removals

#### Data.Binary.Get
```haskell
uncheckedLookAhead :: Int64 -> Get Lazy.ByteString
uncheckedSkip :: Int64 -> Get ()
```

The functions `lookAhead` and `lookAheadM` which disappeared in `binary-0.6.*`
and have been brought back in this release, making it easier to transition from
`binary-0.5.`

## Code

The package is available on
[hackage](http://hackage.haskell.org/package/binary). The code is available on
[github](https://github.com/kolmodin/binary).

Contributors since the last Haskell Platform version:

* Lennart Kolmodin
* Johan Tibell
* Bryan O'Sullivan
* Bas van Dijk
* Paolo Capriotti
* Eyal Lotem
* Gabor Greif
