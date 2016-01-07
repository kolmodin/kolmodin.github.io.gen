---
title: About binary
date: January 9, 2011
tags: binary, haskell
---

About a year ago I started hacking on a project to change the Get monad of the binary package. I've been working off and on since... :)

The change will allow us to incrementally provide the Get monad with input, as well as allow backtracking and a more graceful error handling.

The code was public, but got moved with the haskell.org infrastructure changes. It's now available again;

```sh
darcs get http://code.haskell.org/~kolmodin/code/binary/
```

Johan Tibell writes about the API update in [his recent blog post](http://blog.johantibell.com/2011/01/haskell-library-improvements-id-like-to.html).
Developers familiar with attoparsec code will largely be familiar with the new binary Get monad too, as it's been a heavy influence.
The type for the parse function would essentially be something like this:

```haskell
data Result r =
    Fail ByteString [ByteString] -- an error msg and a trace
  | Partial (ByteString -> Result r) -- incremental input
  | Done r -- finished!
```

A few forks of binary tries to address this too, all in their own way. Currently I know of [cereal](http://hackage.haskell.org/package/cereal) and [binary-strict](http://hackage.haskell.org/package/binary-strict).

## Performance

When benchmarking cereal and the new binary package, cereal comes out on top, at the expense of not being able to consume the input in incremental chunks.
I couldn't find the benchmark suit for binary-strict.

The reason for cereal being faster, I think, is due to that its simpler code when having a simpler state, essentially only a single strict ByteString (the input).
In binary (and attoparsec) it's a bit more complicated, due to the incremental input (in combination with supporting MonadPlus):

```haskell
data S =
  S { -- | The current input chunk
      input      :: !B.ByteString,
      -- | Saved input to be used when backtracking
      next_input :: !B.ByteString,
      -- | Have we requested all input available to parse?
      read_all   :: !Bool
    } deriving Show

newtype Get a =
  C { runCont :: forall r.
                 S ->
                 Failure   r ->
                 Success a r ->
                 Result    r }

type Failure   r = S -> [String] -> String -> Result r
type Success a r = S -> a -> Result r

bindG :: Get a -> (a -> Get b) -> Get b
bindG (C c) f = C $
  \st0 kf ks ->
    c st0 kf (\st1 a -> runCont (f a) st1 kf ks)
```

Unfortunately, this results in bad performance.
I'm guessing that's it's because it's reconstructing the state value
(of type `S`), and the remaining `ByteString` input for each value consumed, thus a lot of allocations.

So, as an experiment, I manually unpacked `S`;

```haskell
-- No longer using S
{-
data S = S { input      :: !B.ByteString,
             next_input :: !B.ByteString,
             read_all   :: !Bool
           } deriving Show
-}

newtype Get a =
  C { runCont :: forall r.
                 -- these three were part of S,
                 -- now they are separate arguments
                 B.ByteString -> -- 1
                 B.ByteString -> -- 2
                 Bool ->         -- 3
                 Failure   r ->
                 Success a r ->
                 Result    r }

type Failure   r = B.ByteString
                   -> B.ByteString
                   -> Bool
                   -> [String]
                   -> String
                   -> Result r
type Success a r = B.ByteString
                   -> B.ByteString
                   -> Bool
                   -> a
                   -> Result r

bindG :: Get a -> (a -> Get b) -> Get b
bindG (C c) f = C $
  \inp next eof kf ks ->
    c inp next eof kf
      (\inp' next' eof' a -> runCont (f a) inp' next' eof' kf ks)
```

With ghc-7, this yields a huge speed boost and reaches about half the speed of the old binary library (and ~30% faster than cereal). Unfortunately, I find the code is less readable and harder to maintain. Maybe it's worth it though.

I got a hint to see [ghc ticket #1349](http://hackage.haskell.org/trac/ghc/ticket/1349). Duncan Coutts [summarizes the issue](http://haskell.1045720.n5.nabble.com/More-speed-please-td3182083.html), this time about the Put monad. There are a lot of details and examples in those mails, suggesting an extension to GHC to control strictness in the arguments of higher order components. It'd allow us to write the prettier version, yet enjoying the nicer properties of the unpacked version. It's unlikely that we'll see the proposal implemented soon, though.

It seems there are four options;

* Go with the manually unpacked code
* Drop support for backtracking
* Drop support for incremental input
* Find something even better, you're all invited :)
