---
title: binary by the numbers
date: February 17, 2011
tags: haskell, binary
---

# New home
I've taken the liberty to fork the `darcs` repo of `binary` and put it on
github. It contains both the latest released version as well as the new
experimental continuation based `Get` monad a branch called `cps`.

> git clone
> [git://github.com/kolmodin/binary](https://github.com/kolmodin/binary)

# Performance
It's interesting to run the benchmark of `binary` on different architectures and
with different versions of `GHC`. Although there recently has been work within
the community with fast writing (`blaze-builder` comes to mind) I've mostly been
working on how to read things fast.

The classic `binary` implementation of the `Get` monad is a state monad while
the new experimental version is continuation based, so fundamentally different.
They also perform differently. To produce the numbers below I ran the benchmark
suite of `binary`. It reads either `Word8`, `Word16`, `Word32` or `Word64` in a
(hopefully) tight loop and then presents how fast it could do it. For example,
see this graph over performance in a 32bit environment;
![](/images/15-binary-32-get.png)

The nice news is that `GHC 7.0.1` always performs better than `GHC 6.12.3`. Also
the experimental `cps` branch (the wide green line) is faster than the classic
`master` branch.
Things seems to be going well in 32bit land. Let's have a look in a 64bit
environment;
![](/images/15-binary-64-get.png)

This gives a different picture. `GHC 7.0.1` still performs better than `GHC
6.12.3`, but we can also see that the `cps` branch can't keep up with the state
monad based `master` branch (in contrast to when compiling for 32bits). Future
work will include to figure out why, and how to fix it.

Lets have a look at how binary performs at writing too;
![](/images/15-binary-put.png)

# Benchmark Environment
The tests have been performed on a Sandy Bridge CPU using GHCs native backend. I
wanted to try the LLVM backend too, but unfortunately LLVM crashes when
compiling the benchmark executable.

