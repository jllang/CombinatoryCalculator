# CombinatoryCalculator
A simple interpreter for combinatory logic using Polish notation and SKI-basis.

## 1. Getting started

This project requires GHC and cabal (and Git).

To clone and install the project, run:
1. `git clone ssh://git@github.com/jllang/CombinatoryCalculator.git`
2. `cd CombinatoryCalculator/`
3. `cabal update`
4. `cabal sandbox init`
5. `cabal install -j8`
   * `j8` is an optional argument, that tells cabal to use 8 threads.

After this procedure, there should be the executable at
`.cabal-sandbox/bin/CombinatoryCalculator`. Also, CombinatoryCalculator can be
used as a DSL in GHCI by running `ghci src/CombinatoryCalculator`.
