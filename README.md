Code for ranking programs based on their test-suite performance is in
the `partition-fest` directory.

For generating tests, see the `testgen` directory. Some of the related
scripts in `util` expect the generator to have been built---using a
sandbox, it's pretty easy to build the generator with the dependencies
specified in the cabal file:

    cabal sandbox init
    cabal install --dependencies-only
    cabal build

Running those commands from inside `testgen` creates a program at
`dist/testgen/testgen`.
