# ILP in Haskell
This is a simple implementation of intuitionistic logic programming in haskell.

## Building
1. Install `ghc` and `cabal`
2. Run `cabal install --only-dependencies` to install required dependencies
3. Run `cabal build` to build

## Testing
Tests are located inside the `test` directory. To load a test, run
`cabal repl` and then `:l test/test.hs`.
