# ILP in Haskell
This is a simple implementation of intuitionistic logic programming in haskell.

## Building
1. Install `ghc` and `cabal` (Haskell platform is the easiest way to get these: [https://www.haskell.org/platform/](https://www.haskell.org/platform/))
2. Run `cabal install --only-dependencies` to install required dependencies
3. Run `cabal build` to build

## Testing
Tests are located inside the `test` directory. To load a test, run
`cabal run test/test.ilp`.

## Examples
```prolog
Welcome to the
+---+ +---+ +-----------+
|   | |   | |           |
|   | |   | |           |
|   | |   | |           |
|   | |   | |    +------+
|   | |   | |    |
|   | |   +----+ |     r
|   | |        | |     e
|   | +--------+ |     p
+---+       +----+     l
Type :h for help

?- :p
foo(X) X Y :- Y = Z.
full(joe).
full(john).
happy(X) :- full(X).
would_be_happy(X) :- {full(X)} => happy(X).
?- happy(X)
X = joe
.
X = john
.
?- would_be_happy(X)
X = X
.
X = joe
.
X = john
.
?- would_be_happy(jill)
Yes
.
?- ^D
Bye.
```

## Editing
All of the ilp implementation is in `src/ILP.hs`. Look at this file if you want to understand my implementation.
