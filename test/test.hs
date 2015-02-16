module Test where

import ILP
import Data.Map

clause1 = Clause "happy" [Var "X"] (Check "eats" [Var "X"])
clause4 = Clause "happy" [Var "X"] (Unify (Var "X") (Atom "phil"))
clause2 = Clause "eats" [Atom "phil"] LTrue
clause3 = Clause "eats" [Atom "joel"] LTrue
database :: Database
database = fromList [("happy", [clause1, clause4]), ("eats", [clause2, clause3])]

testSolveAll = solveAll (Check "happy" [Var "X"]) database
