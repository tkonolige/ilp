{-# LANGUAGE OverloadedStrings #-}
module Test where

import ILP
import Data.Map

database = createDatabase [ Clause "happy" [Var "X"] (Check "eats" [Var "X"])
                          , Clause "happy" [Var "X"] (Unify (Var "X") (Atom "phil"))
                          , Clause "eats" [Atom "phil"] LTrue
                          , Clause "eats" [Atom "joel"] LTrue
                          ]
runTest = solveAll (Check "happy" [Var "X"]) database
