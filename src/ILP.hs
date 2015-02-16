{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module ILP where

import ClassyPrelude hiding (union)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable hiding (sequence_, forM_)

-- for equivalence relations
-- import Data.Equivalence.Monad
-- import Control.Monad.ST.Trans
import UnionFind
import Control.Monad.Trans.State.Lazy (evalStateT)

import Control.Monad.Logic hiding (sequence_, forM_, mapM, forM)

{-
TODO LIST
=========
  - unique variable names in clauses (ex. foo(X) and bar(X))
-}

-- | A variable
data Variable = Var Text -- A variable variable i.e. exactly what you expect
              | Atom Text -- A "literal"
              deriving (Show, Eq, Ord)

type Symbol = Text
data Clause = Clause Symbol [Variable] Body deriving (Show, Eq, Ord)
-- | The AST of an ILP program
data Body = And Body Body
          | Or Body Body
          | Check Symbol [Variable]
          | Unify Variable Variable
          | LTrue
          | LFalse
          | Extend Clause Body -- ILP extend with clause
          deriving (Show, Eq, Ord)

type Database = Map Symbol [Clause]

-- | A backtracking equivalence monad
-- Allows for backtracking and equivalence relations
type BacktrackEquiv = UnionFind Variable Logic

addToDatabase :: Clause -> Database -> Database
addToDatabase clause@(Clause sym _ _) = insertWith (++) sym [clause]

-- | Lookup a symbol in the relation database
-- will return a list of matching clauses
lookupSymbol :: Database -> Symbol -> BacktrackEquiv [Clause]
lookupSymbol d x = case lookup x d of
                     Just a  -> return a
                     Nothing -> mzero

-- | Unify two values, must be lifted into the outermost monad
unify :: Variable -> Variable -> BacktrackEquiv ()
unify a b = do
  descA <- repr a
  descB <- repr b
  case (descA, descB) of
    (Atom a1, Atom a2) | a1 /= a2 -> do
      mzero -- fail, can't union two Atoms variables
    (Atom _, Atom _) -> return () -- we have the same class
    (Atom a, Var v) -> equate (Var v) (Atom a) -- second arguement is the representative
    (Var v, Atom a) -> equate (Var v) (Atom a)
    (Var v1, Var v2) -> equate (Var v1) (Var v2)

-- | Interprets an ILP program with a given database
-- The database is passed through because it is modified in implication
-- TODO: use State?
interpret :: Database -> Body -> BacktrackEquiv ()
interpret database (And b1 b2)          = (interpret database b1) >>- (const $ interpret database b2)
interpret database (Or b1 b2)           = interpret database b1 `interleave` interpret database b2
interpret database (Check sym vars)     = do
  clauses <- lookupSymbol database sym
  foldr1 interleave $ -- try each clause independently TODO: foldr or foldl
    (flip map) clauses -- TODO: 'flip map' should really be 'for', but it doesn't work
      (\(Clause sym' vars' body) -> do
        -- unify variables with arguements
        zipWithM_ unify vars vars'
        -- check the body
        interpret database body
      )
interpret database (Unify var1 var2)    = unify var1 var2
interpret database LTrue                = return ()
interpret database LFalse               = mzero
interpret database (Extend fact clause) = interpret (addToDatabase fact database) clause

-- | Interpret an ILP program with a given query and return the first result
-- The query is given as a Check
-- TODO: foo(a) should return true or false
solve :: Body -> Database -> [(Variable, Variable)]
solve query@(Check _ variables) database = observe $ (flip evalStateT) Map.empty $ do
  interpret database query
  mapM (\x -> repr x >>= return . (,) x) variables

-- | Interpret an ILP program and return all results
solveAll :: Body -> Database -> [[(Variable, Variable)]]
solveAll query@(Check _ variables) database = observeAll $ (flip evalStateT) Map.empty $ do
  interpret database query
  mapM (\x -> repr x >>= return . (,) x) variables
