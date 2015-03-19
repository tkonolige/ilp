{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module ILP ( Variable(..)
           , Symbol
           , Clause(..)
           , Body(..)
           , Database
           , Env
           , createDatabase
           , solve
           , solveAll
           ) where

import ClassyPrelude hiding (union, pack)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable hiding (sequence_, forM_)

-- for equivalence relations
import UnionFind
import Control.Monad.Trans.State.Lazy

import Control.Monad.Logic hiding (sequence_, forM_, mapM, forM)

data Variable = Var String -- A placeholder value
              | Atom String -- A "literal"
              deriving (Show, Eq, Ord)

type Symbol = String
data Clause = Clause Symbol [Variable] Body deriving (Show, Eq, Ord)
-- | The main AST of an ILP program
data Body = And Body Body
          | Or Body Body
          | Check Symbol [Variable]
          | Unify Variable Variable
          | LTrue
          | LFalse
          | Not Body
          | Extend Clause Body -- ILP extend with clause
          | Local Variable Body
          deriving (Show, Eq, Ord)

type Database = Map Symbol [Clause]
type Env = Map Variable Variable

-- | A backtracking equivalence monad
-- Allows for backtracking and equivalence relations
type BacktrackEquiv = UnionFind Variable Logic

-- | Add a Clause to a Database
-- New Clauses are considered before old ones
addToDatabase :: Clause -> Database -> Database
addToDatabase clause@(Clause sym _ _) = insertWith (++) sym [clause]

-- | Create Database from a list of Clauses
createDatabase :: [Clause] -> Database
createDatabase = foldl (flip addToDatabase) Map.empty

-- | Lookup a symbol in the relation database
-- will return a list of matching clauses
lookupSymbol :: Database -> Symbol -> BacktrackEquiv [Clause]
lookupSymbol d x = case lookup x d of
                     Just a  -> return a
                     Nothing -> mzero

-- | Lookup a Variable in the environment
lookup' :: Variable -> Env -> BacktrackEquiv Variable
lookup' (Var t) env = case lookup (Var t) env of
                        Just a -> return a
                        Nothing -> mzero
lookup' (Atom t) _ = return $ Atom t

-- | Unify two values, must be lifted into the outermost monad
unify :: Env -> Variable -> Variable -> BacktrackEquiv ()
unify env a b = do
  descA <- lookup' a env >>= repr
  descB <- lookup' b env >>= repr
  case (descA, descB) of
    (Atom a1, Atom a2) | a1 /= a2 -> mzero -- fail, can't union two Atoms variables
    (Atom _, Atom _) -> return () -- we have the same class, all ok
    (Atom a, Var v)  -> equate (Var v)  (Atom a) -- second arguement is the representative
    (Var v, Atom a)  -> equate (Var v)  (Atom a)
    (Var v1, Var v2) -> equate (Var v1) (Var v2)

-- | Unify with Env associated with each Variable
unify' :: Env -> Env -> Variable -> Variable -> BacktrackEquiv ()
unify' env1 env2 a b = do
  descA <- lookup' a env1 >>= repr
  descB <- lookup' b env2 >>= repr
  case (descA, descB) of
    (Atom a1, Atom a2) | a1 /= a2 -> mzero -- fail, can't union two Atoms variables
    (Atom _, Atom _) -> return () -- we have the same class
    (Atom a, Var v)  -> equate (Var v) (Atom a) -- second arguement is the representative
    (Var v, Atom a)  -> equate (Var v) (Atom a)
    (Var v1, Var v2) -> equate (Var v1) (Var v2)

-- | Interprets an ILP program with a given database
-- The database is passed through because it is modified in implication
interpret :: Env      -- ^ The local environment
          -> Database -- ^ The clause database, manually passed through because it is modified
          -> Int      -- ^ An incrementing number to generate unique local names
          -> Body     -- ^ The body of the clause to interpret
          -> BacktrackEquiv ()
interpret env database i (And b1 b2) = -- Conjunction
  (interpret env database i b1) >>- (const $ interpret env database i b2)

interpret env database i (Or b1 b2) = -- Disjunction
  interpret env database i b1 `interleave` interpret env database i b2

interpret env database i (Check sym args)  = do
  clauses <- lookupSymbol database sym
  foldr1 interleave $ -- try each clause independently
    (flip map) clauses -- TODO: 'flip map' should really be 'for', but it doesn't work
      (\c@(Clause sym' vars body) -> do
        -- lookup values of arguments
        args' <- mapM ((flip lookup') env) args
        -- create new environment
        let env' = Map.fromList $ zip vars args'
        -- unify variables with arguements
        zipWithM_ (unify' env' env) vars args
        -- check the body
        interpret env' database i body
      )

interpret env database i (Unify var1 var2) = unify env var1 var2

interpret env database i LTrue = return ()

interpret env database i LFalse = mzero -- false triggers backtracking

interpret env database i (Not body) = lnot $ interpret env database i body

interpret env database i (Extend (Clause sym vars body) clause) = do
  -- find the values of the variables
  -- TODO: forbid illegal clause bodys, should only be facts? unclear
  vars' <- mapM (\x -> lookup' x env) vars
  interpret env (addToDatabase (Clause sym vars' body) database) i clause

interpret env database i (Local (Var var) body) =
  -- insert a new variable into the environment, it must have a unique name
  interpret (Map.insert (Var var) (Var $ var ++ show i) env) database (i+1) body

isVar :: Variable -> Bool
isVar (Var _) = True
isVar (Atom _ ) = False

-- | Find all variables in a statement and populate a map with them
--   This is used to set up the initial Env for a query
populateEnv :: Body -> Env
populateEnv b = Map.fromList $ map (\a->(a,a)) $ filter isVar $ pop b
  where
    pop (And a b) = pop a ++ pop b
    pop (Or a b) = pop a ++ pop b
    pop (Check _ vars) = vars
    pop (Unify a b) = [a,b]
    pop (Not b) = pop b
    pop _ = [] -- TODO handle Extend

makeQuery :: Body -> Database -> Logic Env
makeQuery query database = do
  env <- (flip execStateT) Map.empty $ interpret (populateEnv query) database 0 query
  return $ Map.filterWithKey (\k _ -> isVar k) env

-- | Interpret an ILP program with a given query and return the first result
-- The query is given as a Check
-- TODO: foo(a) should return true or false
solve :: Body -> Database -> [(Variable, Variable)]
solve q d = Map.toList $ observe $ makeQuery q d

-- | Same as solve, but returns all results
solveAll :: Body -> Database -> [[(Variable, Variable)]]
solveAll q d = map Map.toList $ observeAll $ makeQuery q d
