{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

import ClassyPrelude hiding (union)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable hiding (sequence_, forM_)

-- for equivalence relations
-- import Data.Equivalence.Monad
-- import Control.Monad.ST.Trans
import UnionFind
import Control.Monad.State.Lazy (State, evalState, get, put)

import Control.Monad.Logic hiding (sequence_, forM_, mapM, forM)

-- a variable can either be "literal" or a variable
data Variable = Var Text
              | Atom Text
              deriving (Show, Eq, Ord)

-- representations of variables for the equivalence relation
data Rep = Placeholder (Set Variable) -- any number of Vars can be in a group
         | Concrete Variable          -- only one Atom can represent a group
         deriving (Show, Eq, Ord)

type Symbol = Text
data Clause = Clause Symbol [Variable] Body
data Body = And Body Body
          | Or Body Body
          | Check Symbol [Variable]
          | Unify Variable Variable
          | LTrue
          | LFalse
          | Extend Clause Body -- ILP extend with clause

data Val = VInt Int

type Database = Map Symbol [Clause]

-- | A backtracking equivalence monad
-- The equivalence is on the inside so that backtracking can change it
type BacktrackEquiv = LogicT (UnionFind Variable)

clause1 = Clause "happy" [Var "X"] (Check "eats" [Var "X"])
clause4 = Clause "happy" [Var "X"] (Unify (Var "X") (Atom "phil"))
clause2 = Clause "eats" [Atom "phil"] LTrue
clause3 = Clause "eats" [Atom "joel"] LTrue
database :: Database
database = Map.fromList [("happy", [clause1, clause4]), ("eats", [clause2, clause3])]

addToDatabase :: Clause -> Database -> Database
addToDatabase clause@(Clause sym _ _) = insertWith (++) sym [clause]

{-
-- | union two variable representations
unionRep a b = case (a, b) of
                 (Placeholder a, Placeholder b) -> Placeholder $ Set.union a b
                 (Concrete a   , Placeholder b) -> Concrete a
                 (Placeholder a, Concrete b   ) -> Concrete b
                 (Concrete _   , Concrete _   ) -> undefined -- TODO: should this fail instead
-- | create a Rep from a Variable
toRep v@(Var _ ) = Placeholder $ Set.singleton v
toRep a@(Atom _) = Concrete a
-}

-- | lookup a symbol in the relation database
-- will return a list of matching clauses
lookupSymbol :: Database -> Symbol -> BacktrackEquiv [Clause]
lookupSymbol d x = case lookup x d of
                     Just a  -> return a
                     Nothing -> mzero

-- | unify two values, must be lifted into the outermost monad
-- TODO: forbit unioning two values
unify :: Variable -> Variable -> BacktrackEquiv ()
unify a b = do
  descA <- lift $ repr a
  descB <- lift $ repr b
  case (descA, descB) of
    (Atom a1, Atom a2) | a1 /= a2 -> do
      mzero -- fail, can't union two Atoms variables
    (Atom _, Atom _) -> return () -- we have the same class
    (Atom a, Var v) -> lift $ equate (Var v) (Atom a) -- second arguement is the representative
    (Var v, Atom a) -> lift $ equate (Var v) (Atom a)
    (Var v1, Var v2) -> lift $ equate (Var v1) (Var v2)

{-
-- | A helper to run the equivalence monad with our representation
run :: (forall s. EquivM s Rep Variable a) -> a
run = runEquivM toRep unionRep
-}

-- | interpret an ilp mini-prolog program
-- a state monad cannot be used because logict backtracks incorrectly with State
interpret :: Equivalence Variable -> Database -> Body -> BacktrackEquiv ()
interpret equiv database (And b1 b2)          = (interpret equiv database b1) >>- (const $ interpret equiv database b2)
interpret equiv database (Or b1 b2)           = interpret equiv database b1 `interleave` interpret equiv database b2
interpret equiv database (Check sym vars)     = do
  clauses <- lookupSymbol database sym
  foldr1 interleave $ -- try each clause independently TODO: foldr or foldl
    (flip map) clauses -- TODO: 'flip map' should really be 'for', but it doesn't work
      (\(Clause sym' vars' body) -> do
        traceShowM (sym', vars')
        -- unify variables with arguements
        zipWithM_ unify vars vars'
        -- check the body
        interpret database body
      )
interpret equiv database (Unify var1 var2)    = unify var1 var2
interpret equiv database LTrue                = return ()
interpret equiv database LFalse               = mzero
interpret equiv database (Extend fact clause) = interpret equiv (addToDatabase fact database) clause

solve :: Body -> Database -> [(Variable, Variable)]
solve query@(Check _ variables) database = (flip evalState) Map.empty $ do
  observeT $ do
    interpret database query
    mapM (\x -> lift $ repr x >>= return . (,) x) variables

solveAll :: Body -> Database -> [[(Variable, Variable)]]
solveAll query@(Check _ variables) database = (flip evalState) Map.empty $ do
  observeAllT $ do
    interpret database query
    mapM (\x -> lift $ repr x >>= return . (,) x) variables

test :: BacktrackEquiv Bool
test = m1 `interleave` m2
  where
    m1 = do
      unify (Var "X") (Atom "a")
      mzero
    m2 = do
      lift $ get >>= traceShowM
      unify (Var "X") (Atom "b")
      return True

test2 :: LogicT (State Bool) Bool
test2 = m1 `interleave` m2
  where
    m1 = do
      lift $ put True
      mzero
    m2 = do
      s <- get
      return s

main = return ()
