{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

import ClassyPrelude
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable hiding (sequence_, forM_)

-- for equivalence relations
import Data.Equivalence.Monad
import Control.Monad.ST.Trans

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
--                  v-- this 's' makes sure two BacktrackEquiv monads never mix
type BacktrackEquiv s = LogicT (EquivM s Rep Variable)

clause1 = Clause "happy" [Var "X"] (Check "eats" [Var "X"])
clause4 = Clause "happy" [Var "X"] (Unify (Var "X") (Atom "phil"))
clause2 = Clause "eats" [Atom "phil"] LTrue
clause3 = Clause "eats" [Atom "joel"] LTrue
database :: Database
database = Map.fromList [("happy", [clause1, clause4]), ("eats", [clause2, clause3])]

addToDatabase :: Clause -> Database -> Database
addToDatabase clause@(Clause sym _ _) = insertWith (++) sym [clause]

-- | union two variable representations
unionRep a b = case (a, b) of
                 (Placeholder a, Placeholder b) -> Placeholder $ Set.union a b
                 (Concrete a   , Placeholder b) -> Concrete a
                 (Placeholder a, Concrete b   ) -> Concrete b
                 (Concrete _   , Concrete _   ) -> undefined -- TODO: should this fail instead
-- | create a Rep from a Variable
toRep v@(Var _ ) = Placeholder $ Set.singleton v
toRep a@(Atom _) = Concrete a

-- | lookup a symbol in the relation database
-- will return a list of matching clauses
lookupSymbol :: Database -> Symbol -> BacktrackEquiv s [Clause]
lookupSymbol d x = case lookup x d of
                     Just a  -> return a
                     Nothing -> mzero

-- | unify two values, must be lifted into the outermost monad
-- TODO: forbit unioning two values
unify :: Variable -> Variable -> BacktrackEquiv s ()
unify a b = do
  descA <- lift $ classDesc a
  descB <- lift $ classDesc b
  case (descA, descB) of
    (Concrete c1, Concrete c2) | c1 /= c2 -> do
      traceShowM (a, b, c1, c2)
      mzero -- fail, can't union two concrete variables
    _ -> lift $ equate a b

-- | A helper to run the equivalence monad with our representation
run :: (forall s. EquivM s Rep Variable a) -> a
run = runEquivM toRep unionRep

interpret :: Database -> Body -> BacktrackEquiv s ()
interpret database (And b1 b2)          = (interpret database b1) >>- (const $ interpret database b2)
interpret database (Or b1 b2)           = interpret database b1 `interleave` interpret database b2
interpret database (Check sym vars)     = do
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
interpret database (Unify var1 var2)    = unify var1 var2
interpret database LTrue                = return ()
interpret database LFalse               = mzero
interpret database (Extend fact clause) = interpret (addToDatabase fact database) clause

solve :: Body -> Database -> [(Variable, Rep)]
solve query@(Check _ variables) database = run $ do
  observeT $ do
    interpret database query
    mapM (\x -> lift $ classDesc x >>= return . (,) x) variables

solveAll :: Body -> Database -> [[(Variable, Rep)]]
solveAll query@(Check _ variables) database = run $ do
  observeAllT $ do
    interpret database query
    mapM (\x -> lift $ classDesc x >>= return . (,) x) variables

test :: BacktrackEquiv s Bool
test = m1 `interleave` m2
  where
    m1 = do
      unify (Var "X") (Atom "a")
      mzero
    m2 = do
      unify (Var "X") (Atom "b")
      return True

main = return ()
