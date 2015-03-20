-- A Very slow union-find implementation
module UnionFind where

import Data.Map as M
import Control.Monad.Trans.State.Lazy

-- | Find the representative element of a set
repr' :: Ord a => a -> Map a a -> Maybe a
repr' r d = case M.lookup r d of
             Just a | a == r -> Just r
             Just a          -> repr' a d
             Nothing         -> Nothing -- shouldn't happen

type UnionFind a = StateT (Map a a)

-- | Find the representation element of a set, insert the element if it does not exist
repr :: (Ord b, Monad m) => b -> UnionFind b m b
repr x = do
  d <- get
  case repr' x d of
    Just a  -> return a
    Nothing -> modify (insert x x) >> return x

-- | equate unions the equivalence classes of two elements
-- the representation is taken from the second
equate :: (Ord t, Monad m) => t -> t -> UnionFind t m ()
equate a b = do
  ra <- repr a
  rb <- repr b
  modify (adjust (const rb) ra)
