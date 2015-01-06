{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Oleg Grenrus 2014
-- License   :  MIT
-- Maintainer:  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.SetLike where

import Control.Applicative
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set

-- | This class provides abstraction over small set of set operations.
class Foldable s => SetLike s a where
  empty :: s a
  singleton :: a -> s a
  union :: s a -> s a -> s a
  null :: s a -> Bool
  endoMap :: (a -> a) -> s a -> s a
  endoMap2 :: (a -> a -> a) -> s a -> s a -> s a

instance SetLike [] a where
  empty = []
  singleton x = [x]
  union = (++)
  null = Prelude.null
  endoMap = Prelude.map
  endoMap2 = liftA2

instance Ord a => SetLike Set a where
  empty = Set.empty
  singleton = Set.singleton
  union = Set.union
  null = Set.null
  endoMap = Set.map
  endoMap2 f a b = Set.fromList $ liftA2 f (Set.toList a) (Set.toList b)
