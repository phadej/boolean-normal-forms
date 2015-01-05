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

import Data.Foldable

import Data.Set (Set)
import qualified Data.Set as Set

-- | This class provides abstraction over small set of set operations.
class Foldable s => SetLike s a where
  empty :: s a
  singleton :: a -> s a
  union :: s a -> s a -> s a
  null :: s a -> Bool

instance SetLike [] a where
  empty = []
  singleton x = [x]
  union = (++)
  null = Prelude.null

instance Ord a => SetLike Set a where
  empty = Set.empty
  singleton = Set.singleton
  union = Set.union
  null = Set.null