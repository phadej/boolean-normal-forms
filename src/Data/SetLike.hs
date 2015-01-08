{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
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

import GHC.Exts (Constraint)
import Control.Applicative
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set

-- | This class provides abstraction over small set of set operations.
class Foldable s => SetLike s where
  type SetLikeC s a :: Constraint
  type SetLikeC s a = ()

  empty :: s a
  singleton :: a -> s a
  union :: SetLikeC s a => s a -> s a -> s a
  null :: s a -> Bool
  endoMap :: (SetLikeC s b) => (a -> b) -> s a -> s b
  endoMap2 :: (SetLikeC s c) => (a -> b -> c) -> s a -> s b -> s c

instance SetLike [] where
  empty = []
  singleton x = [x]
  union = (++)
  null = Prelude.null
  endoMap = Prelude.map
  endoMap2 = liftA2

instance SetLike Set where
  type SetLikeC Set a = Ord a

  empty = Set.empty
  singleton = Set.singleton
  union = Set.union
  null = Set.null
  endoMap = Set.map
  endoMap2 f a b = Set.fromList $ liftA2 f (Set.toList a) (Set.toList b)
