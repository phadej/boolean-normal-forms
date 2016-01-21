{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.SetLike
-- Copyright   : (c) 2014-2015 Oleg Grenrus
-- License     : MIT
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Data.SetLike where

import           Control.Applicative
import           Data.Foldable
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Exts (Constraint)

-- | This class provides abstraction over small set of set operations.
class Foldable s => SetLike s where
  type SetLikeC s a :: Constraint
  type SetLikeC s a = ()

  empty     :: s a
  null      :: s a -> Bool
  singleton :: a -> s a
  union     :: SetLikeC s a => s a -> s a -> s a
  liftSet   :: SetLikeC s b => (a -> b) -> s a -> s b
  liftSet2  :: SetLikeC s c => (a -> b -> c) -> s a -> s b -> s c

instance SetLike [] where
  empty = []
  singleton x = [x]
  union = (++)
  null = Prelude.null
  liftSet = Prelude.map
  liftSet2 = liftA2

instance SetLike Set where
  type SetLikeC Set a = Ord a

  empty = Set.empty
  singleton = Set.singleton
  union = Set.union
  null = Set.null
  liftSet = Set.map
  liftSet2 f a b = Set.fromList $ liftA2 f (Set.toList a) (Set.toList b)

type SetLike' s a = (SetLike s, SetLikeC s a)
