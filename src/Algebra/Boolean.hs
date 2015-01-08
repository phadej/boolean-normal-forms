{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Oleg Grenrus 2014
-- License   :  MIT
-- Maintainer:  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Algebra.Boolean (
  Boolean(..),
  all,
  any,
  and,
  or,
  implication,
  -- * Free structure
  FreeBoolean(..),
  liftBoolean,
  lowerBoolean,
  -- * Module re-exports
  module Algebra.Heyting,
  module Algebra.Negable
  ) where

import GHC.Generics
import Algebra.Heyting
import Algebra.Negable
import Data.Foldable hiding (all, any, and, or)
import Prelude hiding ((&&), (||), not, all, any, and, or)

class (Heyting b, Negable b) => Boolean b where
  true :: b
  true = top

  false :: b
  false = bottom

  (&&) :: b -> b -> b
  (&&) = (/\)

  (||) :: b -> b -> b
  (||) = (\/)

  -- Not comes from Negable

instance Boolean Bool where
instance (Boolean a, Boolean b) => Boolean (a, b) where

-- | `meets . map f`.
all :: (Boolean b, Foldable f) => (a -> b) -> f a -> b
all f = getMeet . foldMap (Meet . f)

-- | `joins . map f`.
any :: (Boolean b, Foldable f) => (a -> b) -> f a -> b
any f = getJoin . foldMap (Join . f)

-- | Alias for `meets`.
and :: (Boolean a, Foldable f) => f a -> a
and = meets

-- | Alias for `joins`.
or :: (Boolean a, Foldable f) => f a -> a
or = joins

-- | Implication.
implication :: Boolean b => b -> b -> b
implication a b = not a || b

-- | Free `Boolean`.
data FreeBoolean a = FBValue a
                   | FBTrue
                   | FBFalse
                   | FBNot (FreeBoolean a)
                   | FBAnd (FreeBoolean a) (FreeBoolean a)
                   | FBOr (FreeBoolean a) (FreeBoolean a)
  deriving (Eq, Ord, Show, Read, Functor, Generic, Generic1)

instance JoinSemiLattice (FreeBoolean a) where
  join = FBOr

instance MeetSemiLattice (FreeBoolean a) where
  meet = FBAnd

instance BoundedJoinSemiLattice (FreeBoolean a) where
  bottom = FBFalse

instance BoundedMeetSemiLattice (FreeBoolean a) where
  top = FBTrue

instance Lattice (FreeBoolean a) where
instance BoundedLattice (FreeBoolean a) where

instance Heyting (FreeBoolean a) where
  (~>) = implication
  negation = not

instance Negable (FreeBoolean a) where
  not = FBNot

instance Boolean (FreeBoolean a) where

liftBoolean :: a -> FreeBoolean a
liftBoolean = FBValue

lowerBoolean :: Boolean a => FreeBoolean a -> a
lowerBoolean (FBValue x) = x
lowerBoolean FBTrue = true
lowerBoolean FBFalse = false
lowerBoolean (FBNot x) = not (lowerBoolean x)
lowerBoolean (FBAnd x y) = lowerBoolean x && lowerBoolean y
lowerBoolean (FBOr x y) = lowerBoolean x || lowerBoolean y