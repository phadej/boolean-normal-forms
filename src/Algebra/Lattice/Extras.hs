{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -fno-warn-orphans #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Oleg Grenrus 2014
-- License   :  MIT
-- Maintainer:  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Algebra.Lattice.Extras (
  -- *
  MinMax,
  -- * Infix operators
  fromBool,
  liftLevitated,
  lowerLevitated,
  -- * Free structures
  FreeLattice(..),
  liftFreeLattice,
  lowerFreeLattice,
  FreeBoundedLattice,
  FreeDropped,
  FreeLifted,
  -- * Module re-exports
  module Algebra.Lattice
) where

import Algebra.Lattice
import Control.Applicative
import Control.Monad.Fix
import Control.DeepSeq
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Data.Foldable
import Data.Traversable
import GHC.Generics
import Test.QuickCheck

import Algebra.Lattice.Dropped (Dropped)
import Algebra.Lattice.Lifted (Lifted)

import Algebra.Lattice.Levitated (Levitated)
import qualified Algebra.Lattice.Levitated as Levitated

-- MinMax
newtype MinMax a = MinMax { getMinMax :: a }
  deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1)

instance Ord a => JoinSemiLattice (MinMax a) where
  MinMax a `join` MinMax b = MinMax (a `min` b)

instance (Ord a, Bounded a) => BoundedJoinSemiLattice (MinMax a) where
  bottom = MinMax minBound

instance Ord a => MeetSemiLattice (MinMax a) where
  MinMax a `meet` MinMax b = MinMax (a `max` b)

instance (Ord a, Bounded a) => BoundedMeetSemiLattice (MinMax a) where
  top = MinMax maxBound

instance Functor MinMax where
  fmap f (MinMax x) = MinMax (f x)

instance Foldable MinMax where
  foldMap f (MinMax a) = f a

instance Traversable MinMax where
  traverse f (MinMax a) = MinMax <$> f a

instance Applicative MinMax where
  pure = MinMax
  a <* _ = a
  _ *> a = a
  MinMax f <*> MinMax x = MinMax (f x)

instance Monad MinMax where
  return = MinMax
  _ >> a = a
  MinMax a >>= f = f a

instance MonadFix MinMax where
  mfix f = fix (f . getMinMax)

instance NFData a => NFData (MinMax a) where
  rnf (MinMax a) = rnf a

-- | Lift `Bool`
fromBool :: BoundedLattice b => Bool -> b
fromBool True  = top
fromBool False = bottom

liftLevitated :: a -> Levitated a
liftLevitated = Levitated.Levitate

lowerLevitated :: BoundedLattice a => Levitated a -> a
lowerLevitated Levitated.Top          = top
lowerLevitated Levitated.Bottom       = bottom
lowerLevitated (Levitated.Levitate a) = a

-- | Free lattice. Forms a binary tree.
data FreeLattice a = FreeValue a
                   | FreeMeet (FreeLattice a) (FreeLattice a)
                   | FreeJoin (FreeLattice a) (FreeLattice a)
  deriving (Eq, Ord, Show, Read, Functor)

instance JoinSemiLattice (FreeLattice a) where
  join = FreeJoin

instance MeetSemiLattice (FreeLattice a) where
  meet = FreeMeet

instance Lattice (FreeLattice a) where

type FreeDropped a = Dropped (FreeLattice a)
type FreeLifted a = Lifted (FreeLattice a)

instance Arbitrary a => Arbitrary (FreeLattice a) where
  arbitrary = sized arbitrary'
    where arbitrary' 0 = FreeValue <$> arbitrary
          arbitrary' n = oneof [
            FreeValue <$> arbitrary,
            FreeMeet <$> arbitrary'' <*> arbitrary'',
            FreeJoin <$> arbitrary'' <*> arbitrary''
            ]
            where arbitrary'' = arbitrary' $ n `div` 2

-- | Free bounded lattice.
type FreeBoundedLattice a = Levitated (FreeLattice a)

liftFreeLattice :: a -> FreeLattice a
liftFreeLattice = FreeValue

lowerFreeLattice :: Lattice a => FreeLattice a -> a
lowerFreeLattice (FreeValue a)   = a
lowerFreeLattice (FreeMeet a b)  = lowerFreeLattice a /\ lowerFreeLattice b
lowerFreeLattice (FreeJoin a b)  = lowerFreeLattice a \/ lowerFreeLattice b
