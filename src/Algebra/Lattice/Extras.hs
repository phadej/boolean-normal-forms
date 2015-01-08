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
  -- * Infix operators
  (\/),
  (/\),
  -- * Monoid wrappers for lattices
  Join(..),
  Meet(..),
  -- * Ord lattice
  MinMax(..),
  -- * Foldable joins and meets
  joins,
  meets,
  -- * Extra functions
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

import Algebra.Lattice hiding (joins, meets)
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

infixr 2 \/
infixr 3 /\

-- | Infix version of `join`
(\/) :: JoinSemiLattice a => a -> a -> a
(\/) = join

-- | Infix version of `meet`
(/\) :: MeetSemiLattice a => a -> a ->a
(/\) = meet

-- | Monoid wrapper for MeetSemiLattice
newtype Meet a = Meet { getMeet :: a }
  deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1)

instance MeetSemiLattice a => Semigroup (Meet a) where
  Meet a <> Meet b = Meet (a /\ b)

instance BoundedMeetSemiLattice a => Monoid (Meet a) where
  mempty = Meet top
  Meet a `mappend` Meet b = Meet (a /\ b)

instance Functor Meet where
  fmap f (Meet x) = Meet (f x)

instance Foldable Meet where
  foldMap f (Meet a) = f a

instance Traversable Meet where
  traverse f (Meet a) = Meet <$> f a

instance Applicative Meet where
  pure = Meet
  a <* _ = a
  _ *> a = a
  Meet f <*> Meet x = Meet (f x)

instance Monad Meet where
  return = Meet
  _ >> a = a
  Meet a >>= f = f a

instance MonadFix Meet where
  mfix f = fix (f . getMeet)

instance NFData a => NFData (Meet a) where
  rnf (Meet a) = rnf a

-- | Monoid wrapper for JoinSemiLattice
newtype Join a = Join { getJoin :: a }
  deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1)

instance JoinSemiLattice a => Semigroup (Join a) where
  Join a <> Join b = Join (a \/ b)

instance BoundedJoinSemiLattice a => Monoid (Join a) where
  mempty = Join bottom
  Join a `mappend` Join b = Join (a \/ b)

instance Functor Join where
  fmap f (Join x) = Join (f x)

instance Foldable Join where
  foldMap f (Join a) = f a

instance Traversable Join where
  traverse f (Join a) = Join <$> f a

instance Applicative Join where
  pure = Join
  a <* _ = a
  _ *> a = a
  Join f <*> Join x = Join (f x)

instance Monad Join where
  return = Join
  _ >> a = a
  Join a >>= f = f a

instance MonadFix Join where
  mfix f = fix (f . getJoin)

instance NFData a => NFData (Join a) where
  rnf (Join a) = rnf a

-- All
instance JoinSemiLattice All where
  All a `join` All b = All $ a `join` b

instance BoundedJoinSemiLattice All where
  bottom = All False

instance MeetSemiLattice All where
  All a `meet` All b = All $ a `meet` b

instance BoundedMeetSemiLattice All where
  top = All True

instance Lattice All where
instance BoundedLattice All where

-- Any
instance JoinSemiLattice Any where
  Any a `join` Any b = Any $ a `join` b

instance BoundedJoinSemiLattice Any where
  bottom = Any False

instance MeetSemiLattice Any where
  Any a `meet` Any b = Any $ a `meet` b

instance BoundedMeetSemiLattice Any where
  top = Any True

instance Lattice Any where
instance BoundedLattice Any where

-- Endo
instance JoinSemiLattice a => JoinSemiLattice (Endo a) where
  Endo a `join` Endo b = Endo $ \x -> a x `join` b x

instance BoundedJoinSemiLattice a => BoundedJoinSemiLattice (Endo a) where
  bottom = Endo $ \_ -> bottom

instance MeetSemiLattice a => MeetSemiLattice (Endo a) where
  Endo a `meet` Endo b = Endo $ \x -> a x `meet` b x

instance BoundedMeetSemiLattice a => BoundedMeetSemiLattice (Endo a) where
  top = Endo $ \_ -> top

instance Lattice a => Lattice (Endo a) where
instance BoundedLattice a => BoundedLattice (Endo a) where

-- Const
instance JoinSemiLattice a => JoinSemiLattice (Const a b) where
  Const a `join` Const b = Const $ a `join` b

instance BoundedJoinSemiLattice a => BoundedJoinSemiLattice (Const a b) where
  bottom = Const bottom

instance MeetSemiLattice a => MeetSemiLattice (Const a b) where
  Const a `meet` Const b = Const $ a `meet` b

instance BoundedMeetSemiLattice a => BoundedMeetSemiLattice (Const a b) where
  top = Const top

instance Lattice a => Lattice (Const a b) where
instance BoundedLattice a => BoundedLattice (Const a b) where

-- Min
instance Ord a => JoinSemiLattice (Min a) where
  join = (<>)

instance (Ord a, Bounded a) => BoundedJoinSemiLattice (Min a) where
  bottom = minBound

-- Max
instance Ord a => MeetSemiLattice (Max a) where
  meet = (<>)

instance (Ord a, Bounded a) => BoundedMeetSemiLattice (Max a) where
  top = maxBound

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

-- | The join of a `Foldable` of join-semilattice elements
joins :: (BoundedJoinSemiLattice a, Foldable f) => f a -> a
joins = getJoin . foldMap Join

-- | The meet of a `Foldable` of meet-semilattice elements
meets :: (BoundedMeetSemiLattice a, Foldable f) => f a -> a
meets = getMeet . foldMap Meet

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

instance Functor Levitated where
  fmap _ Levitated.Top = Levitated.Top
  fmap _ Levitated.Bottom = Levitated.Bottom 
  fmap f (Levitated.Levitate a) = Levitated.Levitate (f a)

instance Show a => Show (Levitated a) where
  show Levitated.Top = "Top"
  show Levitated.Bottom = "Bottom"
  show (Levitated.Levitate a) = "Levitate " ++ show a

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
