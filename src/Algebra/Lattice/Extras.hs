{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
  module Algebra.Lattice
) where

import Algebra.Lattice
import Data.Monoid
import GHC.Generics

infixr 2 \/
infixr 3 /\

-- | Infix version of `join`
(\/) :: JoinSemiLattice a => a -> a -> a
(\/) = join

-- | Infix version of `meet`
(/\) :: MeetSemiLattice a => a -> a ->a
(/\) = meet

-- | Monoid wrapper for JoinSemiLattice
newtype Join a = Join { getJoin :: a }
  deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1)

instance BoundedJoinSemiLattice a => Monoid (Join a) where
  mempty = Join bottom
  Join a `mappend` Join b = Join (a \/ b)

-- | Monoid wrapper for MeetSemiLattice
newtype Meet a = Meet { getMeet :: a }
  deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1)

instance BoundedMeetSemiLattice a => Monoid (Meet a) where
  mempty = Meet top
  Meet a `mappend` Meet b = Meet (a /\ b)

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