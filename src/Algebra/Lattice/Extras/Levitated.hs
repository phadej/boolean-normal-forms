{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Oleg Grenrus 2014
-- License   :  MIT
-- Maintainer:  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability :  experimental
-- Portability: non-portable
--
-- Fix until https://github.com/batterseapower/lattices/pull/3 is merged.
--
--------------------------------------------------------------------
module Algebra.Lattice.Extras.Levitated (
    Levitated(..),
    liftLevitated,
    lowerLevitated,
    FreeBoundedLattice,
  ) where

import GHC.Generics
import Algebra.Lattice
import Algebra.Lattice.Extras (FreeLattice)

--
-- Levitated
--

-- | Graft a distinct top and bottom onto an otherwise unbounded lattice.
-- The top is the absorbing element for the join, and the bottom is the absorbing
-- element for the meet.
data Levitated a = Bottom
                 | Levitate a
                 | Top
  deriving (Eq, Ord, Read, Show, Functor, Generic, Generic1)


instance JoinSemiLattice a => JoinSemiLattice (Levitated a) where
    Top        `join` _          = Top
    _          `join` Top        = Top
    Levitate x `join` Levitate y = Levitate (x `join` y)
    Bottom     `join` lev_y      = lev_y
    lev_x      `join` Bottom     = lev_x

instance MeetSemiLattice a => MeetSemiLattice (Levitated a) where
    Top        `meet` lev_y      = lev_y
    lev_x      `meet` Top        = lev_x
    Levitate x `meet` Levitate y = Levitate (x `meet` y)
    Bottom     `meet` _          = Bottom
    _          `meet` Bottom     = Bottom

instance Lattice a => Lattice (Levitated a) where

instance JoinSemiLattice a => BoundedJoinSemiLattice (Levitated a) where
    bottom = Bottom

instance MeetSemiLattice a => BoundedMeetSemiLattice (Levitated a) where
    top = Top

instance Lattice a => BoundedLattice (Levitated a) where

--
liftLevitated :: a -> Levitated a
liftLevitated = Levitate

lowerLevitated :: BoundedLattice a => Levitated a -> a
lowerLevitated Top          = top
lowerLevitated Bottom       = bottom
lowerLevitated (Levitate a) = a

-- | Free bounded lattice.
type FreeBoundedLattice a = Levitated (FreeLattice a)