{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Safe #-}
-- |
-- Module      : Algebra.Boolean.FreeLattice
-- Copyright   : (c) 2014-2015 Oleg Grenrus
-- License     : MIT
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Algebra.Lattice.FreeLattice where

import Algebra.Lattice
import Algebra.Lattice.Dropped (Dropped)
import Algebra.Lattice.Lifted (Lifted)
import Algebra.Lattice.Levitated (Levitated)
import Test.QuickCheck

import Control.Applicative

-- | Free lattice. Forms a binary tree.
data FreeLattice a = FreeValue a
                   | FreeMeet (FreeLattice a) (FreeLattice a)
                   | FreeJoin (FreeLattice a) (FreeLattice a)
  deriving (Eq, Ord, Show, Read, Functor)
-- TODO: Applicative, Monad, Foldable, Traversable, Generic

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

retractFreeLattice :: Lattice a => FreeLattice a -> a
retractFreeLattice (FreeValue a)   = a
retractFreeLattice (FreeMeet a b)  = retractFreeLattice a /\ retractFreeLattice b
retractFreeLattice (FreeJoin a b)  = retractFreeLattice a \/ retractFreeLattice b
