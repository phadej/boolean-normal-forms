{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}
-- |
-- Module      : Algebra.Negable
-- Copyright   : (c) 2014-2015 Oleg Grenrus
-- License     : MIT
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Algebra.Negable (
  Negable(..),
  Neg(..),
  liftNeg,
  retractNeg
)
  where

import           Algebra.Lattice.FreeLattice
import           Control.Monad
import           Data.Monoid
import           Data.Typeable
import           GHC.Generics (Generic, Generic1)

import Algebra.Lattice
import qualified Algebra.Lattice.Levitated as L

import           Prelude hiding (not)
import qualified Prelude as P
import           Control.Applicative
import           Test.QuickCheck

import           Data.Foldable (Foldable)
import           Data.Traversable (Traversable)

-- | Class to represent invertible values.
--
-- Must obey the double negation law: @not (not x) = x@
--
-- The negation appears only in leafs of normal forms, and the underlying proposition class
-- might have built-in negation, thus we use 'Negable'.
class Negable x where
  -- | Negate the value.
  not :: x -> x

instance Negable Bool where
  not = P.not

instance Negable All where
  not (All b) = All (not b)

instance Negable Any where
  not (Any b) = Any (not b)

instance Negable n => Negable (Endo n) where
  not (Endo f) = Endo $ not . f

instance (Negable a, Negable b) => Negable (a, b) where
  not (x, y) = (not x, not y)

instance (Negable a, Negable b) => Negable (Either a b) where
  not (Left x)  = Left $ not x
  not (Right y) = Right $ not y

-- | Free 'Negable'.
data Neg a = Pos a -- ^ Positive value
           | Neg a -- ^ Negative value
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Typeable, Generic, Generic1)

instance Negable (Neg a) where
  not (Pos x)  = Neg x
  not (Neg x)  = Pos x

instance Applicative Neg where
  pure = return
  (<*>) = ap

instance Monad Neg where
  return = Pos
  x >>= f = retractNeg (fmap f x)

instance (Lattice a, Negable a) => JoinSemiLattice (Neg a) where
 Pos a \/ Pos b = Pos (a \/ b)
 Neg a \/ Neg b = Neg (a /\ b)
 Pos a \/ Neg b = Pos (a \/ not b)
 Neg a \/ Pos b = Pos (not a \/ b)

instance (Lattice a, Negable a) => MeetSemiLattice (Neg a) where
 Pos a /\ Pos b = Pos (a /\ b)
 Neg a /\ Neg b = Neg (a \/ b)
 Pos a /\ Neg b = Pos (a /\ not b)
 Neg a /\ Pos b = Pos (not a /\ b)
 
instance (BoundedLattice a, Negable a) => BoundedJoinSemiLattice (Neg a) where
  bottom = Pos bottom
instance (BoundedLattice a, Negable a) => BoundedMeetSemiLattice (Neg a) where                  
  top = Pos top

instance (Lattice a, Negable a) => Lattice (Neg a)
instance (BoundedLattice a, Negable a) => BoundedLattice (Neg a)

liftNeg :: a -> Neg a
liftNeg = Pos

-- | Interpret 'Neg a' using the 'Negable' of 'a'.
--
-- It's also monadic 'join'
retractNeg :: Negable a => Neg a -> a
retractNeg (Pos a) = a
retractNeg (Neg a) = not a

-- Lattices

instance Negable a => Negable (FreeLattice a) where
  not (FreeValue a)   = FreeValue (not a)
  not (FreeMeet a b)  = FreeJoin (not a) (not b)
  not (FreeJoin a b)  = FreeMeet (not a) (not b)

instance Negable a => Negable (L.Levitated a) where
  not (L.Top)         = L.Bottom
  not (L.Bottom)      = L.Top
  not (L.Levitate a)  = L.Levitate (not a)

instance Arbitrary a => Arbitrary (Neg a) where
  arbitrary = f <$> arbitrary <*> arbitrary
    where f True  = Pos
          f False = Neg
