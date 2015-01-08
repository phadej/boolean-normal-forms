{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Oleg Grenrus 2014
-- License   :  MIT
-- Maintainer:  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Algebra.Negable (
  Negable(..),
  Neg(..),
  liftNeg,
  lowerNeg
)
  where

import Data.Monoid
import Data.Typeable
import Algebra.Lattice.Extras

import Algebra.Lattice.Levitated

import Prelude hiding (not)
import qualified Prelude as P

-- | Class to represent invertible values.
--
-- Must obey the double negation law: @not (not x) = x@
--
-- The negation appears only in leafs of normal forms, and the underlying proposition class
-- might have built-in negation, thus we use 'Negable'.
class Negable x where
  -- | Negate the value.
  not :: x -> x

instance Negable (Neg a) where
  not (Pos x)  = Neg x
  not (Neg x)  = Pos x

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
  deriving (Eq, Ord, Show, Read, Functor, Typeable)

liftNeg :: a -> Neg a
liftNeg = Pos

lowerNeg :: Negable a => Neg a -> a
lowerNeg (Pos a) = a
lowerNeg (Neg a) = not a

-- Lattices

instance Negable a => Negable (FreeLattice a) where
  not (FreeValue a)   = FreeValue a
  not (FreeMeet a b)  = FreeJoin (not a) (not b)
  not (FreeJoin a b)  = FreeMeet (not a) (not b)

instance Negable a => Negable (Levitated a) where
  not (Top)         = Bottom
  not (Bottom)      = Top
  not (Levitate a)  = Levitate (not a)
