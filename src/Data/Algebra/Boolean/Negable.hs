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
module Data.Algebra.Boolean.Negable (
  Neg(..),
  Negable(..)
)
  where

import Data.Algebra.Boolean.CoBoolean

import Data.Monoid
import Data.Typeable

import Prelude hiding (not)
import qualified Prelude as P

import qualified Data.Algebra.Boolean as B

-- | Free 'Negable'.
data Neg a = Pos a -- ^ Positive value
           | Neg a -- ^ Negative value
  deriving (Eq, Ord, Show, Read, Functor, Typeable)

instance CoBoolean (Neg a) where
  toBoolean (Pos _) = B.true
  toBoolean (Neg _) = B.false

instance CoBoolean1 Neg where
  toBooleanWith f (Pos x) = f x
  toBooleanWith f (Neg x) = B.not $ f x

-- | Class to represent invertible values.
--
-- Must obey the double negation law: @not (not x) = x@
--
-- The negation appears only in leafs of normal forms, and the underlying proposition class
-- might have built-in negation, thus we use 'Negable'.
class Negable x where
  -- | Invert the value.
  not :: x -> x

instance Negable (Neg a) where
  not (Pos x)  = Neg x
  not (Neg x)  = Pos x

instance Negable Bool where
  not = P.not

instance Monoid m => Negable (Maybe m) where
  not (Just _)  = Nothing
  not Nothing   = Just mempty

instance (Negable a, Negable b) => Negable (a, b) where
  not (x, y) = (not x, not y)

instance (Negable a, Negable b) => Negable (Either a b) where
  not (Left x)  = Left $ not x
  not (Right y) = Right $ not y
