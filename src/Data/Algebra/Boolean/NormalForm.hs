{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Oleg Grenrus 2014
-- License   :  MIT
-- Maintainer:  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Algebra.Boolean.NormalForm (
  NormalForm(..),
  module Data.Algebra.Boolean.CoBoolean
  ) where

import Data.Algebra.Boolean.CoBoolean
import Data.Algebra.Boolean.FreeBoolean

import GHC.Exts (Constraint)

-- | Class unifying different boolean normal forms.
class CoBoolean1 nf => NormalForm nf where
  -- | 'NormalForm' could be constrained, so the 'Set' based implementations could be included.
  type NFConstraint nf a :: Constraint
  type NFConstraint nf a = ()

  -- | Lift a value into normal form.
  toNormalForm :: a -> nf a

  -- | Simplify the formula, if some terms are ⊥ or ⊤.
  simplify :: (NFConstraint nf a) => (a -> Maybe Bool) -> nf a -> nf a

  -- | transform from free boolean form
  fromFreeBoolean :: (NFConstraint nf a) => FreeBoolean a -> nf a

instance NormalForm FreeBoolean where
  toNormalForm     = FBValue
  simplify _       = id
  fromFreeBoolean  = id
