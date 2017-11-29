{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
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
module Data.Algebra.Boolean.FreeBoolean (
  FreeBoolean(..),
  module Data.Algebra.Boolean.CoBoolean
) where

import Data.Typeable (Typeable)
import Data.Foldable (Foldable)
import Control.DeepSeq (NFData(rnf))

import Data.Algebra.Boolean.CoBoolean
import Data.Algebra.Boolean.Negable hiding (not)
import qualified Data.Algebra.Boolean.Negable as Negable

import Prelude hiding ((||),(&&),not,any,all)
import Data.Algebra.Boolean

-- | Free 'Boolean' type, does not perform any optimizations on the structure. Useful only in tests.
--
-- Consider using 'Data.Algebra.Boolean.NNF'.
data FreeBoolean a = FBValue a
                   | FBTrue
                   | FBFalse
                   | FBNot (FreeBoolean a)
                   | FBAnd (FreeBoolean a) (FreeBoolean a)
                   | FBOr (FreeBoolean a) (FreeBoolean a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Typeable)

instance CoBoolean1 FreeBoolean where
  toBooleanWith f (FBValue x)  = f x
  toBooleanWith _ FBTrue       = true
  toBooleanWith _ FBFalse      = false
  toBooleanWith f (FBNot x)    = not $ toBooleanWith f x
  toBooleanWith f (FBOr a b)   = toBooleanWith f a || toBooleanWith f b
  toBooleanWith f (FBAnd a b)  = toBooleanWith f a && toBooleanWith f b

instance CoBoolean a => CoBoolean (FreeBoolean a) where
  toBoolean = toBooleanWith toBoolean

instance Negable (FreeBoolean a) where
  not = FBNot

instance Boolean (FreeBoolean a) where
  true   = FBTrue
  false  = FBFalse
  (||)   = FBOr
  (&&)   = FBAnd
  not    = FBNot

instance NFData a => NFData (FreeBoolean a) where
  rnf (FBValue a) = rnf a
  rnf (FBNot a)   = rnf a
  rnf (FBAnd a b) = rnf a `seq` rnf b
  rnf (FBOr a b)  = rnf a `seq` rnf b
  rnf FBTrue      = ()
  rnf FBFalse     = ()
