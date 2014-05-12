{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
module Data.Algebra.Boolean.NNF.Tree (
  NNF(..),
  module Data.Algebra.Boolean.NormalForm
  ) where

import Prelude hiding ((||),(&&),not)

import Data.Algebra.Boolean.NormalForm
import Data.Algebra.Boolean.Negable hiding (not)
import qualified Data.Algebra.Boolean.Negable as Negable

import Data.Algebra.Boolean

import Data.Typeable (Typeable)
import Data.Foldable (Foldable)

-- | Boolean formula in Negation Normal Form
--
-- 'Boolean' operations will perform transformations as below:
--
-- <<doc-formulae/nnf.svg>>
data NNF a = NNFTrue
           | NNFFalse
           | NNFValue a
           | NNFOr (NNF a) (NNF a)
           | NNFAnd (NNF a) (NNF a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Typeable)

instance CoBoolean a => CoBoolean (NNF a) where
  toBoolean = toBooleanWith toBoolean

instance CoBoolean1 NNF where
  toBooleanWith _ NNFTrue       = true
  toBooleanWith _ NNFFalse      = false
  toBooleanWith f (NNFValue x)  = f x
  toBooleanWith f (NNFOr a b)   = toBooleanWith f a || toBooleanWith f b
  toBooleanWith f (NNFAnd a b)  = toBooleanWith f a && toBooleanWith f b

nnfNot :: Negable a => NNF a -> NNF a
nnfNot (NNFTrue)     = NNFFalse
nnfNot NNFFalse      = NNFTrue
nnfNot (NNFValue x)  = NNFValue $ Negable.not x
nnfNot (NNFOr a b)   = NNFAnd (not a) (not b)
nnfNot (NNFAnd a b)  = NNFOr (not a) (not b)

nnfOr :: NNF a -> NNF a -> NNF a
nnfOr  NNFTrue   _         = NNFTrue
nnfOr  _         NNFTrue   = NNFTrue
nnfOr  NNFFalse  NNFFalse  = NNFFalse
nnfOr  a         b         = NNFOr a b

nnfAnd :: NNF a -> NNF a -> NNF a
nnfAnd NNFFalse  _         = NNFFalse
nnfAnd _         NNFFalse  = NNFFalse
nnfAnd NNFTrue   NNFTrue   = NNFTrue
nnfAnd a         b         = NNFAnd a b

instance Negable a => Negable (NNF a) where
  not    = nnfNot

instance Negable a => Boolean (NNF a) where
  true   = NNFTrue
  false  = NNFFalse
  (||)   = nnfOr
  (&&)   = nnfAnd
  not    = nnfNot

instance NormalForm NNF where
  type NFConstraint NNF a  = Negable a

  toNormalForm             = NNFValue

  simplify f (NNFValue x)  = case f x of
                               Just True   -> NNFTrue
                               Just False  -> NNFFalse
                               Nothing     -> NNFValue x
  simplify _ NNFTrue       = NNFTrue
  simplify _ NNFFalse      = NNFFalse
  simplify f (NNFAnd a b)  = nnfAnd (simplify f a) (simplify f b)
  simplify f (NNFOr a b)   = nnfOr (simplify f a) (simplify f b)

  fromFreeBoolean          = toBooleanWith toNormalForm
