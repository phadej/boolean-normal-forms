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
module Data.Algebra.Boolean.NNF.Set (
  NNF(..),
  module Data.Algebra.Boolean.NormalForm
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Algebra.Boolean.NormalForm
import Data.Algebra.Boolean.Negable hiding (not)
import qualified Data.Algebra.Boolean.Negable as Negable

import Prelude hiding ((||),(&&),not,any,all)
import Data.Algebra.Boolean

import Data.Typeable (Typeable)
import Data.Foldable (Foldable)
import Control.DeepSeq (NFData(rnf))

-- | Boolean formula in Negation Normal Form
--
-- 'Boolean' operations will perform transformations as below:
--
-- <<doc-formulae/nnf.svg>>
data NNF a = NNFTrue
           | NNFFalse
           | NNFValue a
           | NNFOr (Set (NNF a))
           | NNFAnd (Set (NNF a))
  deriving (Eq, Ord, Show, Read, Foldable, Typeable)

instance CoBoolean a => CoBoolean (NNF a) where
  toBoolean = toBooleanWith toBoolean

instance CoBoolean1 NNF where
  toBooleanWith _ NNFTrue       = true
  toBooleanWith _ NNFFalse      = false
  toBooleanWith f (NNFValue x)  = f x
  toBooleanWith f (NNFOr xs)    = any (toBooleanWith f) xs
  toBooleanWith f (NNFAnd xs)   = all (toBooleanWith f) xs

orList :: Ord a => [NNF a] -> NNF a
orList x
  | Set.null s             = NNFFalse
  | Set.member NNFTrue s   = NNFTrue
  | otherwise              = NNFOr s
  where s = Set.fromList $ filter (/= NNFFalse) $ concatMap g x
        g (NNFOr xs)   = concatMap g $ Set.toList xs
        g y            = [y]

andList :: Ord a => [NNF a] -> NNF a
andList x
  | Set.null s             = NNFTrue
  | Set.member NNFFalse s  = NNFFalse
  | otherwise              = NNFAnd s
  where s = Set.fromList $ filter (/= NNFTrue) $ concatMap g x
        g (NNFAnd xs)  = concatMap g $ Set.toList xs
        g y            = [y]

nnfNot :: (Negable a, Ord a) => NNF a -> NNF a
nnfNot NNFTrue       = NNFFalse
nnfNot NNFFalse      = NNFTrue
nnfNot (NNFValue x)  = NNFValue $ Negable.not x
nnfNot (NNFOr xs)    = NNFAnd $ Set.map not xs
nnfNot (NNFAnd xs)   = NNFOr $ Set.map not xs

instance (Ord a, Negable a) => Negable (NNF a) where
  not     = nnfNot

instance (Ord a, Negable a) => Boolean (NNF a) where
  true    = NNFTrue
  false   = NNFFalse

  a || b  = orList [a, b]
  a && b  = andList [a, b]

  not     = nnfNot

instance NormalForm NNF where
  type NFConstraint NNF a  = (Ord a, Negable a)

  toNormalForm             = NNFValue

  simplify f (NNFValue x)  = case f x of
                               Just True   -> NNFTrue
                               Just False  -> NNFFalse
                               Nothing     -> NNFValue x
  simplify _ NNFTrue       = NNFTrue
  simplify _ NNFFalse      = NNFFalse
  simplify f (NNFAnd xs)   = andList $ map (simplify f) $ Set.toList xs
  simplify f (NNFOr xs)    = orList $ map (simplify f) $ Set.toList xs

  fromFreeBoolean          = toBooleanWith toNormalForm

instance NFData a => NFData (NNF a) where
  rnf (NNFValue a) = rnf a
  rnf (NNFOr a) = rnf a
  rnf (NNFAnd a) = rnf a
  rnf _ = ()
