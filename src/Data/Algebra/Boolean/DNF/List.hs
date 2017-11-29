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
module Data.Algebra.Boolean.DNF.List (
  DNF(..),
  fromDoubleList,
  toDoubleList,
  fromNNF,
  module Data.Algebra.Boolean.NormalForm
  ) where

import Prelude hiding ((||),(&&),not,any,all,and,or)
import Data.Monoid
import Data.Either (partitionEithers)
import Data.Typeable (Typeable)
import Data.Foldable (Foldable)
import Control.DeepSeq (NFData(rnf))

import Data.Algebra.Boolean.NormalForm
import Data.Algebra.Boolean.Negable hiding (not)
import qualified Data.Algebra.Boolean.Negable as Negable

import Data.Algebra.Boolean.NNF.Tree

import Data.Algebra.Boolean

-- | Boolean formula in Disjunction Normal Form
--
-- <<doc-formulae/dnf.svg>>
newtype DNF a = DNF { unDNF :: [[a]] }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Typeable)

instance CoBoolean1 DNF where
  toBooleanWith f = any (all f) . unDNF

instance CoBoolean a => CoBoolean (DNF a) where
  toBoolean = toBooleanWith toBoolean

fromDoubleList :: [[a]] -> DNF a
fromDoubleList = DNF

toDoubleList :: DNF a -> [[a]]
toDoubleList = unDNF

dnfNot :: Negable a => DNF a -> DNF a
dnfNot = all or . map (map $ toNormalForm . Negable.not) . unDNF

instance Negable a => Negable (DNF a) where
  not                 = dnfNot

instance Negable a => Boolean (DNF a) where
  true                = DNF [[]]
  false               = DNF []
  (DNF a) || (DNF b)  = DNF (a <> b)
  (DNF a) && (DNF b)  = DNF [ a' <> b' | a' <- a, b' <- b ]
  not                 = dnfNot

fromNNF :: Negable a => NNF a -> DNF a
fromNNF = toBooleanWith toNormalForm

instance NormalForm DNF where
  type NFConstraint DNF a  = Negable a

  toNormalForm x = DNF [[x]]

  simplify f = DNF . q . h . map (g . map f') . unDNF
    where f' x   = case f x of
                     Just b   -> Right b
                     Nothing  -> Left x
          h :: [Either a Bool] -> Either [a] Bool
          h disj | True `elem` r   = Right True
                 | null l          = Right False
                 | otherwise       = Left l
            where (l, r)  = partitionEithers disj
          g :: [Either a Bool] -> Either [a] Bool
          g conj | False `elem` r  = Right False
                 | null l          = Right True
                 | otherwise       = Left l
            where (l, r)  = partitionEithers conj
          q :: Either [[a]] Bool -> [[a]]
          q (Right True)           = [[]]
          q (Right False)          = []
          q (Left x)               = x

  fromFreeBoolean = fromNNF . toBooleanWith toNormalForm

instance NFData a => NFData (DNF a) where
  rnf (DNF xss) = rnf xss
