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
module Data.Algebra.Boolean.CNF.List (
  CNF(..),
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

import Data.Algebra.Boolean.NormalForm
import Data.Algebra.Boolean.Negable hiding (not)
import qualified Data.Algebra.Boolean.Negable as Negable

import Data.Algebra.Boolean.NNF.Tree

import Data.Algebra.Boolean

-- | Boolean formula in Conjunction Normal Form
--
-- <<doc-formulae/cnf.svg>>
newtype CNF a = CNF { unCNF :: [[a]] }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Typeable)

instance CoBoolean1 CNF where
  toBooleanWith f = all (any f) . unCNF

instance CoBoolean a => CoBoolean (CNF a) where
  toBoolean = toBooleanWith toBoolean

fromDoubleList :: [[a]] -> CNF a
fromDoubleList = CNF

toDoubleList :: CNF a -> [[a]]
toDoubleList = unCNF

cnfNot :: Negable a => CNF a -> CNF a
cnfNot = any and . map (map $ toNormalForm . Negable.not) . unCNF

instance Negable a => Negable (CNF a) where
  not                 = cnfNot

instance Negable a => Boolean (CNF a) where
  false               = CNF [[]]
  true                = CNF []
  (CNF a) && (CNF b)  = CNF (a <> b)
  (CNF a) || (CNF b)  = CNF [ a' <> b' | a' <- a, b' <- b ]
  not                 = cnfNot

fromNNF :: Negable a => NNF a -> CNF a
fromNNF = toBooleanWith toNormalForm

instance NormalForm CNF where
  type NFConstraint CNF a  = Negable a

  toNormalForm x = CNF [[x]]

  simplify f = CNF . p . g . map (h . map f') . unCNF
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
          p :: Either [[a]] Bool -> [[a]]
          p (Right True)           = []
          p (Right False)          = [[]]
          p (Left x)               = x

  fromFreeBoolean = fromNNF . toBooleanWith toNormalForm
