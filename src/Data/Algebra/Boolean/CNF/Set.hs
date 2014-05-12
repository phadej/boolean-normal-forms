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
module Data.Algebra.Boolean.CNF.Set (
  CNF(..),
  fromDoubleList,
  toDoubleList,
  fromNNF,
  module Data.Algebra.Boolean.NormalForm
  ) where

import Prelude hiding ((||),(&&),not,and,or,any,all)
import Data.Monoid
import Data.Typeable (Typeable)
import Data.Foldable (Foldable)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (sortBy, foldl')
import Data.Function (on)

import Data.Algebra.Boolean.NormalForm
import Data.Algebra.Boolean.Negable hiding (not)
import qualified Data.Algebra.Boolean.Negable as Negable

import Data.Algebra.Boolean.NNF.Tree

import Data.Algebra.Boolean

-- | Boolean formula in Conjunction Normal Form
--
-- <<doc-formulae/cnf.svg>>
newtype CNF a = CNF { unCNF :: Set (Set a) }
  deriving (Eq, Ord, Show, Read, Foldable, Typeable)

instance CoBoolean1 CNF where
  toBooleanWith f = all (any f) . unCNF

instance CoBoolean a => CoBoolean (CNF a) where
  toBoolean = toBooleanWith toBoolean

toDoubleList :: CNF a -> [[a]]
toDoubleList = map Set.toList . Set.toList . unCNF

fromDoubleList :: (Ord a) => [[a]] -> CNF a
fromDoubleList = CNF . Set.fromList . map Set.fromList

cnfNot :: (Ord a, Negable a) => CNF a -> CNF a
cnfNot = any and . map (map $ toNormalForm . Negable.not) . toDoubleList

instance (Ord a, Negable a) => Negable (CNF a) where
  not                 = cnfNot

instance (Ord a, Negable a) => Boolean (CNF a) where
  false               = CNF $ Set.singleton Set.empty
  true                = CNF Set.empty
  (CNF a) && (CNF b)  = CNF (a <> b)
  (CNF a) || (CNF b)  = CNF $ Set.fromList [a' <> b' | a' <- Set.toList a, b' <- Set.toList b ]
  not                 = cnfNot

fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _        = error "fromLeft called on Right value"

optimize :: Ord a => Set (Set a) -> Set (Set a)
optimize = Set.fromList . f . sortBy (flip compare `on` Set.size) . Set.toList
  where f disj = foldl' g disj disj
        g disj item = filter (not . Set.isProperSubsetOf item) disj

fromNNF :: (Ord a, Negable a) => NNF a -> CNF a
fromNNF = toBooleanWith toNormalForm

instance NormalForm CNF where
  type NFConstraint CNF a = (Negable a, Ord a)

  toNormalForm = CNF . Set.singleton . Set.singleton

  simplify f = CNF . optimize . p . g . Set.map h . Set.map (Set.map f') . unCNF
    where f' x   = case f x of
                     Just b   -> Right b
                     Nothing  -> Left x
          h :: Ord a => Set (Either a Bool) -> Either (Set a) Bool
          h disj | Right True `Set.member` disj   = Right True
                 | Set.null l                     = Right False
                 | otherwise                      = Left l
            where l  = Set.mapMonotonic fromLeft $ fst $ Set.split (Right minBound) disj
          g :: Ord a => Set (Either a Bool) -> Either (Set a) Bool
          g conj | Right False `Set.member` conj  = Right False
                 | Set.null l                     = Right True
                 | otherwise                      = Left l
            where l  = Set.mapMonotonic fromLeft $ fst $ Set.split (Right minBound) conj
          p :: Either (Set (Set a)) Bool -> Set (Set a)
          p (Right True)                          = Set.empty
          p (Right False)                         = Set.singleton Set.empty
          p (Left x)                              = x

  fromFreeBoolean = fromNNF . toBooleanWith toNormalForm
