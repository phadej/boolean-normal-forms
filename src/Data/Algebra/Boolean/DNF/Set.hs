{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Oleg Grenrus 2014
-- License   :  MIT
-- Maintainer:  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Algebra.Boolean.DNF.Set (
  DNF(..),
  fromDoubleList,
  toDoubleList,
  fromNNF,
  module Data.Algebra.Boolean.NormalForm
  ) where

import Prelude hiding ((||),(&&),not,and,or,any,all)

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

import Data.Typeable (Typeable)
import Data.Foldable (Foldable)
import Control.DeepSeq (NFData(rnf))

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (sortBy, foldl')
import Data.Function (on)

import Data.Algebra.Boolean.NormalForm
import Data.Algebra.Boolean.Negable hiding (not)
import qualified Data.Algebra.Boolean.Negable as Negable

import Data.Algebra.Boolean.NNF.Tree

import Data.Algebra.Boolean

-- | Boolean formula in Disjunction Normal Form
--
-- <<doc-formulae/dnf.svg>>
newtype DNF a = DNF { unDNF :: Set (Set a) }
  deriving (Eq, Ord, Show, Read, Foldable, Typeable)

instance CoBoolean1 DNF where
  toBooleanWith f = any (all f) . unDNF

instance CoBoolean a => CoBoolean (DNF a) where
  toBoolean = toBooleanWith toBoolean

toDoubleList :: DNF a -> [[a]]
toDoubleList = map Set.toList . Set.toList . unDNF

fromDoubleList :: (Ord a) => [[a]] -> DNF a
fromDoubleList = DNF . Set.fromList . map Set.fromList

dnfNot :: (Ord a, Negable a) => DNF a -> DNF a
dnfNot = all or . map (map $ toNormalForm . Negable.not) . toDoubleList

instance (Ord a, Negable a) => Negable (DNF a) where
  not                 = dnfNot

instance (Ord a, Negable a) => Boolean (DNF a) where
  true                = DNF $ Set.singleton Set.empty
  false               = DNF Set.empty
  (DNF a) || (DNF b)  = DNF (a <> b)
  (DNF a) && (DNF b)  = DNF $ Set.fromList [a' <> b' | a' <- Set.toList a, b' <- Set.toList b ]
  not                 = dnfNot

fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _        = error "fromLeft called on Right value"

optimize :: Ord a => Set (Set a) -> Set (Set a)
optimize = Set.fromList . f . sortBy (compare `on` Set.size) . Set.toList
  where f conj = foldl' g conj conj
        g conj item = filter (not . Set.isProperSubsetOf item) conj

fromNNF :: (Ord a, Negable a) => NNF a -> DNF a
fromNNF = toBooleanWith toNormalForm

instance NormalForm DNF where
  type NFConstraint DNF a = (Negable a, Ord a)

  toNormalForm = DNF . Set.singleton . Set.singleton

  simplify f = DNF . optimize . q . h . Set.map g . Set.map (Set.map f') . unDNF
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
          q :: Either (Set (Set a)) Bool -> Set (Set a)
          q (Right True)                          = Set.singleton Set.empty
          q (Right False)                         = Set.empty
          q (Left x)                              = x

  fromFreeBoolean = fromNNF . toBooleanWith toNormalForm

instance NFData a => NFData (DNF a) where
  rnf (DNF xss) = rnf xss
