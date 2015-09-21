{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module CNF (tests) where

import LatticeLaws
import NegableLaws
import BooleanLaws

import Test.Tasty
import Test.QuickCheck
import Test.QuickCheck.Gen

import Control.Applicative
import Data.Function (on)
import Data.Proxy
import Data.SetLike

import Algebra.Boolean.NormalForms

scaled' :: (Int -> Int) -> Gen a -> Gen a
scaled' f (MkGen m) = MkGen (\r n -> m r (f n))

instance (Arbitrary a, Ord a) => Arbitrary (CNFList a) where
  arbitrary = retractFreeLattice . (fmap liftCNF) <$> scaled' (min 3) arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (CNFSet a) where
  arbitrary = retractFreeLattice . (fmap liftCNF) <$> scaled' (min 10) arbitrary

tests :: TestTree
tests = testGroup "CNF implementation"
  [ testGroup "List Bool"
      [ latticeLaws (eq (Proxy :: Proxy (CNFList Bool)))
      , booleanLaws (eq (Proxy :: Proxy (CNFList Bool)))
      ]
  , testGroup "Set Bool"
      [ latticeLaws (eq (Proxy :: Proxy (CNFSet Bool)))
      , negableLaws (eq (Proxy :: Proxy (CNFSet Bool)))
      , booleanLaws (eq (Proxy :: Proxy (CNFSet Bool)))
      ]
  ]

eq :: (a ~ CNF c1 c2 x, Eq x, SetLike' c1 x, SetLike c2, BoundedLattice x) => Proxy a -> a -> a -> Bool
eq _ = (==) `on` retractCNF
