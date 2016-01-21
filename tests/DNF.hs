{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module DNF (tests) where

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

instance (Arbitrary a, Ord a) => Arbitrary (DNFList a) where
  arbitrary = retractFreeLattice . (fmap liftDNF) <$> scaled' (min 3) arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (DNFSet a) where
  arbitrary = retractFreeLattice . (fmap liftDNF) <$> scaled' (min 10) arbitrary

tests :: TestTree
tests = testGroup "DNF implementation"
  [ testGroup "List Bool"
      [ latticeLaws (eq (Proxy :: Proxy (DNFList Bool)))
      , booleanLaws (eq (Proxy :: Proxy (DNFList Bool)))
      ]
  , testGroup "Set Bool"
      [ latticeLaws (eq (Proxy :: Proxy (DNFSet Bool)))
      , negableLaws (eq (Proxy :: Proxy (DNFSet Bool)))
      , booleanLaws (eq (Proxy :: Proxy (DNFSet Bool)))
      ]
  ]

eq :: (a ~ DNF c1 c2 x, Eq x, SetLike' c1 x, SetLike c2, BoundedLattice x) => Proxy a -> a -> a -> Bool
eq _ = (==) `on` retractDNF
