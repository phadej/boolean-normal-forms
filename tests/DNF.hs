{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DNF (tests) where

import LatticeLaws

import Test.Tasty
import Test.QuickCheck

import Control.Applicative
import Data.Function (on)

import Algebra.Boolean.NormalForms

instance  Arbitrary a => Arbitrary (DNF a) where
  arbitrary = lowerFreeLattice . (fmap liftDNF) <$> arbitrary

tests :: TestTree
tests = testGroup "DNF list implementation"
  [ latticeLaws eq
  ]

eq :: DNF Bool -> DNF Bool -> Bool
eq = (==) `on` lowerDNF
