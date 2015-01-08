{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CNF (tests) where

import LatticeLaws

import Test.Tasty
import Test.QuickCheck

import Control.Applicative
import Data.Function (on)

import Algebra.Boolean.NormalForms

instance  Arbitrary a => Arbitrary (CNF a) where
  arbitrary = lowerFreeLattice . (fmap liftCNF) <$> arbitrary

tests :: TestTree
tests = testGroup "CNF list implementation"
  [ latticeLaws eq
  ]

eq :: CNF Bool -> CNF Bool -> Bool
eq = (==) `on` lowerCNF
