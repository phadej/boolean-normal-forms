{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module NNF (tests) where

import LatticeLaws
import BooleanLaws
import NegableLaws

import Test.Tasty
import Test.QuickCheck

import Control.Applicative
import Data.Function (on)

import Algebra.Boolean.NormalForms

instance (Show a, Arbitrary a) => Arbitrary (NNF a) where
  arbitrary = retractFreeBoolean . (fmap liftNNF) <$> arbitrary

tests :: TestTree
tests = testGroup "NNF tree implementation"
  [ negableLaws eq
  , latticeLaws eq
  , booleanLaws eq
  ]

eq :: NNF Bool -> NNF Bool -> Bool
eq = (==) `on` retractNNF
