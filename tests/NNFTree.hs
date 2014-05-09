{-# LANGUAGE ScopedTypeVariables #-}
module NNFTree (tests) where

import BooleanLaws
import NegableLaws
import SimplifyLaws
import BooleanModelLaws

import FreeBoolean

import Test.Tasty
import Test.QuickCheck

import Control.Applicative
import Data.Function (on)

import Data.Algebra.Boolean.Negable (Negable)
import Data.Algebra.Boolean.NNF.Tree

instance (Negable a, Arbitrary a) => Arbitrary (NNF a) where
  arbitrary = fromFreeBoolean <$> arbitrary

tests :: TestTree
tests = testGroup "NNF tree implementation"
  [ monotoneLaws eq
  , nonMonotoneLaws eq
  , negableLaws eq
  , simplifyLaws (undefined :: NNF (Either Bool Bool))
  , booleanModelLaws (undefined :: NNF (Either Bool Bool))
  ]

eq :: NNF Bool -> NNF Bool -> Bool
eq = (==) `on` toBool
