{-# LANGUAGE ScopedTypeVariables #-}
module NNFSet (tests) where

import BooleanLaws
import NegableLaws
import SimplifyLaws
import BooleanModelLaws

import FreeBoolean

import Test.Tasty
import Test.QuickCheck

import Control.Applicative
import Data.Monoid
import Data.Function (on)

import Data.Algebra.Boolean.Negable (Negable)
import Data.Algebra.Boolean.NNF.Set

instance (Ord a, Negable a, Arbitrary a) => Arbitrary (NNF a) where
  arbitrary = fromFreeBoolean <$> arbitrary

tests :: TestTree
tests = testGroup "NNF set implementation"
  [ monotoneLaws eq
  , nonMonotoneLaws eq
  , negableLaws eq
  , simplifyLaws (undefined :: NNF (Either Bool Bool))
  , booleanModelLaws (undefined :: NNF (Either Bool Bool))
  ]

eq :: NNF (Maybe (Sum Int)) -> NNF (Maybe (Sum Int)) -> Bool
eq = (==) `on` toBool
