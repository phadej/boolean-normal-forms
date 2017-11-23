{-# LANGUAGE ScopedTypeVariables #-}
module CNFSet (tests) where

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
import Data.Algebra.Boolean.CNF.Set

instance (Ord a, Negable a, Arbitrary a) => Arbitrary (CNF a) where
  arbitrary = fromFreeBoolean <$> arbitrary

tests :: TestTree
tests = testGroup "CNF set implementation"
  [ monotoneLaws eq
  , nonMonotoneLaws eq
  , negableLaws eq
  , simplifyLaws (undefined :: CNF (Either Bool Bool))
  , booleanModelLaws (undefined :: CNF (Either Bool Bool))
  ]

eq :: CNF (Maybe (Sum Int)) -> CNF (Maybe (Sum Int)) -> Bool
eq = (==) `on` toBool
