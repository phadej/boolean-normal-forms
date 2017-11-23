{-# LANGUAGE ScopedTypeVariables #-}
module DNFSet (tests) where

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
import Data.Algebra.Boolean.DNF.Set

instance (Ord a, Negable a, Arbitrary a) => Arbitrary (DNF a) where
  arbitrary = fromFreeBoolean <$> arbitrary

tests :: TestTree
tests = testGroup "DNF set implementation"
  [ monotoneLaws eq
  , nonMonotoneLaws eq
  , negableLaws eq
  , simplifyLaws (undefined :: DNF (Either Bool Bool))
  , booleanModelLaws (undefined :: DNF (Either Bool Bool))
  ]

eq :: DNF (Maybe (Sum Int)) -> DNF (Maybe (Sum Int)) -> Bool
eq = (==) `on` toBool
