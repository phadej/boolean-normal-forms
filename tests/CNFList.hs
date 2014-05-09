{-# LANGUAGE ScopedTypeVariables #-}
module CNFList (tests) where

import BooleanLaws
import SimplifyLaws
import BooleanModelLaws

import FreeBoolean

import Test.Tasty
import Test.QuickCheck

import Control.Applicative
import Data.Function (on)

import Data.Algebra.Boolean.Negable (Negable, Neg(..))
import Data.Algebra.Boolean.CNF.List

instance Arbitrary a => Arbitrary (Neg a) where
  arbitrary = oneof [ Pos <$> arbitrary, Pos <$> arbitrary ]

instance (Negable a, Arbitrary a) => Arbitrary (CNF a) where
  arbitrary = fromFreeBoolean <$> arbitrary

tests :: TestTree
tests = testGroup "CNF list implementation"
  [ monotoneLaws eq
  , simplifyLaws (undefined :: CNF (Either Bool Bool))
  , booleanModelLaws (undefined :: CNF (Either Bool Bool))
  ]

eq :: CNF (Neg Int) -> CNF (Neg Int) -> Bool
eq = (==) `on` toBool
