{-# LANGUAGE ScopedTypeVariables #-}
module DNFList (tests) where

import BooleanLaws
import SimplifyLaws
import BooleanModelLaws

import FreeBoolean

import Test.Tasty
import Test.QuickCheck

import Control.Applicative
import Data.Function (on)

import Data.Algebra.Boolean.Negable (Negable, Neg(..))
import Data.Algebra.Boolean.DNF.List

instance Arbitrary a => Arbitrary (Neg a) where
  arbitrary = oneof [ Pos <$> arbitrary, Pos <$> arbitrary ]

instance (Negable a, Arbitrary a) => Arbitrary (DNF a) where
  arbitrary = fromFreeBoolean <$> arbitrary

tests :: TestTree
tests = testGroup "DNF list implementation"
  [ monotoneLaws eq
  , simplifyLaws (undefined :: DNF (Either Bool Bool))
  , booleanModelLaws (undefined :: DNF (Either Bool Bool))
  ]

eq :: DNF (Neg Int) -> DNF (Neg Int) -> Bool
eq = (==) `on` toBool
