{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- TODO: write Arbitrary instance for NNF
module NNF (tests) where

--import BooleanLaws
import NegableLawsNew
--import SimplifyLaws
--import BooleanModelLaws

--import FreeBoolean

import Test.Tasty
import Test.QuickCheck

import Control.Applicative
import Data.Function (on)

import Algebra.Boolean.NormalForms

instance Arbitrary a => Arbitrary (NNF a) where
  arbitrary = lowerBoolean <$> arbitrary

tests :: TestTree
tests = testGroup "NNF tree implementation"
  [ negableLaws eq ]
{-
  [ monotoneLaws eq
  , nonMonotoneLaws eq
  , negableLaws eq
  , simplifyLaws (undefined :: NNF (Either Bool Bool))
  , booleanModelLaws (undefined :: NNF (Either Bool Bool))
  ]
-}

eq :: NNF Bool -> NNF Bool -> Bool
eq = (==) `on` lowerNNF
