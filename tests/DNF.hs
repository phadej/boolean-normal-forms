{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DNF (tests) where

import LatticeLaws

import Test.Tasty
import Test.QuickCheck
import Test.QuickCheck.Gen

import Control.Applicative
import Data.Function (on)

import Algebra.Boolean.NormalForms

scaled :: (Int -> Int) -> Gen a -> Gen a
scaled f (MkGen m) = MkGen (\r n -> m r (f n))

instance  Arbitrary a => Arbitrary (DNF a) where
  arbitrary = lowerFreeLattice . (fmap liftDNF) <$> scaled (min 10) arbitrary

tests :: TestTree
tests = testGroup "DNF list implementation"
  [ latticeLaws eq
  ]

eq :: DNF Bool -> DNF Bool -> Bool
eq = (==) `on` lowerDNF
