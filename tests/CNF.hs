{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CNF (tests) where

import LatticeLaws

import Test.Tasty
import Test.QuickCheck
import Test.QuickCheck.Gen

import Control.Applicative
import Data.Function (on)

import Algebra.Boolean.NormalForms

scaled :: (Int -> Int) -> Gen a -> Gen a
scaled f (MkGen m) = MkGen (\r n -> m r (f n))

instance  Arbitrary a => Arbitrary (CNF a) where
  arbitrary = lowerFreeLattice . (fmap liftCNF) <$> scaled (min 10) arbitrary

tests :: TestTree
tests = testGroup "CNF list implementation"
  [ latticeLaws eq
  ]

eq :: CNF Bool -> CNF Bool -> Bool
eq = (==) `on` lowerCNF
