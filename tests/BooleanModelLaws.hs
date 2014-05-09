{-# LANGUAGE ConstraintKinds #-}
module BooleanModelLaws (booleanModelLaws) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import FreeBoolean

import Data.Algebra.Boolean.NormalForm

modelProperty :: (CoBoolean a, NormalForm nf, NFConstraint nf a) => nf a -> FreeBoolean a-> Property
modelProperty t b = collect (freeBooleanHeight b) $
  toBoolWith toBool b == toBoolWith toBool (fromFreeBoolean b `asTypeOf` t)

booleanModelLaws :: (Arbitrary a, Show a, CoBoolean a, NormalForm nf, NFConstraint nf a) => nf a -> TestTree
booleanModelLaws t = QC.testProperty "Boolean model" $ modelProperty t
