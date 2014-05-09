module FreeBoolean (
  freeBooleanHeight,
  module Data.Algebra.Boolean.FreeBoolean
  ) where

import Test.Tasty.QuickCheck

import Control.Applicative

import Data.Algebra.Boolean.FreeBoolean

instance Arbitrary a => Arbitrary (FreeBoolean a) where
  arbitrary = sized arbitrary'
    where arbitrary' 0 = frequency [
            (10, FBValue <$> arbitrary),
            (1, pure FBTrue),
            (1, pure FBFalse)
            ]
          arbitrary' n = oneof [
            FBValue <$> arbitrary,
            FBNot <$> arbitrary'',
            FBAnd <$> arbitrary'' <*> arbitrary'',
            FBOr <$> arbitrary'' <*> arbitrary''
            ]
            where arbitrary'' = arbitrary' $ n `div` 2

freeBooleanHeight :: FreeBoolean a -> Int
freeBooleanHeight FBTrue = 0
freeBooleanHeight FBFalse = 0
freeBooleanHeight (FBValue _) = 0
freeBooleanHeight (FBNot x) = 1 + freeBooleanHeight x
freeBooleanHeight (FBAnd a b) = 1 + max (freeBooleanHeight a) (freeBooleanHeight b)
freeBooleanHeight (FBOr a b) = 1 + max (freeBooleanHeight a) (freeBooleanHeight b)
