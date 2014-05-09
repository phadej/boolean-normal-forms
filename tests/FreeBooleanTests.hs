module FreeBooleanTests where

import BooleanLaws
import NegableLaws
import SimplifyLaws
import BooleanModelLaws

import FreeBoolean

import Data.Function (on)

import Test.Tasty

tests :: TestTree
tests = testGroup "Free boolean"
  [ monotoneLaws eq
  , nonMonotoneLaws eq
  , negableLaws eq
  , simplifyLaws (undefined :: FreeBoolean (Either Bool Bool))
  , booleanModelLaws (undefined :: FreeBoolean (Either Bool Bool))
  ]

eq :: FreeBoolean Int -> FreeBoolean Int -> Bool
eq = (==) `on` toBoolWith (1==)
