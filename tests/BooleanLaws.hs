module BooleanLaws (booleanLaws) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Prelude hiding ((||),(&&),not)
import Algebra.Boolean

complementationOrProp :: Boolean a => (a -> a -> Bool) -> a -> Bool
complementationOrProp eq x = (x || not x) `eq` true

complementationAndProp :: Boolean a => (a -> a -> Bool) -> a -> Bool
complementationAndProp eq x = (x && not x) `eq` false

deMorgan1Prop :: Boolean a => (a -> a -> Bool) -> a -> a -> Bool
deMorgan1Prop eq x y = (not x && not y) `eq` not (x || y)

deMorgan2Prop :: Boolean a => (a -> a -> Bool) -> a -> a -> Bool
deMorgan2Prop eq x y = (not x || not y) `eq` not (x && y)

booleanLaws :: (Arbitrary a, Show a, Boolean a) => (a -> a -> Bool) -> TestTree
booleanLaws eq = testGroup "Boolean laws"
  [ QC.testProperty "complementation or"   $ complementationOrProp eq
  , QC.testProperty "complementation and"  $ complementationAndProp eq
  , QC.testProperty "de Morgan 1"          $ deMorgan1Prop eq
  , QC.testProperty "de Morgan 2"          $ deMorgan2Prop eq
  ]
