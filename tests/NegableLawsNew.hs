module NegableLawsNew (negableLaws) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Prelude hiding (not)
import Algebra.Boolean

doubleNegationProp :: Negable a => (a -> a -> Bool) -> a -> Bool
doubleNegationProp eq x = not (not x) `eq` x

negableLaws :: (Arbitrary a, Show a, Negable a) => (a -> a -> Bool) -> TestTree
negableLaws eq = testGroup "Negable laws"
  [ QC.testProperty "double negation"      $ doubleNegationProp eq
  ]
