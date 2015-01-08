module NegableLaws (negableLaws) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Prelude hiding (not)
import Algebra.Boolean

doubleNegationProp :: (Show a, Negable a) => (a -> a -> Bool) -> a -> Bool
doubleNegationProp eq x = lhs `eq` rhs
  where rhs = not (not x)
        lhs = x

negableLaws :: (Arbitrary a, Show a, Negable a) => (a -> a -> Bool) -> TestTree
negableLaws eq = testGroup "Negable laws"
  [ QC.testProperty "double negation"      $ doubleNegationProp eq
  ]
