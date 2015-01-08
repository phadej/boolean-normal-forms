module NegableLawsNew (negableLaws) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Prelude hiding (not)
import Algebra.Boolean

import Debug.Trace

debug x = traceShow x x

doubleNegationProp :: (Show a, Negable a) => (a -> a -> Bool) -> a -> Bool
doubleNegationProp eq x = lhs `eq` rhs
  where rhs = not (not x)
        lhs = x

negableLaws :: (Arbitrary a, Show a, Negable a) => (a -> a -> Bool) -> TestTree
negableLaws eq = testGroup "Negable laws"
  [ QC.testProperty "double negation"      $ doubleNegationProp eq
  ]
