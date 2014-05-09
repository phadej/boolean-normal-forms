module BooleanLaws (monotoneLaws, nonMonotoneLaws) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Prelude hiding ((||),(&&),not)
import Data.Algebra.Boolean

associativityOrProp :: Boolean a => (a -> a -> Bool) -> a -> a -> a -> Bool
associativityOrProp eq x y z = (x || (y || z)) `eq` ((x || y) || z)

associativityAndProp :: Boolean a => (a -> a -> Bool) -> a -> a -> a -> Bool
associativityAndProp eq x y z = (x || (y || z)) `eq` ((x || y) || z)

commutativeOrProp :: Boolean a => (a -> a -> Bool) -> a -> a -> Bool
commutativeOrProp eq x y = (x || y) `eq` (y || x)

commutativeAndProp :: Boolean a => (a -> a -> Bool) -> a -> a -> Bool
commutativeAndProp eq x y = (x && y) `eq` (y && x)

distributivity1Prop :: Boolean a => (a -> a -> Bool) -> a -> a -> a -> Bool
distributivity1Prop eq x y z = (x && (y || z)) `eq` ((x && y) || (x && z))

distributivity2Prop :: Boolean a => (a -> a -> Bool) -> a -> a -> a -> Bool
distributivity2Prop eq x y z = (x || (y && z)) `eq` ((x || y) && (x || z))

identityOrProp :: Boolean a => (a -> a -> Bool) -> a -> Bool
identityOrProp eq x = (x || false) `eq` x

identityAndProp :: Boolean a => (a -> a -> Bool) -> a -> Bool
identityAndProp eq x = (x && true) `eq` x

annihilatorOrProp :: Boolean a => (a -> a -> Bool) -> a -> Bool
annihilatorOrProp eq x = (x || true) `eq` true

annihilatorAndProp :: Boolean a => (a -> a -> Bool) -> a -> Bool
annihilatorAndProp eq x = (x && false) `eq` false

idempotenceOrProp :: Boolean a => (a -> a -> Bool) -> a -> Bool
idempotenceOrProp eq x = (x || x) `eq` x

idempotenceAndProp :: Boolean a => (a -> a -> Bool) -> a -> Bool
idempotenceAndProp eq x = (x && x) `eq` x

absorption1Prop :: Boolean a => (a -> a -> Bool) -> a -> a -> Bool
absorption1Prop eq x y = (x && (x || y)) `eq` x

absorption2Prop :: Boolean a => (a -> a -> Bool) -> a -> a -> Bool
absorption2Prop eq x y = (x || (x && y)) `eq` x

complementationOrProp :: Boolean a => (a -> a -> Bool) -> a -> Bool
complementationOrProp eq x = (x || not x) `eq` true

complementationAndProp :: Boolean a => (a -> a -> Bool) -> a -> Bool
complementationAndProp eq x = (x && not x) `eq` false

doubleNegationProp :: Boolean a => (a -> a -> Bool) -> a -> Bool
doubleNegationProp eq x = not (not x) `eq` x

deMorgan1Prop :: Boolean a => (a -> a -> Bool) -> a -> a -> Bool
deMorgan1Prop eq x y = (not x && not y) `eq` not (x || y)

deMorgan2Prop :: Boolean a => (a -> a -> Bool) -> a -> a -> Bool
deMorgan2Prop eq x y = (not x || not y) `eq` not (x && y)

monotoneLaws :: (Arbitrary a, Show a, Boolean a) => (a -> a -> Bool) -> TestTree
monotoneLaws eq = testGroup "Monotone boolean laws"
  [ QC.testProperty "associativity or"     $ associativityOrProp eq
  , QC.testProperty "associativity and"    $ associativityAndProp eq
  , QC.testProperty "commutativity or"     $ commutativeOrProp eq
  , QC.testProperty "commutativity and"    $ commutativeAndProp eq
  , QC.testProperty "distributivity 1"     $ distributivity1Prop eq
  , QC.testProperty "distributivity 2"     $ distributivity2Prop eq
  , QC.testProperty "identity or"          $ identityOrProp eq
  , QC.testProperty "identity and"         $ identityAndProp eq
  , QC.testProperty "annihilator or"       $ annihilatorOrProp eq
  , QC.testProperty "annihilator and"      $ annihilatorAndProp eq
  , QC.testProperty "idempotence or"       $ idempotenceOrProp eq
  , QC.testProperty "idempotence and"      $ idempotenceAndProp eq
  , QC.testProperty "absorption 1"         $ absorption1Prop eq
  , QC.testProperty "absorption 2"         $ absorption2Prop eq
  ]

nonMonotoneLaws :: (Arbitrary a, Show a, Boolean a) => (a -> a -> Bool) -> TestTree
nonMonotoneLaws eq = testGroup "Non monotone boolean laws"
  [ QC.testProperty "complementation or"   $ complementationOrProp eq
  , QC.testProperty "complementation and"  $ complementationAndProp eq
  , QC.testProperty "double negation"      $ doubleNegationProp eq
  , QC.testProperty "de Morgan 1"          $ deMorgan1Prop eq
  , QC.testProperty "de Morgan 2"          $ deMorgan2Prop eq
  ]
