module LatticeLaws (latticeLaws) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Algebra.Lattice.Extras

associativityJoinProp :: JoinSemiLattice a => (a -> a -> Bool) -> a -> a -> a -> Bool
associativityJoinProp eq x y z = (x \/ (y \/ z)) `eq` ((x \/ y) \/ z)

associativityMeetProp :: MeetSemiLattice a => (a -> a -> Bool) -> a -> a -> a -> Bool
associativityMeetProp eq x y z = (x /\ (y /\ z)) `eq` ((x /\ y) /\ z)

commutativeJoinProp :: JoinSemiLattice a => (a -> a -> Bool) -> a -> a -> Bool
commutativeJoinProp eq x y = (x \/ y) `eq` (y \/ x)

commutativeMeetProp :: MeetSemiLattice a => (a -> a -> Bool) -> a -> a -> Bool
commutativeMeetProp eq x y = (x /\ y) `eq` (y /\ x)

distributivity1Prop :: Lattice a => (a -> a -> Bool) -> a -> a -> a -> Bool
distributivity1Prop eq x y z = (x /\ (y \/ z)) `eq` ((x /\ y) \/ (x /\ z))

distributivity2Prop :: Lattice a => (a -> a -> Bool) -> a -> a -> a -> Bool
distributivity2Prop eq x y z = (x \/ (y /\ z)) `eq` ((x \/ y) /\ (x \/ z))

identityJoinProp :: BoundedJoinSemiLattice a => (a -> a -> Bool) -> a -> Bool
identityJoinProp eq x = (x \/ bottom) `eq` x

identityMeetProp :: BoundedMeetSemiLattice a => (a -> a -> Bool) -> a -> Bool
identityMeetProp eq x = (x /\ top) `eq` x

annihilatorJoinProp :: (Lattice a, BoundedMeetSemiLattice a) => (a -> a -> Bool) -> a -> Bool
annihilatorJoinProp eq x = (x \/ top) `eq` top

annihilatorMeetProp :: (Lattice a, BoundedJoinSemiLattice a)  => (a -> a -> Bool) -> a -> Bool
annihilatorMeetProp eq x = (x /\ bottom) `eq` bottom

idempotenceJoinProp :: JoinSemiLattice a => (a -> a -> Bool) -> a -> Bool
idempotenceJoinProp eq x = (x \/ x) `eq` x

idempotenceMeetProp :: MeetSemiLattice a => (a -> a -> Bool) -> a -> Bool
idempotenceMeetProp eq x = (x /\ x) `eq` x

absorption1Prop :: Lattice a => (a -> a -> Bool) -> a -> a -> Bool
absorption1Prop eq x y = (x /\ (x \/ y)) `eq` x

absorption2Prop :: Lattice a => (a -> a -> Bool) -> a -> a -> Bool
absorption2Prop eq x y = (x \/ (x /\ y)) `eq` x

latticeLaws :: (Arbitrary a, Show a, BoundedLattice a) => (a -> a -> Bool) -> TestTree
latticeLaws eq = testGroup "Lattice laws"
  [ QC.testProperty "associativity join"     $ associativityJoinProp eq
  , QC.testProperty "associativity meet"    $ associativityMeetProp eq
  , QC.testProperty "commutativity join"     $ commutativeJoinProp eq
  , QC.testProperty "commutativity meet"    $ commutativeMeetProp eq
  , QC.testProperty "distributivity 1"     $ distributivity1Prop eq
  , QC.testProperty "distributivity 2"     $ distributivity2Prop eq
  , QC.testProperty "identity join"          $ identityJoinProp eq
  , QC.testProperty "identity meet"         $ identityMeetProp eq
  , QC.testProperty "annihilator join"       $ annihilatorJoinProp eq
  , QC.testProperty "annihilator meet"      $ annihilatorMeetProp eq
  , QC.testProperty "idempotence join"       $ idempotenceJoinProp eq
  , QC.testProperty "idempotence meet"      $ idempotenceMeetProp eq
  , QC.testProperty "absorption 1"         $ absorption1Prop eq
  , QC.testProperty "absorption 2"         $ absorption2Prop eq
  ]