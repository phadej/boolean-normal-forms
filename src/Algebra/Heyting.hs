module Algebra.Heyting where

import Algebra.Lattice

class BoundedJoinSemiLattice a => Heyting a where
  -- | Implication.
  (~>) :: a -> a -> a

  -- | negation
  negation :: a -> a
  negation x = x ~> bottom