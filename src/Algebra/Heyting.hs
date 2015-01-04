--------------------------------------------------------------------
-- |
-- Copyright :  © Oleg Grenrus 2014
-- License   :  MIT
-- Maintainer:  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Algebra.Heyting where

import Algebra.Lattice

class BoundedJoinSemiLattice a => Heyting a where
  -- | Implication.
  (~>) :: a -> a -> a

  -- | negation
  negation :: a -> a
  negation x = x ~> bottom