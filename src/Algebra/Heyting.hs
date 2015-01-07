--------------------------------------------------------------------
-- |
-- Copyright :  © Oleg Grenrus 2014
-- License   :  MIT
-- Maintainer:  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Algebra.Heyting (
  Heyting(..),
  module Algebra.Lattice.Extras
  ) where

import Algebra.Lattice.Extras

infixr 1 ~>
infix 1 <~>

-- | Intuitionistic logic.
class BoundedLattice a => Heyting a where
  -- | Implication.
  (~>) :: a -> a -> a

  -- | Equivalence.
  (<~>) :: a -> a -> a
  a <~> b = a ~> b /\ b ~> a

  -- | Negation.
  negation :: a -> a
  negation x = x ~> bottom

instance Heyting Bool where
  a ~> b = not a \/ b

instance (Heyting a, Heyting b) => Heyting (a, b) where
  (a, b) ~> (c, d) = (a ~> c, b ~> d)
