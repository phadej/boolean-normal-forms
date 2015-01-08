{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Oleg Grenrus 2014
-- License   :  MIT
-- Maintainer:  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Conjunction where

import GHC.Generics
import Data.Semigroup
import Data.Foldable
import Data.SetLike
import Algebra.Boolean
import Prelude hiding (not)

-- | Free bounded meet semilattice
newtype Conjunction c a = Conjunction { getConjunction :: c a }
  deriving (Eq, Ord, Read, Show, Generic, Generic1)

instance (SetLike c, SetLikeC c a) => MeetSemiLattice (Conjunction c a) where
  Conjunction a `meet` Conjunction b = Conjunction (a `union` b)

instance (SetLike c, SetLikeC c a) => BoundedMeetSemiLattice (Conjunction c a) where
  top = Conjunction empty

instance (SetLike c, SetLikeC c a) => Semigroup (Conjunction c a) where
  (<>) = meet

instance (SetLike c, SetLikeC c a) => Monoid (Conjunction c a) where
  mempty = top
  mappend = meet

instance (SetLike c, SetLikeC c a, JoinSemiLattice a) => JoinSemiLattice (Conjunction c a) where
  Conjunction as `join` Conjunction bs = Conjunction $ endoMap2 join as bs

instance (SetLike c, SetLikeC c a, BoundedJoinSemiLattice a) => BoundedJoinSemiLattice (Conjunction c a) where
  bottom = Conjunction $ singleton bottom

-- | Uses De Morgan's law @¬ (P ∧ Q) = ¬ P ∨ ¬ Q@. `not` is expensive (combinatorial explosion).
instance (Negable a, SetLike c, SetLikeC c a, BoundedJoinSemiLattice a) => Negable (Conjunction c a) where
  not (Conjunction as) = Conjunction $ singleton $ joins $ endoMap not as

instance (SetLike c, SetLikeC c a, JoinSemiLattice a) => Lattice (Conjunction c a) where
instance (SetLike c, SetLikeC c a, BoundedJoinSemiLattice a) => BoundedLattice (Conjunction c a) where
instance (Negable a, SetLike c, SetLikeC c a, BoundedJoinSemiLattice a) => Boolean (Conjunction c a) where
instance (Negable a, SetLike c, SetLikeC c a, BoundedJoinSemiLattice a) => Heyting (Conjunction c a) where
  (~>) = implication
  negation = not

lowerConjunction :: (BoundedMeetSemiLattice a, SetLike c) => Conjunction c a -> a
lowerConjunction = getMeet . foldMap Meet . getConjunction

liftConjunction :: SetLike c => a -> Conjunction c a
liftConjunction = Conjunction . singleton