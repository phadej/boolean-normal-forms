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
module Data.Disjunction where

import GHC.Generics
import Data.Semigroup
import Data.Foldable
import Data.SetLike
import Algebra.Boolean
import Prelude hiding (not)

-- | Free bounded join semilattice
newtype Disjunction c a = Disjunction { getDisjunction :: c a }
  deriving (Eq, Ord, Read, Show, Generic, Generic1)

instance (SetLike c, SetLikeC c a) => JoinSemiLattice (Disjunction c a) where
  Disjunction a `join` Disjunction b = Disjunction (a `union` b)

instance (SetLike c, SetLikeC c a) => BoundedJoinSemiLattice (Disjunction c a) where
  bottom = Disjunction empty

instance (SetLike c, SetLikeC c a) => Semigroup (Disjunction c a) where
  (<>) = join

instance (SetLike c, SetLikeC c a) => Monoid (Disjunction c a) where
  mempty = bottom
  mappend = join

instance (SetLike c, SetLikeC c a, MeetSemiLattice a) => MeetSemiLattice (Disjunction c a) where
  Disjunction as `meet` Disjunction bs = Disjunction $ endoMap2 meet as bs

instance (SetLike c, SetLikeC c a, BoundedMeetSemiLattice a) => BoundedMeetSemiLattice (Disjunction c a) where
  top = Disjunction $ singleton top

-- | Uses De Morgan's law @¬ (P ∨ Q) = ¬ P ∧ ¬ Q@. `not` is expensive (combinatorial explosion).
instance (Negable a, SetLike c, SetLikeC c a, SetLikeC c (Disjunction c a), BoundedMeetSemiLattice a) => Negable (Disjunction c a) where
  not = meets . endoMap (liftDisjunction . not) . getDisjunction

instance (SetLike c, SetLikeC c a, MeetSemiLattice a) => Lattice (Disjunction c a) where
instance (SetLike c, SetLikeC c a, BoundedMeetSemiLattice a) => BoundedLattice (Disjunction c a) where
instance (Negable a, SetLike c, SetLikeC c a, SetLikeC c (Disjunction c a), BoundedMeetSemiLattice a) => Boolean (Disjunction c a) where
instance (Negable a, SetLike c, SetLikeC c a, SetLikeC c (Disjunction c a), BoundedMeetSemiLattice a) => Heyting (Disjunction c a) where
  (~>) = implication
  negation = not

lowerDisjunction :: (BoundedJoinSemiLattice a, SetLike c) => Disjunction c a -> a
lowerDisjunction = getJoin . foldMap Join . getDisjunction

liftDisjunction :: SetLike c => a -> Disjunction c a
liftDisjunction = Disjunction . singleton