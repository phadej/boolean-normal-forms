{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |
-- Module      : Algebra.Boolean.NormalForms
-- Copyright   : (c) 2014-2015 Oleg Grenrus
-- License     : MIT
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Algebra.Boolean.NormalForms (
  -- * Negation normal form
  -- | <<doc-formulae/nnf.svg>>
  NNF,
  liftNNF,
  retractNNF,
  -- * Disjunction normal form
  -- | <<doc-formulae/dnf.svg>>
  DNF,
  liftDNF,
  retractDNF,
  DNFList,
  DNFSet,
  -- * Conjunction normal form
  -- | <<doc-formulae/cnf.svg>>
  CNF,
  liftCNF,
  retractCNF,
  CNFList,
  CNFSet,
  -- * Module re-exports
  module Algebra.Lattice.Levitated,
  module Algebra.Lattice.FreeLattice,
  module Algebra.Boolean,
  module Data.Disjunction,
  module Data.Conjunction
  ) where

import Algebra.Boolean
import Algebra.Lattice.FreeLattice
import Algebra.Lattice.Levitated
import Data.Conjunction
import Data.Disjunction
import Data.Set (Set)
import Data.SetLike

import GHC.Generics (Generic)
import Data.Foldable (Foldable)

import Prelude hiding (not)

type NNF a = FreeBoundedLattice (Neg a)

liftNNF :: a -> NNF a
liftNNF = return . liftFreeLattice . liftNeg

retractNNF :: Boolean a => NNF a -> a
retractNNF = retractLevitated . fmap (retractFreeLattice . fmap retractNeg)

-- CNF

newtype CNF c1 c2 a = CNF (Conjunction c1 (Disjunction c2 a))
  deriving (Generic)

getCNF :: CNF c1 c2 a -> Conjunction c1 (Disjunction c2 a)
getCNF (CNF cnf) = cnf

deriving instance Eq (c1 (Disjunction c2 a)) => Eq (CNF c1 c2 a)
deriving instance Ord (c1 (Disjunction c2 a)) => Ord (CNF c1 c2 a)
deriving instance Show (c1 (Disjunction c2 a)) => Show (CNF c1 c2 a)

instance (SetLike' c1 (Disjunction c2 a)) => MeetSemiLattice (CNF c1 c2 a) where
  CNF a /\ CNF b = CNF (a /\ b)
instance (SetLike' c1 (Disjunction c2 a), SetLike' c2 a) => JoinSemiLattice (CNF c1 c2 a) where
  CNF a \/ CNF b = CNF (a \/ b)
instance (SetLike' c1 (Disjunction c2 a)) => BoundedMeetSemiLattice (CNF c1 c2 a) where
  top = CNF top
instance (SetLike' c1 (Disjunction c2 a), SetLike' c2 a) => BoundedJoinSemiLattice (CNF c1 c2 a) where
  bottom = CNF bottom
instance (SetLike' c1 (Disjunction c2 a), SetLike' c2 a) => Lattice (CNF c1 c2 a)
instance (SetLike' c1 (Disjunction c2 a), SetLike' c2 a) => BoundedLattice (CNF c1 c2 a)

instance (SetLike' c1 (Disjunction c2 a), SetLike' c2 a, Negable a, SetLike' c1 (CNF c1 c2 a), SetLike' c2 (CNF c1 c2 a)) => Negable (CNF c1 c2 a) where
  not = joins . liftSet (meets . liftSet (liftCNF . not) . getDisjunction) . getConjunction . getCNF

instance (SetLike' c1 (Disjunction c2 a), SetLike' c2 a, Negable a, SetLike' c1 (CNF c1 c2 a), SetLike' c2 (CNF c1 c2 a)) => Heyting (CNF c1 c2 a) where
  (~>) = implication

instance (SetLike' c1 (Disjunction c2 a), SetLike' c2 a, Negable a, SetLike' c1 (CNF c1 c2 a), SetLike' c2 (CNF c1 c2 a)) => Boolean (CNF c1 c2 a)

-- DNF

newtype DNF c1 c2 a = DNF (Disjunction c1 (Conjunction c2 a))
  deriving (Generic)

getDNF :: DNF c1 c2 a -> Disjunction c1 (Conjunction c2 a)
getDNF (DNF dnf) = dnf

deriving instance Eq (c1 (Conjunction c2 a)) => Eq (DNF c1 c2 a)
deriving instance Ord (c1 (Conjunction c2 a)) => Ord (DNF c1 c2 a)
deriving instance Show (c1 (Conjunction c2 a)) => Show (DNF c1 c2 a)

instance (SetLike' c1 (Conjunction c2 a), SetLike' c2 a) => MeetSemiLattice (DNF c1 c2 a) where
  DNF a /\ DNF b = DNF (a /\ b)
instance (SetLike' c1 (Conjunction c2 a)) => JoinSemiLattice (DNF c1 c2 a) where
  DNF a \/ DNF b = DNF (a \/ b)
instance (SetLike' c1 (Conjunction c2 a), SetLike' c2 a) => BoundedMeetSemiLattice (DNF c1 c2 a) where
  top = DNF top
instance (SetLike' c1 (Conjunction c2 a)) => BoundedJoinSemiLattice (DNF c1 c2 a) where
  bottom = DNF bottom
instance (SetLike' c1 (Conjunction c2 a), SetLike' c2 a) => Lattice (DNF c1 c2 a)
instance (SetLike' c1 (Conjunction c2 a), SetLike' c2 a) => BoundedLattice (DNF c1 c2 a)

instance (SetLike' c1 (Conjunction c2 a), SetLike' c2 a, Negable a, SetLike' c1 (DNF c1 c2 a), SetLike' c2 (DNF c1 c2 a)) => Negable (DNF c1 c2 a) where
  not = meets . liftSet (joins . liftSet (liftDNF . not) . getConjunction) . getDisjunction . getDNF

instance (SetLike' c1 (Conjunction c2 a), SetLike' c2 a, Negable a, SetLike' c1 (DNF c1 c2 a), SetLike' c2 (DNF c1 c2 a)) => Heyting (DNF c1 c2 a) where
  (~>) = implication

instance (SetLike' c1 (Conjunction c2 a), SetLike' c2 a, Negable a, SetLike' c1 (DNF c1 c2 a), SetLike' c2 (DNF c1 c2 a)) => Boolean (DNF c1 c2 a)

-- Rest

type CNFList a = CNF [] [] a
type DNFList a = DNF [] [] a

type CNFSet a = CNF Set Set a
type DNFSet a = DNF Set Set a

-- | @Boolean a => DNF a -> a@
retractDNF :: (BoundedLattice a, SetLike c1, SetLike c2, SetLikeC c1 a) => DNF c1 c2 a -> a
retractDNF = retractDisjunction . Disjunction . liftSet retractConjunction . getDisjunction . getDNF

-- | @a -> DNF a@
liftDNF :: (SetLike c1, SetLike c2) => a -> DNF c1 c2 a
liftDNF = DNF . liftDisjunction . liftConjunction

-- | @Boolean a => CNF a -> a@
retractCNF :: (BoundedLattice a, SetLike c1, SetLike c2, SetLikeC c1 a) => CNF c1 c2 a -> a
retractCNF = retractConjunction . Conjunction . liftSet retractDisjunction . getConjunction . getCNF

-- | @a -> CNF a@
liftCNF :: (SetLike c1, SetLike c2) => a -> CNF c1 c2 a
liftCNF = CNF . liftConjunction . liftDisjunction
