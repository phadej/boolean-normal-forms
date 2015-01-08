{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
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
module Algebra.Boolean.NormalForms (
  -- * Negation normal form
  -- | <<doc-formulae/nnf.svg>>
  NNF,
  liftNNF,
  lowerNNF,
  -- * Disjunction normal form
  -- | <<doc-formulae/dnf.svg>>
  DNF,
  liftDNF,
  lowerDNF,
  DNF',
  -- * Conjunction normal form
  -- | <<doc-formulae/cnf.svg>>
  CNF,
  liftCNF,
  lowerCNF,
  CNF',
  -- * Module re-exports
  module Algebra.Lattice.Extras.Levitated,
  module Algebra.Boolean,
  module Data.Disjunction,
  module Data.Conjunction
  ) where

import Algebra.Lattice.Extras.Levitated
import Algebra.Boolean hiding (liftLevitated, lowerLevitated, FreeBoundedLattice)
import Data.Disjunction
import Data.Conjunction
import Data.SetLike

type NNF a = FreeBoundedLattice (Neg a)

liftNNF :: a -> NNF a
liftNNF = liftLevitated . liftFreeLattice . liftNeg

lowerNNF :: Boolean a => NNF a -> a
lowerNNF = lowerLevitated . fmap (lowerFreeLattice . fmap lowerNeg)

type CNF' c1 c2 a = Conjunction c1 (Disjunction c2 a)
type DNF' c1 c2 a = Disjunction c1 (Conjunction c2 a)

type CNF a = CNF' [] [] a
type DNF a = DNF' [] [] a

-- | @Boolean a => DNF a -> a@
lowerDNF :: (Boolean a, SetLike c1, SetLike c2, SetLikeC c1 a) => DNF' c1 c2 a -> a
lowerDNF = lowerDisjunction . Disjunction . endoMap lowerConjunction . getDisjunction

-- | @a -> DNF a@
liftDNF :: (SetLike c1, SetLike c2) => a -> DNF' c1 c2 a
liftDNF = liftDisjunction . liftConjunction

-- | @Boolean a => CNF a -> a@
lowerCNF :: (Boolean a, SetLike c1, SetLike c2, SetLikeC c1 a) => CNF' c1 c2 a -> a
lowerCNF = lowerConjunction . Conjunction . endoMap lowerDisjunction . getConjunction

-- | @a -> CNF a@
liftCNF :: (SetLike c1, SetLike c2) => a -> CNF' c1 c2 a
liftCNF = liftConjunction . liftDisjunction
