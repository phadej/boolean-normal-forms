{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Algebra.Boolean
-- Copyright   : (c) 2014-2015 Oleg Grenrus
-- License     : MIT
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Algebra.Boolean (
  -- * Boolean
  Boolean(..),
  all,
  any,
  and,
  or,
  implication,
  -- * Heyting
  Heyting(..),
  -- * Free structure
  FreeBoolean(..),
  liftFreeBoolean,
  retractFreeBoolean,
  -- * Module re-exports
  module Algebra.Lattice,
  module Algebra.Negable
  ) where

import Algebra.Lattice
import Algebra.Lattice.Levitated
import Algebra.Negable
import Control.Applicative
import Control.Monad
import Data.Foldable hiding (all, any, and, or)
import GHC.Generics (Generic, Generic1)
import Prelude hiding ((&&), (||), not, all, any, and, or)
import Test.QuickCheck

import Data.Traversable (Traversable)

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

instance (Negable a, Lattice a) => Heyting (Levitated a) where
  negation = not
  (~>) = implication

-- Boolean

class (Heyting b, Negable b) => Boolean b where
  true :: b
  true = top

  false :: b
  false = bottom

  (&&) :: b -> b -> b
  (&&) = (/\)

  (||) :: b -> b -> b
  (||) = (\/)

  -- Not comes from Negable

instance Boolean Bool where
instance (Boolean a, Boolean b) => Boolean (a, b) where

instance (Negable a, Lattice a) => Boolean (Levitated a) where

-- | `meets . map f`.
all :: (Boolean b, Foldable f) => (a -> b) -> f a -> b
all f = getMeet . foldMap (Meet . f)

-- | `joins . map f`.
any :: (Boolean b, Foldable f) => (a -> b) -> f a -> b
any f = getJoin . foldMap (Join . f)

-- | Alias for `meets`.
and :: (Boolean a, Foldable f) => f a -> a
and = meets

-- | Alias for `joins`.
or :: (Boolean a, Foldable f) => f a -> a
or = joins

-- | Implication.
implication :: Boolean b => b -> b -> b
implication a b = not a || b

-- | Free `Boolean`.
data FreeBoolean a = FBValue a
                   | FBTrue
                   | FBFalse
                   | FBNot (FreeBoolean a)
                   | FBAnd (FreeBoolean a) (FreeBoolean a)
                   | FBOr (FreeBoolean a) (FreeBoolean a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, Generic1)

instance Applicative FreeBoolean where
  pure = return
  (<*>) = ap

instance Monad FreeBoolean where
  return = FBValue
  FBValue x >>= f = f x
  FBTrue    >>= _ = FBTrue
  FBFalse   >>= _ = FBFalse
  FBNot x   >>= f = FBNot (x >>= f)
  FBAnd x y >>= f = FBAnd (x >>= f) (y >>= f)
  FBOr x y  >>= f = FBOr (x >>= f) (y >>= f)

instance JoinSemiLattice (FreeBoolean a) where
  join = FBOr

instance MeetSemiLattice (FreeBoolean a) where
  meet = FBAnd

instance BoundedJoinSemiLattice (FreeBoolean a) where
  bottom = FBFalse

instance BoundedMeetSemiLattice (FreeBoolean a) where
  top = FBTrue

instance Lattice (FreeBoolean a) where
instance BoundedLattice (FreeBoolean a) where

instance Heyting (FreeBoolean a) where
  (~>) = implication
  negation = not

instance Negable (FreeBoolean a) where
  not = FBNot

instance Boolean (FreeBoolean a) where

liftFreeBoolean :: a -> FreeBoolean a
liftFreeBoolean = FBValue

retractFreeBoolean :: Boolean a => FreeBoolean a -> a
retractFreeBoolean (FBValue x) = x
retractFreeBoolean FBTrue = true
retractFreeBoolean FBFalse = false
retractFreeBoolean (FBNot x) = not (retractFreeBoolean x)
retractFreeBoolean (FBAnd x y) = retractFreeBoolean x && retractFreeBoolean y
retractFreeBoolean (FBOr x y) = retractFreeBoolean x || retractFreeBoolean y

instance Arbitrary a => Arbitrary (FreeBoolean a) where
  arbitrary = sized arbitrary'
    where arbitrary' 0 = frequency [
            (10, FBValue <$> arbitrary),
            (1, pure FBTrue),
            (1, pure FBFalse)
            ]
          arbitrary' n = oneof [
            FBValue <$> arbitrary,
            FBNot <$> arbitrary'',
            FBAnd <$> arbitrary'' <*> arbitrary'',
            FBOr <$> arbitrary'' <*> arbitrary''
            ]
            where arbitrary'' = arbitrary' $ n `div` 2
