--------------------------------------------------------------------
-- |
-- Copyright :  © Oleg Grenrus 2014
-- License   :  MIT
-- Maintainer:  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Algebra.Boolean (
  Boolean(..),
  all,
  any,
  and,
  or,
  -- * Module re-exports
  module Algebra.Heyting,
  module Algebra.Negable
  ) where

import Algebra.Heyting
import Algebra.Negable
import Data.Foldable hiding (all, any, and, or)
import Prelude hiding ((&&), (||), not, all, any, and, or)

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

-- | `meets . map f`
all :: (Boolean b, Foldable f) => (a -> b) -> f a -> b
all f = getMeet . foldMap (Meet . f)

-- | `joins . map f`
any :: (Boolean b, Foldable f) => (a -> b) -> f a -> b
any f = getJoin . foldMap (Join . f)

-- | Alias for `meets`
and :: (Boolean a, Foldable f) => f a -> a
and = meets

or :: (Boolean a, Foldable f) => f a -> a
or = joins