--------------------------------------------------------------------
-- |
-- Copyright :  © Oleg Grenrus 2014
-- License   :  MIT
-- Maintainer:  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Algebra.Boolean.CoBoolean (
  CoBoolean(..),
  CoBoolean1(..),
  toBool,
  toBoolWith
) where

import Data.Algebra.Boolean

-- | A class to values translable to booleans.
class CoBoolean a where
  -- | Cast value to 'Boolean'.
  toBoolean :: Boolean b => a -> b

-- | Less polymorphic version of 'toBoolean'.
toBool :: CoBoolean a => a -> Bool
toBool = toBoolean

-- | A polymorphic class of values translable to booleans.
class CoBoolean1 b where
  -- | Cast value to 'Boolean'.
  toBooleanWith :: Boolean c => (a -> c) -> b a -> c

-- | Less polymorphic version of 'toBooleanWith'.
toBoolWith :: CoBoolean1 b => (a -> Bool) -> b a -> Bool
toBoolWith = toBooleanWith

instance CoBoolean Bool where
  toBoolean = fromBool

instance (CoBoolean f, CoBoolean g) => CoBoolean (Either f g) where
  toBoolean (Left x)   = toBoolean x
  toBoolean (Right x)  = toBoolean x

instance CoBoolean (Maybe a) where
  toBoolean (Just _) = true
  toBoolean Nothing  = false

instance CoBoolean1 Maybe where
  toBooleanWith f (Just x) = f x
  toBooleanWith _ Nothing  = false
