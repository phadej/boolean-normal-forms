--------------------------------------------------------------------
-- |
-- Copyright :  © Oleg Grenrus 2014
-- License   :  MIT
-- Maintainer:  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Algebra.Boolean where

import Algebra.Heyting
import Algebra.Negable

import Prelude hiding ((&&), (||), not)

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
