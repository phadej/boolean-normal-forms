{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module SimplifyLaws (simplifyLaws) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Algebra.Boolean.CoBoolean
import Data.Algebra.Boolean.NormalForm

simplifyProperty :: (CoBoolean a, NormalForm nf, NFConstraint nf (Either a Bool)) => nf (Either a Bool) -> Bool
simplifyProperty nf = toBooleanWith toBoolean (simplify f nf) == (toBooleanWith toBoolean nf :: Bool)
  where f (Right b)  = Just b
        f (Left _)   = Nothing

simplifyLaws :: forall a nf. (CoBoolean a, NormalForm nf, NFConstraint nf (Either a Bool), Show (nf (Either a Bool)), Arbitrary (nf (Either a Bool))) => nf (Either a Bool) -> TestTree
simplifyLaws _ = QC.testProperty "simplify property" prop
  where prop :: nf (Either a Bool) -> Bool
        prop = simplifyProperty
