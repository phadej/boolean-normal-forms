module Main (main) where

import qualified NNF
import qualified NNFTree
import qualified NNFSet
import qualified DNFList
import qualified DNFSet
import qualified CNFList
import qualified CNFSet
import qualified FreeBooleanTests as FBT

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
  NNF.tests,
  NNFTree.tests,
  NNFSet.tests,
  CNFList.tests,
  CNFSet.tests,
  DNFList.tests,
  DNFSet.tests,
  FBT.tests
  ]
