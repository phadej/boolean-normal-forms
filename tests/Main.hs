module Main (main) where

import qualified NNF
import qualified DNF
import qualified CNF

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
  NNF.tests,
  DNF.tests,
  CNF.tests
  ]
