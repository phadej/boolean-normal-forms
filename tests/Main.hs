module Main (main) where

import qualified NNF

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
  NNF.tests
  ]
