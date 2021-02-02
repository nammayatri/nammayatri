module Main where

import EulerHS.Prelude
import FareCalculator
import Flow.Allocation
import Test.Tasty

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = do
  let unitTests = testGroup "Unit tests" [flowTests, fareCalculator, allocation]
  return $ testGroup "Tests" [unitTests]

flowTests :: TestTree
flowTests =
  testGroup
    "Flow tests"
    []
