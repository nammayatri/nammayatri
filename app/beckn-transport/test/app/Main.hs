module Main where

import EulerHS.Prelude
import qualified Logic.ActiveDrivers
import Test.Tasty

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = do
  let unitTests = testGroup "Unit tests" [logicTests]
  return $ testGroup "Tests" [unitTests]

logicTests :: TestTree
logicTests =
  testGroup
    "Logic tests"
    [Logic.ActiveDrivers.runTests]
