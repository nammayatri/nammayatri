module Main where

import EulerHS.Prelude
import qualified Flow.ActiveDrivers
import Test.Tasty

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = do
  let unitTests = testGroup "Unit tests" [flowTests]
  return $ testGroup "Tests" [unitTests]

flowTests :: TestTree
flowTests =
  testGroup
    "Flow tests"
    [Flow.ActiveDrivers.runTests]
