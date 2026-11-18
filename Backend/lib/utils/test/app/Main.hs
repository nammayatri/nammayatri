module Main (main) where

import DynamicLogicSpec (dynamicLogicTests)
import Kernel.Prelude
import Test.Tasty

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs =
  return $ testGroup "Tests" [unitTests]
  where
    unitTests =
      testGroup
        "Unit tests"
        [ dynamicLogicTests
        ]
