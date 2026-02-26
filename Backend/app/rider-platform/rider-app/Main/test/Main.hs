module Main (main) where

import Kernel.Prelude
import PPF.ReconTest (reconTests)
import PPF.StateMachineTest (stateMachineTests)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs =
  return $
    testGroup
      "Rider App Tests"
      [ testGroup
          "PPF (Payment Protection Framework)"
          [ stateMachineTests,
            reconTests
          ]
      ]
