module Main (main) where

import KaalChakraJobs (kaalChakraJobsTests)
import KaalChakraTemplate (kaalChakraTemplateTests)
import Kernel.Prelude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs =
  return $ testGroup "Tests" [unitTests]
  where
    unitTests =
      testGroup
        "Unit tests"
        [ kaalChakraJobsTests,
          kaalChakraTemplateTests
        ]
