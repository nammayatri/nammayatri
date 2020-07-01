module Main where

import AppCaseList as CL
import AppConfirmRide as ACR
import EulerHS.Prelude
import HealthCheck as HC
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = do
  acrSpec <- testSpec "AppConfirmRide" ACR.spec
  clSpec <- testSpec "AppCaseList" CL.spec
  hcSpec <- testSpec "HealthCheck" HC.spec

  return $ testGroup "tests" [acrSpec, clSpec, hcSpec]
