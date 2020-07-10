module Main where

import AppCaseList as CL
import AppConfirmRide as ACR
import EulerHS.Prelude
import Fixtures (startServers)
import HealthCheck as HC
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = do
  acrSpec <- testSpec "AppConfirmRide" ACR.spec
  clSpec <- testSpec "AppCaseList" CL.spec
  hcSpec <- testSpec "HealthCheck" HC.spec

  return $
    withResource
      startServers
      (\(appTid, tbeTid) -> killThread appTid >> killThread tbeTid)
      ( \_ ->
          testGroup
            "tests"
            [ testGroup "HealthCheck" [hcSpec],
              after AllSucceed "HealthCheck" $ testGroup "Other" [clSpec, acrSpec]
            ]
      )
