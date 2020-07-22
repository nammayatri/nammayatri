module Main where

import AppCancelRide as CR
import AppCaseList as CL
import AppConfirmRide as ACR
import EulerHS.Prelude
import Fixtures (startServers)
import HealthCheck as HC
import SuccessFlow as SF
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = do
  acrSpec <- testSpec "AppConfirmRide" ACR.spec
  clSpec <- testSpec "AppCaseList" CL.spec
  sfSpec <- testSpec "AppCancelRide" SF.spec
  crSpec <- testSpec "SuccessFlow" CR.spec
  hcSpec <- testSpec "HealthCheck" HC.spec

  return $
    withResource
      startServers
      (\(appTid, tbeTid, gatewayTid) -> killThread appTid >> killThread tbeTid >> killThread gatewayTid)
      ( \_ ->
          testGroup
            "tests"
            [ testGroup "HealthCheck" [hcSpec],
              after AllSucceed "HealthCheck" $ testGroup "Other" [clSpec, acrSpec, sfSpec, crSpec]
            ]
      )
