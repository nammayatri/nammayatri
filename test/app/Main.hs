module Main where

import AppCancelRide as CR
import AppCaseList as CL
import DriverCancelRide as DCR
import EulerHS.Prelude
import Fixtures (startServers)
import HealthCheck as HC
import MockAppSearch as MAS
import SuccessFlow as SF
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = do
  clSpec <- testSpec "AppCaseList" CL.spec
  sfSpec <- testSpec "SuccessFlow" SF.spec
  crSpec <- testSpec "AppCancelRide" CR.spec
  dcrSpec <- testSpec "DriverCancelRide" DCR.spec
  hcSpec <- testSpec "HealthCheck" HC.spec
  maSpec <- testSpec "MockAppSearch" MAS.spec
  return $
    withResource
      startServers
      (\(appTid, tbeTid, gatewayTid, mockAppTid, mockProvTid) -> traverse_ killThread [appTid, tbeTid, gatewayTid, mockAppTid, mockProvTid])
      ( \_ ->
          testGroup
            "tests"
            [ testGroup "HealthCheck" [hcSpec],
              after AllSucceed "HealthCheck" $ testGroup "Other" [clSpec, sfSpec, maSpec, crSpec, dcrSpec]
            ]
      )
