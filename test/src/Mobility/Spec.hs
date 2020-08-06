module Mobility.Spec where

import EulerHS.Prelude
import Mobility.AppCancelRide as CR
import Mobility.AppCaseList as CL
import Mobility.DriverCancelRide as DCR
import Mobility.Fixtures (startServers)
import Mobility.HealthCheck as HC
import Mobility.SuccessFlow as SF
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  hcSpec <- testSpec "HealthCheck" HC.spec
  clSpec <- testSpec "AppCaseList" CL.spec
  sfSpec <- testSpec "SuccessFlow" SF.spec
  crSpec <- testSpec "AppCancelRide" CR.spec
  dcrSpec <- testSpec "DriverCancelRide" DCR.spec
  return $
    withResource
      startServers
      (\(appTid, tbeTid, gatewayTid) -> traverse_ killThread [appTid, tbeTid, gatewayTid])
      ( \_ ->
          testGroup
            "Mobility"
            [ hcSpec,
              after AllSucceed "HealthCheck" $
                testGroup "APIs" [clSpec, sfSpec, crSpec, dcrSpec]
            ]
      )
