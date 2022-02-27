module Mobility.Spec where

import EulerHS.Prelude
import qualified Mobility.AppCancelRide as CR
import qualified Mobility.DriverCancelRide as DCR
import qualified Mobility.DriversRejectRide as DRR
import qualified Mobility.HealthCheck as HC
import qualified Mobility.LocationUpdates as LU
import qualified Mobility.NearestDrivers as ND
import qualified Mobility.Serviceability as SRV
import qualified Mobility.SuccessFlow as SF
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  hcSpec <- testSpec "HealthCheck" HC.spec
  sfSpec <- testSpec "SuccessFlow" SF.spec
  drrSpec <- testSpec "DriversRejectRide" DRR.spec
  crSpec <- testSpec "AppCancelRide" CR.spec
  dcrSpec <- testSpec "DriverCancelRide" DCR.spec
  srvSpec <- testSpec "Serviceability" SRV.spec
  ndSpec <- testSpec "NearestDriver" ND.spec

  -- these tests pass only when the real google maps api key is supplied
  locationUpdatesSpec <- testSpec "LocationUpdates" LU.spec
  ------------------------------------------------------------------
  return $
    testGroup
      "Mobility"
      [ hcSpec,
        after AllSucceed "HealthCheck" $
          testGroup
            "APIs"
            [ ndSpec,
              srvSpec,
              sfSpec,
              after AllSucceed "SuccessFlow" $
                testGroup
                  "Flows"
                  [ drrSpec,
                    crSpec,
                    dcrSpec
                  ]
            ],
        testGroup "LocationUpdates" [locationUpdatesSpec]
      ]
