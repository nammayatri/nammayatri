module Mobility.ARDU.Spec where

import EulerHS.Prelude
{-
import qualified Mobility.ARDU.AppCancelRide as CR
import qualified Mobility.ARDU.DriverCancelRide as DCR
import qualified Mobility.ARDU.DriversRejectRide as DRR
-}
import qualified Mobility.ARDU.HealthCheck as HC
--import qualified Mobility.ARDU.LocationUpdates as LU
--import qualified Mobility.ARDU.NearestDrivers as ND
--import qualified Mobility.ARDU.Serviceability as SRV
import qualified Mobility.ARDU.SuccessFlow as SF
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  hcSpec <- testSpec "HealthCheck" HC.spec
  sfSpec <- testSpec "SuccessFlow" SF.spec
  --  drrSpec <- testSpec "DriversRejectRide" DRR.spec
  --  crSpec <- testSpec "AppCancelRide" CR.spec
  --  dcrSpec <- testSpec "DriverCancelRide" DCR.spec
  --  srvSpec <- testSpec "Serviceability" SRV.spec
  --  ndSpec <- testSpec "NearestDriver" ND.spec

  --  locationUpdatesSpec <- testSpec "LocationUpdates" LU.spec
  ------------------------------------------------------------------
  return $
    testGroup
      "Mobility"
      [ hcSpec,
        after AllSucceed "HealthCheck" $
          testGroup
            "APIs"
            [ -- ndSpec,
              -- srvSpec,
              sfSpec
              {-
                            after AllSucceed "SuccessFlow" $
                              testGroup
                                "Flows"
                                [ drrSpec,
                                  crSpec,
                                  dcrSpec
                                ]
              -}
            ]
            --              testGroup "LocationUpdates" [locationUpdatesSpec]
      ]
