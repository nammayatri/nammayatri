module Mobility.Transporter.Spec where

import EulerHS.Prelude
import qualified Mobility.Transporter.AppCancelRide as CR
import qualified Mobility.Transporter.DriverCancelRide as DCR
import qualified Mobility.Transporter.DriversRejectRide as DRR
import qualified Mobility.Transporter.HealthCheck as HC
import qualified Mobility.Transporter.LocationUpdates as LU
import qualified Mobility.Transporter.NearestDrivers as ND
import qualified Mobility.Transporter.Serviceability as SRV
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  hcSpec <- testSpec "HealthCheck" HC.spec
  drrSpec <- testSpec "DriversRejectRide" DRR.spec
  crSpec <- testSpec "AppCancelRide" CR.spec
  dcrSpec <- testSpec "DriverCancelRide" DCR.spec
  srvSpec <- testSpec "Serviceability" SRV.spec
  ndSpec <- testSpec "NearestDriver" ND.spec

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
              drrSpec,
              crSpec,
              dcrSpec,
              locationUpdatesSpec
            ]
      ]
