module Mobility.Transporter.Spec where

import qualified Beckn.External.Maps as Maps
import EulerHS.Prelude
import qualified Mobility.Transporter.AppCancelRide as CR
import qualified Mobility.Transporter.DriverCancelRide as DCR
import qualified Mobility.Transporter.DriversRejectRide as DRR
import qualified Mobility.Transporter.HealthCheck as HC
import qualified Mobility.Transporter.LocationUpdates as LU
import qualified Mobility.Transporter.MapsConfig as MapsConfig
import qualified Mobility.Transporter.NearestDrivers as ND
import qualified Mobility.Transporter.Serviceability as SRV
import qualified Mobility.Transporter.Utils as Utils
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: Maps.MapsServiceConfig -> IO TestTree
mkTestTree googleCfg = do
  hcSpec <- testSpec "HealthCheck" HC.spec
  mapsCfgSpec <- testSpec "MapsConfig" MapsConfig.spec
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
          withResource
            Utils.clearCachedMapsConfig
            (const $ pure ())
            $ \_ ->
              testGroup
                "Merchant configs"
                [mapsCfgSpec],
        after AllSucceed "Merchant configs" $
          withResource
            (Utils.changeCachedMapsConfig googleCfg)
            (const Utils.clearCachedMapsConfig)
            $ \_ ->
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
