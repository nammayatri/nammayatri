 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.Transporter.Spec where

import EulerHS.Prelude
import qualified Mobility.Transporter.AppCancelRide as CR
import qualified Mobility.Transporter.DriverCancelRide as DCR
import qualified Mobility.Transporter.DriversRejectRide as DRR
import qualified Mobility.Transporter.HealthCheck as HC
import qualified Mobility.Transporter.LocationUpdates as LU
import qualified Mobility.Transporter.MapsConfig as MapsConfig
import qualified Mobility.Transporter.NearestDrivers as ND
import qualified Mobility.Transporter.Serviceability as SRV
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
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
          testGroup
            "Merchant configs"
            [mapsCfgSpec],
        after AllSucceed "Merchant configs" $
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
