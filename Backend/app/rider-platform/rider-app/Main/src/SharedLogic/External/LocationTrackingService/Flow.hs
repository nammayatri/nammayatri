{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.External.LocationTrackingService.Flow where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.API.VehicleTrackingOnRoute as VehicleTracking
import SharedLogic.External.LocationTrackingService.Types

data VehicleTracking = ByRoute Text | ByTrips [Text]

vehicleTrackingOnRoute :: (CoreMetrics m, MonadFlow m, HasLocationService m r) => VehicleTracking -> m [VehicleTrackingOnRouteResp]
vehicleTrackingOnRoute vehicleTracking = do
  ltsCfg <- asks (.ltsCfg)
  let url = ltsCfg.url
  let req =
        case vehicleTracking of
          ByRoute routeCode -> VehicleTrackingOnRouteReq (Just routeCode) Nothing
          ByTrips tripCodes -> VehicleTrackingOnRouteReq Nothing (Just tripCodes)
  vehicleTrackingOnRouteResp <-
    callAPI url (VehicleTracking.vehicleTrackingOnRoute req) "vehicleTrackingOnRoute" VehicleTracking.vehicleTrackingOnRouteAPI
      >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_VEHICLE_TRACKING_API") url)
  logDebug $ "lts vehicle tracking on route: " <> show vehicleTrackingOnRouteResp
  return vehicleTrackingOnRouteResp
