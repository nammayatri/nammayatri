{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.External.LocationTrackingService.Flow where

import qualified Data.Text as T
import Domain.Types.DriverLocation
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import Domain.Types.Vehicle.Variant (Variant)
import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.API.EndRide as EndRideAPI
import qualified SharedLogic.External.LocationTrackingService.API.NearBy as NearByAPI
import qualified SharedLogic.External.LocationTrackingService.API.StartRide as StartRideAPI
import SharedLogic.External.LocationTrackingService.Types

rideStart :: (CoreMetrics m, MonadFlow m) => LocationTrackingeServiceConfig -> Id DR.Ride -> Double -> Double -> Id DM.Merchant -> Id DP.Person -> m APISuccess
rideStart rsCfg rideId lat lon merchantId driverId = do
  let url = rsCfg.url
  let req =
        StartRideReq
          { lat,
            lon,
            merchantId,
            driverId
          }
  callAPI url (StartRideAPI.startRide rideId req) "rideStart" StartRideAPI.locationTrackingServiceAPI
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_START_RIDE_API") url)

rideEnd :: (CoreMetrics m, MonadFlow m) => LocationTrackingeServiceConfig -> Id DR.Ride -> Double -> Double -> Id DM.Merchant -> Id DP.Person -> m EndRideRes
rideEnd rsCfg rideId lat lon merchantId driverId = do
  let url = rsCfg.url
  let req =
        EndRideReq
          { lat,
            lon,
            merchantId,
            driverId
          }
  callAPI url (EndRideAPI.endRide rideId req) "rideEnd" EndRideAPI.locationTrackingServiceAPI
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_END_RIDE_API") url)

nearBy :: (CoreMetrics m, MonadFlow m) => LocationTrackingeServiceConfig -> Double -> Double -> Maybe Variant -> Int -> Id DM.Merchant -> m [DriverLocation]
nearBy rsCfg lat lon vehicle_typ radius merchantId = do
  let url = rsCfg.url
  let vehicleType = T.pack . show <$> vehicle_typ
  let vt = case vehicleType of
        Just v -> v
        _ -> ""
  let req =
        NearByReq
          { lat,
            lon,
            radius,
            vehicleType = vt,
            merchantId = merchantId
          }
  callAPI url (NearByAPI.nearBy req) "nearBy" NearByAPI.locationTrackingServiceAPI
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NEAR_BY_API") url)
