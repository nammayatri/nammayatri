{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.External.LocationTrackingService.Flow where

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
import qualified SharedLogic.External.LocationTrackingService.API.DriversLocation as DriversLocationAPI
import qualified SharedLogic.External.LocationTrackingService.API.EndRide as EndRideAPI
import qualified SharedLogic.External.LocationTrackingService.API.NearBy as NearByAPI
import qualified SharedLogic.External.LocationTrackingService.API.RideDetails as RideDetailsAPI
import qualified SharedLogic.External.LocationTrackingService.API.StartRide as StartRideAPI
import SharedLogic.External.LocationTrackingService.Types

rideStart :: (CoreMetrics m, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig]) => Id DR.Ride -> Double -> Double -> Id DM.Merchant -> Id DP.Person -> m APISuccess
rideStart rideId lat lon merchantId driverId = do
  ltsCfg <- asks (.ltsCfg)
  let url = ltsCfg.url
  let req =
        StartRideReq
          { lat,
            lon,
            merchantId,
            driverId
          }
  rideStartRes <-
    callAPI url (StartRideAPI.startRide rideId req) "rideStart" StartRideAPI.locationTrackingServiceAPI
      >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_START_RIDE_API") url)
  logDebug $ "lts rideStart: " <> show rideStartRes
  return rideStartRes

rideEnd :: (CoreMetrics m, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig]) => Id DR.Ride -> Double -> Double -> Id DM.Merchant -> Id DP.Person -> m EndRideRes
rideEnd rideId lat lon merchantId driverId = do
  ltsCfg <- asks (.ltsCfg)
  let url = ltsCfg.url
  let req =
        EndRideReq
          { lat,
            lon,
            merchantId,
            driverId
          }
  rideEndRes <-
    callAPI url (EndRideAPI.endRide rideId req) "rideEnd" EndRideAPI.locationTrackingServiceAPI
      >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_END_RIDE_API") url)
  logDebug $ "lts rideEnd: " <> show rideEndRes
  return rideEndRes

nearBy :: (CoreMetrics m, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig]) => Double -> Double -> Maybe Bool -> Maybe Variant -> Int -> Id DM.Merchant -> m [DriverLocation]
nearBy lat lon onRide vt radius merchantId = do
  ltsCfg <- asks (.ltsCfg)
  let url = ltsCfg.url
  let req =
        NearByReq
          { lat,
            lon,
            onRide,
            radius,
            vehicleType = vt,
            merchantId = merchantId
          }
  nearByRes <-
    callAPI url (NearByAPI.nearBy req) "nearBy" NearByAPI.locationTrackingServiceAPI
      >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NEAR_BY_API") url)
  logDebug $ "lts nearBy: " <> show nearByRes
  return nearByRes

rideDetails :: (CoreMetrics m, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig]) => Id DR.Ride -> DR.RideStatus -> Id DM.Merchant -> Id DP.Person -> Double -> Double -> m APISuccess
rideDetails rideId rideStatus merchantId driverId lat lon = do
  ltsCfg <- asks (.ltsCfg)
  let url = ltsCfg.url
  let req =
        RideDetailsReq
          { rideId,
            rideStatus,
            merchantId,
            driverId,
            lat,
            lon
          }
  rideDetailsRes <-
    callAPI url (RideDetailsAPI.rideDetails req) "rideDetails" RideDetailsAPI.locationTrackingServiceAPI
      >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_RIDE_DETAILS_API") url)
  logDebug $ "lts rideDetails: " <> show rideDetailsRes
  return rideDetailsRes

driversLocation :: (CoreMetrics m, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig]) => [Id DP.Person] -> m [DriverLocation]
driversLocation driverIds = do
  ltsCfg <- asks (.ltsCfg)
  let url = ltsCfg.url
  let req =
        DriversLocationReq
          { driverIds
          }
  driversLocationRes <-
    callAPI url (DriversLocationAPI.driversLocation req) "driversLocation" DriversLocationAPI.locationTrackingServiceAPI
      >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_DRIVERS_LOCATION_API") url)
  logDebug $ "lts driversLocation: " <> show driversLocationRes
  return driversLocationRes
