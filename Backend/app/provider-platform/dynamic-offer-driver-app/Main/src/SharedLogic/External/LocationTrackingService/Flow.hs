{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.External.LocationTrackingService.Flow where

import qualified Data.Either as Either
import Domain.Types.DriverLocation
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import Domain.Types.VehicleVariant
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.API.DriverBlockTill as DriverBlockTill
import qualified SharedLogic.External.LocationTrackingService.API.DriverLocation as DriverLocationAPI
import qualified SharedLogic.External.LocationTrackingService.API.DriversLocation as DriversLocationAPI
import qualified SharedLogic.External.LocationTrackingService.API.EndRide as EndRideAPI
import qualified SharedLogic.External.LocationTrackingService.API.NearBy as NearByAPI
import qualified SharedLogic.External.LocationTrackingService.API.RideDetails as RideDetailsAPI
import qualified SharedLogic.External.LocationTrackingService.API.StartRide as StartRideAPI
import SharedLogic.External.LocationTrackingService.Types

rideStart :: (CoreMetrics m, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig], HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => Id DR.Ride -> Double -> Double -> Id DM.Merchant -> Id DP.Person -> Maybe RideInfo -> m APISuccess
rideStart rideId lat lon merchantId driverId rideInfo = do
  ltsCfg <- asks (.ltsCfg)
  let url = ltsCfg.url
  let req =
        StartRideReq
          { lat,
            lon,
            merchantId,
            driverId,
            rideInfo
          }
  rideStartRes <-
    withShortRetry $
      callAPI url (StartRideAPI.startRide rideId req) "rideStart" StartRideAPI.locationTrackingServiceAPI
        >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_START_RIDE_API") url)
  logDebug $ "lts rideStart: " <> show rideStartRes
  return rideStartRes

rideEnd :: (CoreMetrics m, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig], HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => Id DR.Ride -> Double -> Double -> Id DM.Merchant -> Id DP.Person -> Maybe (Id DR.Ride) -> Maybe RideInfo -> m EndRideRes
rideEnd rideId lat lon merchantId driverId mbNextRideId rideInfo = do
  ltsCfg <- asks (.ltsCfg)
  let url = ltsCfg.url
  let req =
        EndRideReq
          { lat,
            lon,
            merchantId,
            driverId,
            nextRideId = mbNextRideId,
            rideInfo = rideInfo
          }
  rideEndRes <-
    withShortRetry $
      callAPI url (EndRideAPI.endRide rideId req) "rideEnd" EndRideAPI.locationTrackingServiceAPI
        >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_END_RIDE_API") url)
  logDebug $ "lts rideEnd: " <> show rideEndRes
  return rideEndRes

nearBy :: (CoreMetrics m, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig], HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m, Forkable m) => Double -> Double -> Maybe Bool -> Maybe [VehicleVariant] -> Int -> Id DM.Merchant -> Maybe Text -> Maybe Text -> m [DriverLocation]
nearBy lat lon onRide vt radius merchantId groupId groupId2 = do
  ltsCfg <- asks (.ltsCfg)
  let req =
        NearByReq
          { lat,
            lon,
            onRide,
            radius,
            vehicleType = vt,
            merchantId = merchantId,
            groupId,
            groupId2
          }
  -- Call both APIs (primary and secondary cloud) concurrently and combine results
  let callNearByAPI url = do
        withShortRetry $
          callAPI url (NearByAPI.nearBy req) "nearBy" NearByAPI.locationTrackingServiceAPI
            >>= \case
              Right locations -> pure locations
              Left err -> do
                logError $ "Failed to call nearBy API for url: " <> show url <> ", error: " <> show err
                pure []

  primaryAwaitable <- awaitableFork "primaryLTS" $ callNearByAPI ltsCfg.url
  mbSecondaryAwaitable <- forM ltsCfg.secondaryUrl $ awaitableFork "secondaryLTS" . callNearByAPI

  primaryResult <- Either.fromRight [] <$> L.await Nothing primaryAwaitable
  secondaryResult <- maybe (pure []) (fmap (Either.fromRight []) . L.await Nothing) mbSecondaryAwaitable

  let combinedLocations = primaryResult <> secondaryResult
  logDebug $ "lts nearBy: " <> show combinedLocations
  return combinedLocations

rideDetails :: (CoreMetrics m, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig], HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => Id DR.Ride -> DR.RideStatus -> Id DM.Merchant -> Id DP.Person -> Double -> Double -> Maybe Bool -> Maybe RideInfo -> m APISuccess
rideDetails rideId rideStatus merchantId driverId lat lon isFutureRide rideInfo = do
  ltsCfg <- asks (.ltsCfg)
  let url = ltsCfg.url
  let req =
        RideDetailsReq
          { rideId,
            rideStatus,
            merchantId,
            driverId,
            lat,
            lon,
            isFutureRide,
            rideInfo
          }
  rideDetailsRes <-
    withShortRetry $
      callAPI url (RideDetailsAPI.rideDetails req) "rideDetails" RideDetailsAPI.locationTrackingServiceAPI
        >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_RIDE_DETAILS_API") url)
  logDebug $ "lts rideDetails: " <> show rideDetailsRes
  return rideDetailsRes

driversLocation :: (CoreMetrics m, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig], HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => [Id DP.Person] -> m [DriverLocation]
driversLocation driverIds = do
  ltsCfg <- asks (.ltsCfg)
  let url = ltsCfg.url
  let req =
        DriversLocationReq
          { driverIds
          }
  driversLocationRes <-
    withShortRetry $
      callAPI url (DriversLocationAPI.driversLocation req) "driversLocation" DriversLocationAPI.locationTrackingServiceAPI
        >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_DRIVERS_LOCATION_API") url)
  logDebug $ "lts driversLocation: " <> show driversLocationRes
  return driversLocationRes

driverLocation :: (CoreMetrics m, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig], HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => Id DR.Ride -> Id DM.Merchant -> Id DP.Person -> m DriverLocationResp
driverLocation rideId merchantId driverId = do
  ltsCfg <- asks (.ltsCfg)
  let url = ltsCfg.url
  let req =
        DriverLocationReq
          { driverId,
            merchantId
          }
  driverLocationRes <-
    withShortRetry $
      callAPI url (DriverLocationAPI.driverLocation rideId req) "driverLocation" DriverLocationAPI.locationTrackingServiceAPI
        >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_DRIVER_LOCATION_API") url)
  logDebug $ "lts driverLocation: " <> show driverLocationRes
  return driverLocationRes

blockDriverLocationsTill :: (CoreMetrics m, MonadFlow m, HasLocationService m r, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => Id DM.Merchant -> Id DP.Person -> UTCTime -> m APISuccess
blockDriverLocationsTill merchantId driverId blockTill = do
  ltsCfg <- asks (.ltsCfg)
  let url = ltsCfg.url
  let req =
        DriverBlockTillReq
          { ..
          }
  blockLocationsTillResp <-
    withShortRetry $
      callAPI url (DriverBlockTill.blockDriverLocationsTill req) "driverBlockTill" DriverBlockTill.locationTrackingServiceAPI
        >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_DRIVER_BLOCK_TILL_API") url)
  logDebug $ "lts driver block till: " <> show blockLocationsTillResp
  return blockLocationsTillResp
