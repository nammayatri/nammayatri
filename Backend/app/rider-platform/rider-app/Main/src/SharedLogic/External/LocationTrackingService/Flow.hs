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
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.API.NearbyDrivers as NearByAPI
import qualified SharedLogic.External.LocationTrackingService.API.Sos as SosAPI
import qualified SharedLogic.External.LocationTrackingService.API.VehicleTrackingOnRoute as VehicleTracking
import SharedLogic.External.LocationTrackingService.Types

data VehicleTracking = ByRoute Text | ByTrips [Text]

vehicleTrackingOnRoute :: (CoreMetrics m, MonadFlow m, HasLocationService m r, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => VehicleTracking -> m [VehicleTrackingOnRouteResp]
vehicleTrackingOnRoute vehicleTracking = do
  ltsCfg <- asks (.ltsCfg)
  let url = localUrl ltsCfg
  let req =
        case vehicleTracking of
          ByRoute routeCode -> VehicleTrackingOnRouteReq (Just routeCode) Nothing
          ByTrips tripCodes -> VehicleTrackingOnRouteReq Nothing (Just tripCodes)
  vehicleTrackingOnRouteResp <-
    withShortRetry $
      callAPI url (VehicleTracking.vehicleTrackingOnRoute req) "vehicleTrackingOnRoute" VehicleTracking.vehicleTrackingOnRouteAPI
        >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_VEHICLE_TRACKING_API") url)
  logDebug $ "lts vehicle tracking on route: " <> show vehicleTrackingOnRouteResp
  return vehicleTrackingOnRouteResp

nearBy :: (CoreMetrics m, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig], HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m, Forkable m) => NearByDriverReq -> m [NearByDriverRes]
nearBy req = do
  ltsCfg <- asks (.ltsCfg)
  let callNearByAPI url = do
        withShortRetry $
          callAPI url (NearByAPI.nearBy req) "nearBy" NearByAPI.locationTrackingServiceAPI
            >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NEAR_BY_API") url)

  let peerCallNearByAPI url = do
        withShortRetry $
          callAPI url (NearByAPI.nearBy req) "nearBy" NearByAPI.locationTrackingServiceAPI
            >>= \case
              Right locations -> pure locations
              Left err -> do
                logError $ "Failed to call nearBy API for peer url: " <> show url <> ", error: " <> show err
                pure []

  localAwaitable <- awaitableFork "localLTS" $ callNearByAPI (localUrl ltsCfg)
  peerAwaitables <- forM (peerUrls ltsCfg) $ \(_, url) -> awaitableFork "peerLTS" (peerCallNearByAPI url)

  localResult <-
    L.await Nothing localAwaitable >>= \case
      Left err -> throwError $ InternalError $ "Failed to call nearBy API for local url: " <> show (localUrl ltsCfg) <> ", error: " <> show err
      Right result -> pure result
  peerResults <- forM peerAwaitables $ \awaitable -> Either.fromRight [] <$> L.await Nothing awaitable

  let combinedLocations = localResult <> concat peerResults
  logDebug $ "lts nearBy: " <> show combinedLocations
  return combinedLocations

entityUpsertForSos :: (CoreMetrics m, MonadFlow m, HasLocationService m r, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => Text -> Text -> Text -> Maybe SosErssConfigReq -> m ()
entityUpsertForSos sosId personId merchantId mbBroadcasterConfig = do
  ltsCfg <- asks (.ltsCfg)
  let url = localUrl ltsCfg
      req = SosAPI.EntityUpsertReq {personId = personId, merchantId = merchantId, entityInfo = SosAPI.EntityStart, broadcasterConfig = mbBroadcasterConfig}
  void $
    withShortRetry $
      callAPI url (SosAPI.entityUpsertForSos sosId req) "entityUpsertForSos" SosAPI.entityUpsertAPI
        >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_ENTITY_UPSERT_API") url)
