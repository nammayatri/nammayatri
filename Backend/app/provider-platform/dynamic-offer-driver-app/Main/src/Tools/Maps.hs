{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Maps
  ( module Reexport,
    autoComplete,
    getDistance,
    getEstimatedPickupDistances,
    getDistances,
    getPlaceDetails,
    getPlaceName,
    getRoutes,
    snapToRoad,
    snapToRoadWithFallback,
    getPickupRoutes,
    getTripRoutes,
    getDistanceForCancelRide,
    getServiceConfigForRectifyingSnapToRoadDistantPointsFailure,
    getDistanceForScheduledRides,
  )
where

import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig as DOSC
import Domain.Types.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Utils as Reexport hiding
  ( autoComplete,
    getDistance,
    getDistances,
    getPlaceDetails,
    getPlaceName,
    getRoutes,
    snapToRoad,
    snapToRoadWithFallback,
  )
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Cac.MerchantServiceUsageConfig as QOMC
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC
import Tools.Error

getDistance ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b,
    ToJSON a,
    ToJSON b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maybe Text ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistance = runWithServiceConfig Maps.getDistance (.getDistances)

getDistanceForCancelRide ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b,
    ToJSON a,
    ToJSON b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maybe Text ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistanceForCancelRide = runWithServiceConfig Maps.getDistance (.getDistancesForCancelRide)

getDistances ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b,
    ToJSON a,
    ToJSON b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maybe Text ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances = runWithServiceConfig Maps.getDistances (.getDistances)

getEstimatedPickupDistances ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b,
    ToJSON a,
    ToJSON b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maybe Text ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getEstimatedPickupDistances = runWithServiceConfig Maps.getDistances (.getEstimatedPickupDistances)

getDistanceForScheduledRides ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b,
    ToJSON a,
    ToJSON b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maybe Text ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistanceForScheduledRides = runWithServiceConfig Maps.getDistance (.getDistancesForScheduledRides)

getRoutes :: (ServiceFlow m r) => Id Merchant -> Id MerchantOperatingCity -> Maybe Text -> GetRoutesReq -> m GetRoutesResp
getRoutes merchantId merchantOpCityId entityId req = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantNotFound merchantOpCityId.getId)
  runWithServiceConfig (callGetRoutesWrapper transporterConfig.isAvoidToll) (.getRoutes) merchantId merchantOpCityId entityId req

getPickupRoutes :: (ServiceFlow m r) => Id Merchant -> Id MerchantOperatingCity -> Maybe Text -> GetRoutesReq -> m GetRoutesResp
getPickupRoutes merchantId merchantOpCityId entityId req = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantNotFound merchantOpCityId.getId)
  runWithServiceConfig (callGetRoutesWrapper transporterConfig.isAvoidToll) (.getPickupRoutes) merchantId merchantOpCityId entityId req

getTripRoutes :: (ServiceFlow m r) => Id Merchant -> Id MerchantOperatingCity -> Maybe Text -> GetRoutesReq -> m GetRoutesResp
getTripRoutes merchantId merchantOpCityId entityId req = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantNotFound merchantOpCityId.getId)
  runWithServiceConfig (callGetRoutesWrapper transporterConfig.isAvoidToll) (.getTripRoutes) merchantId merchantOpCityId entityId req

snapToRoad ::
  ( ServiceFlow m r
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maybe Text ->
  SnapToRoadReq ->
  m SnapToRoadResp
snapToRoad = runWithServiceConfig Maps.snapToRoad (.snapToRoad)

autoComplete :: (ServiceFlow m r, HasShortDurationRetryCfg r c) => Id Merchant -> Id MerchantOperatingCity -> Maybe Text -> AutoCompleteReq -> m AutoCompleteResp
autoComplete = runWithServiceConfig Maps.autoComplete (.autoComplete)

getPlaceName :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> Maybe Text -> GetPlaceNameReq -> m GetPlaceNameResp
getPlaceName = runWithServiceConfig Maps.getPlaceName (.getPlaceName)

getPlaceDetails :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> Maybe Text -> GetPlaceDetailsReq -> m GetPlaceDetailsResp
getPlaceDetails = runWithServiceConfig Maps.getPlaceDetails (.getPlaceDetails)

snapToRoadWithFallback ::
  ( ServiceFlow m r,
    HasFlowEnv m r '["snapToRoadSnippetThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["droppedPointsThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["maxStraightLineRectificationThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["osrmMatchThreshold" ::: HighPrecMeters]
  ) =>
  Maybe MapsServiceConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Bool ->
  Maybe Text ->
  SnapToRoadReq ->
  m ([Maps.MapsService], Either String SnapToRoadResp)
snapToRoadWithFallback rectifyDistantPointsFailureUsing _merchantId merchantOperatingCityId includeRectifiedDistance entityId = Maps.snapToRoadWithFallback entityId rectifyDistantPointsFailureUsing includeRectifiedDistance handler
  where
    handler = Maps.SnapToRoadHandler {..}

    getConfidenceThreshold = do
      transporterConfig <- SCTC.findByMerchantOpCityId merchantOperatingCityId Nothing >>= fromMaybeM (MerchantNotFound merchantOperatingCityId.getId)
      pure $ transporterConfig.snapToRoadConfidenceThreshold

    getProvidersList = do
      merchantConfig <- QOMC.findByMerchantOpCityId merchantOperatingCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
      let snapToRoadProvidersList = merchantConfig.snapToRoadProvidersList
      when (null snapToRoadProvidersList) $ throwError $ InternalError ("No snap to road service provider configured for the merchant, merchantOperatingCityId:" <> merchantOperatingCityId.getId)
      pure snapToRoadProvidersList

    getProviderConfig provider = do
      merchantMapsServiceConfig <-
        QOMSC.findByServiceAndCity (DOSC.MapsService provider) merchantOperatingCityId
          >>= fromMaybeM (MerchantServiceConfigNotFound merchantOperatingCityId.getId "Maps" (show provider))
      case merchantMapsServiceConfig.serviceConfig of
        DOSC.MapsServiceConfig msc -> pure msc
        _ -> throwError $ InternalError "Unknown Service Config"

callGetRoutesWrapper :: ServiceFlow m r => Bool -> Maybe Text -> MapsServiceConfig -> GetRoutesReq -> m GetRoutesResp
callGetRoutesWrapper isAvoidToll entityId = Maps.getRoutes entityId isAvoidToll

getServiceConfigForRectifyingSnapToRoadDistantPointsFailure ::
  ServiceFlow m r =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  m MapsServiceConfig
getServiceConfigForRectifyingSnapToRoadDistantPointsFailure _merchantId merchantOpCityId = do
  orgMapsConfig <- QOMC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  orgMapsServiceConfig <-
    QOMSC.findByServiceAndCity (DOSC.MapsService orgMapsConfig.rectifyDistantPointsFailure) merchantOpCityId
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "Maps" (show orgMapsConfig.rectifyDistantPointsFailure))
  case orgMapsServiceConfig.serviceConfig of
    DOSC.MapsServiceConfig msc -> return msc
    _ -> throwError $ InternalError "Unknown Service Config"

runWithServiceConfig ::
  ServiceFlow m r =>
  (Maybe Text -> MapsServiceConfig -> req -> m resp) ->
  (MerchantServiceUsageConfig -> MapsService) ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maybe Text ->
  req ->
  m resp
runWithServiceConfig func getCfg _merchantId merchantOpCityId entityId req = do
  orgMapsConfig <- QOMC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  orgMapsServiceConfig <-
    QOMSC.findByServiceAndCity (DOSC.MapsService $ getCfg orgMapsConfig) merchantOpCityId
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "Maps" (show $ getCfg orgMapsConfig))
  case orgMapsServiceConfig.serviceConfig of
    DOSC.MapsServiceConfig msc -> func entityId msc req
    _ -> throwError $ InternalError "Unknown Service Config"
