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
    runWithMapsCallHandler,
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

buildMapsCallHandler ::
  ServiceFlow m r =>
  (MerchantServiceUsageConfig -> [MapsService]) ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maps.MapsCallHandler m
buildMapsCallHandler getMapsService _merchantId merchantOperatingCityId = Maps.MapsCallHandler {..}
  where
    getMapsProvidersList = do
      merchantConfig <- QOMC.findByMerchantOpCityId merchantOperatingCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
      let mapsProviderList = getMapsService merchantConfig
      when (null mapsProviderList) $ throwError $ InternalError ("No maps service provider configured for the merchant, merchantOperatingCityId:" <> merchantOperatingCityId.getId)
      pure mapsProviderList

    getMapsProviderConfig provider = do
      merchantMapsServiceConfig <-
        QOMSC.findByServiceAndCity (DOSC.MapsService provider) merchantOperatingCityId
          >>= fromMaybeM (MerchantServiceConfigNotFound merchantOperatingCityId.getId "Maps" (show provider))
      case merchantMapsServiceConfig.serviceConfig of
        DOSC.MapsServiceConfig msc -> pure msc
        _ -> throwError $ InternalError "Unknown Service Config"

runWithMapsCallHandler ::
  ServiceFlow m r =>
  (MerchantServiceUsageConfig -> [MapsService]) ->
  (Maps.MapsCallHandler m -> req -> m resp) ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  req ->
  m resp
runWithMapsCallHandler getMapsService func merchantId merchantOpCityId req = do
  let mapsCallHandler = buildMapsCallHandler getMapsService merchantId merchantOpCityId
  func mapsCallHandler req

getDistance ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistance = runWithMapsCallHandler (.getDistancesPriorityList) Maps.callGetDistanceWithFallback

getDistanceForCancelRide ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistanceForCancelRide = runWithMapsCallHandler (.getDistancesForCancelRidePriorityList) Maps.callGetDistanceWithFallback

getDistances ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances = runWithMapsCallHandler (.getDistancesPriorityList) Maps.callGetDistancesWithFallback

getEstimatedPickupDistances ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getEstimatedPickupDistances = runWithMapsCallHandler (.getEstimatedPickupDistancesPriorityList) Maps.callGetDistancesWithFallback

getDistanceForScheduledRides ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistanceForScheduledRides = runWithMapsCallHandler (.getDistancesForScheduledRidesPriorityList) Maps.callGetDistanceWithFallback

getRoutes :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> GetRoutesReq -> m GetRoutesResp
getRoutes merchantId merchantOpCityId req = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantNotFound merchantOpCityId.getId)

  runWithMapsCallHandler (.getRoutesPriorityList) (Maps.callGetRoutesWithFallback transporterConfig.isAvoidToll) merchantId merchantOpCityId req

getPickupRoutes :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> GetRoutesReq -> m GetRoutesResp
getPickupRoutes merchantId merchantOpCityId req = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantNotFound merchantOpCityId.getId)

  runWithMapsCallHandler (.getPickupRoutesPriorityList) (Maps.callGetRoutesWithFallback transporterConfig.isAvoidToll) merchantId merchantOpCityId req

getTripRoutes :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> GetRoutesReq -> m GetRoutesResp
getTripRoutes merchantId merchantOpCityId req = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantNotFound merchantOpCityId.getId)

  runWithMapsCallHandler (.getTripRoutesPriorityList) (Maps.callGetRoutesWithFallback transporterConfig.isAvoidToll) merchantId merchantOpCityId req

snapToRoad ::
  ( ServiceFlow m r
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  SnapToRoadReq ->
  m SnapToRoadResp
snapToRoad = runWithServiceConfig Maps.snapToRoad (.snapToRoad)

autoComplete :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> AutoCompleteReq -> m AutoCompleteResp
autoComplete = runWithMapsCallHandler (.autoCompletePriorityList) Maps.callAutoCompleteWithFallback

getPlaceName :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> GetPlaceNameReq -> m GetPlaceNameResp
getPlaceName = runWithMapsCallHandler (.getPlaceNamePriorityList) Maps.callGetPlaceNameWithFallback

getPlaceDetails :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> GetPlaceDetailsReq -> m GetPlaceDetailsResp
getPlaceDetails = runWithMapsCallHandler (.getPlaceDetailsPriorityList) Maps.callGetPlaceDetailsWithFallback

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
  SnapToRoadReq ->
  m ([Maps.MapsService], Either String SnapToRoadResp)
snapToRoadWithFallback rectifyDistantPointsFailureUsing _merchantId merchantOperatingCityId = Maps.snapToRoadWithFallback rectifyDistantPointsFailureUsing handler
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
  (MapsServiceConfig -> req -> m resp) ->
  (MerchantServiceUsageConfig -> MapsService) ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func getCfg _merchantId merchantOpCityId req = do
  orgMapsConfig <- QOMC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  orgMapsServiceConfig <-
    QOMSC.findByServiceAndCity (DOSC.MapsService $ getCfg orgMapsConfig) merchantOpCityId
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "Maps" (show $ getCfg orgMapsConfig))
  case orgMapsServiceConfig.serviceConfig of
    DOSC.MapsServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"
