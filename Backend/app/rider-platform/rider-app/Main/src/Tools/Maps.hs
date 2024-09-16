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
    getDistances,
    getPlaceDetails,
    getPlaceName,
    getRoutes,
    snapToRoad,
    getPickupRoutes,
    getTripRoutes,
    getDistanceForCancelRide,
    getDistanceForScheduledRides,
    buildMapsCallHandler,
  )
where

import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity (..))
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Domain.Types.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
import Domain.Types.Person (Person)
import Kernel.External.Maps as Reexport hiding
  ( autoComplete,
    getDistance,
    getDistances,
    getPlaceDetails,
    getPlaceName,
    getRoutes,
    snapToRoad,
  )
import qualified Kernel.External.Maps as Maps
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as SMerchant
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import qualified Storage.CachedQueries.Person as CQP
import Tools.Error

buildMapsCallHandler ::
  ServiceFlow m r =>
  (MerchantServiceUsageConfig -> [MapsService]) ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maps.MapsCallHandler m
buildMapsCallHandler getMapsService merchantId merchantOpCityId = Maps.MapsCallHandler {..}
  where
    getMapsProvidersList = do
      merchantServiceUsageConfig <- QMSUC.findByMerchantOperatingCityId merchantOpCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
      let mapsServices = getMapsService merchantServiceUsageConfig
      when (null mapsServices) $ throwError $ InternalError "No Maps Service Configured"
      pure mapsServices

    getMapsProviderConfig mapsService = do
      merchantMapsServiceConfig <-
        QMSC.findByMerchantOpCityIdAndService merchantId merchantOpCityId (DMSC.MapsService mapsService)
          >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Maps" (show mapsService))
      case merchantMapsServiceConfig.serviceConfig of
        DMSC.MapsServiceConfig msc -> pure msc
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

getRoutes :: (ServiceFlow m r, Maps.MapsFlow m r) => Maybe Bool -> Id Person -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> GetRoutesReq -> m GetRoutesResp
getRoutes isAvoidToll personId merchantId mbMOCId req = do
  merchant <- SMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mOCId <- getMerchantOperatingCityId personId mbMOCId
  runWithMapsCallHandler (.getRoutesPriorityList) (Maps.callGetRoutesWithFallback $ fromMaybe merchant.isAvoidToll isAvoidToll) merchantId mOCId req

getPickupRoutes :: ServiceFlow m r => Id Person -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> GetRoutesReq -> m GetRoutesResp
getPickupRoutes personId merchantId mbMOCId req = do
  merchant <- SMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mOCId <- getMerchantOperatingCityId personId mbMOCId
  runWithMapsCallHandler (.getPickupRoutesPriorityList) (Maps.callGetRoutesWithFallback merchant.isAvoidToll) merchantId mOCId req

getTripRoutes :: ServiceFlow m r => Id Person -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> GetRoutesReq -> m GetRoutesResp
getTripRoutes personId merchantId mbMOCId req = do
  merchant <- SMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mOCId <- getMerchantOperatingCityId personId mbMOCId
  runWithMapsCallHandler (.getTripRoutesPriorityList) (Maps.callGetRoutesWithFallback merchant.isAvoidToll) merchantId mOCId req

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

getPlaceName :: (ServiceFlow m r, Maps.MapsFlow m r) => Id Merchant -> Id MerchantOperatingCity -> GetPlaceNameReq -> m GetPlaceNameResp
getPlaceName = runWithMapsCallHandler (.getPlaceNamePriorityList) Maps.callGetPlaceNameWithFallback

getPlaceDetails :: (ServiceFlow m r, Maps.MapsFlow m r) => Id Merchant -> Id MerchantOperatingCity -> GetPlaceDetailsReq -> m GetPlaceDetailsResp
getPlaceDetails = runWithMapsCallHandler (.getPlaceDetailsPriorityList) Maps.callGetPlaceDetailsWithFallback

runWithServiceConfig ::
  ServiceFlow m r =>
  (MapsServiceConfig -> req -> m resp) ->
  (MerchantServiceUsageConfig -> MapsService) ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func getCfg merchantId merchantOperatingCityId req = do
  merchantConfig <- QMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
  merchantMapsServiceConfig <-
    QMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (DMSC.MapsService $ getCfg merchantConfig)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Maps" (show $ getCfg merchantConfig))
  case merchantMapsServiceConfig.serviceConfig of
    DMSC.MapsServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"

getMerchantOperatingCityId :: ServiceFlow m r => Id Person -> Maybe (Id MerchantOperatingCity) -> m (Id MerchantOperatingCity)
getMerchantOperatingCityId personId = \case
  Just mOprCityId -> pure mOprCityId
  Nothing -> CQP.findCityInfoById personId >>= fmap (.merchantOperatingCityId) . fromMaybeM (PersonCityInformationNotFound personId.getId)
