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
    getFrfsAutocompleteDistances,
    snapToRoad,
    getPickupRoutes,
    getTripRoutes,
    getDistanceForCancelRide,
    getDistanceForScheduledRides,
    getMerchantOperatingCityId,
    getMultimodalWalkDistance,
    getMultimodalJourneyDistances,
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

getMultimodalWalkDistance ::
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
getMultimodalWalkDistance = runWithServiceConfig Maps.getDistance (.getMultimodalWalkDistance)

getMultimodalJourneyDistances ::
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
getMultimodalJourneyDistances = runWithServiceConfig Maps.getDistances (.getMultimodalWalkDistance)

getRoutes :: ServiceFlow m r => Maybe Bool -> Id Person -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> Maybe Text -> GetRoutesReq -> m GetRoutesResp
getRoutes isAvoidToll personId merchantId mbMOCId entityId req = do
  merchant <- SMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mOCId <- getMerchantOperatingCityId personId mbMOCId
  runWithServiceConfig (callGetRoutesWrapper $ fromMaybe merchant.isAvoidToll isAvoidToll) (.getRoutes) merchantId mOCId entityId req

getPickupRoutes :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> MapsService -> Maybe Text -> GetRoutesReq -> m GetRoutesResp
getPickupRoutes merchantId merchantOperatingCityId service entityId req = do
  merchant <- SMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  merchantMapsServiceConfig <-
    QMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (DMSC.MapsService service)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Maps" (show service))
  case merchantMapsServiceConfig.serviceConfig of
    DMSC.MapsServiceConfig msc -> Maps.getRoutes entityId merchant.isAvoidToll msc req
    _ -> throwError $ InternalError "Unknown Service Config"

getFrfsAutocompleteDistances ::
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
getFrfsAutocompleteDistances = runWithServiceConfig Maps.getDistances (.getFrfsAutocompleteDistances)

getTripRoutes :: ServiceFlow m r => Id Person -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> Maybe Text -> GetRoutesReq -> m GetRoutesResp
getTripRoutes personId merchantId mbMOCId entityId req = do
  merchant <- SMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mOCId <- getMerchantOperatingCityId personId mbMOCId
  runWithServiceConfig (callGetRoutesWrapper merchant.isAvoidToll) (.getTripRoutes) merchantId mOCId entityId req

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

callGetRoutesWrapper :: ServiceFlow m r => Bool -> Maybe Text -> MapsServiceConfig -> GetRoutesReq -> m GetRoutesResp
callGetRoutesWrapper isAvoidToll entityId = Maps.getRoutes entityId isAvoidToll

runWithServiceConfig ::
  ServiceFlow m r =>
  (Maybe Text -> MapsServiceConfig -> req -> m resp) ->
  (MerchantServiceUsageConfig -> MapsService) ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maybe Text ->
  req ->
  m resp
runWithServiceConfig func getCfg merchantId merchantOperatingCityId entityId req = do
  merchantConfig <- QMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
  merchantMapsServiceConfig <-
    QMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (DMSC.MapsService $ getCfg merchantConfig)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Maps" (show $ getCfg merchantConfig))
  case merchantMapsServiceConfig.serviceConfig of
    DMSC.MapsServiceConfig msc -> func entityId msc req
    _ -> throwError $ InternalError "Unknown Service Config"

getMerchantOperatingCityId :: ServiceFlow m r => Id Person -> Maybe (Id MerchantOperatingCity) -> m (Id MerchantOperatingCity)
getMerchantOperatingCityId personId = \case
  Just mOprCityId -> pure mOprCityId
  Nothing -> CQP.findCityInfoById personId >>= fmap (.merchantOperatingCityId) . fromMaybeM (PersonCityInformationNotFound personId.getId)
