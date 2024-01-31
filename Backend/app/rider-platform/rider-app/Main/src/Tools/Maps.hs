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
  )
where

import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Domain.Types.Merchant.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
import qualified Domain.Types.Merchant.MerchantServiceUsageConfig as DMSUC
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity (..))
import Domain.Types.Person (Person)
import qualified Domain.Types.PickedServices as DPickedServices
import qualified Domain.Types.SearchRequest as DSR
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
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.Queries.PickedServices as QPickedServices
import Tools.Error

data MapsHandler m req resp = MapsHandler
  { serviceMethod :: Maps.MapsServiceUsageMethod,
    apiCall :: MapsServiceConfig -> req -> m resp,
    getPickedService :: DPickedServices.PickedServices -> MapsService,
    getServiceUsage :: DMSUC.MerchantServiceUsageConfig -> Maps.MapsServiceUsage
  }

-- not used
getDistance ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maybe (Id DSR.SearchRequest) ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistance = runWithServiceConfig (MapsHandler Maps.GetDistances Maps.getDistance (.getDistances) (.getDistances))

getDistanceForCancelRide ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maybe (Id DSR.SearchRequest) ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistanceForCancelRide = runWithServiceConfig (MapsHandler Maps.GetDistancesForCancelRide Maps.getDistance (.getDistancesForCancelRide) (.getDistancesForCancelRide))

getDistances ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maybe (Id DSR.SearchRequest) ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances = runWithServiceConfig (MapsHandler Maps.GetDistances Maps.getDistances (.getDistances) (.getDistances))

getRoutes :: ServiceFlow m r => Id Person -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> Maybe (Id DSR.SearchRequest) -> GetRoutesReq -> m GetRoutesResp
getRoutes personId merchantId mbMOCId mbSearchRequestId req = do
  merchant <- SMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mOCId <- getMerchantOperatingCityId personId mbMOCId
  runWithServiceConfig (MapsHandler Maps.GetRoutes (Maps.getRoutes merchant.isAvoidToll) (.getRoutes) (.getRoutes)) merchantId mOCId mbSearchRequestId req

getPickupRoutes :: ServiceFlow m r => Id Person -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> Maybe (Id DSR.SearchRequest) -> GetRoutesReq -> m GetRoutesResp
getPickupRoutes personId merchantId mbMOCId mbSearchRequestId req = do
  merchant <- SMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mOCId <- getMerchantOperatingCityId personId mbMOCId
  runWithServiceConfig (MapsHandler Maps.GetPickupRoutes (Maps.getRoutes merchant.isAvoidToll) (.getPickupRoutes) (.getPickupRoutes)) merchantId mOCId mbSearchRequestId req

getTripRoutes :: ServiceFlow m r => Id Person -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> Maybe (Id DSR.SearchRequest) -> GetRoutesReq -> m GetRoutesResp
getTripRoutes personId merchantId mbMOCId mbSearchRequestId req = do
  merchant <- SMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mOCId <- getMerchantOperatingCityId personId mbMOCId
  runWithServiceConfig (MapsHandler Maps.GetTripRoutes (Maps.getRoutes merchant.isAvoidToll) (.getTripRoutes) (.getTripRoutes)) merchantId mOCId mbSearchRequestId req

-- not used
snapToRoad ::
  ( ServiceFlow m r
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maybe (Id DSR.SearchRequest) ->
  SnapToRoadReq ->
  m SnapToRoadResp
snapToRoad = runWithServiceConfig (MapsHandler Maps.SnapToRoad Maps.snapToRoad (.snapToRoad) (.snapToRoad))

autoComplete :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> Maybe (Id DSR.SearchRequest) -> AutoCompleteReq -> m AutoCompleteResp
autoComplete = runWithServiceConfig (MapsHandler Maps.AutoComplete Maps.autoComplete (.autoComplete) (.autoComplete))

getPlaceName :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> Maybe (Id DSR.SearchRequest) -> GetPlaceNameReq -> m GetPlaceNameResp
getPlaceName = runWithServiceConfig (MapsHandler Maps.GetPlaceName Maps.getPlaceName (.getPlaceName) (.getPlaceName))

getPlaceDetails :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> Maybe (Id DSR.SearchRequest) -> GetPlaceDetailsReq -> m GetPlaceDetailsResp
getPlaceDetails = runWithServiceConfig (MapsHandler Maps.GetPlaceDetails Maps.getPlaceDetails (.getPlaceDetails) (.getPlaceDetails))

runWithServiceConfig ::
  ServiceFlow m r =>
  MapsHandler m req resp ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maybe (Id DSR.SearchRequest) ->
  req ->
  m resp
runWithServiceConfig h merchantId merchantOperatingCityId mbSearchRequestId req = do
  pickedService <- case mbSearchRequestId of
    Nothing -> do
      merchantConfig <- CQMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
      pickService merchantOperatingCityId (h.getServiceUsage merchantConfig) h.serviceMethod
    Just searchRequestId -> do
      mbPickedServices <- QPickedServices.findByPrimaryKey (cast @DSR.SearchRequest @DPickedServices.PickedServices searchRequestId)
      case mbPickedServices of
        Just pickedServices -> pure $ h.getPickedService pickedServices
        Nothing -> do
          merchantConfig <- CQMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
          pickedServices <- buildPickedServices searchRequestId merchantOperatingCityId merchantConfig
          QPickedServices.create pickedServices
          pure $ h.getPickedService pickedServices
  merchantMapsServiceConfig <-
    CQMSC.findByMerchantIdAndService merchantId (DMSC.MapsService pickedService)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Maps" (show pickedService))
  case merchantMapsServiceConfig.serviceConfig of
    DMSC.MapsServiceConfig msc -> h.apiCall msc req
    _ -> throwError $ InternalError "Unknown Service Config"

getMerchantOperatingCityId :: ServiceFlow m r => Id Person -> Maybe (Id MerchantOperatingCity) -> m (Id MerchantOperatingCity)
getMerchantOperatingCityId personId = \case
  Just mOprCityId -> pure mOprCityId
  Nothing -> CQP.findCityInfoById personId >>= fmap (.merchantOperatingCityId) . fromMaybeM (PersonCityInformationNotFound personId.getId)

buildPickedServices :: (MonadFlow m) => Id DSR.SearchRequest -> Id MerchantOperatingCity -> MerchantServiceUsageConfig -> m DPickedServices.PickedServices
buildPickedServices searchRequestId' merchantOperatingCityId merchantConfig = do
  let searchRequestId = cast @DSR.SearchRequest @DPickedServices.PickedServices searchRequestId'
  now <- getCurrentTime
  let pickService' field = pickService merchantOperatingCityId (field merchantConfig)
  autoComplete' <- pickService' (.autoComplete) Maps.AutoComplete
  getDistances' <- pickService' (.getDistances) Maps.GetDistances
  getDistancesForCancelRide' <- pickService' (.getDistancesForCancelRide) Maps.GetDistancesForCancelRide
  getPickupRoutes' <- pickService' (.getPickupRoutes) Maps.GetPickupRoutes
  getPlaceDetails' <- pickService' (.getPlaceDetails) Maps.GetPlaceDetails
  getPlaceName' <- pickService' (.getPlaceName) Maps.GetPlaceName
  getRoutes' <- pickService' (.getRoutes) Maps.GetRoutes
  getTripRoutes' <- pickService' (.getTripRoutes) Maps.GetTripRoutes
  snapToRoad' <- pickService' (.snapToRoad) Maps.SnapToRoad
  pure
    DPickedServices.PickedServices
      { autoComplete = autoComplete',
        getDistances = getDistances',
        getDistancesForCancelRide = getDistancesForCancelRide',
        getPickupRoutes = getPickupRoutes',
        getPlaceDetails = getPlaceDetails',
        getPlaceName = getPlaceName',
        getRoutes = getRoutes',
        getTripRoutes = getTripRoutes',
        snapToRoad = snapToRoad',
        createdAt = now,
        updatedAt = now,
        ..
      }
