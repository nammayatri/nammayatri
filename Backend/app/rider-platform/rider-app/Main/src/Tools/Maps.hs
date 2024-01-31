{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Tools.Maps
  ( module Reexport,
    autoComplete,
    getPlaceDetails,
    getPlaceName,
    getRoutes,
    getPickupRoutes,
    getTripRoutes,
    getDistanceForCancelRide,
  )
where

import Control.Lens
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
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
getDistanceForCancelRide = runWithServiceConfig Maps.GetDistancesForCancelRide Maps.getDistance (.getDistancesForCancelRide) #getDistancesForCancelRide

getRoutes :: ServiceFlow m r => Id Person -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> Maybe (Id DSR.SearchRequest) -> GetRoutesReq -> m GetRoutesResp
getRoutes personId merchantId mbMOCId mbSearchRequestId req = do
  merchant <- SMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mOCId <- getMerchantOperatingCityId personId mbMOCId
  runWithServiceConfig Maps.GetRoutes (Maps.getRoutes merchant.isAvoidToll) (.getRoutes) #getRoutes merchantId mOCId mbSearchRequestId req

getPickupRoutes :: ServiceFlow m r => Id Person -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> Maybe (Id DSR.SearchRequest) -> GetRoutesReq -> m GetRoutesResp
getPickupRoutes personId merchantId mbMOCId mbSearchRequestId req = do
  merchant <- SMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mOCId <- getMerchantOperatingCityId personId mbMOCId
  runWithServiceConfig Maps.GetPickupRoutes (Maps.getRoutes merchant.isAvoidToll) (.getPickupRoutes) #getPickupRoutes merchantId mOCId mbSearchRequestId req

getTripRoutes :: ServiceFlow m r => Id Person -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> Maybe (Id DSR.SearchRequest) -> GetRoutesReq -> m GetRoutesResp
getTripRoutes personId merchantId mbMOCId mbSearchRequestId req = do
  merchant <- SMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mOCId <- getMerchantOperatingCityId personId mbMOCId
  runWithServiceConfig Maps.GetTripRoutes (Maps.getRoutes merchant.isAvoidToll) (.getTripRoutes) #getTripRoutes merchantId mOCId mbSearchRequestId req

autoComplete :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> Maybe (Id DSR.SearchRequest) -> AutoCompleteReq -> m AutoCompleteResp
autoComplete = runWithServiceConfig Maps.AutoComplete Maps.autoComplete (.autoComplete) #autoComplete

getPlaceName :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> Maybe (Id DSR.SearchRequest) -> GetPlaceNameReq -> m GetPlaceNameResp
getPlaceName = runWithServiceConfig Maps.GetPlaceName Maps.getPlaceName (.getPlaceName) #getPlaceName

getPlaceDetails :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> Maybe (Id DSR.SearchRequest) -> GetPlaceDetailsReq -> m GetPlaceDetailsResp
getPlaceDetails = runWithServiceConfig Maps.GetPlaceDetails Maps.getPlaceDetails (.getPlaceDetails) #getPlaceDetails

runWithServiceConfig ::
  ServiceFlow m r =>
  Maps.MapsServiceUsageMethod ->
  (MapsServiceConfig -> req -> m resp) ->
  (DMSUC.MerchantServiceUsageConfig -> Maps.MapsServiceUsage) ->
  Lens' DPickedServices.PickedServices (Maybe MapsService) ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maybe (Id DSR.SearchRequest) ->
  req ->
  m resp
runWithServiceConfig serviceMethod apiCall getServiceUsage pickedServiceLens merchantId merchantOperatingCityId mbSearchRequestId req = do
  merchantConfig <- CQMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
  pickedService <- case mbSearchRequestId of
    Nothing -> do
      pickedService <- Maps.pickService merchantOperatingCityId (getServiceUsage merchantConfig) serviceMethod
      logDebug $ "Do not store picked_service: method: " <> show serviceMethod <> "; service: " <> show pickedService
      pure pickedService
    Just searchRequestId -> do
      mbPickedServices <- QPickedServices.findByPrimaryKey (cast @DSR.SearchRequest @DPickedServices.PickedServices searchRequestId)
      case mbPickedServices of
        Nothing -> do
          pickedServices <- buildPickedServices searchRequestId merchantOperatingCityId
          pickedService <- Maps.pickService merchantOperatingCityId (getServiceUsage merchantConfig) serviceMethod
          logDebug $ "Creating picked_service entry: searchRequestId: " <> show searchRequestId <> "; method: " <> show serviceMethod <> "; service: " <> show pickedService
          QPickedServices.create (pickedServices & pickedServiceLens ?~ pickedService)
          pure pickedService
        Just pickedServices -> do
          case pickedServices ^. pickedServiceLens of
            Nothing -> do
              pickedService <- Maps.pickService merchantOperatingCityId (getServiceUsage merchantConfig) serviceMethod
              logDebug $ "Updating picked_service entry: searchRequestId: " <> show searchRequestId <> "; method: " <> show serviceMethod <> "; service: " <> show pickedService
              QPickedServices.updateByPrimaryKey (pickedServices & pickedServiceLens ?~ pickedService)
              pure pickedService
            Just pickedService -> do
              logDebug $ "Service already picked: searchRequestId: " <> show searchRequestId <> "; method: " <> show serviceMethod <> "; service: " <> show pickedService
              pure pickedService
  merchantMapsServiceConfig <-
    CQMSC.findByMerchantIdAndService merchantId (DMSC.MapsService pickedService)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Maps" (show pickedService))
  case merchantMapsServiceConfig.serviceConfig of
    DMSC.MapsServiceConfig msc -> apiCall msc req
    _ -> throwError $ InternalError "Unknown Service Config"

getMerchantOperatingCityId :: ServiceFlow m r => Id Person -> Maybe (Id MerchantOperatingCity) -> m (Id MerchantOperatingCity)
getMerchantOperatingCityId personId = \case
  Just mOprCityId -> pure mOprCityId
  Nothing -> CQP.findCityInfoById personId >>= fmap (.merchantOperatingCityId) . fromMaybeM (PersonCityInformationNotFound personId.getId)

buildPickedServices :: (MonadFlow m) => Id DSR.SearchRequest -> Id MerchantOperatingCity -> m DPickedServices.PickedServices
buildPickedServices searchRequestId' merchantOperatingCityId = do
  let searchRequestId = cast @DSR.SearchRequest @DPickedServices.PickedServices searchRequestId'
  now <- getCurrentTime
  pure
    DPickedServices.PickedServices
      { autoComplete = Nothing,
        getDistancesForCancelRide = Nothing,
        getPickupRoutes = Nothing,
        getPlaceDetails = Nothing,
        getPlaceName = Nothing,
        getRoutes = Nothing,
        getTripRoutes = Nothing,
        createdAt = now,
        updatedAt = now,
        ..
      }
