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
    runWithServiceConfig1,
  )
where

import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Domain.Types.Merchant.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
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
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.Queries.PickedServices as QPickedServices
import Tools.Error

getDistance ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistance = runWithServiceConfig Maps.getDistance (.getDistances)

getDistanceForCancelRide ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistanceForCancelRide = runWithServiceConfig Maps.getDistance (.getDistancesForCancelRide)

getDistances ::
  ( ServiceFlow m r,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances = runWithServiceConfig Maps.getDistances (.getDistances)

getRoutes :: ServiceFlow m r => Id Person -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> GetRoutesReq -> m GetRoutesResp
getRoutes personId merchantId mbMOCId req = do
  merchant <- SMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mOCId <- getMerchantOperatingCityId personId mbMOCId
  runWithServiceConfig (Maps.getRoutes merchant.isAvoidToll) (.getRoutes) merchantId mOCId req

getPickupRoutes :: ServiceFlow m r => Id Person -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> GetRoutesReq -> m GetRoutesResp
getPickupRoutes personId merchantId mbMOCId req = do
  merchant <- SMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mOCId <- getMerchantOperatingCityId personId mbMOCId
  runWithServiceConfig (Maps.getRoutes merchant.isAvoidToll) (.getPickupRoutes) merchantId mOCId req

getTripRoutes :: ServiceFlow m r => Id Person -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> GetRoutesReq -> m GetRoutesResp
getTripRoutes personId merchantId mbMOCId req = do
  merchant <- SMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mOCId <- getMerchantOperatingCityId personId mbMOCId
  runWithServiceConfig (Maps.getRoutes merchant.isAvoidToll) (.getTripRoutes) merchantId mOCId req

snapToRoad ::
  ( ServiceFlow m r
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  SnapToRoadReq ->
  m SnapToRoadResp
snapToRoad = runWithServiceConfig Maps.snapToRoad (.snapToRoad)

autoComplete :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> AutoCompleteReq -> m AutoCompleteResp
autoComplete = runWithServiceConfig Maps.autoComplete (.autoComplete)

getPlaceName :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> GetPlaceNameReq -> m GetPlaceNameResp
getPlaceName = runWithServiceConfig Maps.getPlaceName (.getPlaceName)

getPlaceDetails :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> GetPlaceDetailsReq -> m GetPlaceDetailsResp
getPlaceDetails = runWithServiceConfig Maps.getPlaceDetails (.getPlaceDetails)

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
    QMSC.findByMerchantIdAndService merchantId (DMSC.MapsService $ getCfg merchantConfig)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Maps" (show $ getCfg merchantConfig))
  case merchantMapsServiceConfig.serviceConfig of
    DMSC.MapsServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"

runWithServiceConfig1 ::
  ServiceFlow m r =>
  (MapsServiceConfig -> req -> m resp) ->
  Maybe (Id DSR.SearchRequest) ->
  (DPickedServices.PickedServices -> MapsService) ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig1 func mbSearchRequestId getCfg merchantId merchantOperatingCityId req = do
  pickedService <- case mbSearchRequestId of
    Nothing -> do
      merchantConfig <- QMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
      pickService merchantConfig
    Just searchRequestId -> do
      mbPickedServices <- QPickedServices.findByPrimaryKey (cast @DSR.SearchRequest @DPickedServices.PickedServices searchRequestId)
      case mbPickedServices of
        Just pickedServices -> pure $ getCfg pickedServices
        Nothing -> do
          merchantConfig <- QMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
          pickedServices <- buildPickedServices merchantConfig
          QPickedServices.create pickedServices
          pure $ getCfg pickedServices
  merchantMapsServiceConfig <-
    QMSC.findByMerchantIdAndService merchantId (DMSC.MapsService pickedService)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Maps" (show pickedService))
  case merchantMapsServiceConfig.serviceConfig of
    DMSC.MapsServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"

getMerchantOperatingCityId :: ServiceFlow m r => Id Person -> Maybe (Id MerchantOperatingCity) -> m (Id MerchantOperatingCity)
getMerchantOperatingCityId personId = \case
  Just mOprCityId -> pure mOprCityId
  Nothing -> CQP.findCityInfoById personId >>= fmap (.merchantOperatingCityId) . fromMaybeM (PersonCityInformationNotFound personId.getId)

pickService :: MerchantServiceUsageConfig -> m Maps.MapsService
pickService = error "TODO"

buildPickedServices :: MerchantServiceUsageConfig -> m DPickedServices.PickedServices
buildPickedServices = error "TODO"
