module Tools.Maps
  ( module Reexport,
    autoComplete,
    getDistance,
    getDistances,
    getEstimatedPickupDistances,
    getPlaceDetails,
    getPlaceName,
    getRoutes,
    snapToRoad,
  )
where

import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig as DOSC
import Domain.Types.Merchant.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
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
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QOMC
import Tools.Error
import Tools.Metrics

getDistance ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistance = runWithServiceConfig Maps.getDistance (.getDistances)

getDistances ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances = runWithServiceConfig Maps.getDistances (.getDistances)

getEstimatedPickupDistances ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getEstimatedPickupDistances = runWithServiceConfig Maps.getDistances (.getEstimatedPickupDistances)

getRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> GetRoutesReq -> m GetRoutesResp
getRoutes = runWithServiceConfig Maps.getRoutes (.getRoutes)

snapToRoad ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasField "snapToRoadSnippetThreshold" r HighPrecMeters
  ) =>
  Id Merchant ->
  SnapToRoadReq ->
  m SnapToRoadResp
snapToRoad = runWithServiceConfig Maps.snapToRoad (.snapToRoad)

autoComplete :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> AutoCompleteReq -> m AutoCompleteResp
autoComplete = runWithServiceConfig Maps.autoComplete (.autoComplete)

getPlaceName :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> GetPlaceNameReq -> m GetPlaceNameResp
getPlaceName = runWithServiceConfig Maps.getPlaceName (.getPlaceName)

getPlaceDetails :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> GetPlaceDetailsReq -> m GetPlaceDetailsResp
getPlaceDetails = runWithServiceConfig Maps.getPlaceDetails (.getPlaceDetails)

runWithServiceConfig ::
  (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) =>
  (MapsServiceConfig -> req -> m resp) ->
  (MerchantServiceUsageConfig -> MapsService) ->
  Id Merchant ->
  req ->
  m resp
runWithServiceConfig func getCfg orgId req = do
  orgMapsConfig <- QOMC.findByMerchantId orgId >>= fromMaybeM (MerchantServiceUsageConfigNotFound orgId.getId)
  orgMapsServiceConfig <-
    QOMSC.findByMerchantIdAndService orgId (DOSC.MapsService $ getCfg orgMapsConfig)
      >>= fromMaybeM (MerchantServiceConfigNotFound orgId.getId (getCfg orgMapsConfig))
  case orgMapsServiceConfig.serviceConfig of
    DOSC.MapsServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"
