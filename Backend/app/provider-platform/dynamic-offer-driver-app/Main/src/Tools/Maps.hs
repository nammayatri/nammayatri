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
    getPickupRoutes,
    getTripRoutes,
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
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
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
    HasCoordinates b,
    ToJSON a,
    ToJSON b,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  Id Merchant ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistance merchantId = runWithServiceConfig (Maps.getDistance (getId merchantId)) (.getDistances) merchantId

getDistances ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b,
    ToJSON a,
    ToJSON b,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  Id Merchant ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances merchantId = runWithServiceConfig (Maps.getDistances (getId merchantId)) (.getDistances) merchantId

getEstimatedPickupDistances ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b,
    ToJSON a,
    ToJSON b,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  Id Merchant ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getEstimatedPickupDistances merchantId = runWithServiceConfig (Maps.getDistances (getId merchantId)) (.getEstimatedPickupDistances) merchantId

getRoutes ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  Id Merchant ->
  GetRoutesReq ->
  m GetRoutesResp
getRoutes merchantId = runWithServiceConfig (Maps.getRoutes (getId merchantId)) (.getRoutes) merchantId

getPickupRoutes ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  Id Merchant ->
  GetRoutesReq ->
  m GetRoutesResp
getPickupRoutes merchantId = runWithServiceConfig (Maps.getRoutes (getId merchantId)) (.getPickupRoutes) merchantId

getTripRoutes ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  Id Merchant ->
  GetRoutesReq ->
  m GetRoutesResp
getTripRoutes merchantId = runWithServiceConfig (Maps.getRoutes (getId merchantId)) (.getTripRoutes) merchantId

snapToRoad ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasField "snapToRoadSnippetThreshold" r HighPrecMeters,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  Id Merchant ->
  SnapToRoadReq ->
  m SnapToRoadResp
snapToRoad merchantId = runWithServiceConfig (Maps.snapToRoad (getId merchantId)) (.snapToRoad) merchantId

autoComplete ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  Id Merchant ->
  AutoCompleteReq ->
  m AutoCompleteResp
autoComplete merchantId = runWithServiceConfig (Maps.autoComplete (getId merchantId)) (.autoComplete) merchantId

getPlaceName ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  Id Merchant ->
  GetPlaceNameReq ->
  m GetPlaceNameResp
getPlaceName merchantId = runWithServiceConfig (Maps.getPlaceName (getId merchantId)) (.getPlaceName) merchantId

getPlaceDetails ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  Id Merchant ->
  GetPlaceDetailsReq ->
  m GetPlaceDetailsResp
getPlaceDetails merchantId = runWithServiceConfig (Maps.getPlaceDetails (getId merchantId)) (.getPlaceDetails) merchantId

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
