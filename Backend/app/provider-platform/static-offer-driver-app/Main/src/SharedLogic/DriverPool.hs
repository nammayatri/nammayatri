{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverPool
  ( calculateDriverPool,
    module Reexport,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.FarePolicy.FareProduct as SFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Vehicle as SV
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverPool.Config as Reexport
import SharedLogic.DriverPool.Types as Reexport
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.TransporterConfig as QTConf
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Maps as Google
import qualified Tools.Maps as Maps
import Tools.Metrics

calculateDriverPool ::
  ( EncFlow m r,
    HasCacheConfig r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasDriverPoolConfig r,
    CoreMetrics m,
    HasCoordinates a,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  a ->
  Id DM.Merchant ->
  Maybe SV.Variant ->
  SFP.FareProductType ->
  Maybe PoolRadiusStep ->
  m [DriverPoolResult]
calculateDriverPool pickup merchantId variant fareProductType mRadiusStep = do
  let pickupLatLong = getCoordinates pickup
  radius <- getRadius
  mbDriverPositionInfoExpiry <- asks (.driverPoolCfg.driverPositionInfoExpiry)
  nearestDriversResult <-
    measuringDurationToLog INFO "calculateDriverPool" $
      QP.getNearestDrivers
        pickupLatLong
        radius
        merchantId
        variant
        fareProductType
        mbDriverPositionInfoExpiry
  case nearestDriversResult of
    [] -> pure []
    (a : xs) -> do
      approxDriverPool' <- buildDriverPoolResults merchantId pickupLatLong (a :| xs)
      filterOutDriversWithDistanceAboveThreshold radius approxDriverPool'
  where
    getRadius = do
      transporterConfig <-
        QTConf.findByMerchantId merchantId
          >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
      let maxRadius = transporterConfig.maxRadius.getMeters
          minRadius = transporterConfig.minRadius.getMeters
          radiusStepSize = transporterConfig.radiusStepSize.getMeters
      case mRadiusStep of
        Just radiusStep ->
          return $ min (minRadius + radiusStepSize * radiusStep) maxRadius
        Nothing -> return maxRadius

buildDriverPoolResults ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  Id DM.Merchant ->
  LatLong ->
  NonEmpty QP.NearestDriversResult ->
  m (NonEmpty DriverPoolResult)
buildDriverPoolResults orgId pickup ndResults = do
  distDurs <-
    Maps.getEstimatedPickupDistances orgId $
      Maps.GetDistancesReq
        { origins = ndResults,
          destinations = pickup :| [],
          travelMode = Just Maps.CAR
        }
  return $ mkDriverPoolResult <$> distDurs
  where
    mkDriverPoolResult distDur = do
      let QP.NearestDriversResult {..} = distDur.origin
      DriverPoolResult
        { distanceToPickup = distDur.distance,
          durationToPickup = distDur.duration,
          ..
        }

filterOutDriversWithDistanceAboveThreshold ::
  ( EsqDBFlow m r,
    CoreMetrics m
  ) =>
  Int ->
  NonEmpty DriverPoolResult ->
  m [DriverPoolResult]
filterOutDriversWithDistanceAboveThreshold threshold driverPoolResults = do
  pure $ NE.filter filterFunc driverPoolResults
  where
    filterFunc drPoolRes = drPoolRes.distanceToPickup <= fromIntegral threshold
