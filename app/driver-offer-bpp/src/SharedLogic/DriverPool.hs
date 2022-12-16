{-# LANGUAGE DerivingVia #-}

module SharedLogic.DriverPool
  ( calculateDriverPool,
    calculateDriverPoolWithActualDist,
    incrementAcceptanceCount,
    getLatestAcceptanceRatio,
    incrementTotalCount,
    module Reexport,
  )
where

import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Id
import qualified Beckn.Types.SlidingWindowCounters as SWC
import Beckn.Utils.Common
import qualified Beckn.Utils.SlidingWindowCounters as SWC
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Domain.Types.Vehicle.Variant (Variant)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import SharedLogic.DriverPool.Config as Reexport
import SharedLogic.DriverPool.Types as Reexport
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.Person as QP
import Tools.Maps as Maps
import Tools.Metrics

mkAcceptanceKey :: Text -> Text
mkAcceptanceKey driverId = driverId <> "-quote-accepted"

withWindowOptions ::
  ( Redis.HedisFlow m r,
    HasField "acceptanceWindowOptions" r SWC.SlidingWindowOptions,
    L.MonadFlow m
  ) =>
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withWindowOptions fn = do
  asks (.acceptanceWindowOptions) >>= fn

incrementTotalCount ::
  ( Redis.HedisFlow m r,
    HasField "acceptanceWindowOptions" r SWC.SlidingWindowOptions,
    L.MonadFlow m
  ) =>
  Id DP.Person ->
  m ()
incrementTotalCount driverId = withWindowOptions $ SWC.incrementTotalCount driverId.getId

incrementAcceptanceCount ::
  ( Redis.HedisFlow m r,
    HasField "acceptanceWindowOptions" r SWC.SlidingWindowOptions,
    L.MonadFlow m
  ) =>
  Id DP.Person ->
  m ()
incrementAcceptanceCount driverId = withWindowOptions $ SWC.incrementWindowCount (mkAcceptanceKey driverId.getId)

getLatestAcceptanceRatio ::
  (L.MonadFlow m, Redis.HedisFlow m r) =>
  Id DP.Driver ->
  SWC.SlidingWindowOptions ->
  m Double
getLatestAcceptanceRatio driverId = SWC.getLatestRatio (getId driverId) mkAcceptanceKey

calculateDriverPool ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasDriverPoolConfig r,
    CoreMetrics m,
    HasCoordinates a
  ) =>
  Maybe Variant ->
  a ->
  Id DM.Merchant ->
  Bool ->
  PoolRadiusStep ->
  m [DriverPoolResult]
calculateDriverPool mbVariant pickup merchantId onlyNotOnRide radiusStep = do
  radius <- getRadius
  let coord = getCoordinates pickup
  mbDriverPositionInfoExpiry <- asks (.driverPoolCfg.driverPositionInfoExpiry)
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPool" $
      Esq.runInReplica $
        QP.getNearestDrivers
          mbVariant
          coord
          radius
          merchantId
          onlyNotOnRide
          mbDriverPositionInfoExpiry
  return $ makeDriverPoolResult <$> approxDriverPool
  where
    getRadius = do
      minRadius <- fromIntegral <$> asks (.driverPoolCfg.minRadiusOfSearch)
      maxRadius <- fromIntegral <$> asks (.driverPoolCfg.maxRadiusOfSearch)
      radiusStepSize <- asks (.driverPoolCfg.radiusStep)
      return $ min (minRadius + radiusStepSize * radiusStep) maxRadius
    makeDriverPoolResult :: QP.NearestDriversResult -> DriverPoolResult
    makeDriverPoolResult QP.NearestDriversResult {..} = do
      DriverPoolResult
        { distanceToPickup = distanceToDriver,
          variant = variant,
          ..
        }

calculateDriverPoolWithActualDist ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasDriverPoolConfig r,
    CoreMetrics m,
    HasCoordinates a
  ) =>
  Maybe Variant ->
  a ->
  Id DM.Merchant ->
  Bool ->
  PoolRadiusStep ->
  m [DriverPoolWithActualDistResult]
calculateDriverPoolWithActualDist mbVariant pickup merchantId onlyNotOnRide radiusStep = do
  driverPool <- calculateDriverPool mbVariant pickup merchantId onlyNotOnRide radiusStep
  case driverPool of
    [] -> return []
    (a : pprox) -> do
      mbActualDistanceThreshold <- asks (.driverPoolCfg.actualDistanceThreshold)
      driverPoolWithActialDist <- computeActualDistance merchantId pickup (a :| pprox)
      let filtDriverPoolWithActialDist = case mbActualDistanceThreshold of
            Nothing -> NE.toList driverPoolWithActialDist
            Just threshold -> NE.filter (filterFunc threshold) driverPoolWithActialDist
      logDebug $ "secondly filtered driver pool" <> show filtDriverPoolWithActialDist
      return filtDriverPoolWithActialDist
  where
    filterFunc threshold estDist = getMeters estDist.actualDistanceToPickup <= fromIntegral threshold

computeActualDistance ::
  ( CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasCoordinates a
  ) =>
  Id DM.Merchant ->
  a ->
  NonEmpty DriverPoolResult ->
  m (NonEmpty DriverPoolWithActualDistResult)
computeActualDistance orgId pickup driverPoolResults = do
  let pickupLatLong = getCoordinates pickup
  getDistanceResults <-
    Maps.getDistances orgId $
      Maps.GetDistancesReq
        { origins = driverPoolResults,
          destinations = pickupLatLong :| [],
          travelMode = Just Maps.CAR
        }
  logDebug $ "get distance results" <> show getDistanceResults
  return $ mkDriverPoolWithActualDistResult <$> getDistanceResults
  where
    mkDriverPoolWithActualDistResult distDur = do
      DriverPoolWithActualDistResult
        { driverPoolResult = distDur.origin,
          actualDistanceToPickup = distDur.distance,
          actualDurationToPickup = distDur.duration
        }
