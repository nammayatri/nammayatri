{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module SharedLogic.DriverPool
  ( calculateDriverPool,
    calculateDriverPoolWithActualDist,
    incrementTotalQuotesCount,
    incrementQuoteAcceptedCount,
    decrementTotalQuotesCount,
    getTotalQuotesSent,
    getLatestAcceptanceRatio,
    incrementTotalRidesCount,
    isThresholdRidesCompleted,
    incrementCancellationCount,
    getLatestCancellationRatio,
    getCurrentWindowAvailability,
    getQuotesCount,
    getPopupDelay,
    getValidSearchRequestCount,
    removeSearchReqIdFromMap,
    updateDriverSpeedInRedis,
    getDriverAverageSpeed,
    PoolCalculationStage (..),
    module Reexport,
  )
where

import Data.Fixed
import Data.List (partition)
import Data.List.Extra (notNull)
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Domain.Types.SearchRequest
import qualified Domain.Types.TransporterConfig as DTC
import Domain.Types.Vehicle.Variant (Variant)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWC
import qualified Kernel.Utils.CalculateDistance as CD
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Config (HasSendSearchRequestJobConfig)
import SharedLogic.DriverPool.Config as Reexport
import SharedLogic.DriverPool.Types as Reexport
import SharedLogic.DriverSpeedCalculation (HasSpeedCalculationConfig)
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.TransporterConfig as TC
import qualified Storage.Queries.Person as QP
import Tools.Maps as Maps
import Tools.Metrics

data PoolCalculationStage = Estimate | DriverSelection

mkTotalQuotesKey :: Text -> Text
mkTotalQuotesKey driverId = "driver-offer:DriverPool:Total-quotes:DriverId-" <> driverId

mkQuotesAcceptedKey :: Text -> Text
mkQuotesAcceptedKey driverId = "driver-offer:DriverPool:Quote-accepted:DriverId-" <> driverId

mkTotalRidesKey :: Text -> Text
mkTotalRidesKey driverId = "driver-offer:DriverPool:Total-Rides:DriverId-" <> driverId

mkRideCancelledKey :: Text -> Text
mkRideCancelledKey driverId = "driver-offer:DriverPool:Ride-cancelled:DriverId-" <> driverId

mkAvailableTimeKey :: Text -> Text
mkAvailableTimeKey driverId = "driver-offer:DriverPool:Available-time:DriverId-" <> driverId

windowFromTransporterConfig :: (L.MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> (DTC.TransporterConfig -> SWC.SlidingWindowOptions) -> m SWC.SlidingWindowOptions
windowFromTransporterConfig merchantId windowKey = maybe defaultWindow windowKey <$> TC.findByMerchantId merchantId
  where
    defaultWindow = SWC.SlidingWindowOptions 7 SWC.Days

withAcceptanceRatioWindowOption ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withAcceptanceRatioWindowOption merchantId fn = windowFromTransporterConfig merchantId (.acceptanceRatioWindowOption) >>= fn

withCancellationRatioWindowOption ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withCancellationRatioWindowOption merchantId fn = windowFromTransporterConfig merchantId (.cancellationRatioWindowOption) >>= fn

withAvailabilityTimeWindowOption ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withAvailabilityTimeWindowOption merchantId fn = windowFromTransporterConfig merchantId (.availabilityTimeWindowOption) >>= fn

withMinQuotesToQualifyIntelligentPoolWindowOption ::
  ( Redis.HedisFlow m r,
    Reexport.HasDriverPoolConfig r
  ) =>
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withMinQuotesToQualifyIntelligentPoolWindowOption fn = do
  asks (.intelligentPoolConfig.minQuotesToQualifyForIntelligentPoolWindowOption) >>= fn

decrementTotalQuotesCount ::
  ( Redis.HedisFlow m r,
    Reexport.HasDriverPoolConfig r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Driver ->
  Id SearchRequest ->
  m ()
decrementTotalQuotesCount merchantId driverId sreqId = do
  mbSreqCounted <- find (\(srId, (_, isCounted)) -> srId == sreqId.getId && isCounted) <$> getSearchRequestInfoMap merchantId driverId
  whenJust mbSreqCounted $
    const . Redis.withCrossAppRedis $ withAcceptanceRatioWindowOption merchantId $ SWC.decrementWindowCount (mkTotalQuotesKey driverId.getId)

incrementTotalQuotesCount ::
  ( Redis.HedisFlow m r,
    Reexport.HasDriverPoolConfig r,
    HasSendSearchRequestJobConfig r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Person ->
  Id SearchRequest ->
  UTCTime ->
  m ()
incrementTotalQuotesCount merchantId driverId searchReqId validTill = do
  now <- getCurrentTime
  srCount <- getValidSearchRequestCount merchantId (cast driverId) now
  Redis.withCrossAppRedis $ withMinQuotesToQualifyIntelligentPoolWindowOption $ SWC.incrementWindowCount (mkQuotesCountKey driverId.getId) -- total quotes sent count in different sliding window (used in driver pool for random vs intelligent filtering)
  let shouldCount = srCount < 1
  addSearchRequestInfoToCache searchReqId merchantId (cast driverId) (validTill, shouldCount)
  when shouldCount $
    Redis.withCrossAppRedis $ withAcceptanceRatioWindowOption merchantId $ SWC.incrementWindowCount (mkTotalQuotesKey driverId.getId) -- for acceptance ratio calculation

mkParallelSearchRequestKey :: Id DM.Merchant -> Id DP.Driver -> Text
mkParallelSearchRequestKey mId dId = "driver-offer:DriverPool:Search-Req-Validity-Map-" <> mId.getId <> dId.getId

addSearchRequestInfoToCache ::
  ( Redis.HedisFlow m r,
    HasSendSearchRequestJobConfig r
  ) =>
  Id SearchRequest ->
  Id DM.Merchant ->
  Id DP.Driver ->
  (UTCTime, Bool) ->
  m ()
addSearchRequestInfoToCache searchReqId merchantId driverId valueToPut = do
  singleBatchProcessTime <- fromIntegral <$> asks (.sendSearchRequestJobCfg.singleBatchProcessTime)
  Redis.withCrossAppRedis $ Redis.hSetExp (mkParallelSearchRequestKey merchantId driverId) searchReqId.getId valueToPut singleBatchProcessTime

getSearchRequestInfoMap ::
  ( Redis.HedisFlow m r,
    MonadReader r m
  ) =>
  Id DM.Merchant ->
  Id DP.Driver ->
  m [(Text, (UTCTime, Bool))]
getSearchRequestInfoMap mId dId = Redis.withCrossAppRedis $ Redis.hGetAll $ mkParallelSearchRequestKey mId dId

getValidSearchRequestCount ::
  ( Redis.HedisFlow m r,
    MonadReader r m
  ) =>
  Id DM.Merchant ->
  Id DP.Driver ->
  UTCTime ->
  m Int
getValidSearchRequestCount merchantId driverId now = Redis.withCrossAppRedis $ do
  searchRequestValidityMap <- getSearchRequestInfoMap merchantId driverId
  let (valid, old) = partition ((> now) . fst . snd) searchRequestValidityMap
  when (notNull old) $ Redis.hDel (mkParallelSearchRequestKey merchantId driverId) (map fst old)
  pure $ length valid

removeSearchReqIdFromMap ::
  ( Redis.HedisFlow m r,
    MonadTime m
  ) =>
  Id DM.Merchant ->
  Id DP.Person ->
  Id SearchRequest ->
  m ()
removeSearchReqIdFromMap merchantId driverId = Redis.withCrossAppRedis . Redis.hDel (mkParallelSearchRequestKey merchantId $ cast driverId) . (: []) .(.getId)

incrementQuoteAcceptedCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Person ->
  m ()
incrementQuoteAcceptedCount merchantId driverId = Redis.withCrossAppRedis . withAcceptanceRatioWindowOption merchantId $ SWC.incrementWindowCount (mkQuotesAcceptedKey driverId.getId)

getTotalQuotesSent ::
  ( Redis.HedisFlow m r,
    Reexport.HasDriverPoolConfig r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Person ->
  m Int
getTotalQuotesSent merchantId driverId =
  sum . catMaybes <$> (Redis.withCrossAppRedis . withAcceptanceRatioWindowOption merchantId $ SWC.getCurrentWindowValues (mkTotalQuotesKey driverId.getId))

getLatestAcceptanceRatio ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Driver ->
  m Double
getLatestAcceptanceRatio merchantId driverId = Redis.withCrossAppRedis . withAcceptanceRatioWindowOption merchantId $ SWC.getLatestRatio (getId driverId) mkQuotesAcceptedKey mkTotalQuotesKey

incrementTotalRidesCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Person ->
  m ()
incrementTotalRidesCount merchantId driverId = Redis.withCrossAppRedis . withCancellationRatioWindowOption merchantId $ SWC.incrementWindowCount (mkTotalRidesKey driverId.getId)

getTotalRidesCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Driver ->
  m Int
getTotalRidesCount merchantId driverId = sum . catMaybes <$> (Redis.withCrossAppRedis . withCancellationRatioWindowOption merchantId $ SWC.getCurrentWindowValues (mkTotalRidesKey driverId.getId))

incrementCancellationCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Person ->
  m ()
incrementCancellationCount merchantId driverId = Redis.withCrossAppRedis . withCancellationRatioWindowOption merchantId $ SWC.incrementWindowCount (mkRideCancelledKey driverId.getId)

getLatestCancellationRatio ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Driver ->
  m Double
getLatestCancellationRatio merchantId driverId = Redis.withCrossAppRedis . withCancellationRatioWindowOption merchantId $ SWC.getLatestRatio driverId.getId mkRideCancelledKey mkTotalRidesKey

getCurrentWindowAvailability ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    FromJSON a
  ) =>
  Id DM.Merchant ->
  Id DP.Driver ->
  m [Maybe a]
getCurrentWindowAvailability merchantId driverId = Redis.withCrossAppRedis . withAvailabilityTimeWindowOption merchantId $ SWC.getCurrentWindowValues (mkAvailableTimeKey driverId.getId)

mkQuotesCountKey :: Text -> Text
mkQuotesCountKey driverId = "driver-offer:DriverPool:Total-quotes-sent:DriverId-" <> driverId

getQuotesCount ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    Reexport.HasDriverPoolConfig r,
    FromJSON a,
    Num a
  ) =>
  Id DP.Driver ->
  m a
getQuotesCount driverId = sum . catMaybes <$> (Redis.withCrossAppRedis . withMinQuotesToQualifyIntelligentPoolWindowOption $ SWC.getCurrentWindowValues (mkQuotesCountKey driverId.getId))

isThresholdRidesCompleted ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Reexport.HasDriverPoolConfig r
  ) =>
  Id DP.Driver ->
  Id DM.Merchant ->
  CancellationScoreRelatedConfig ->
  m Bool
isThresholdRidesCompleted driverId merchantId cancellationScoreRelatedConfig = do
  let thresholdRidesCount = fromMaybe 5 cancellationScoreRelatedConfig.thresholdRidesCount
  totalRides <- getTotalRidesCount merchantId driverId
  pure $ totalRides >= thresholdRidesCount

getPopupDelay ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Reexport.HasDriverPoolConfig r
  ) =>
  Id DM.Merchant ->
  Id DP.Driver ->
  Double ->
  CancellationScoreRelatedConfig ->
  m Seconds
getPopupDelay merchantId driverId cancellationRatio cancellationScoreRelatedConfig = do
  defaultPopupDelay <- asks (.defaultPopupDelay)
  let cancellationRatioThreshold = fromIntegral $ fromMaybe 40 cancellationScoreRelatedConfig.thresholdCancellationScore
  (defaultPopupDelay +)
    <$> if cancellationRatio * 100 > cancellationRatioThreshold
      then do
        isThresholdRidesDone <- isThresholdRidesCompleted driverId merchantId cancellationScoreRelatedConfig
        pure $
          if isThresholdRidesDone
            then fromMaybe (Seconds 0) cancellationScoreRelatedConfig.popupDelayToAddAsPenalty
            else Seconds 0
      else pure $ Seconds 0

mkDriverLocationUpdatesKey :: Id DM.Merchant -> Id DP.Person -> Text
mkDriverLocationUpdatesKey mId dId = "driver-offer:DriverPool:mId-" <> mId.getId <> ":dId:" <> dId.getId

updateDriverSpeedInRedis ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    HasSpeedCalculationConfig r
  ) =>
  Id DM.Merchant ->
  Id DP.Person ->
  LatLong ->
  UTCTime ->
  m ()
updateDriverSpeedInRedis merchantId driverId points timeStamp = Redis.withCrossAppRedis $ do
  speedCalculationConfig <- asks (.speedCalculationConfig)
  now <- getCurrentTime
  let driverLocationUpdatesKey = mkDriverLocationUpdatesKey merchantId driverId
  locationUpdatesList :: [(LatLong, UTCTime)] <-
    sortOn (Down . snd)
      . ((points, timeStamp) :)
      . filter
        ( \(_, time) ->
            time > addUTCTime (fromIntegral $ (-60) * speedCalculationConfig.locationUpdateSampleTime.getMinutes) now
        )
      . concat
      <$> Redis.safeGet driverLocationUpdatesKey
  Redis.set driverLocationUpdatesKey locationUpdatesList

getDriverAverageSpeed ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    HasSpeedCalculationConfig r
  ) =>
  Id DM.Merchant ->
  Id DP.Person ->
  m (Maybe Double)
getDriverAverageSpeed merchantId driverId = Redis.withCrossAppRedis $ do
  speedCalculationConfig <- asks (.speedCalculationConfig)
  let driverLocationUpdatesKey = mkDriverLocationUpdatesKey merchantId driverId
  locationUpdatesList :: [(LatLong, UTCTime)] <- concat <$> Redis.safeGet driverLocationUpdatesKey
  let locationUpdatesCount = length locationUpdatesList
  if locationUpdatesCount > speedCalculationConfig.minLocationUpdates
    then do
      let locationUpdatesPairs = zip (drop 1 locationUpdatesList) (take (locationUpdatesCount - 1) locationUpdatesList)
          (totalDistanceTravelled, totalTimeTaken) :: (HighPrecMeters, Centi) =
            foldr
              ( \((locationA, timeA), (locationB, timeB)) (accDis, accTime) -> do
                  let distance = CD.distanceBetweenInMeters locationB locationA
                      timeTaken = fromInteger . floor $ diffUTCTime timeB timeA
                  (accDis + distance, accTime + timeTaken)
              )
              (0, 0)
              locationUpdatesPairs
      pure . Just . fromRational . toRational $ totalDistanceTravelled.getHighPrecMeters.getCenti / totalTimeTaken
    else pure Nothing

calculateDriverPool ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasMaxParallelSearchRequests r
  ) =>
  PoolCalculationStage ->
  DriverPoolConfig ->
  Maybe Variant ->
  a ->
  Id DM.Merchant ->
  Bool ->
  Maybe PoolRadiusStep ->
  m [DriverPoolResult]
calculateDriverPool poolStage driverPoolCfg mbVariant pickup merchantId onlyNotOnRide mRadiusStep = do
  let radius = getRadius mRadiusStep
  let coord = getCoordinates pickup
  now <- getCurrentTime
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPool" $
      Esq.runInReplica $
        QP.getNearestDrivers
          mbVariant
          coord
          radius
          merchantId
          onlyNotOnRide
          driverPoolCfg.driverPositionInfoExpiry
  maxParallelSearchRequests <- asks (.maxParallelSearchRequests)
  driversWithLessThanNParallelRequests <- case poolStage of
    DriverSelection -> filterM (fmap (< maxParallelSearchRequests) . getParallelSearchRequestCount now) approxDriverPool
    Estimate -> pure approxDriverPool --estimate stage we dont need to consider actual parallel request counts
  pure $ makeDriverPoolResult <$> driversWithLessThanNParallelRequests
  where
    getParallelSearchRequestCount now dObj = getValidSearchRequestCount merchantId (cast dObj.driverId) now
    getRadius mRadiusStep_ = do
      let maxRadius = fromIntegral driverPoolCfg.maxRadiusOfSearch
      case mRadiusStep_ of
        Just radiusStep -> do
          let minRadius = fromIntegral driverPoolCfg.minRadiusOfSearch
          let radiusStepSize = fromIntegral driverPoolCfg.radiusStepSize
          min (minRadius + radiusStepSize * radiusStep) maxRadius
        Nothing -> maxRadius
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
    CoreMetrics m,
    HasDriverPoolConfig r,
    HasCoordinates a
  ) =>
  PoolCalculationStage ->
  DriverPoolConfig ->
  Maybe Variant ->
  a ->
  Id DM.Merchant ->
  Bool ->
  Maybe PoolRadiusStep ->
  m [DriverPoolWithActualDistResult]
calculateDriverPoolWithActualDist poolCalculationStage driverPoolCfg mbVariant pickup merchantId onlyNotOnRide mRadiusStep = do
  driverPool <- calculateDriverPool poolCalculationStage driverPoolCfg mbVariant pickup merchantId onlyNotOnRide mRadiusStep
  case driverPool of
    [] -> return []
    (a : pprox) -> do
      driverPoolWithActualDist <- computeActualDistance merchantId pickup (a :| pprox)
      let filtDriverPoolWithActualDist = case driverPoolCfg.actualDistanceThreshold of
            Nothing -> NE.toList driverPoolWithActualDist
            Just threshold -> NE.filter (filterFunc threshold) driverPoolWithActualDist
      logDebug $ "secondly filtered driver pool" <> show filtDriverPoolWithActualDist
      return filtDriverPoolWithActualDist
  where
    filterFunc threshold estDist = getMeters estDist.actualDistanceToPickup <= fromIntegral threshold

computeActualDistance ::
  ( CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasDriverPoolConfig r,
    HasCoordinates a
  ) =>
  Id DM.Merchant ->
  a ->
  NonEmpty DriverPoolResult ->
  m (NonEmpty DriverPoolWithActualDistResult)
computeActualDistance orgId pickup driverPoolResults = do
  let pickupLatLong = getCoordinates pickup
  defaultPopupDelay <- asks (.defaultPopupDelay)
  getDistanceResults <-
    Maps.getEstimatedPickupDistances orgId $
      Maps.GetDistancesReq
        { origins = driverPoolResults,
          destinations = pickupLatLong :| [],
          travelMode = Just Maps.CAR
        }
  logDebug $ "get distance results" <> show getDistanceResults
  return $ mkDriverPoolWithActualDistResult defaultPopupDelay <$> getDistanceResults
  where
    mkDriverPoolWithActualDistResult defaultPopupDelay distDur = do
      DriverPoolWithActualDistResult
        { driverPoolResult = distDur.origin,
          actualDistanceToPickup = distDur.distance,
          actualDurationToPickup = distDur.duration,
          rideRequestPopupDelayDuration = defaultPopupDelay,
          cancellationRatio = Nothing,
          acceptanceRatio = Nothing,
          driverAvailableTime = Nothing,
          isPartOfIntelligentPool = False
        }
