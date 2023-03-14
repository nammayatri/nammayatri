{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool
  ( isBatchNumExceedLimit,
    cleanupDriverPoolBatches,
    getNextDriverPoolBatch,
    getPoolBatchNum,
    module Reexport,
  )
where

import qualified Control.Monad as CM
import Control.Monad.Extra (maybeM)
import qualified Data.HashMap as HM
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person (Driver)
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.TransporterConfig (TransporterConfig)
import EulerHS.Prelude hiding (id)
import Kernel.Randomizer (randomizeList)
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Config (HasSendSearchRequestJobConfig)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config as Reexport
import SharedLogic.DriverPool
import qualified SharedLogic.DriverPool.Config as DP
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import qualified Storage.CachedQueries.TransporterConfig as TC
import Tools.Maps as Maps
import Tools.Metrics

isBatchNumExceedLimit ::
  ( Redis.HedisFlow m r,
    HasSendSearchRequestJobConfig r
  ) =>
  Id DSR.SearchRequest ->
  m Bool
isBatchNumExceedLimit searchReqId = do
  maxNumberOfBatches <- asks (.sendSearchRequestJobCfg.driverPoolBatchesCfg.maxNumberOfBatches)
  currentBatchNum <- getPoolBatchNum searchReqId
  return $ currentBatchNum >= maxNumberOfBatches

driverPoolKey :: Id DSR.SearchRequest -> Text
driverPoolKey searchReqId = "Driver-Offer:DriverPool:SearchReqId-" <> searchReqId.getId

driverPoolBatchKey :: Id DSR.SearchRequest -> PoolBatchNum -> Text
driverPoolBatchKey searchReqId batchNum = driverPoolKey searchReqId <> ":BatchNum-" <> show batchNum

prepareDriverPoolBatch ::
  ( EncFlow m r,
    HasCacheConfig r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    DP.HasDriverPoolConfig r,
    HasSendSearchRequestJobConfig r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  DriverPoolConfig ->
  DSR.SearchRequest ->
  PoolBatchNum ->
  m [DriverPoolWithActualDistResult]
prepareDriverPoolBatch driverPoolCfg searchReq batchNum = withLogTag ("BatchNum-" <> show batchNum) $ do
  previousBatchesDrivers <- getPreviousBatchesDrivers
  logDebug $ "PreviousBatchesDrivers-" <> show previousBatchesDrivers
  prepareDriverPoolBatch' previousBatchesDrivers
  where
    prepareDriverPoolBatch' previousBatchesDrivers = do
      radiusStep <- getPoolRadiusStep searchReq.id
      allNearbyDrivers <- calcDriverPool radiusStep
      sortingType <- asks (.sendSearchRequestJobCfg.driverPoolBatchesCfg.poolSortingType)
      batchSize <- asks (.sendSearchRequestJobCfg.driverPoolBatchesCfg.driverBatchSize)
      logDebug $ "DriverPool-" <> show allNearbyDrivers
      let onlyNewDrivers = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` previousBatchesDrivers) allNearbyDrivers
      if length onlyNewDrivers < batchSize
        then do
          isAtMaxRadiusStep' <- isAtMaxRadiusStep radiusStep
          if isAtMaxRadiusStep'
            then calculatePool batchSize sortingType onlyNewDrivers allNearbyDrivers
            else do
              incrementPoolRadiusStep searchReq.id
              prepareDriverPoolBatch' previousBatchesDrivers
        else calculatePool batchSize sortingType onlyNewDrivers allNearbyDrivers
      where
        calculatePool batchSize sortingType onlyNewDrivers allNearbyDrivers = do
          driverPoolBatch <- mkDriverPoolBatch batchSize sortingType onlyNewDrivers
          logDebug $ "DriverPoolBatch-" <> show driverPoolBatch
          finalPoolBatch <-
            if length driverPoolBatch < batchSize
              then do
                filledBatch <- fillBatch searchReq.providerId sortingType batchSize allNearbyDrivers driverPoolBatch
                logDebug $ "FilledDriverPoolBatch-" <> show filledBatch
                pure filledBatch
              else do
                pure driverPoolBatch
          incrementDriverRequestCount finalPoolBatch searchReq.id
          cacheBatch finalPoolBatch
          pure finalPoolBatch

        mkDriverPoolBatch batchSize sortingType onlyNewDrivers = do
          case sortingType of
            Intelligent -> makeIntelligentDriverPool batchSize searchReq.providerId onlyNewDrivers
            Random -> makeRandomDriverPool batchSize onlyNewDrivers

        makeIntelligentDriverPool batchSize merchantId onlyNewDrivers = do
          transporterConfig <- TC.findByMerchantId merchantId
          minQuotesToQualifyForIntelligentPool <- asks (.intelligentPoolConfig.minQuotesToQualifyForIntelligentPool)
          (sortedDriverPool, randomizedDriverPool) <-
            bimapM (sortWithDriverScore merchantId transporterConfig) randomizeAndLimitSelection
              =<< splitDriverPoolForSorting minQuotesToQualifyForIntelligentPool onlyNewDrivers
          takeDriversUsingPoolPercentage (sortedDriverPool, randomizedDriverPool) batchSize

        makeRandomDriverPool batchSize onlyNewDrivers = take batchSize <$> randomizeAndLimitSelection onlyNewDrivers

    takeDriversUsingPoolPercentage :: (MonadFlow m) => ([a], [a]) -> Int -> m [a]
    takeDriversUsingPoolPercentage (sortedDriverPool, randomizedDriverPool) driversCount = do
      let intelligentPoolPercentage = fromMaybe 50 driverPoolCfg.intelligentPoolPercentage
          intelligentCount = min (length sortedDriverPool) ((driversCount + 1) * intelligentPoolPercentage `div` 100)
          randomCount = driversCount - intelligentCount
          (sortedPart, restSorted) = splitAt intelligentCount sortedDriverPool
          (randomPart, restRandom) = splitAt randomCount randomizedDriverPool
          poolBatch = sortedPart <> randomPart
          driversFromRestCount = take (driversCount - length poolBatch) (restRandom <> restSorted) -- taking rest of drivers if poolBatch length is less then driverCount requried.
          finalPoolBatch = poolBatch <> driversFromRestCount
      logDebug $ "IntelligentDriverPool - SortedDriversCount " <> show (length sortedPart)
      logDebug $ "IntelligentDriverPool - RandomizedDriversCount " <> show (length randomPart)
      logDebug $ "IntelligentDriverPool - finalPoolBatch " <> show (length finalPoolBatch)
      pure finalPoolBatch

    calcDriverPool radiusStep = do
      let vehicleVariant = searchReq.vehicleVariant
          merchantId = searchReq.providerId
      let pickupLoc = searchReq.fromLocation
      let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
      calculateDriverPoolWithActualDist DriverSelection driverPoolCfg (Just vehicleVariant) pickupLatLong merchantId True (Just radiusStep)
    fillBatch merchantId sortingType batchSize allNearbyDrivers batch = do
      transporterConfig <- TC.findByMerchantId merchantId
      let batchDriverIds = batch <&> (.driverPoolResult.driverId)
      let driversNotInBatch = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` batchDriverIds) allNearbyDrivers
      driversWithValidReqAmount <- filterM (\dpr -> checkRequestCount searchReq.id dpr.driverPoolResult.driverId driverPoolCfg) driversNotInBatch
      minQuotesToQualifyForIntelligentPool <- asks (.intelligentPoolConfig.minQuotesToQualifyForIntelligentPool)
      let fillSize = batchSize - length batch
      (batch <>)
        <$> case sortingType of
          Intelligent -> do
            (sortedDriverPool, randomizedDriverPool) <-
              bimapM (sortWithDriverScore merchantId transporterConfig) randomizeAndLimitSelection
                =<< splitDriverPoolForSorting minQuotesToQualifyForIntelligentPool driversWithValidReqAmount -- snd means taking drivers who recieved less then X(config- minQuotesToQualifyForIntelligentPool) quotes
            takeDriversUsingPoolPercentage (sortedDriverPool, randomizedDriverPool) fillSize
          Random -> pure $ take fillSize driversWithValidReqAmount
    cacheBatch batch = do
      logDebug $ "Caching batch-" <> show batch
      Redis.withCrossAppRedis $ Redis.setExp (driverPoolBatchKey searchReq.id batchNum) batch (60 * 10)
    -- splitDriverPoolForSorting :: minQuotes Int -> [DriverPool Array] -> ([GreaterThanMinQuotesDP], [LessThanMinQuotesDP])
    splitDriverPoolForSorting minQuotes =
      foldrM
        ( \dObj (gtX, ltX) -> do
            driverQuotesCount <- getQuotesCount dObj.driverPoolResult.driverId
            pure $
              if driverQuotesCount >= minQuotes
                then (dObj : gtX, ltX)
                else (gtX, dObj : ltX)
        )
        ([], [])

    isAtMaxRadiusStep radiusStep = do
      let minRadiusOfSearch = fromIntegral @_ @Double driverPoolCfg.minRadiusOfSearch
      let maxRadiusOfSearch = fromIntegral @_ @Double driverPoolCfg.maxRadiusOfSearch
      let radiusStepSize = fromIntegral @_ @Double driverPoolCfg.radiusStepSize
      let maxRadiusStep = ceiling $ (maxRadiusOfSearch - minRadiusOfSearch) / radiusStepSize
      return $ maxRadiusStep <= radiusStep
    getPreviousBatchesDrivers = do
      batches <- forM [0 .. (batchNum - 1)] \num -> do
        getDriverPoolBatch searchReq.id num
      return $ (.driverPoolResult.driverId) <$> concat batches
    -- util function
    bimapM fna fnb (a, b) = (,) <$> fna a <*> fnb b

getDriverPoolBatch ::
  ( Redis.HedisFlow m r
  ) =>
  Id DSR.SearchRequest ->
  PoolBatchNum ->
  m [DriverPoolWithActualDistResult]
getDriverPoolBatch searchReqId batchNum = do
  Redis.withCrossAppRedis $
    Redis.get (driverPoolBatchKey searchReqId batchNum)
      >>= maybe whenFoundNothing whenFoundSomething
  where
    whenFoundNothing = do
      logWarning "Unexpected empty driver pool batch cache."
      return []
    whenFoundSomething = \case
      [] -> do
        logWarning "Unexpected empty driver pool batch."
        return []
      a -> return a

sortWithDriverScore ::
  ( Redis.HedisFlow m r,
    HasCacheConfig r,
    EsqDBFlow m r,
    HasDriverPoolConfig r,
    MonadFlow m
  ) =>
  Id Merchant ->
  Maybe TransporterConfig ->
  [DriverPoolWithActualDistResult] ->
  m [DriverPoolWithActualDistResult]
sortWithDriverScore _ Nothing dp = logInfo "Weightages not available in DB, going with random selection" *> randomizeAndLimitSelection dp
sortWithDriverScore merchantId (Just transporterConfig) dp = do
  logTagInfo "Weightage config for intelligent driver pool" $ show transporterConfig
  let driverIds = map (.driverPoolResult.driverId) dp
  cancellationScoreRelatedConfig <- asks (.cancellationScoreRelatedConfig)
  cancellationRatios <-
    getRatios
      ( \dId -> do
          isThresholdRidesDone <- isThresholdRidesCompleted dId merchantId cancellationScoreRelatedConfig
          if isThresholdRidesDone
            then getLatestCancellationRatio merchantId dId
            else pure 0
      )
      driverIds
  acceptanceRatios <- getRatios (getLatestAcceptanceRatio merchantId) driverIds
  driversAvailableTime <- map (second (sum . catMaybes)) <$> getRatios (getCurrentWindowAvailability merchantId) driverIds
  let driverCancellationScore = getScoreWithWeight (transporterConfig.cancellationRatioWeightage) cancellationRatios
  let driverAcceptanceScore = getScoreWithWeight (transporterConfig.acceptanceRatioWeightage) acceptanceRatios
  let driverAvailabilityScore = getScoreWithWeight (transporterConfig.availabilityTimeWeightage) driversAvailableTime
  let overallScore = calculateOverallScore [driverAcceptanceScore, driverAvailabilityScore, driverCancellationScore]
  driverPoolWithoutTie <- breakSameScoreTies $ groupByScore overallScore
  let sortedDriverPool = concatMap snd . sortOn (Down . fst) $ HM.toList driverPoolWithoutTie
  logTagInfo "Driver Acceptance Score" $ show driverAcceptanceScore
  logTagInfo "Driver Cancellation Score" $ show driverCancellationScore
  logTagInfo "Driver Availability Score" $ show driverAvailabilityScore
  logTagInfo "Overall Score" $ show (map (second getDriverId) overallScore)
  mapM (updateDriverPoolResult cancellationScoreRelatedConfig (HM.fromList cancellationRatios) (HM.fromList acceptanceRatios) (HM.fromList driversAvailableTime)) sortedDriverPool
  where
    -- if two drivers have same score in the end then the driver who has less number of ride requests till now will be preffered.
    breakSameScoreTies scoreMap = do
      mapM
        ( \dObjArr ->
            if length dObjArr > 1
              then do
                map snd . sortOn (Down . Down . fst)
                  <$> mapM
                    ( \dObj -> do
                        quotes <- getTotalQuotesSent merchantId (cast dObj.driverPoolResult.driverId)
                        pure (quotes, dObj)
                    )
                    dObjArr
              else pure dObjArr
        )
        scoreMap
    groupByScore =
      foldr'
        ( \(score, dObj) scoreMap -> do
            case HM.lookup score scoreMap of
              Just res -> HM.insert score (dObj : res) scoreMap
              Nothing -> HM.insert score [dObj] scoreMap
        )
        HM.empty
    updateDriverPoolResult cancellationScoreRelatedConfig cancellationRatioMap acceptanceRatioMap driversAvailableTimeMap dObj =
      maybeM
        (pure dObj)
        (addIntelligentPoolInfo cancellationScoreRelatedConfig dObj)
        (pure $ (,,) <$> HM.lookup (getDriverId dObj) cancellationRatioMap <*> HM.lookup (getDriverId dObj) acceptanceRatioMap <*> HM.lookup (getDriverId dObj) driversAvailableTimeMap)
    addIntelligentPoolInfo cancellationScoreRelatedConfig dObj (cancellationRatio, acceptanceRatio, driverAvailableTime) = do
      popupDelay <- getPopupDelay merchantId dObj.driverPoolResult.driverId cancellationRatio cancellationScoreRelatedConfig
      pure $
        dObj
          { rideRequestPopupDelayDuration = popupDelay,
            isPartOfIntelligentPool = True,
            cancellationRatio = Just cancellationRatio,
            acceptanceRatio = Just acceptanceRatio,
            driverAvailableTime = Just driverAvailableTime
          }
    calculateOverallScore scoresList = map (\dObj -> (,dObj) . sum $ mapMaybe (HM.lookup (getDriverId dObj)) scoresList) dp
    getRatios fn arr = mapM (\dId -> (dId.getId,) <$> fn dId) arr
    getDriverId = getId . (.driverPoolResult.driverId)
    getScoreWithWeight weight driverParamsValue = HM.fromList $ map (second (fromIntegral weight *)) driverParamsValue

randomizeAndLimitSelection ::
  (MonadFlow m) =>
  [DriverPoolWithActualDistResult] ->
  m [DriverPoolWithActualDistResult]
randomizeAndLimitSelection = randomizeList

poolBatchNumKey :: Id DSR.SearchRequest -> Text
poolBatchNumKey searchReqId = "Driver-Offer:Allocator:PoolBatchNum:SearchReqId-" <> searchReqId.getId

poolRadiusStepKey :: Id DSR.SearchRequest -> Text
poolRadiusStepKey searchReqId = "Driver-Offer:Allocator:PoolRadiusStep:SearchReqId-" <> searchReqId.getId

cleanupDriverPoolBatches ::
  ( Redis.HedisFlow m r
  ) =>
  Id DSR.SearchRequest ->
  m ()
cleanupDriverPoolBatches searchReqId = do
  Redis.withCrossAppRedis $ do
    Redis.delByPattern (driverPoolKey searchReqId <> "*")
    Redis.del (poolRadiusStepKey searchReqId)
    Redis.del (poolBatchNumKey searchReqId)
  logInfo "Cleanup redis."

getNextDriverPoolBatch ::
  ( EncFlow m r,
    HasCacheConfig r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    EsqDBFlow m r,
    DP.HasDriverPoolConfig r,
    Redis.HedisFlow m r,
    HasSendSearchRequestJobConfig r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  DriverPoolConfig ->
  DSR.SearchRequest ->
  m [DriverPoolWithActualDistResult]
getNextDriverPoolBatch driverPoolConfig searchReq = withLogTag "getNextDriverPoolBatch" do
  batchNum <- getPoolBatchNum searchReq.id
  incrementBatchNum searchReq.id
  prepareDriverPoolBatch driverPoolConfig searchReq batchNum

getPoolBatchNum :: (Redis.HedisFlow m r) => Id DSR.SearchRequest -> m PoolBatchNum
getPoolBatchNum searchReqId = do
  res <- Redis.withCrossAppRedis $ Redis.get (poolBatchNumKey searchReqId)
  case res of
    Just i -> return i
    Nothing -> do
      let expTime = 600
      Redis.withCrossAppRedis $ Redis.setExp (poolBatchNumKey searchReqId) (0 :: Integer) expTime
      return 0

incrementBatchNum ::
  ( Redis.HedisFlow m r
  ) =>
  Id DSR.SearchRequest ->
  m ()
incrementBatchNum searchReqId = do
  res <- Redis.withCrossAppRedis $ Redis.incr (poolBatchNumKey searchReqId)
  logInfo $ "Increment batch num to " <> show res <> "."
  return ()

getPoolRadiusStep :: (Redis.HedisFlow m r) => Id DSR.SearchRequest -> m PoolRadiusStep
getPoolRadiusStep searchReqId = do
  res <- Redis.withCrossAppRedis $ Redis.get (poolRadiusStepKey searchReqId)
  case res of
    Just i -> return i
    Nothing -> do
      let expTime = 600
      Redis.withCrossAppRedis $ Redis.setExp (poolRadiusStepKey searchReqId) (0 :: Integer) expTime
      return 0

incrementPoolRadiusStep ::
  ( Redis.HedisFlow m r
  ) =>
  Id DSR.SearchRequest ->
  m ()
incrementPoolRadiusStep searchReqId = do
  res <- Redis.withCrossAppRedis $ Redis.incr (poolRadiusStepKey searchReqId)
  logInfo $ "Increment radius step to " <> show res <> "."
  return ()

driverRequestCountKey :: Id DSR.SearchRequest -> Id Driver -> Text
driverRequestCountKey searchReqId driverId = "Driver-Request-Count-Key:SearchReqId-DriverId" <> searchReqId.getId <> driverId.getId

checkRequestCount :: (Redis.HedisFlow m r, HasDriverPoolConfig r) => Id DSR.SearchRequest -> Id Driver -> DriverPoolConfig -> m Bool
checkRequestCount searchReqId driverId driverPoolConfig = maybe True (\count -> (count :: Int) < driverPoolConfig.driverRequestCountLimit) <$> Redis.withCrossAppRedis (Redis.get (driverRequestCountKey searchReqId driverId))

incrementDriverRequestCount :: (Redis.HedisFlow m r) => [DriverPoolWithActualDistResult] -> Id DSR.SearchRequest -> m ()
incrementDriverRequestCount finalPoolBatch searchReqId = do
  CM.mapM_
    ( \dpr ->
        Redis.withCrossAppRedis do
          void $ Redis.incr (driverRequestCountKey searchReqId dpr.driverPoolResult.driverId)
          Redis.expire (driverRequestCountKey searchReqId dpr.driverPoolResult.driverId) 7200
    )
    finalPoolBatch
