{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool
  ( isBatchNumExceedLimit,
    cleanupDriverPoolBatches,
    getNextDriverPoolBatch,
    getPoolBatchNum,
    module Reexport,
  )
where

import qualified Control.Monad as CM
import qualified Data.HashMap as HM
import qualified Data.List as DL
import qualified Domain.Types.DriverInformation as DriverInfo
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.DriverIntelligentPoolConfig
import Domain.Types.Merchant.DriverPoolConfig
import Domain.Types.Merchant.TransporterConfig (TransporterConfig)
import Domain.Types.Person (Driver)
import qualified Domain.Types.SearchRequest as DSR
import EulerHS.Prelude hiding (id)
import Kernel.Randomizer (randomizeList)
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowCounters
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config as Reexport
import SharedLogic.DriverPool
import Storage.CachedQueries.CacheConfig (CacheFlow, HasCacheConfig)
import qualified Storage.CachedQueries.Merchant.DriverIntelligentPoolConfig as DIP
import qualified Storage.CachedQueries.Merchant.TransporterConfig as TC
import Tools.Maps as Maps
import Tools.Metrics

isBatchNumExceedLimit ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  DriverPoolConfig ->
  Id DSR.SearchRequest ->
  m Bool
isBatchNumExceedLimit driverPoolConfig searchReqId = do
  let maxNumberOfBatches = driverPoolConfig.maxNumberOfBatches
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
    Redis.HedisFlow m r
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
      transporterConfig <- TC.findByMerchantId searchReq.providerId >>= fromMaybeM (TransporterConfigDoesNotExist searchReq.providerId.getId)
      intelligentPoolConfig <- DIP.findByMerchantId searchReq.providerId >>= fromMaybeM (InternalError "Intelligent Pool Config not found")
      allNearbyDriversCurrentlyNotOnRide <- calcDriverPool radiusStep
      let reduceRadiusValue = driverPoolCfg.radiusShrinkValueForDriversOnRide
      allNearbyDriversCurrentlyOnRide <- calcDriverCurrentlyOnRidePool radiusStep reduceRadiusValue transporterConfig
      let allNearbyDrivers = allNearbyDriversCurrentlyOnRide ++ allNearbyDriversCurrentlyNotOnRide
      let sortingType = driverPoolCfg.poolSortingType
      let batchSize = driverPoolCfg.driverBatchSize
      logDebug $ "DriverPool-" <> show allNearbyDrivers
      let onlyNewDrivers = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` previousBatchesDrivers) allNearbyDrivers
      if length onlyNewDrivers < batchSize
        then do
          isAtMaxRadiusStep' <- isAtMaxRadiusStep radiusStep
          if isAtMaxRadiusStep'
            then calculatePool batchSize sortingType onlyNewDrivers allNearbyDrivers intelligentPoolConfig transporterConfig
            else do
              incrementPoolRadiusStep searchReq.id
              prepareDriverPoolBatch' previousBatchesDrivers
        else calculatePool batchSize sortingType onlyNewDrivers allNearbyDrivers intelligentPoolConfig transporterConfig
      where
        calculatePool batchSize sortingType onlyNewDrivers allNearbyDrivers intelligentPoolConfig transporterConfig = do
          driverPoolBatch <- mkDriverPoolBatch batchSize sortingType onlyNewDrivers intelligentPoolConfig transporterConfig
          logDebug $ "DriverPoolBatch-" <> show driverPoolBatch
          finalPoolBatch <-
            if length driverPoolBatch < batchSize
              then do
                filledBatch <- fillBatch searchReq.providerId sortingType batchSize allNearbyDrivers driverPoolBatch intelligentPoolConfig
                logDebug $ "FilledDriverPoolBatch-" <> show filledBatch
                pure filledBatch
              else do
                pure driverPoolBatch
          incrementDriverRequestCount finalPoolBatch searchReq.id
          cacheBatch finalPoolBatch
          pure finalPoolBatch

        mkDriverPoolBatch batchSize sortingType onlyNewDrivers intelligentPoolConfig transporterConfig = do
          case sortingType of
            Intelligent -> makeIntelligentDriverPool batchSize searchReq.providerId onlyNewDrivers intelligentPoolConfig transporterConfig
            Random -> makeRandomDriverPool batchSize onlyNewDrivers

        makeIntelligentDriverPool batchSize merchantId onlyNewDrivers intelligentPoolConfig transporterConfig = do
          let sortWithDriverScore' = sortWithDriverScore merchantId (Just transporterConfig) intelligentPoolConfig
          (sortedDriverPool, randomizedDriverPool) <-
            bimapM (sortWithDriverScore' [AcceptanceRatio, CancellationRatio, AvailableTime, DriverSpeed] True) (sortWithDriverScore' [AvailableTime, DriverSpeed] False)
              =<< splitDriverPoolForSorting merchantId intelligentPoolConfig.minQuotesToQualifyForIntelligentPool onlyNewDrivers
          takeDriversUsingPoolPercentage (sortedDriverPool, randomizedDriverPool) batchSize intelligentPoolConfig

        makeRandomDriverPool batchSize onlyNewDrivers = take batchSize <$> randomizeAndLimitSelection onlyNewDrivers

    takeDriversUsingPoolPercentage (sortedDriverPool, randomizedDriverPool) driversCount intelligentPoolConfig = do
      let intelligentPoolPercentage = fromMaybe 50 intelligentPoolConfig.intelligentPoolPercentage
          intelligentCount = min (length sortedDriverPool) ((driversCount + 1) * intelligentPoolPercentage `div` 100)
          randomCount = driversCount - intelligentCount
          (sortedPart, restSorted) = splitAt intelligentCount sortedDriverPool
          (randomPart, restRandom) = splitAt randomCount randomizedDriverPool
          poolBatch = sortedPart <> randomPart
          driversFromRestCount = take (driversCount - length poolBatch) (restRandom <> restSorted) -- taking rest of drivers if poolBatch length is less then driverCount requried.
          finalPoolBatch = poolBatch <> driversFromRestCount
      let (silentDrivers, activeDrivers) = DL.partition ((== Just DriverInfo.SILENT) . (.driverPoolResult.mode)) finalPoolBatch
      let finalPoolBatchWithSilentDrivers = activeDrivers <> silentDrivers
      logDebug $ "IntelligentDriverPool - SortedDriversCount " <> show (length sortedPart)
      logDebug $ "IntelligentDriverPool - RandomizedDriversCount " <> show (length randomPart)
      logDebug $ "IntelligentDriverPool - finalPoolBatch " <> show (length finalPoolBatch)
      logDebug $ "IntelligentDriverPool - finalPoolBatchWithSilentDrivers " <> show (length finalPoolBatchWithSilentDrivers)
      pure finalPoolBatchWithSilentDrivers

    calcDriverPool radiusStep = do
      let vehicleVariant = searchReq.vehicleVariant
          merchantId = searchReq.providerId
      let pickupLoc = searchReq.fromLocation
      let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
      calculateDriverPoolWithActualDist DriverSelection driverPoolCfg (Just vehicleVariant) pickupLatLong merchantId True (Just radiusStep)
    calcDriverCurrentlyOnRidePool radiusStep reduceRadiusValue transporterConfig = do
      let merchantId = searchReq.providerId
      if transporterConfig.includeDriverCurrentlyOnRide && (radiusStep - 1) > 0
        then do
          let vehicleVariant = searchReq.vehicleVariant
          let pickupLoc = searchReq.fromLocation
          let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
          calculateDriverCurrentlyOnRideWithActualDist DriverSelection driverPoolCfg (Just vehicleVariant) pickupLatLong merchantId (Just $ radiusStep - 1) reduceRadiusValue
        else pure []
    fillBatch merchantId sortingType batchSize allNearbyDrivers batch intelligentPoolConfig = do
      let batchDriverIds = batch <&> (.driverPoolResult.driverId)
      let driversNotInBatch = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` batchDriverIds) allNearbyDrivers
      driversWithValidReqAmount <- filterM (\dpr -> checkRequestCount searchReq.id dpr.driverPoolResult.driverId driverPoolCfg) driversNotInBatch
      let fillSize = batchSize - length batch
      transporterConfig <- TC.findByMerchantId merchantId
      (batch <>)
        <$> case sortingType of
          Intelligent -> do
            let sortWithDriverScore' = sortWithDriverScore merchantId transporterConfig intelligentPoolConfig
            (sortedDriverPool, randomizedDriverPool) <-
              bimapM (sortWithDriverScore' [AcceptanceRatio, CancellationRatio, AvailableTime, DriverSpeed] True) (sortWithDriverScore' [AvailableTime, DriverSpeed] False)
                =<< splitDriverPoolForSorting merchantId intelligentPoolConfig.minQuotesToQualifyForIntelligentPool driversWithValidReqAmount -- snd means taking drivers who recieved less then X(config- minQuotesToQualifyForIntelligentPool) quotes
            takeDriversUsingPoolPercentage (sortedDriverPool, randomizedDriverPool) fillSize intelligentPoolConfig
          Random -> pure $ take fillSize driversWithValidReqAmount
    cacheBatch batch = do
      logDebug $ "Caching batch-" <> show batch
      Redis.withCrossAppRedis $ Redis.setExp (driverPoolBatchKey searchReq.id batchNum) batch (60 * 10)
    -- splitDriverPoolForSorting :: minQuotes Int -> [DriverPool Array] -> ([GreaterThanMinQuotesDP], [LessThanMinQuotesDP])
    splitDriverPoolForSorting merchantId minQuotes =
      foldrM
        ( \dObj (gtX, ltX) -> do
            driverQuotesCount <- getQuotesCount merchantId dObj.driverPoolResult.driverId
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
    Redis.safeGet (driverPoolBatchKey searchReqId batchNum)
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
    MonadFlow m
  ) =>
  Id Merchant ->
  Maybe TransporterConfig ->
  DriverIntelligentPoolConfig ->
  [IntelligentFactors] ->
  Bool ->
  [DriverPoolWithActualDistResult] ->
  m [DriverPoolWithActualDistResult]
sortWithDriverScore _ Nothing _ _ _ dp = logInfo "Weightages not available in DB, going with random selection" *> randomizeAndLimitSelection dp
sortWithDriverScore merchantId (Just transporterConfig) intelligentPoolConfig factorsToCalculate isPartOfIntelligentPool dp = do
  logTagInfo "Weightage config for intelligent driver pool" $ show transporterConfig
  let driverIds = map (.driverPoolResult.driverId) dp
  let cancellationScoreRelatedConfig = CancellationScoreRelatedConfig transporterConfig.popupDelayToAddAsPenalty transporterConfig.thresholdCancellationScore transporterConfig.minRidesForCancellationScore
  calculatedScores <- mapM (fetchScore merchantId driverIds intelligentPoolConfig cancellationScoreRelatedConfig) factorsToCalculate
  let overallScore = calculateOverallScore calculatedScores
  driverPoolWithoutTie <- breakSameScoreTies $ groupByScore overallScore
  let sortedDriverPool = concatMap snd . sortOn (Down . fst) $ HM.toList driverPoolWithoutTie
  logTagInfo "Overall Score" $ show (map (second getDriverId) overallScore)
  mapM (updateDriverPoolResult cancellationScoreRelatedConfig $ zip calculatedScores factorsToCalculate) sortedDriverPool
  where
    updateDriverPoolResult cancellationScoreRelatedConfig factorOverallScore dObj = do
      let intelligentScores =
            foldl
              ( \accIntelligentScores (scoreMap, factor) -> do
                  let res :: Maybe Double = HM.lookup (getDriverId dObj) scoreMap
                  case factor of
                    AcceptanceRatio -> accIntelligentScores {acceptanceRatio = res} :: IntelligentScores
                    CancellationRatio -> accIntelligentScores {cancellationRatio = res} :: IntelligentScores
                    AvailableTime -> accIntelligentScores {availableTime = res} :: IntelligentScores
                    DriverSpeed -> accIntelligentScores {driverSpeed = res} :: IntelligentScores
              )
              (IntelligentScores Nothing Nothing Nothing Nothing 0)
              factorOverallScore
      addIntelligentPoolInfo cancellationScoreRelatedConfig dObj intelligentScores
    addIntelligentPoolInfo cancellationScoreRelatedConfig dObj is@IntelligentScores {..} = do
      popupDelay <-
        maybe
          (pure transporterConfig.defaultPopupDelay)
          (\cr -> getPopupDelay merchantId dObj.driverPoolResult.driverId cr cancellationScoreRelatedConfig transporterConfig.defaultPopupDelay)
          cancellationRatio
      pure $
        dObj
          { isPartOfIntelligentPool = isPartOfIntelligentPool,
            intelligentScores = is {rideRequestPopupDelayDuration = popupDelay}
          }

    getDriverId = getId . (.driverPoolResult.driverId)
    calculateOverallScore scoresList = map (\dObj -> (,dObj) . (/ (fromIntegral $ length scoresList)) . sum $ mapMaybe (HM.lookup (getDriverId dObj)) scoresList) dp
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

fetchScore ::
  ( Redis.HedisFlow m r,
    HasCacheConfig r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id Merchant ->
  [Id Driver] ->
  DriverIntelligentPoolConfig ->
  CancellationScoreRelatedConfig ->
  IntelligentFactors ->
  m (HM.Map Text Double)
fetchScore merchantId driverIds intelligentPoolConfig cancellationScoreRelatedConfig factor =
  HM.fromList <$> case factor of
    AcceptanceRatio | intelligentPoolConfig.acceptanceRatioWeightage /= 0 -> do
      acceptanceRatios <- getRatios (getLatestAcceptanceRatio merchantId) driverIds
      getScoreWithWeight (intelligentPoolConfig.acceptanceRatioWeightage) acceptanceRatios
    CancellationRatio | intelligentPoolConfig.cancellationRatioWeightage /= 0 -> do
      cancellationRatios <-
        getRatios (getLatestCancellationRatio cancellationScoreRelatedConfig merchantId) driverIds
      getScoreWithWeight (intelligentPoolConfig.cancellationRatioWeightage) cancellationRatios
    AvailableTime | intelligentPoolConfig.availabilityTimeWeightage /= 0 -> do
      let maxAvailbaleTime = fromIntegral $ intelligentPoolConfig.availabilityTimeWindowOption.period * convertPeriodTypeToSeconds (intelligentPoolConfig.availabilityTimeWindowOption.periodType)
      driversAvailableTimeRatio <- map (second ((/ maxAvailbaleTime) . sum . catMaybes)) <$> getRatios (getCurrentWindowAvailability merchantId) driverIds
      getScoreWithWeight (intelligentPoolConfig.availabilityTimeWeightage) driversAvailableTimeRatio
    DriverSpeed | intelligentPoolConfig.driverSpeedWeightage /= 0 -> do
      averageSpeeds <- getRatios (getDriverAverageSpeed merchantId . cast) driverIds
      getSpeedScore (intelligentPoolConfig.driverSpeedWeightage) averageSpeeds
    _ -> pure []
  where
    getSpeedScore weight driverSpeeds = pure $ map (\(driverId, driverSpeed) -> (driverId, (1 - driverSpeed / intelligentPoolConfig.speedNormalizer) * fromIntegral weight)) driverSpeeds
    getRatios fn arr = mapM (\dId -> (dId.getId,) <$> fn dId) arr
    getScoreWithWeight weight driverParamsValue = pure $ map (second (fromIntegral weight *)) driverParamsValue

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
    Redis.HedisFlow m r
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

checkRequestCount :: Redis.HedisFlow m r => Id DSR.SearchRequest -> Id Driver -> DriverPoolConfig -> m Bool
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
