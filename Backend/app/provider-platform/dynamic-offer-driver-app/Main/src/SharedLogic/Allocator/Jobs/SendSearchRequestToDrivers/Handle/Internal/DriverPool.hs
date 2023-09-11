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
    getNextDriverPoolBatch,
    getPoolBatchNum,
    module Reexport,
  )
where

import qualified Control.Monad as CM
import Data.Foldable.Extra (notNull)
import qualified Data.HashMap as HM
import qualified Data.List as DL
import Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import qualified Domain.Types.DriverInformation as DriverInfo
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.DriverIntelligentPoolConfig
import Domain.Types.Merchant.DriverPoolConfig
import Domain.Types.Merchant.MerchantConfig
import Domain.Types.Person (Driver)
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import EulerHS.Prelude hiding (id)
import Kernel.Randomizer (randomizeList)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow, EsqLocDBFlow, EsqLocRepDBFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowCounters
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config as Reexport
import SharedLogic.DriverPool
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.GoHomeConfig as CQGHC
import qualified Storage.CachedQueries.Merchant.DriverIntelligentPoolConfig as DIP
import qualified Storage.CachedQueries.Merchant.MerchantConfig as TC
import Tools.Maps as Maps

isBatchNumExceedLimit ::
  ( EsqDBFlow m r,
    CacheFlow m r
  ) =>
  DriverPoolConfig ->
  Id DST.SearchTry ->
  m Bool
isBatchNumExceedLimit driverPoolConfig searchTryId = do
  let maxNumberOfBatches = driverPoolConfig.maxNumberOfBatches
  currentBatchNum <- getPoolBatchNum searchTryId
  return $ currentBatchNum >= maxNumberOfBatches

previouslyAttemptedDriversKey :: Id DST.SearchTry -> Text
previouslyAttemptedDriversKey searchTryId = "Driver-Offer:PreviouslyAttemptedDrivers:SearchTryId-" <> searchTryId.getId

prepareDriverPoolBatch ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    EsqLocDBFlow m r,
    EsqLocRepDBFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  DriverPoolConfig ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  PoolBatchNum ->
  m ([DriverPoolWithActualDistResult], Bool)
prepareDriverPoolBatch driverPoolCfg searchReq searchTry batchNum = withLogTag ("BatchNum-" <> show batchNum) $ do
  previousBatchesDrivers <- getPreviousBatchesDrivers
  logDebug $ "PreviousBatchesDrivers-" <> show previousBatchesDrivers
  prepareDriverPoolBatch' previousBatchesDrivers True
  where
    getPreviousBatchesDrivers = do
      batches <- previouslyAttemptedDrivers searchTry.id
      return $ (.driverPoolResult.driverId) <$> batches

    prepareDriverPoolBatch' previousBatchesDrivers doGoHomePooling = do
      radiusStep <- getPoolRadiusStep searchReq.id
      transporterConfig <- TC.findByMerchantId searchReq.providerId >>= fromMaybeM (TransporterConfigDoesNotExist searchReq.providerId.getId)
      intelligentPoolConfig <- DIP.findByMerchantId searchReq.providerId >>= fromMaybeM (InternalError "Intelligent Pool Config not found")
      blockListedDrivers <- Redis.withCrossAppRedis $ Redis.getList (mkBlockListedDriversKey searchReq.id)
      logDebug $ "Blocked Driver List-" <> show blockListedDrivers
      goHomeConfig <- CQGHC.findByMerchantId searchReq.providerId
      allNearbyGoHomeDrivers <-
        if batchNum == 0 && goHomeConfig.enableGoHome && doGoHomePooling
          then calcGoHomeDriverPool goHomeConfig
          else return []
      (currentDriverPoolBatch, isGoToPool) <-
        if notNull allNearbyGoHomeDrivers
          then (,True) <$> calculateGoHomeBatch transporterConfig intelligentPoolConfig allNearbyGoHomeDrivers blockListedDrivers
          else do
            allNearbyDriversCurrentlyNotOnRide <- calcDriverPool radiusStep
            allNearbyDriversCurrentlyOnRide <- calcDriverCurrentlyOnRidePool radiusStep transporterConfig
            (,False) <$> calculateNormalBatch transporterConfig intelligentPoolConfig (allNearbyDriversCurrentlyOnRide <> allNearbyDriversCurrentlyNotOnRide) radiusStep blockListedDrivers goHomeConfig
      incrementDriverRequestCount currentDriverPoolBatch searchTry.id
      cacheBatch currentDriverPoolBatch
      pure (addDistanceSplitConfigBasedDelaysForDriversWithinBatch currentDriverPoolBatch, isGoToPool)
      where
        calculateGoHomeBatch transporterConfig intelligentPoolConfig allNearbyGoHomeDrivers blockListedDrivers = do
          let allNearbyGoHomeDrivers' = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` blockListedDrivers) allNearbyGoHomeDrivers
          logDebug $ "GoHomeDriverPool-" <> show allNearbyGoHomeDrivers'
          let onlyNewGoHomeDrivers = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` previousBatchesDrivers) allNearbyGoHomeDrivers'
          goHomeDriverPoolBatch <- mkDriverPoolBatch onlyNewGoHomeDrivers intelligentPoolConfig transporterConfig
          logDebug $ "GoHomeDriverPoolBatch-" <> show goHomeDriverPoolBatch
          pure goHomeDriverPoolBatch

        calculateNormalBatch transporterConfig intelligentPoolConfig normalDriverPool radiusStep blockListedDrivers goHomeConfig = do
          logDebug $ "NormalDriverPool-" <> show normalDriverPool
          allNearbyNonGoHomeDrivers <- filterM (\dpr -> (CQDGR.getDriverGoHomeRequestInfo dpr.driverPoolResult.driverId searchReq.providerId (Just goHomeConfig)) <&> (/= Just DDGR.ACTIVE) . (.status)) normalDriverPool
          let allNearbyDrivers = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` blockListedDrivers) allNearbyNonGoHomeDrivers
          let onlyNewNormalDrivers = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` previousBatchesDrivers) allNearbyDrivers
          if length onlyNewNormalDrivers < batchSize && not (isAtMaxRadiusStep radiusStep)
            then do
              incrementPoolRadiusStep searchReq.id
              (batch, _) <- prepareDriverPoolBatch' previousBatchesDrivers False
              pure batch
            else do
              normalDriverPoolBatch <- mkDriverPoolBatch onlyNewNormalDrivers intelligentPoolConfig transporterConfig
              logDebug $ "NormalDriverPoolBatch-" <> show normalDriverPoolBatch
              if length normalDriverPoolBatch < batchSize
                then do
                  filledBatch <- fillBatch searchReq.providerId normalDriverPool normalDriverPoolBatch intelligentPoolConfig goHomeConfig
                  logDebug $ "FilledDriverPoolBatch-" <> show filledBatch
                  pure filledBatch
                else do
                  pure normalDriverPoolBatch

        mkDriverPoolBatch onlyNewDrivers intelligentPoolConfig transporterConfig = do
          case sortingType of
            Intelligent -> makeIntelligentDriverPool searchReq.providerId onlyNewDrivers intelligentPoolConfig transporterConfig
            Random -> makeRandomDriverPool onlyNewDrivers

        makeIntelligentDriverPool merchantId onlyNewDrivers intelligentPoolConfig transporterConfig = do
          let sortWithDriverScore' = sortWithDriverScore merchantId (Just transporterConfig) intelligentPoolConfig driverPoolCfg
          (sortedDriverPool, randomizedDriverPool) <-
            bimapM (sortWithDriverScore' [AcceptanceRatio, CancellationRatio, AvailableTime, DriverSpeed, ActualPickupDistance] True) (sortWithDriverScore' [AvailableTime, DriverSpeed, ActualPickupDistance] False)
              =<< splitDriverPoolForSorting merchantId intelligentPoolConfig.minQuotesToQualifyForIntelligentPool onlyNewDrivers
          let sortedDriverPoolWithSilentSort = splitSilentDriversAndSortWithDistance sortedDriverPool
          let randomizedDriverPoolWithSilentSort = splitSilentDriversAndSortWithDistance randomizedDriverPool
          takeDriversUsingPoolPercentage (sortedDriverPoolWithSilentSort, randomizedDriverPoolWithSilentSort) batchSize intelligentPoolConfig

        makeRandomDriverPool onlyNewDrivers = take batchSize <$> randomizeAndLimitSelection onlyNewDrivers

        takeDriversUsingPoolPercentage :: (MonadFlow m) => ([DriverPoolWithActualDistResult], [DriverPoolWithActualDistResult]) -> Int -> DriverIntelligentPoolConfig -> m [DriverPoolWithActualDistResult]
        takeDriversUsingPoolPercentage (sortedDriverPool, randomizedDriverPool) driversCount intelligentPoolConfig = do
          let intelligentPoolPercentage = fromMaybe 50 intelligentPoolConfig.intelligentPoolPercentage
              intelligentCount = min (length sortedDriverPool) ((driversCount + 1) * intelligentPoolPercentage `div` 100)
              randomCount = driversCount - intelligentCount
              (sortedPart, restSorted) = splitAt intelligentCount sortedDriverPool
              (randomPart, restRandom) = splitAt randomCount randomizedDriverPool
              poolBatch = sortedPart <> randomPart
              driversFromRestCount = take (driversCount - length poolBatch) (restRandom <> restSorted) -- taking rest of drivers if poolBatch length is less then driverCount requried.
          logDebug $ "IntelligentDriverPool - SortedDriversCount " <> show (length sortedPart)
          logDebug $ "IntelligentDriverPool - RandomizedDriversCount " <> show (length randomPart)
          logDebug $ "IntelligentDriverPool - DriversFromRestCount " <> show (length driversFromRestCount)
          pure $ poolBatch <> driversFromRestCount

        addDistanceSplitConfigBasedDelaysForDriversWithinBatch filledPoolBatch =
          fst $
            foldl'
              ( \(finalBatch, restBatch) splitConfig -> do
                  let (splitToAddDelay, newRestBatch) = splitAt splitConfig.batchSplitSize restBatch
                      splitWithDelay = map (\driverWithDistance -> driverWithDistance {keepHiddenForSeconds = splitConfig.batchSplitDelay}) splitToAddDelay
                  (finalBatch <> splitWithDelay, newRestBatch)
              )
              ([], filledPoolBatch)
              driverPoolCfg.distanceBasedBatchSplit

        calcGoHomeDriverPool goHomeConfig = do
          calculateGoHomeDriverPool $
            CalculateGoHomeDriverPoolReq
              { poolStage = DriverSelection,
                driverPoolCfg = driverPoolCfg,
                goHomeCfg = goHomeConfig,
                variant = Just searchTry.vehicleVariant,
                fromLocation = searchReq.fromLocation,
                toLocation = searchReq.toLocation,
                merchantId = searchReq.providerId
              }

        calcDriverPool radiusStep = do
          let vehicleVariant = searchTry.vehicleVariant
              merchantId = searchReq.providerId
          let pickupLoc = searchReq.fromLocation
          let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
          calculateDriverPoolWithActualDist DriverSelection driverPoolCfg (Just vehicleVariant) pickupLatLong merchantId True (Just radiusStep)
        calcDriverCurrentlyOnRidePool radiusStep transporterConfig = do
          let merchantId = searchReq.providerId
          if transporterConfig.includeDriverCurrentlyOnRide && (radiusStep - 1) > 0
            then do
              let vehicleVariant = searchTry.vehicleVariant
              let pickupLoc = searchReq.fromLocation
              let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
              calculateDriverCurrentlyOnRideWithActualDist DriverSelection driverPoolCfg (Just vehicleVariant) pickupLatLong merchantId (Just $ radiusStep - 1)
            else pure []
        fillBatch merchantId allNearbyDrivers batch intelligentPoolConfig goHomeConfig = do
          let batchDriverIds = batch <&> (.driverPoolResult.driverId)
          let driversNotInBatch = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` batchDriverIds) allNearbyDrivers
          driversWithValidReqAmount <- filterM (\dpr -> checkRequestCount searchTry.id dpr.driverPoolResult.driverId driverPoolCfg) driversNotInBatch
          nonGoHomeDriversWithValidReqCount <- filterM (\dpr -> (CQDGR.getDriverGoHomeRequestInfo dpr.driverPoolResult.driverId searchReq.providerId (Just goHomeConfig)) <&> (/= Just DDGR.ACTIVE) . (.status)) driversWithValidReqAmount
          let fillSize = batchSize - length batch
          transporterConfig <- TC.findByMerchantId merchantId
          (batch <>)
            <$> case sortingType of
              Intelligent -> do
                let sortWithDriverScore' = sortWithDriverScore merchantId transporterConfig intelligentPoolConfig driverPoolCfg
                (sortedDriverPool, randomizedDriverPool) <-
                  bimapM (sortWithDriverScore' [AcceptanceRatio, CancellationRatio, AvailableTime, DriverSpeed, ActualPickupDistance] True) (sortWithDriverScore' [AvailableTime, DriverSpeed, ActualPickupDistance] False)
                    =<< splitDriverPoolForSorting merchantId intelligentPoolConfig.minQuotesToQualifyForIntelligentPool nonGoHomeDriversWithValidReqCount -- snd means taking drivers who recieved less then X(config- minQuotesToQualifyForIntelligentPool) quotes
                let sortedDriverPoolWithSilentSort = splitSilentDriversAndSortWithDistance sortedDriverPool
                let randomizedDriverPoolWithSilentSort = splitSilentDriversAndSortWithDistance randomizedDriverPool
                takeDriversUsingPoolPercentage (sortedDriverPoolWithSilentSort, randomizedDriverPoolWithSilentSort) fillSize intelligentPoolConfig
              Random -> pure $ take fillSize nonGoHomeDriversWithValidReqCount
        cacheBatch batch = do
          logDebug $ "Caching batch-" <> show batch
          batches <- previouslyAttemptedDrivers searchTry.id
          Redis.withCrossAppRedis $ Redis.setExp (previouslyAttemptedDriversKey searchTry.id) (batches <> batch) (60 * 30)
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
          maxRadiusStep <= radiusStep
        -- util function
        bimapM fna fnb (a, b) = (,) <$> fna a <*> fnb b

        sortingType = driverPoolCfg.poolSortingType
        batchSize = driverPoolCfg.driverBatchSize

splitSilentDriversAndSortWithDistance :: [DriverPoolWithActualDistResult] -> [DriverPoolWithActualDistResult]
splitSilentDriversAndSortWithDistance drivers = do
  let (silentDrivers, activeDrivers) = bimap (sortOn (.actualDistanceToPickup)) (sortOn (.actualDistanceToPickup)) $ DL.partition ((== Just DriverInfo.SILENT) . (.driverPoolResult.mode)) drivers
  activeDrivers <> silentDrivers

previouslyAttemptedDrivers ::
  ( Redis.HedisFlow m r
  ) =>
  Id DST.SearchTry ->
  m [DriverPoolWithActualDistResult]
previouslyAttemptedDrivers searchTryId = do
  Redis.withCrossAppRedis $
    Redis.safeGet (previouslyAttemptedDriversKey searchTryId)
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
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id Merchant ->
  Maybe MerchantConfig ->
  DriverIntelligentPoolConfig ->
  DriverPoolConfig ->
  [IntelligentFactors] ->
  Bool ->
  [DriverPoolWithActualDistResult] ->
  m [DriverPoolWithActualDistResult]
sortWithDriverScore _ Nothing _ _ _ _ dp = logInfo "Weightages not available in DB, going with random selection" *> randomizeAndLimitSelection dp
sortWithDriverScore merchantId (Just transporterConfig) intelligentPoolConfig driverPoolCfg factorsToCalculate isPartOfIntelligentPool dp = do
  logTagInfo "Weightage config for intelligent driver pool" $ show transporterConfig
  let driverIds = map (.driverPoolResult.driverId) dp
  let driverActualDistances = map ((.driverPoolResult.driverId) &&& (.driverPoolResult.distanceToPickup)) dp
  let cancellationScoreRelatedConfig = CancellationScoreRelatedConfig transporterConfig.popupDelayToAddAsPenalty transporterConfig.thresholdCancellationScore transporterConfig.minRidesForCancellationScore
  calculatedScores <- mapM (fetchScore merchantId driverActualDistances driverIds intelligentPoolConfig driverPoolCfg cancellationScoreRelatedConfig) factorsToCalculate
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
                    ActualPickupDistance -> accIntelligentScores {actualPickupDistanceScore = res} :: IntelligentScores
                    CancellationRatio -> accIntelligentScores {cancellationRatio = res} :: IntelligentScores
                    AvailableTime -> accIntelligentScores {availableTime = res} :: IntelligentScores
                    DriverSpeed -> accIntelligentScores {driverSpeed = res} :: IntelligentScores
              )
              (IntelligentScores Nothing Nothing Nothing Nothing Nothing 0)
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
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  [(Id Driver, Meters)] ->
  [Id Driver] ->
  DriverIntelligentPoolConfig ->
  DriverPoolConfig ->
  CancellationScoreRelatedConfig ->
  IntelligentFactors ->
  m (HM.Map Text Double)
fetchScore merchantId driverActualDistanceList driverIds intelligentPoolConfig driverPoolCfg cancellationScoreRelatedConfig factor =
  HM.fromList <$> case factor of
    AcceptanceRatio | intelligentPoolConfig.acceptanceRatioWeightage /= 0 -> do
      acceptanceRatios <- getRatios (getLatestAcceptanceRatio merchantId) driverIds
      getScoreWithWeight (intelligentPoolConfig.acceptanceRatioWeightage) acceptanceRatios
    CancellationRatio | intelligentPoolConfig.cancellationRatioWeightage /= 0 -> do
      cancellationRatios <-
        getRatios (getLatestCancellationRatio cancellationScoreRelatedConfig merchantId) driverIds
      getScoreWithWeight (intelligentPoolConfig.cancellationRatioWeightage) cancellationRatios
    AvailableTime | intelligentPoolConfig.availabilityTimeWeightage /= 0 -> do
      let maxAvailbaleTime = fromIntegral $ intelligentPoolConfig.availabilityTimeWindowOption.period * convertPeriodTypeToSeconds intelligentPoolConfig.availabilityTimeWindowOption.periodType
      driversAvailableTimeRatio <- map (second ((/ maxAvailbaleTime) . sum . catMaybes)) <$> getRatios (getCurrentWindowAvailability merchantId) driverIds
      getScoreWithWeight (intelligentPoolConfig.availabilityTimeWeightage) driversAvailableTimeRatio
    DriverSpeed | intelligentPoolConfig.driverSpeedWeightage /= 0 -> do
      averageSpeeds <- getRatios (getDriverAverageSpeed merchantId . cast) driverIds
      getSpeedScore (intelligentPoolConfig.driverSpeedWeightage) averageSpeeds
    ActualPickupDistance | intelligentPoolConfig.actualPickupDistanceWeightage /= 0 -> do
      pure $ map (bimap (.getId) ((* (fromIntegral intelligentPoolConfig.actualPickupDistanceWeightage)) . fromIntegral . flip div (fromMaybe (Meters 1) (driverPoolCfg.actualDistanceThreshold)))) driverActualDistanceList
    _ -> pure []
  where
    getSpeedScore weight driverSpeeds = pure $ map (\(driverId, driverSpeed) -> (driverId, (1 - driverSpeed / intelligentPoolConfig.speedNormalizer) * fromIntegral weight)) driverSpeeds
    getRatios fn = mapM (\dId -> (dId.getId,) <$> fn dId)
    getScoreWithWeight weight driverParamsValue = pure $ map (second (fromIntegral weight *)) driverParamsValue

randomizeAndLimitSelection ::
  MonadFlow m =>
  [DriverPoolWithActualDistResult] ->
  m [DriverPoolWithActualDistResult]
randomizeAndLimitSelection = randomizeList

poolBatchNumKey :: Id DST.SearchTry -> Text
poolBatchNumKey searchTryId = "Driver-Offer:Allocator:PoolBatchNum:SearchTryId-" <> searchTryId.getId

poolRadiusStepKey :: Id DSR.SearchRequest -> Text
poolRadiusStepKey searchReqId = "Driver-Offer:Allocator:PoolRadiusStep:SearchReqId-" <> searchReqId.getId

-- cleanupDriverPoolBatches ::
--   ( Redis.HedisFlow m r
--   ) =>
--   Id DSR.SearchRequest ->
--   m ()
-- cleanupDriverPoolBatches searchReqId = do
--   Redis.withCrossAppRedis $ do
--     Redis.delByPattern (previouslyAttemptedDriversKey searchReqId <> "*")
--     Redis.del (poolRadiusStepKey searchReqId)
--     Redis.del (poolBatchNumKey searchReqId)
--   logInfo "Cleanup redis."

getNextDriverPoolBatch ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqLocDBFlow m r,
    EsqLocRepDBFlow m r,
    EsqDBFlow m r
  ) =>
  DriverPoolConfig ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  m ([DriverPoolWithActualDistResult], Bool)
getNextDriverPoolBatch driverPoolConfig searchReq searchTry = withLogTag "getNextDriverPoolBatch" do
  batchNum <- getPoolBatchNum searchTry.id
  incrementBatchNum searchTry.id
  prepareDriverPoolBatch driverPoolConfig searchReq searchTry batchNum

getPoolBatchNum :: (Redis.HedisFlow m r) => Id DST.SearchTry -> m PoolBatchNum
getPoolBatchNum searchTryId = do
  res <- Redis.withCrossAppRedis $ Redis.get (poolBatchNumKey searchTryId)
  case res of
    Just i -> return i
    Nothing -> do
      let expTime = 600
      Redis.withCrossAppRedis $ Redis.setExp (poolBatchNumKey searchTryId) (0 :: Integer) expTime
      return 0

incrementBatchNum ::
  ( Redis.HedisFlow m r
  ) =>
  Id DST.SearchTry ->
  m ()
incrementBatchNum searchTryId = do
  res <- Redis.withCrossAppRedis $ Redis.incr (poolBatchNumKey searchTryId)
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

driverRequestCountKey :: Id DST.SearchTry -> Id Driver -> Text
driverRequestCountKey searchTryId driverId = "Driver-Request-Count-Key:SearchTryId-" <> searchTryId.getId <> ":DriverId-" <> driverId.getId

checkRequestCount :: Redis.HedisFlow m r => Id DST.SearchTry -> Id Driver -> DriverPoolConfig -> m Bool
checkRequestCount searchTryId driverId driverPoolConfig =
  maybe True (\count -> (count :: Int) < driverPoolConfig.driverRequestCountLimit)
    <$> Redis.withCrossAppRedis (Redis.get (driverRequestCountKey searchTryId driverId))

incrementDriverRequestCount :: (Redis.HedisFlow m r) => [DriverPoolWithActualDistResult] -> Id DST.SearchTry -> m ()
incrementDriverRequestCount finalPoolBatch searchTryId = do
  CM.mapM_
    ( \dpr ->
        Redis.withCrossAppRedis do
          void $ Redis.incr (driverRequestCountKey searchTryId dpr.driverPoolResult.driverId)
          Redis.expire (driverRequestCountKey searchTryId dpr.driverPoolResult.driverId) 7200
    )
    finalPoolBatch
