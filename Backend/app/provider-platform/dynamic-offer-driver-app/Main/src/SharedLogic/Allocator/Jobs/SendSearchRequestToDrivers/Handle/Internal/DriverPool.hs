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
import Control.Monad.Extra (partitionM)
import Data.Foldable.Extra (notNull)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as DL
import Domain.Types.Common
import Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import qualified Domain.Types.DriverInformation as DriverInfo
import Domain.Types.DriverPoolConfig
import Domain.Types.GoHomeConfig (GoHomeConfig)
import Domain.Types.Merchant.DriverIntelligentPoolConfig
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Merchant.TransporterConfig (TransporterConfig)
import Domain.Types.Person (Driver)
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import EulerHS.Prelude hiding (id)
import Kernel.Randomizer (randomizeList)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowCounters
import Lib.Queries.GateInfo
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config as Reexport
import SharedLogic.DriverPool
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant.DriverIntelligentPoolConfig as DIP
import qualified Storage.CachedQueries.Merchant.TransporterConfig as TC
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

previouslyAttemptedDriversKey :: Id DST.SearchTry -> Maybe Bool -> Text
previouslyAttemptedDriversKey searchTryId mbIsOnRide = do
  case mbIsOnRide of
    Just isOnRide -> "Driver-Offer:PreviouslyAttemptedDrivers:SearchTryId-" <> searchTryId.getId <> ":IsOnRide-" <> show isOnRide
    Nothing -> "Driver-Offer:PreviouslyAttemptedDrivers:SearchTryId-" <> searchTryId.getId

prepareDriverPoolBatch ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    LT.HasLocationService m r
  ) =>
  DriverPoolConfig ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  PoolBatchNum ->
  GoHomeConfig ->
  m DriverPoolWithActualDistResultWithFlags
prepareDriverPoolBatch driverPoolCfg searchReq searchTry startingbatchNum goHomeConfig = withLogTag ("startingbatchNum- (" <> show startingbatchNum <> ")") $ do
  previousBatchesDrivers <- getPreviousBatchesDrivers Nothing
  let merchantOpCityId = searchReq.merchantOperatingCityId
  logDebug $ "PreviousBatchesDrivers-" <> show previousBatchesDrivers
  (finalPool, poolType, nextScheduleTime) <- prepareDriverPoolBatch' previousBatchesDrivers startingbatchNum True merchantOpCityId (Just searchReq.transactionId)
  incrementDriverRequestCount finalPool searchTry.id
  pure $ buildDriverPoolWithActualDistResultWithFlags finalPool poolType nextScheduleTime previousBatchesDrivers
  where
    buildDriverPoolWithActualDistResultWithFlags finalPool poolType nextScheduleTime prevBatchDrivers =
      DriverPoolWithActualDistResultWithFlags
        { driverPoolWithActualDistResult = finalPool,
          poolType = poolType,
          prevBatchDrivers = prevBatchDrivers,
          nextScheduleTime = nextScheduleTime
        }
    getPreviousBatchesDrivers ::
      ( EncFlow m r,
        EsqDBReplicaFlow m r,
        EsqDBFlow m r,
        CacheFlow m r,
        LT.HasLocationService m r
      ) =>
      Maybe Bool ->
      m [Id Driver]
    getPreviousBatchesDrivers mbOnRide = do
      batches <- previouslyAttemptedDrivers searchTry.id mbOnRide
      return $ (.driverPoolResult.driverId) <$> batches

    calculateWithFallback [] _ = return $ PrepareDriverPoolBatchEntity [] NormalPool Nothing []
    calculateWithFallback (prorityPoolType : fallbackPoolTypes) fn = do
      poolResult <- fn prorityPoolType
      if notNull poolResult.currentDriverPoolBatch
        then return poolResult
        else calculateWithFallback fallbackPoolTypes fn

    prepareDriverPoolBatch' previousBatchesDrivers batchNum doSpecialPooling merchantOpCityId_ txnId = withLogTag ("BatchNum - " <> show batchNum) $ do
      radiusStep <- getPoolRadiusStep searchTry.id
      transporterConfig <- TC.findByMerchantOpCityId merchantOpCityId_ Nothing Nothing >>= fromMaybeM (TransporterConfigDoesNotExist merchantOpCityId_.getId)
      intelligentPoolConfig <- DIP.findByMerchantOpCityId merchantOpCityId_ txnId (Just "driverId") >>= fromMaybeM (InternalError "Intelligent Pool Config not found")
      blockListedDriversForSearch <- Redis.withCrossAppRedis $ Redis.getList (mkBlockListedDriversKey searchReq.id)
      blockListedDriversForRider <- maybe (pure []) (\riderId -> Redis.withCrossAppRedis $ Redis.getList (mkBlockListedDriversForRiderKey riderId)) searchReq.riderId
      let blockListedDrivers = blockListedDriversForSearch <> blockListedDriversForRider
      logDebug $ "Blocked Driver List-" <> show blockListedDrivers
      let poolTypesWithFallback =
            case batchNum of
              (-2) | goHomeConfig.enableGoHome && doGoHomePooling && maybe False (\tag -> tag `elem` transporterConfig.specialLocationTags) searchReq.specialLocationTag && not (null transporterConfig.specialDrivers) -> [SpecialDriversPool, SkipPool, GoHomePool, SkipPool, NormalPool]
              (-2) | isJust searchReq.pickupZoneGateId -> [SpecialZoneQueuePool] <> [SkipPool] <> [GoHomePool | goHomeConfig.enableGoHome && doGoHomePooling] <> [SkipPool, NormalPool] ---- refactor logic here based on batchNum
              (-2) -> [SkipPool]
              (-1) | goHomeConfig.enableGoHome && doGoHomePooling -> [GoHomePool, SkipPool, NormalPool]
              (-1) -> [SkipPool]
              _ -> [NormalPool]
      logDebug $ "poolTypesWithFallback: " <> show poolTypesWithFallback
      let shouldDoMicroBatching = batchNum /= -1
      prepareDriverPoolBatchEntity <-
        calculateWithFallback poolTypesWithFallback $ \poolType -> do
          allDriversNotOnRide <- calcDriverPool poolType radiusStep merchantOpCityId_
          case poolType of
            SkipPool -> do
              incrementBatchNum searchTry.id
              (batch, poolType', nextScheduleTime) <- prepareDriverPoolBatch' previousBatchesDrivers (batchNum + 1) True merchantOpCityId_
              pure $ PrepareDriverPoolBatchEntity batch poolType' nextScheduleTime []
            SpecialZoneQueuePool -> do
              (driversInQueue, _) <- splitDriverFromGateAndRest allDriversNotOnRide
              logDebug $ "SpecialPickupZonePoolBatch DriversInQueue -" <> show driversInQueue
              (goHomeDriversInQueue, goHomeInQueueNotToDestination) <-
                case searchReq.toLocation of
                  Just toLoc | isGoHomeAvailable searchTry.tripCategory -> do
                    let goHomeReq =
                          CalculateGoHomeDriverPoolReq
                            { poolStage = DriverSelection,
                              driverPoolCfg = driverPoolCfg,
                              goHomeCfg = goHomeConfig,
                              variant = Just searchTry.vehicleVariant,
                              fromLocation = searchReq.fromLocation,
                              toLocation = toLoc, -- last or all ?
                              merchantId = searchReq.providerId,
                              isRental = isRentalTrip searchTry.tripCategory
                            }
                    filterOutGoHomeDriversAccordingToHomeLocation (map (convertDriverPoolWithActualDistResultToNearestGoHomeDriversResult False) driversInQueue) goHomeReq merchantOpCityId_
                  _ -> pure ([], [])
              logDebug $ "SpecialPickupZonePoolBatch goHomeDriversInQueue -" <> show goHomeDriversInQueue
              let goHomeDriversInQueueId = map (\d -> d.driverPoolResult.driverId) goHomeDriversInQueue
              let normalDriversInQueue = filter (\d -> not (d.driverPoolResult.driverId `elem` goHomeDriversInQueueId)) driversInQueue
              normalDriversInQueueBatch <- mkDriverPoolBatch merchantOpCityId_ (take driverPoolCfg.driverBatchSize normalDriversInQueue) intelligentPoolConfig transporterConfig
              let (finalNormalDriversInQueue, scheduleIn) =
                    if notNull goHomeDriversInQueue
                      then (addKeepHiddenInSeconds goHomeConfig.goHomeBatchDelay normalDriversInQueueBatch, Seconds 2 * goHomeConfig.goHomeBatchDelay)
                      else (normalDriversInQueueBatch, goHomeConfig.goHomeBatchDelay)
                  specialPickupZonePoolBatch = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` (blockListedDrivers <> goHomeInQueueNotToDestination)) $ goHomeDriversInQueue <> finalNormalDriversInQueue
              logDebug $ "SpecialPickupZonePoolBatch-" <> show specialPickupZonePoolBatch
              pure $ PrepareDriverPoolBatchEntity (addSpecialZoneInfo searchReq.driverDefaultExtraFee $ specialPickupZonePoolBatch) poolType (Just scheduleIn) []
            SpecialDriversPool -> do
              logDebug $ "SpecialCase-allDriversNotOnRide-" <> show allDriversNotOnRide
              let onlySpecialDrivers = filter (\dpr -> (getId dpr.driverPoolResult.driverId) `elem` transporterConfig.specialDrivers) allDriversNotOnRide
              let onlySpecialDriversNotBlocked = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` blockListedDrivers) onlySpecialDrivers
              logDebug $ "SpecialCase-onlySpecialDriversNotBlocked-" <> show onlySpecialDriversNotBlocked
              specialDriversPool <- mkDriverPoolBatch merchantOpCityId_ onlySpecialDriversNotBlocked intelligentPoolConfig transporterConfig
              logDebug $ "SpecialDriversPool-" <> show specialDriversPool
              pure $ PrepareDriverPoolBatchEntity specialDriversPool poolType (Just goHomeConfig.goHomeBatchDelay) []
            GoHomePool -> do
              goHomePool <- calcGoHomeDriverPool transporterConfig.specialDrivers merchantOpCityId_
              goHomePoolPostCalc <- calculateGoHomeBatch merchantOpCityId_ transporterConfig intelligentPoolConfig goHomePool blockListedDrivers
              pure $ PrepareDriverPoolBatchEntity goHomePoolPostCalc poolType (Just goHomeConfig.goHomeBatchDelay) []
            NormalPool -> do
              let allNearbyDriversCurrentlyNotOnRide = filterSpecialDrivers transporterConfig.specialDrivers allDriversNotOnRide
              (normalBatchNotOnRide, normalBatchOnRide) <- calculateNormalBatch merchantOpCityId_ transporterConfig intelligentPoolConfig allNearbyDriversCurrentlyNotOnRide radiusStep blockListedDrivers poolType batchNum False
              pure $ PrepareDriverPoolBatchEntity normalBatchNotOnRide poolType Nothing normalBatchOnRide
      cacheBatch prepareDriverPoolBatchEntity.currentDriverPoolBatch Nothing
      cacheBatch prepareDriverPoolBatchEntity.currentDriverPoolBatchOnRide (Just True)
      let cumulativePool =
            if shouldDoMicroBatching
              then
                addDistanceSplitConfigBasedDelaysForDriversWithinBatch prepareDriverPoolBatchEntity.currentDriverPoolBatch
                  <> addDistanceSplitConfigBasedDelaysForOnRideDriversWithinBatch prepareDriverPoolBatchEntity.currentDriverPoolBatchOnRide
              else prepareDriverPoolBatchEntity.currentDriverPoolBatch <> prepareDriverPoolBatchEntity.currentDriverPoolBatchOnRide
      poolWithSpecialZoneInfo <-
        if isJust searchReq.specialLocationTag && prepareDriverPoolBatchEntity.poolType /= SpecialZoneQueuePool
          then addSpecialZonePickupInfo cumulativePool
          else pure cumulativePool
      pure (poolWithSpecialZoneInfo, prepareDriverPoolBatchEntity.poolType, prepareDriverPoolBatchEntity.nextScheduleTime)
      where
        addSpecialZonePickupInfo pool = do
          (driversFromGate, restDrivers) <- splitDriverFromGateAndRest pool
          pure $ restDrivers <> addSpecialZoneInfo searchReq.driverDefaultExtraFee driversFromGate

        addSpecialZoneInfo driverDefaultExtraFee pool = map (\driverWithDistance -> driverWithDistance {pickupZone = True, specialZoneExtraTip = driverDefaultExtraFee}) pool
        addKeepHiddenInSeconds keepHiddenFor pool = map (\driverWithDistance -> driverWithDistance {keepHiddenForSeconds = keepHiddenFor}) pool
        splitDriverFromGateAndRest pool =
          case searchReq.pickupZoneGateId of
            Just pickupZoneGateId ->
              partitionM
                ( \dd -> do
                    mbDriverGate <- findGateInfoIfDriverInsideGatePickupZone (LatLong dd.driverPoolResult.lat dd.driverPoolResult.lon)
                    pure $ case mbDriverGate of
                      Just driverGate -> driverGate.id.getId == pickupZoneGateId
                      Nothing -> False
                )
                pool
            Nothing -> pure ([], pool)

        calculateGoHomeBatch mOCityId transporterConfig intelligentPoolConfig allNearbyGoHomeDrivers blockListedDrivers = do
          let allNearbyGoHomeDrivers' = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` blockListedDrivers) allNearbyGoHomeDrivers
          logDebug $ "GoHomeDriverPool-" <> show allNearbyGoHomeDrivers'
          let onlyNewGoHomeDrivers = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` previousBatchesDrivers) allNearbyGoHomeDrivers'
          goHomeDriverPoolBatch <- mkDriverPoolBatch mOCityId onlyNewGoHomeDrivers intelligentPoolConfig transporterConfig
          logDebug $ "GoHomeDriverPoolBatch-" <> show goHomeDriverPoolBatch
          pure goHomeDriverPoolBatch

        calculateNormalBatch mOCityId transporterConfig intelligentPoolConfig normalDriverPool radiusStep blockListedDrivers txnId' poolType batchNum' isOnRide = do
          logDebug $ "NormalDriverPool-" <> show normalDriverPool
          allNearbyNonGoHomeDrivers <- filterM (\dpr -> (CQDGR.getDriverGoHomeRequestInfo dpr.driverPoolResult.driverId mOCityId (Just goHomeConfig)) <&> (/= Just DDGR.ACTIVE) . (.status)) normalDriverPool
          let allNearbyDrivers = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` blockListedDrivers) allNearbyNonGoHomeDrivers
          let onlyNewNormalDrivers = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` previousBatchesDrivers) allNearbyDrivers
          (normalBatchNotOnRide, normalBatchOnRide) <- getBatch mOCityId transporterConfig intelligentPoolConfig normalDriverPool radiusStep blockListedDrivers onlyNewNormalDrivers poolType batchNum' isOnRide
          if not isOnRide
            then do
              previousDriverOnRide <- getPreviousBatchesDrivers (Just True)
              validDriversFromPreviousBatch <-
                filterM
                  ( \driverPool -> do
                      isHasValidRequests <- checkRequestCount searchTry.id driverPool.driverPoolResult.driverId driverPoolCfg
                      let isPreviousBatchDriver = (driverPool.driverPoolResult.driverId) `elem` previousDriverOnRide
                      return $ isHasValidRequests && isPreviousBatchDriver
                  )
                  allNearbyDrivers
              logDebug $ "NormalDriverPoolBatchOnRide-" <> show normalBatchOnRide
              logDebug $ "ValidDriversFromPreviousBatchOnRide-" <> show validDriversFromPreviousBatch
              let finalBatchOnRide = take (totalBatchSize - batchSize) $ normalBatchOnRide <> validDriversFromPreviousBatch
              return (normalBatchNotOnRide, finalBatchOnRide)
            else do
              return (normalBatchNotOnRide, normalBatchOnRide)

        getBatch mOCityId transporterConfig intelligentPoolConfig normalDriverPool radiusStep blockListedDrivers onlyNewNormalDrivers poolType batchNum' isOnRide = do
          if length onlyNewNormalDrivers < batchSize && not (isAtMaxRadiusStep radiusStep) && (not isOnRide)
            then do
              incrementPoolRadiusStep searchTry.id
              (batch, _, _) <- prepareDriverPoolBatch' previousBatchesDrivers batchNum False mOCityId txnId
              pure (batch, [])
            else do
              normalDriverPoolBatch <- mkDriverPoolBatch mOCityId onlyNewNormalDrivers intelligentPoolConfig transporterConfig
              logDebug $ "NormalDriverPoolBatch-" <> show normalDriverPoolBatch
              normalBatchOnRide <- do
                if batchNum >= 0 && not isOnRide
                  then do
                    allNearbyDriversCurrentlyOnRide <- calcDriverCurrentlyOnRidePool poolType radiusStep transporterConfig merchantOpCityId_ batchNum'
                    fst <$> calculateNormalBatch merchantOpCityId_ transporterConfig intelligentPoolConfig allNearbyDriversCurrentlyOnRide radiusStep blockListedDrivers poolType batchNum' True
                  else pure []
              if length normalDriverPoolBatch < batchSize && not isOnRide
                then do
                  filledBatch <- fillBatch mOCityId normalDriverPool normalDriverPoolBatch intelligentPoolConfig blockListedDrivers
                  logDebug $ "FilledDriverPoolBatch-" <> show filledBatch
                  pure (filledBatch, normalBatchOnRide)
                else do
                  pure (normalDriverPoolBatch, normalBatchOnRide)

        filterSpecialDrivers specialDrivers = filter (\dpr -> not ((getId dpr.driverPoolResult.driverId) `elem` specialDrivers))

        mkDriverPoolBatch mOCityId onlyNewDrivers intelligentPoolConfig transporterConfig = do
          case sortingType of
            Intelligent -> makeIntelligentDriverPool mOCityId onlyNewDrivers intelligentPoolConfig transporterConfig
            Random -> makeRandomDriverPool onlyNewDrivers

        makeIntelligentDriverPool mOCityId onlyNewDrivers intelligentPoolConfig transporterConfig = do
          let sortWithDriverScore' = sortWithDriverScore mOCityId (Just transporterConfig) intelligentPoolConfig driverPoolCfg
          (sortedDriverPool, randomizedDriverPool) <-
            bimapM (sortWithDriverScore' [AcceptanceRatio, CancellationRatio, AvailableTime, DriverSpeed, ActualPickupDistance, RideFrequency] True) (sortWithDriverScore' [AvailableTime, DriverSpeed, ActualPickupDistance] False)
              =<< splitDriverPoolForSorting mOCityId intelligentPoolConfig.minQuotesToQualifyForIntelligentPool onlyNewDrivers
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
          pure $ splitSilentDriversAndSortWithDistance (poolBatch <> driversFromRestCount)

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

        addDistanceSplitConfigBasedDelaysForOnRideDriversWithinBatch filledPoolBatch =
          fst $
            foldl'
              ( \(finalBatch, restBatch) splitConfig -> do
                  let (splitToAddDelay, newRestBatch) = splitAt splitConfig.batchSplitSize restBatch
                      splitWithDelay = map (\driverWithDistance -> driverWithDistance {keepHiddenForSeconds = splitConfig.batchSplitDelay}) splitToAddDelay
                  (finalBatch <> splitWithDelay, newRestBatch)
              )
              ([], filledPoolBatch)
              driverPoolCfg.onRideBatchSplitConfig

        calcGoHomeDriverPool specialDrivers opCityId = do
          case (searchReq.toLocation, isGoHomeAvailable searchTry.tripCategory) of
            (Just toLoc, True) -> do
              calculateGoHomeDriverPoolBatch <-
                calculateGoHomeDriverPool
                  ( CalculateGoHomeDriverPoolReq
                      { poolStage = DriverSelection,
                        driverPoolCfg = driverPoolCfg,
                        goHomeCfg = goHomeConfig,
                        variant = Just searchTry.vehicleVariant,
                        fromLocation = searchReq.fromLocation,
                        toLocation = toLoc, -- last or all ?
                        merchantId = searchReq.providerId,
                        isRental = isRentalTrip searchTry.tripCategory
                      }
                  )
                  opCityId
              return $ filterSpecialDrivers specialDrivers calculateGoHomeDriverPoolBatch
            _ -> return []

        calcDriverPool poolType radiusStep merchantOpCityId = do
          let vehicleVariant = searchTry.vehicleVariant
              merchantId = searchReq.providerId
          let pickupLoc = searchReq.fromLocation
          let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
          calculateDriverPoolWithActualDist DriverSelection poolType driverPoolCfg (Just vehicleVariant) pickupLatLong merchantId merchantOpCityId True (Just radiusStep) (isRentalTrip searchTry.tripCategory)
        calcDriverCurrentlyOnRidePool poolType radiusStep transporterConfig merchantOpCityId batchNum' = do
          let merchantId = searchReq.providerId
          if transporterConfig.includeDriverCurrentlyOnRide
            then do
              let vehicleVariant = searchTry.vehicleVariant
              let pickupLoc = searchReq.fromLocation
              let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
              calculateDriverCurrentlyOnRideWithActualDist DriverSelection poolType driverPoolCfg (Just vehicleVariant) pickupLatLong merchantId merchantOpCityId (Just radiusStep) (isRentalTrip searchTry.tripCategory) (toInteger batchNum')
            else pure []
        fillBatch merchantOpCityId allNearbyDrivers batch intelligentPoolConfig blockListedDrivers = do
          let batchDriverIds = batch <&> (.driverPoolResult.driverId)
          let driversNotInBatch = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` batchDriverIds) allNearbyDrivers
          driversWithValidReqAmount <- filterM (\dpr -> checkRequestCount searchTry.id dpr.driverPoolResult.driverId driverPoolCfg) driversNotInBatch
          nonGoHomeDriversWithValidReqCount <- filterM (\dpr -> (CQDGR.getDriverGoHomeRequestInfo dpr.driverPoolResult.driverId merchantOpCityId (Just goHomeConfig)) <&> (/= Just DDGR.ACTIVE) . (.status)) driversWithValidReqAmount
          let nonGoHomeNormalDriversWithValidReqCount = filter (\ngd -> ngd.driverPoolResult.driverId `notElem` blockListedDrivers) nonGoHomeDriversWithValidReqCount
          let fillSize = batchSize - length batch
          transporterConfig <- TC.findByMerchantOpCityId merchantOpCityId Nothing Nothing
          (batch <>)
            <$> case sortingType of
              Intelligent -> do
                let sortWithDriverScore' = sortWithDriverScore merchantOpCityId transporterConfig intelligentPoolConfig driverPoolCfg
                (sortedDriverPool, randomizedDriverPool) <-
                  bimapM (sortWithDriverScore' [AcceptanceRatio, CancellationRatio, AvailableTime, DriverSpeed, ActualPickupDistance, RideFrequency] True) (sortWithDriverScore' [AvailableTime, DriverSpeed, ActualPickupDistance] False)
                    =<< splitDriverPoolForSorting merchantOpCityId intelligentPoolConfig.minQuotesToQualifyForIntelligentPool nonGoHomeNormalDriversWithValidReqCount -- snd means taking drivers who recieved less then X(config- minQuotesToQualifyForIntelligentPool) quotes
                let sortedDriverPoolWithSilentSort = splitSilentDriversAndSortWithDistance sortedDriverPool
                let randomizedDriverPoolWithSilentSort = splitSilentDriversAndSortWithDistance randomizedDriverPool
                takeDriversUsingPoolPercentage (sortedDriverPoolWithSilentSort, randomizedDriverPoolWithSilentSort) fillSize intelligentPoolConfig
              Random -> pure $ take fillSize nonGoHomeNormalDriversWithValidReqCount
        cacheBatch batch isOnRide = do
          logDebug $ "Caching batch-" <> show batch
          batches <- previouslyAttemptedDrivers searchTry.id isOnRide
          Redis.withCrossAppRedis $ Redis.setExp (previouslyAttemptedDriversKey searchTry.id isOnRide) (batches <> batch) (60 * 30)
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
        totalBatchSize = driverPoolCfg.totalBatchSize

splitSilentDriversAndSortWithDistance :: [DriverPoolWithActualDistResult] -> [DriverPoolWithActualDistResult]
splitSilentDriversAndSortWithDistance drivers = do
  let (silentDrivers, activeDrivers) = bimap (sortOn (.actualDistanceToPickup)) (sortOn (.actualDistanceToPickup)) $ DL.partition ((== Just DriverInfo.SILENT) . (.driverPoolResult.mode)) drivers
  activeDrivers <> silentDrivers

previouslyAttemptedDrivers ::
  ( Redis.HedisFlow m r
  ) =>
  Id DST.SearchTry ->
  Maybe Bool ->
  m [DriverPoolWithActualDistResult]
previouslyAttemptedDrivers searchTryId isOnRide = do
  Redis.withCrossAppRedis $
    Redis.safeGet (previouslyAttemptedDriversKey searchTryId isOnRide)
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
  Id MerchantOperatingCity ->
  Maybe TransporterConfig ->
  DriverIntelligentPoolConfig ->
  DriverPoolConfig ->
  [IntelligentFactors] ->
  Bool ->
  [DriverPoolWithActualDistResult] ->
  m [DriverPoolWithActualDistResult]
sortWithDriverScore _ Nothing _ _ _ _ dp = logInfo "Weightages not available in DB, going with random selection" *> randomizeAndLimitSelection dp
sortWithDriverScore merchantOpCityId (Just transporterConfig) intelligentPoolConfig driverPoolCfg factorsToCalculate isPartOfIntelligentPool dp = do
  logTagInfo "Weightage config for intelligent driver pool" $ show transporterConfig
  let driverIds = map (.driverPoolResult.driverId) dp
  let driverActualDistances = map ((.driverPoolResult.driverId) &&& (.driverPoolResult.distanceToPickup)) dp
  let cancellationScoreRelatedConfig = CancellationScoreRelatedConfig transporterConfig.popupDelayToAddAsPenalty transporterConfig.thresholdCancellationScore transporterConfig.minRidesForCancellationScore
  calculatedScores <- mapM (fetchScore merchantOpCityId driverActualDistances driverIds intelligentPoolConfig driverPoolCfg cancellationScoreRelatedConfig) factorsToCalculate
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
                    RideFrequency -> accIntelligentScores {rideFrequency = res} :: IntelligentScores
              )
              (IntelligentScores Nothing Nothing Nothing Nothing Nothing Nothing 0)
              factorOverallScore
      addIntelligentPoolInfo cancellationScoreRelatedConfig dObj intelligentScores
    addIntelligentPoolInfo cancellationScoreRelatedConfig dObj is@IntelligentScores {..} = do
      popupDelay <-
        maybe
          (pure transporterConfig.defaultPopupDelay)
          (\cr -> getPopupDelay merchantOpCityId dObj.driverPoolResult.driverId cr cancellationScoreRelatedConfig transporterConfig.defaultPopupDelay)
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
                        quotes <- getTotalQuotesSent merchantOpCityId (cast dObj.driverPoolResult.driverId)
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
  Id MerchantOperatingCity ->
  [(Id Driver, Meters)] ->
  [Id Driver] ->
  DriverIntelligentPoolConfig ->
  DriverPoolConfig ->
  CancellationScoreRelatedConfig ->
  IntelligentFactors ->
  m (HM.HashMap Text Double)
fetchScore merchantOpCityId driverActualDistanceList driverIds intelligentPoolConfig driverPoolCfg cancellationScoreRelatedConfig factor =
  HM.fromList <$> case factor of
    AcceptanceRatio | intelligentPoolConfig.acceptanceRatioWeightage /= 0 -> do
      acceptanceRatios <- getRatios (getLatestAcceptanceRatio merchantOpCityId) driverIds
      getScoreWithWeight (intelligentPoolConfig.acceptanceRatioWeightage) acceptanceRatios
    CancellationRatio | intelligentPoolConfig.cancellationRatioWeightage /= 0 -> do
      cancellationRatios <-
        getRatios (getLatestCancellationRatio cancellationScoreRelatedConfig merchantOpCityId) driverIds
      getScoreWithWeight (intelligentPoolConfig.cancellationRatioWeightage) cancellationRatios
    AvailableTime | intelligentPoolConfig.availabilityTimeWeightage /= 0 -> do
      let maxAvailbaleTime = fromIntegral $ intelligentPoolConfig.availabilityTimeWindowOption.period * convertPeriodTypeToSeconds intelligentPoolConfig.availabilityTimeWindowOption.periodType
      driversAvailableTimeRatio <- map (second ((/ maxAvailbaleTime) . sum . catMaybes)) <$> getRatios (getCurrentWindowAvailability merchantOpCityId) driverIds
      getScoreWithWeight (intelligentPoolConfig.availabilityTimeWeightage) driversAvailableTimeRatio
    DriverSpeed | intelligentPoolConfig.driverSpeedWeightage /= 0 -> do
      averageSpeeds <- getRatios (getDriverAverageSpeed merchantOpCityId . cast) driverIds
      getSpeedScore (intelligentPoolConfig.driverSpeedWeightage) averageSpeeds
    ActualPickupDistance | intelligentPoolConfig.actualPickupDistanceWeightage /= 0 -> do
      pure $ map (bimap (.getId) ((* (fromIntegral intelligentPoolConfig.actualPickupDistanceWeightage)) . fromIntegral . flip div (fromMaybe (Meters 1) (driverPoolCfg.actualDistanceThreshold)))) driverActualDistanceList
    RideFrequency | intelligentPoolConfig.numRidesWeightage /= 0 -> do
      driverIdAndNumRides <- mapM (getTotalRidesCount merchantOpCityId) driverIds <&> zip (getId <$> driverIds)
      logDebug $ "Intelligent pool :- [(DriverId, numRides)] - " <> show driverIdAndNumRides
      return $ second (\numRides -> fromIntegral intelligentPoolConfig.numRidesWeightage * fromIntegral numRides / fromIntegral intelligentPoolConfig.maxNumRides) <$> driverIdAndNumRides
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

poolRadiusStepKey :: Id DST.SearchTry -> Text
poolRadiusStepKey searchTryId = "Driver-Offer:Allocator:PoolRadiusStep:SearchTryId-" <> searchTryId.getId

getNextDriverPoolBatch ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    LT.HasLocationService m r
  ) =>
  DriverPoolConfig ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  GoHomeConfig ->
  m DriverPoolWithActualDistResultWithFlags
getNextDriverPoolBatch driverPoolConfig searchReq searchTry goHomeConfig = withLogTag "getNextDriverPoolBatch" do
  batchNum <- getPoolBatchNum searchTry.id
  incrementBatchNum searchTry.id
  prepareDriverPoolBatch driverPoolConfig searchReq searchTry batchNum goHomeConfig

getPoolBatchNum :: (Redis.HedisFlow m r) => Id DST.SearchTry -> m PoolBatchNum
getPoolBatchNum searchTryId = do
  res <- Redis.withCrossAppRedis $ Redis.get (poolBatchNumKey searchTryId)
  case res of
    Just i -> return i
    Nothing -> do
      let expTime = 600
      Redis.withCrossAppRedis $ Redis.setExp (poolBatchNumKey searchTryId) (-2 :: Integer) expTime
      return (-2)

incrementBatchNum ::
  ( Redis.HedisFlow m r
  ) =>
  Id DST.SearchTry ->
  m ()
incrementBatchNum searchTryId = do
  res <- Redis.withCrossAppRedis $ Redis.incr (poolBatchNumKey searchTryId)
  logInfo $ "Increment batch num to " <> show res <> "."
  return ()

getPoolRadiusStep :: (Redis.HedisFlow m r) => Id DST.SearchTry -> m PoolRadiusStep
getPoolRadiusStep searchTryId = do
  res <- Redis.withCrossAppRedis $ Redis.get (poolRadiusStepKey searchTryId)
  case res of
    Just i -> return i
    Nothing -> do
      let expTime = 600
      Redis.withCrossAppRedis $ Redis.setExp (poolRadiusStepKey searchTryId) (0 :: Integer) expTime
      return 0

incrementPoolRadiusStep ::
  ( Redis.HedisFlow m r
  ) =>
  Id DST.SearchTry ->
  m ()
incrementPoolRadiusStep searchTryId = do
  res <- Redis.withCrossAppRedis $ Redis.incr (poolRadiusStepKey searchTryId)
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

data PrepareDriverPoolBatchEntity = PrepareDriverPoolBatchEntity
  { currentDriverPoolBatch :: [DriverPoolWithActualDistResult],
    poolType :: PoolType,
    nextScheduleTime :: Maybe Seconds,
    currentDriverPoolBatchOnRide :: [DriverPoolWithActualDistResult]
  }
