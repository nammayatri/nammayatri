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
import qualified Data.HashSet as HashSet
import qualified Data.List as DL
import qualified Data.Map as Map
import Domain.Types.Common
import Domain.Types.DriverGoHomeRequest as DDGR
import qualified Domain.Types.DriverInformation as DriverInfo
import Domain.Types.DriverIntelligentPoolConfig
import Domain.Types.DriverPoolConfig
import Domain.Types.GoHomeConfig (GoHomeConfig)
import Domain.Types.Merchant (Merchant)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Driver)
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.ServiceTierType as DVST
import Domain.Types.TransporterConfig (TransporterConfig)
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (getDbHash)
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
import qualified Storage.Cac.DriverIntelligentPoolConfig as CDIP
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.RiderDriverCorrelation as RDC
import Tools.Maps as Maps
import Utils.Common.Cac.KeyNameConstants

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
previouslyAttemptedDriversKey searchTryId consideOnRideDrivers = do
  case consideOnRideDrivers of
    Just consideOnRideDrivers' -> "Driver-Offer:PreviouslyAttemptedDrivers:SearchTryId-" <> searchTryId.getId <> ":consideOnRideDrivers-" <> show consideOnRideDrivers'
    Nothing -> "Driver-Offer:PreviouslyAttemptedDrivers:SearchTryId-" <> searchTryId.getId

getCustomerFavDrivers ::
  ( EncFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Maybe Text ->
  Id Merchant ->
  m [Id Driver]
getCustomerFavDrivers mPhoneNum merchantId = maybe (pure []) fetchFavDrivers mPhoneNum
  where
    fetchFavDrivers phoneNum = do
      numberHash <- getDbHash phoneNum
      mbRiderDetails <- QRD.findByMobileNumberHashAndMerchant numberHash merchantId
      case mbRiderDetails of
        Just rider -> map (.driverId) <$> RDC.findFavDriversForRider rider.id True
        Nothing -> pure []

rearrangefavDriverProrityInNormalBatch ::
  ( EncFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  [Id Driver] ->
  [DriverPoolWithActualDistResult] ->
  DriverPoolConfig ->
  Bool ->
  m [DriverPoolWithActualDistResult]
rearrangefavDriverProrityInNormalBatch customerFavDrivers currentPoolBatch driverPoolConfig addFavInBegining
  | not (driverPoolConfig.favDriver) = pure currentPoolBatch
  | otherwise = do
    let customerFavDriversSet = HashSet.fromList customerFavDrivers
        matches = filter (\driverPool -> (.driverId) (driverPoolResult driverPool) `HashSet.member` customerFavDriversSet) currentPoolBatch
        nonMatches = filter (\driverPool -> not $ (.driverId) (driverPoolResult driverPool) `HashSet.member` customerFavDriversSet) currentPoolBatch
    pure $
      if addFavInBegining
        then matches ++ nonMatches
        else nonMatches ++ matches

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
  [TripQuoteDetail] ->
  PoolBatchNum ->
  GoHomeConfig ->
  Maybe Text ->
  m DriverPoolWithActualDistResultWithFlags
prepareDriverPoolBatch driverPoolCfg searchReq searchTry tripQuoteDetails startingbatchNum goHomeConfig customerPhoneNum = withLogTag ("startingbatchNum- (" <> show startingbatchNum <> ")") $ do
  customerFavDrivers <- getCustomerFavDrivers customerPhoneNum searchReq.providerId
  logDebug $ "CustomerFavDrivers : " <> show customerFavDrivers
  previousBatchesDrivers <- getPreviousBatchesDrivers Nothing
  previousBatchesDriversOnRide <- getPreviousBatchesDrivers (Just True)
  let merchantOpCityId = searchReq.merchantOperatingCityId
  logDebug $ "PreviousBatchesDrivers-" <> show previousBatchesDrivers
  PrepareDriverPoolBatchEntity {..} <- prepareDriverPoolBatch' previousBatchesDrivers startingbatchNum True merchantOpCityId searchReq.transactionId customerFavDrivers
  let finalPool = currentDriverPoolBatch <> currentDriverPoolBatchOnRide
  incrementDriverRequestCount finalPool searchTry.id
  pure $ buildDriverPoolWithActualDistResultWithFlags finalPool poolType nextScheduleTime (previousBatchesDrivers <> previousBatchesDriversOnRide)
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
      if notNull poolResult.currentDriverPoolBatch || notNull poolResult.currentDriverPoolBatchOnRide
        then return poolResult
        else calculateWithFallback fallbackPoolTypes fn

    prepareDriverPoolBatch' previousBatchesDrivers batchNum doSpecialPooling merchantOpCityId_ txnId customerFavDrivers = withLogTag ("BatchNum - " <> show batchNum) $ do
      radiusStep <- getPoolRadiusStep searchTry.id
      transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId_ Nothing >>= fromMaybeM (TransporterConfigDoesNotExist merchantOpCityId_.getId)
      intelligentPoolConfig <- CDIP.findByMerchantOpCityId merchantOpCityId_ (Just ((TransactionId . Id) txnId)) >>= fromMaybeM (InternalError "Intelligent Pool Config not found")
      blockListedDriversForSearch <- Redis.withCrossAppRedis $ Redis.getList (mkBlockListedDriversKey searchReq.id)
      blockListedDriversForRider <- maybe (pure []) (\riderId -> Redis.withCrossAppRedis $ Redis.getList (mkBlockListedDriversForRiderKey riderId)) searchReq.riderId
      let blockListedDrivers = blockListedDriversForSearch <> blockListedDriversForRider
      logDebug $ "Blocked Driver List-" <> show blockListedDrivers
      let poolTypesWithFallback =
            case batchNum of
              (-1) | goHomeConfig.enableGoHome && doSpecialPooling && maybe False (\tag -> tag `elem` transporterConfig.specialLocationTags) searchReq.specialLocationTag && not (null transporterConfig.specialDrivers) -> [SpecialDriversPool, GoHomePool, NormalPool]
              (-1) | isJust searchReq.pickupZoneGateId -> [SpecialZoneQueuePool, SkipPool]
              (-1) -> [SkipPool]
              0 | goHomeConfig.enableGoHome && doSpecialPooling -> [GoHomePool, NormalPool]
              _ -> [NormalPool]
      logDebug $ "poolTypesWithFallback: " <> show poolTypesWithFallback
      let shouldDoMicroBatching = batchNum /= -1
      prepareDriverPoolBatchEntity <-
        calculateWithFallback poolTypesWithFallback $ \poolType -> do
          allDriversNotOnRide <- calcDriverPool poolType radiusStep merchantOpCityId_
          case poolType of
            SkipPool -> do
              incrementBatchNum searchTry.id
              prepareDriverPoolBatch' previousBatchesDrivers (batchNum + 1) True merchantOpCityId_ txnId customerFavDrivers
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
                              serviceTiers = tripQuoteDetails <&> (.vehicleServiceTier),
                              fromLocation = searchReq.fromLocation,
                              toLocation = toLoc, -- last or all ?
                              merchantId = searchReq.providerId,
                              isRental = isRentalTrip searchTry.tripCategory,
                              isInterCity = isInterCityTrip searchTry.tripCategory
                            }
                    filterOutGoHomeDriversAccordingToHomeLocation (map (convertDriverPoolWithActualDistResultToNearestGoHomeDriversResult False) driversInQueue) goHomeReq merchantOpCityId_
                  _ -> pure ([], [])
              logDebug $ "SpecialPickupZonePoolBatch goHomeDriversInQueue -" <> show goHomeDriversInQueue
              let goHomeDriversInQueueId = map (\d -> d.driverPoolResult.driverId) goHomeDriversInQueue
              let normalDriversInQueue = filter (\d -> not (d.driverPoolResult.driverId `elem` goHomeDriversInQueueId)) driversInQueue
              let normalDriversInQueue' = bookAnyFilters transporterConfig normalDriversInQueue []
              normalDriversInQueueBatch <- mkDriverPoolBatch merchantOpCityId_ (take driverPoolCfg.driverBatchSize normalDriversInQueue') intelligentPoolConfig transporterConfig batchSize False
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
              specialDriversPool <- mkDriverPoolBatch merchantOpCityId_ onlySpecialDriversNotBlocked intelligentPoolConfig transporterConfig batchSize False
              logDebug $ "SpecialDriversPool-" <> show specialDriversPool
              pure $ PrepareDriverPoolBatchEntity specialDriversPool poolType (Just goHomeConfig.goHomeBatchDelay) []
            GoHomePool -> do
              goHomePool <- calcGoHomeDriverPool transporterConfig.specialDrivers merchantOpCityId_
              goHomePoolPostCalc <- calculateGoHomeBatch merchantOpCityId_ transporterConfig intelligentPoolConfig goHomePool blockListedDrivers
              pure $ PrepareDriverPoolBatchEntity goHomePoolPostCalc poolType (Just goHomeConfig.goHomeBatchDelay) []
            NormalPool -> do
              (normalBatchNotOnRide, normalBatchOnRide) <- calculateNormalBatch merchantOpCityId_ transporterConfig intelligentPoolConfig allDriversNotOnRide radiusStep blockListedDrivers txnId poolType
              logDebug $ "NormalBatchNotOnRide-" <> show normalBatchNotOnRide
              logDebug $ "NormalBatchOnRide-" <> show normalBatchOnRide
              pure $ PrepareDriverPoolBatchEntity normalBatchNotOnRide poolType Nothing normalBatchOnRide
      cacheBatch prepareDriverPoolBatchEntity.currentDriverPoolBatch Nothing
      cacheBatch prepareDriverPoolBatchEntity.currentDriverPoolBatchOnRide (Just True)
      let (poolNotOnRide, poolOnRide) =
            if shouldDoMicroBatching
              then
                ( addDistanceSplitConfigBasedDelaysForDriversWithinBatch prepareDriverPoolBatchEntity.currentDriverPoolBatch,
                  addDistanceSplitConfigBasedDelaysForOnRideDriversWithinBatch prepareDriverPoolBatchEntity.currentDriverPoolBatchOnRide
                )
              else (prepareDriverPoolBatchEntity.currentDriverPoolBatch, prepareDriverPoolBatchEntity.currentDriverPoolBatchOnRide)
      (poolWithSpecialZoneInfoNotOnRide, poolWithSpecialZoneInfoOnRide) <-
        if isJust searchReq.specialLocationTag && prepareDriverPoolBatchEntity.poolType /= SpecialZoneQueuePool
          then (,) <$> addSpecialZonePickupInfo poolNotOnRide <*> addSpecialZonePickupInfo poolOnRide
          else pure (poolNotOnRide, poolOnRide)
      pure $ PrepareDriverPoolBatchEntity poolWithSpecialZoneInfoNotOnRide prepareDriverPoolBatchEntity.poolType prepareDriverPoolBatchEntity.nextScheduleTime poolWithSpecialZoneInfoOnRide
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
          let onlyNewGoHomeDriversWithMultipleSeriveTier = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` previousBatchesDrivers) allNearbyGoHomeDrivers'
          let onlyNewGoHomeDrivers =
                if isBookAny (tripQuoteDetails <&> (.vehicleServiceTier))
                  then do selectMinDowngrade transporterConfig.bookAnyVehicleDowngradeLevel onlyNewGoHomeDriversWithMultipleSeriveTier
                  else do onlyNewGoHomeDriversWithMultipleSeriveTier
          goHomeDriverPoolBatch <- mkDriverPoolBatch mOCityId onlyNewGoHomeDrivers intelligentPoolConfig transporterConfig batchSize False
          logDebug $ "GoHomeDriverPoolBatch-" <> show goHomeDriverPoolBatch
          goHomeDriverPoolBatchWithFav <- rearrangefavDriverProrityInNormalBatch customerFavDrivers goHomeDriverPoolBatch driverPoolCfg False
          logDebug $ "GoHomeDriverPoolBatch with fav rearrange : " <> show goHomeDriverPoolBatchWithFav
          pure goHomeDriverPoolBatch

        filtersForNormalBatch mOCityId transporterConfig normalDriverPool blockListedDrivers previousBatchesDrivers' = do
          let normalDriverPoolWithSpecialDriverFilter = filterSpecialDrivers transporterConfig.specialDrivers normalDriverPool
          allNearbyNonGoHomeDrivers <- filterM (\dpr -> (CQDGR.getDriverGoHomeRequestInfo dpr.driverPoolResult.driverId mOCityId (Just goHomeConfig)) <&> (/= Just DDGR.ACTIVE) . (.status)) normalDriverPoolWithSpecialDriverFilter
          let allNearbyDrivers = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` blockListedDrivers) allNearbyNonGoHomeDrivers
          pure $ bookAnyFilters transporterConfig allNearbyDrivers previousBatchesDrivers'

        calculateNormalBatch mOCityId transporterConfig intelligentPoolConfig normalDriverPool radiusStep blockListedDrivers txnId' poolType = do
          logDebug $ "NormalDriverPool-" <> show normalDriverPool
          onlyNewNormalDrivers <- filtersForNormalBatch mOCityId transporterConfig normalDriverPool blockListedDrivers previousBatchesDrivers
          -- yaha karegai toh filter ho jaega
          (normalBatchNotOnRide, normalBatchOnRide', mbRadiusThreshold) <- getDriverPoolNotOnRide mOCityId transporterConfig intelligentPoolConfig normalDriverPool radiusStep blockListedDrivers onlyNewNormalDrivers txnId'
          normalBatchOnRide <-
            case mbRadiusThreshold of
              Just radiusStepThreshold -> do
                getDriverPoolOnRide mOCityId transporterConfig intelligentPoolConfig radiusStepThreshold blockListedDrivers poolType
              Nothing -> pure normalBatchOnRide'
          pure (normalBatchNotOnRide, normalBatchOnRide)

        getDriverPoolNotOnRide mOCityId transporterConfig intelligentPoolConfig normalDriverPool radiusStep blockListedDrivers onlyNewNormalDrivers txnId' = do
          if length onlyNewNormalDrivers < batchSize && not (isAtMaxRadiusStep radiusStep)
            then do
              incrementPoolRadiusStep searchTry.id
              batchEntity <- prepareDriverPoolBatch' previousBatchesDrivers batchNum False mOCityId txnId' customerFavDrivers
              pure (batchEntity.currentDriverPoolBatch, batchEntity.currentDriverPoolBatchOnRide, Nothing)
            else do
              normalDriverPoolBatch <- mkDriverPoolBatch mOCityId onlyNewNormalDrivers intelligentPoolConfig transporterConfig batchSize False
              -- yaha karegai toh jagah kam bachegi : yaha karegai
              if length normalDriverPoolBatch < batchSize
                then do
                  filledBatch <- fillBatch transporterConfig mOCityId normalDriverPool normalDriverPoolBatch intelligentPoolConfig blockListedDrivers
                  logDebug $ "FilledDriverPoolBatch-" <> show filledBatch
                  normalDriverPoolBatchWithFav <- rearrangefavDriverProrityInNormalBatch customerFavDrivers filledBatch driverPoolCfg True
                  logDebug $ "FilledDriverPoolBatch with fav rearrange : " <> show filledBatch
                  pure (normalDriverPoolBatchWithFav, [], Just radiusStep)
                else do
                  normalDriverPoolBatchWithFav <- rearrangefavDriverProrityInNormalBatch customerFavDrivers normalDriverPoolBatch driverPoolCfg True
                  logDebug $ "NormalDriverPoolBatch with fav rearrange : " <> show normalDriverPoolBatchWithFav
                  pure (normalDriverPoolBatchWithFav, [], Just radiusStep)

        getDriverPoolOnRide mOCityId transporterConfig intelligentPoolConfig radiusStep blockListedDrivers poolType = do
          if poolType == NormalPool && driverPoolCfg.enableForwardBatching && searchTry.isAdvancedBookingEnabled
            then do
              previousDriverOnRide <- getPreviousBatchesDrivers (Just True)
              allNearbyDriversCurrentlyOnRide <- calcDriverCurrentlyOnRidePool poolType radiusStep transporterConfig merchantOpCityId_ batchNum
              logDebug $ "NormalDriverPoolBatchOnRideCurrentlyOnRide-" <> show allNearbyDriversCurrentlyOnRide
              onlyNewNormalDriversOnRide <- filtersForNormalBatch mOCityId transporterConfig allNearbyDriversCurrentlyOnRide blockListedDrivers previousDriverOnRide
              normalDriverPoolBatchOnRide <- mkDriverPoolBatch mOCityId onlyNewNormalDriversOnRide intelligentPoolConfig transporterConfig batchSizeOnRide True
              validDriversFromPreviousBatch <-
                filterM
                  ( \dpr -> do
                      isHasValidRequests <- checkRequestCount searchTry.id (isBookAny $ tripQuoteDetails <&> (.vehicleServiceTier)) dpr.driverPoolResult.driverId dpr.driverPoolResult.serviceTier dpr.driverPoolResult.serviceTierDowngradeLevel driverPoolCfg
                      let isPreviousBatchDriver = dpr.driverPoolResult.driverId `elem` previousDriverOnRide
                      return $ isHasValidRequests && isPreviousBatchDriver
                  )
                  allNearbyDriversCurrentlyOnRide
              logDebug $ "NormalDriverPoolBatchOnRide-" <> show normalDriverPoolBatchOnRide
              logDebug $ "ValidDriversFromPreviousBatchOnRide-" <> show validDriversFromPreviousBatch
              let finalBatchOnRide = take batchSizeOnRide $ normalDriverPoolBatchOnRide <> validDriversFromPreviousBatch
              pure finalBatchOnRide
            else pure []

        filterSpecialDrivers specialDrivers = filter (\dpr -> not ((getId dpr.driverPoolResult.driverId) `elem` specialDrivers))

        bookAnyFilters transporterConfig allNearbyDrivers previousBatchesDrivers' = do
          let onlyNewNormalDriversWithMultipleSeriveTier = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` previousBatchesDrivers') allNearbyDrivers
          if isBookAny (tripQuoteDetails <&> (.vehicleServiceTier))
            then do selectMinDowngrade transporterConfig.bookAnyVehicleDowngradeLevel onlyNewNormalDriversWithMultipleSeriveTier
            else do onlyNewNormalDriversWithMultipleSeriveTier

        -- This function takes a list of DriverPoolWithActualDistResult and returns a list with unique driverId entries with the minimum serviceTierDowngradeLevel.
        selectMinDowngrade :: Int -> [DriverPoolWithActualDistResult] -> [DriverPoolWithActualDistResult]
        selectMinDowngrade config results = Map.elems $ foldr insertOrUpdate Map.empty filtered
          where
            insertOrUpdate :: DriverPoolWithActualDistResult -> Map.Map Text DriverPoolWithActualDistResult -> Map.Map Text DriverPoolWithActualDistResult
            insertOrUpdate result currentMap =
              let driver = result.driverPoolResult
                  key = driver.driverId.getId
               in Map.insertWith (minByDowngradeLevel) key result currentMap

            minByDowngradeLevel :: DriverPoolWithActualDistResult -> DriverPoolWithActualDistResult -> DriverPoolWithActualDistResult
            minByDowngradeLevel new old =
              if new.driverPoolResult.serviceTierDowngradeLevel < old.driverPoolResult.serviceTierDowngradeLevel
                then new
                else old

            filtered = filter (\d -> d.driverPoolResult.serviceTierDowngradeLevel >= config) results

        mkDriverPoolBatch mOCityId onlyNewDrivers intelligentPoolConfig transporterConfig batchSize' isOnRidePool = do
          case sortingType of
            Intelligent -> makeIntelligentDriverPool mOCityId onlyNewDrivers intelligentPoolConfig transporterConfig batchSize' isOnRidePool
            Random -> makeRandomDriverPool onlyNewDrivers batchSize'

        makeIntelligentDriverPool mOCityId onlyNewDrivers intelligentPoolConfig transporterConfig batchSize' isOnRidePool = do
          let sortWithDriverScore' = sortWithDriverScore mOCityId transporterConfig intelligentPoolConfig driverPoolCfg
          (sortedDriverPool, randomizedDriverPool) <-
            bimapM (sortWithDriverScore' [AcceptanceRatio, CancellationRatio, AvailableTime, DriverSpeed, ActualPickupDistance, RideFrequency] True isOnRidePool) (sortWithDriverScore' [AvailableTime, DriverSpeed, ActualPickupDistance] False isOnRidePool)
              =<< splitDriverPoolForSorting mOCityId intelligentPoolConfig.minQuotesToQualifyForIntelligentPool onlyNewDrivers
          let sortedDriverPoolWithSilentSort = splitSilentDriversAndSortWithDistance sortedDriverPool
          let randomizedDriverPoolWithSilentSort = splitSilentDriversAndSortWithDistance randomizedDriverPool
          takeDriversUsingPoolPercentage (sortedDriverPoolWithSilentSort, randomizedDriverPoolWithSilentSort) batchSize' intelligentPoolConfig

        makeRandomDriverPool onlyNewDrivers batchSize' = take batchSize' <$> randomizeAndLimitSelection onlyNewDrivers

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
                        serviceTiers = tripQuoteDetails <&> (.vehicleServiceTier),
                        fromLocation = searchReq.fromLocation,
                        toLocation = toLoc, -- last or all ?
                        merchantId = searchReq.providerId,
                        isRental = isRentalTrip searchTry.tripCategory,
                        isInterCity = isInterCityTrip searchTry.tripCategory
                      }
                  )
                  opCityId
              return $ filterSpecialDrivers specialDrivers calculateGoHomeDriverPoolBatch
            _ -> return []

        calcDriverPool poolType radiusStep merchantOpCityId = do
          let serviceTiers = tripQuoteDetails <&> (.vehicleServiceTier)
              merchantId = searchReq.providerId
          let pickupLoc = searchReq.fromLocation
          let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
          calculateDriverPoolWithActualDist DriverSelection poolType driverPoolCfg serviceTiers pickupLatLong merchantId merchantOpCityId True (Just radiusStep) (isRentalTrip searchTry.tripCategory) (isInterCityTrip searchTry.tripCategory)
        calcDriverCurrentlyOnRidePool poolType radiusStep transporterConfig merchantOpCityId batchNum' = do
          let merchantId = searchReq.providerId
          if transporterConfig.includeDriverCurrentlyOnRide && driverPoolCfg.enableForwardBatching && radiusStep > 0
            then do
              let serviceTiers = tripQuoteDetails <&> (.vehicleServiceTier)
              let pickupLoc = searchReq.fromLocation
              let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
              calculateDriverCurrentlyOnRideWithActualDist DriverSelection poolType driverPoolCfg serviceTiers pickupLatLong merchantId merchantOpCityId (Just radiusStep) (isRentalTrip searchTry.tripCategory) (isInterCityTrip searchTry.tripCategory) (toInteger batchNum')
            else pure []
        fillBatch transporterConfig merchantOpCityId allNearbyDrivers batch intelligentPoolConfig blockListedDrivers = do
          let batchDriverIds = batch <&> (.driverPoolResult.driverId)
          let driversNotInBatch = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` batchDriverIds) allNearbyDrivers
          driversWithValidReqAmount <- filterM (\dpr -> checkRequestCount searchTry.id (isBookAny $ tripQuoteDetails <&> (.vehicleServiceTier)) dpr.driverPoolResult.driverId dpr.driverPoolResult.serviceTier dpr.driverPoolResult.serviceTierDowngradeLevel driverPoolCfg) driversNotInBatch
          nonGoHomeDriversWithValidReqCount <- filterM (\dpr -> (CQDGR.getDriverGoHomeRequestInfo dpr.driverPoolResult.driverId merchantOpCityId (Just goHomeConfig)) <&> (/= Just DDGR.ACTIVE) . (.status)) driversWithValidReqAmount
          let nonGoHomeNormalDriversWithValidReqCount = filter (\ngd -> ngd.driverPoolResult.driverId `notElem` blockListedDrivers) nonGoHomeDriversWithValidReqCount
          let nonGoHomeNormalDriversWithValidReqCountWithServiceTier =
                if isBookAny (tripQuoteDetails <&> (.vehicleServiceTier))
                  then do selectMinDowngrade transporterConfig.bookAnyVehicleDowngradeLevel nonGoHomeNormalDriversWithValidReqCount
                  else do nonGoHomeNormalDriversWithValidReqCount
          let fillSize = batchSize - length batch
          (batch <>)
            <$> case sortingType of
              Intelligent -> do
                let sortWithDriverScore' = sortWithDriverScore merchantOpCityId transporterConfig intelligentPoolConfig driverPoolCfg
                (sortedDriverPool, randomizedDriverPool) <-
                  bimapM (sortWithDriverScore' [AcceptanceRatio, CancellationRatio, AvailableTime, DriverSpeed, ActualPickupDistance, RideFrequency] True False) (sortWithDriverScore' [AvailableTime, DriverSpeed, ActualPickupDistance] False False)
                    =<< splitDriverPoolForSorting merchantOpCityId intelligentPoolConfig.minQuotesToQualifyForIntelligentPool nonGoHomeNormalDriversWithValidReqCountWithServiceTier -- snd means taking drivers who recieved less then X(config- minQuotesToQualifyForIntelligentPool) quotes
                let sortedDriverPoolWithSilentSort = splitSilentDriversAndSortWithDistance sortedDriverPool
                let randomizedDriverPoolWithSilentSort = splitSilentDriversAndSortWithDistance randomizedDriverPool
                takeDriversUsingPoolPercentage (sortedDriverPoolWithSilentSort, randomizedDriverPoolWithSilentSort) fillSize intelligentPoolConfig
              Random -> pure $ take fillSize nonGoHomeNormalDriversWithValidReqCountWithServiceTier
        cacheBatch batch consideOnRideDrivers = do
          logDebug $ "Caching batch-" <> show batch
          batches <- previouslyAttemptedDrivers searchTry.id consideOnRideDrivers
          Redis.withCrossAppRedis $ Redis.setExp (previouslyAttemptedDriversKey searchTry.id consideOnRideDrivers) (batches <> batch) (60 * 30)
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
        batchSizeOnRide = driverPoolCfg.batchSizeOnRide

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
previouslyAttemptedDrivers searchTryId consideOnRideDrivers = do
  Redis.withCrossAppRedis $
    Redis.safeGet (previouslyAttemptedDriversKey searchTryId consideOnRideDrivers)
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
  TransporterConfig ->
  DriverIntelligentPoolConfig ->
  DriverPoolConfig ->
  [IntelligentFactors] ->
  Bool ->
  Bool ->
  [DriverPoolWithActualDistResult] ->
  m [DriverPoolWithActualDistResult]
sortWithDriverScore merchantOpCityId transporterConfig intelligentPoolConfig driverPoolCfg factorsToCalculate isPartOfIntelligentPool isOnRidePool dp = do
  logTagInfo "Weightage config for intelligent driver pool" $ show transporterConfig
  let driverIds = map (.driverPoolResult.driverId) dp
  let driverActualDistances = map ((.driverPoolResult.driverId) &&& (.driverPoolResult.distanceToPickup)) dp
  let cancellationScoreRelatedConfig = CancellationScoreRelatedConfig transporterConfig.popupDelayToAddAsPenalty transporterConfig.thresholdCancellationScore transporterConfig.minRidesForCancellationScore
  calculatedScores <- mapM (fetchScore merchantOpCityId driverActualDistances driverIds intelligentPoolConfig driverPoolCfg cancellationScoreRelatedConfig isOnRidePool) factorsToCalculate
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
  Bool ->
  IntelligentFactors ->
  m (HM.HashMap Text Double)
fetchScore merchantOpCityId driverActualDistanceList driverIds intelligentPoolConfig driverPoolCfg cancellationScoreRelatedConfig isOnRidePool factor =
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
      pure $ map (bimap (.getId) ((* (fromIntegral intelligentPoolConfig.actualPickupDistanceWeightage)) . fromIntegral . flip div (fromMaybe (Meters 1) (if isOnRidePool then driverPoolCfg.actualDistanceThresholdOnRide else driverPoolCfg.actualDistanceThreshold)))) driverActualDistanceList
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
  [TripQuoteDetail] ->
  GoHomeConfig ->
  Maybe Text ->
  m DriverPoolWithActualDistResultWithFlags
getNextDriverPoolBatch driverPoolConfig searchReq searchTry tripQuoteDetails goHomeConfig customerPhoneNum = withLogTag "getNextDriverPoolBatch" do
  batchNum <- getPoolBatchNum searchTry.id
  incrementBatchNum searchTry.id
  prepareDriverPoolBatch driverPoolConfig searchReq searchTry tripQuoteDetails batchNum goHomeConfig customerPhoneNum

getPoolBatchNum :: (Redis.HedisFlow m r) => Id DST.SearchTry -> m PoolBatchNum
getPoolBatchNum searchTryId = do
  res <- Redis.withCrossAppRedis $ Redis.get (poolBatchNumKey searchTryId)
  case res of
    Just i -> return i
    Nothing -> do
      let expTime = 600
      Redis.withCrossAppRedis $ Redis.setExp (poolBatchNumKey searchTryId) (-1 :: Integer) expTime
      return (-1)

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

isBookAny :: [DVST.ServiceTierType] -> Bool
isBookAny vehicleServiceTiers = (length vehicleServiceTiers) > 1

driverRequestCountKey :: Id DST.SearchTry -> Id Driver -> DVST.ServiceTierType -> Text
driverRequestCountKey searchTryId driverId vehicleServiceTier = "Driver-Request-Count-Key:SearchTryId-" <> searchTryId.getId <> ":DriverId-" <> driverId.getId <> ":ServiceTier-" <> show vehicleServiceTier

checkRequestCount :: Redis.HedisFlow m r => Id DST.SearchTry -> Bool -> Id Driver -> DVST.ServiceTierType -> Int -> DriverPoolConfig -> m Bool
checkRequestCount searchTryId isBookAnyRequest driverId vehicleServiceTier serviceTierDowngradeLevel driverPoolConfig =
  maybe True (\count -> (count :: Int) < getDriverRequestCountLimit serviceTierDowngradeLevel isBookAnyRequest driverPoolConfig)
    <$> Redis.withCrossAppRedis (Redis.get (driverRequestCountKey searchTryId driverId vehicleServiceTier))

getDriverRequestCountLimit :: Int -> Bool -> DriverPoolConfig -> Int
getDriverRequestCountLimit serviceTierDowngradeLevel isBookAnyRequest driverPoolConfig =
  if serviceTierDowngradeLevel < 0 && isBookAnyRequest -- if BookAny and serviceTierDowngradeLevel is less than 0, then limit is 1
    then 1
    else driverPoolConfig.driverRequestCountLimit

incrementDriverRequestCount :: (Redis.HedisFlow m r) => [DriverPoolWithActualDistResult] -> Id DST.SearchTry -> m ()
incrementDriverRequestCount finalPoolBatch searchTryId = do
  CM.mapM_
    ( \dpr ->
        Redis.withCrossAppRedis do
          void $ Redis.incr (driverRequestCountKey searchTryId dpr.driverPoolResult.driverId dpr.driverPoolResult.serviceTier)
          Redis.expire (driverRequestCountKey searchTryId dpr.driverPoolResult.driverId dpr.driverPoolResult.serviceTier) 7200
    )
    finalPoolBatch

data PrepareDriverPoolBatchEntity = PrepareDriverPoolBatchEntity
  { currentDriverPoolBatch :: [DriverPoolWithActualDistResult],
    poolType :: PoolType,
    nextScheduleTime :: Maybe Seconds,
    currentDriverPoolBatchOnRide :: [DriverPoolWithActualDistResult]
  }
