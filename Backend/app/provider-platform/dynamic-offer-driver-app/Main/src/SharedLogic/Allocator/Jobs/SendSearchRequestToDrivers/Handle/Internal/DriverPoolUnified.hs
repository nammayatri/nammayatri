module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPoolUnified where

import Control.Monad.Extra (partitionM)
import Data.Aeson
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.List as DL
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Domain.Types.Common
import Domain.Types.DriverGoHomeRequest as DDGR
import Domain.Types.DriverPoolConfig
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import Domain.Types.GoHomeConfig (GoHomeConfig)
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Driver)
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleServiceTier as DVST
import EulerHS.Prelude hiding (id)
import Kernel.Storage.Clickhouse.Config
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.DatastoreLatencyCalculator
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Queries.GateInfo
import qualified Lib.Types.SpecialLocation as SL
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.AirportEntryFee as AirportEntryFee
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool as SDP
import qualified SharedLogic.Beckn.Common as DTS
import SharedLogic.DriverPool
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import Storage.Beam.SpecialZone ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.RiderDriverCorrelation as QFavDrivers
import Tools.Maps as Maps

getNextDriverPoolBatch ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    LT.HasLocationService m r,
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    Redis.HedisLTSFlowEnv r,
    HasField "secondaryLTSHedisEnv" r (Maybe Redis.HedisEnv),
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r,
    HasField "enableLtsPoolDataForPooling" r Bool
  ) =>
  DriverPoolConfig ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  [TripQuoteDetail] ->
  Maybe DMPM.PaymentMethodInfo ->
  GoHomeConfig ->
  m DriverPoolWithActualDistResultWithFlags
getNextDriverPoolBatch driverPoolConfig searchReq searchTry tripQuoteDetails paymentMethodInfo goHomeConfig = withLogTag "getNextDriverPoolBatch" do
  logDebug $ "Doing Special Driver Pooling for seachReq:- " <> show searchReq
  batchNum <- SDP.getPoolBatchNum searchTry.id
  SDP.incrementBatchNum searchTry.id
  cityServiceTiers <- CQVST.findAllByMerchantOpCityIdInRideFlow searchReq.merchantOperatingCityId (searchReq.area >>= SL.pickupSpecialZoneIdFromArea)
  merchant <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound searchReq.providerId.getId)
  withTimeAPI "driverPooling" "prepareDriverPoolBatch" $ prepareDriverPoolBatch cityServiceTiers merchant driverPoolConfig searchReq searchTry tripQuoteDetails batchNum goHomeConfig paymentMethodInfo

assignTagsToDrivers :: [Id Driver] -> DriverPoolTags -> [DriverPoolWithActualDistResult] -> [DriverPoolWithActualDistResult]
assignTagsToDrivers driverIds driverTag =
  map
    ( \dp ->
        if dp.driverPoolResult.driverId `elem` driverIds
          then
            dp
              { driverPoolResult =
                  (driverPoolResult dp :: DriverPoolResult)
                    { driverTags =
                        insertInObject
                          (((driverTags :: DriverPoolResult -> Value) . driverPoolResult) dp)
                          [driverTag]
                    }
              }
          else dp
    )

prepareDriverPoolBatch ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    LT.HasLocationService m r,
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    Redis.HedisLTSFlowEnv r,
    HasField "secondaryLTSHedisEnv" r (Maybe Redis.HedisEnv),
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r,
    HasField "enableLtsPoolDataForPooling" r Bool
  ) =>
  [DVST.VehicleServiceTier] ->
  DM.Merchant ->
  DriverPoolConfig ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  [TripQuoteDetail] ->
  PoolBatchNum ->
  GoHomeConfig ->
  Maybe DMPM.PaymentMethodInfo ->
  m DriverPoolWithActualDistResultWithFlags
prepareDriverPoolBatch cityServiceTiers merchant driverPoolCfg searchReq searchTry tripQuoteDetails startingbatchNum goHomeConfig paymentMethodInfo = withLogTag ("startingbatchNum- (" <> show startingbatchNum <> ")" <> " for txnId:- " <> show searchReq.transactionId) $ do
  isValueAddNP <- CQVAN.isValueAddNP searchReq.bapId
  previousBatchesDrivers <- getPreviousBatchesDrivers Nothing
  previousBatchesDriversOnRide <- getPreviousBatchesDrivers (Just True)
  let merchantOpCityId = searchReq.merchantOperatingCityId
  logDebug $ "PreviousBatchesDrivers-" <> show previousBatchesDrivers
  SDP.PrepareDriverPoolBatchEntity {..} <- withTimeAPI "driverPooling" "prepareDriverPoolBatch'" $ prepareDriverPoolBatch' previousBatchesDrivers startingbatchNum merchantOpCityId searchReq.transactionId isValueAddNP
  let finalPool = currentDriverPoolBatch <> currentDriverPoolBatchOnRide
  SDP.incrementDriverRequestCount finalPool searchTry.id
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
        LT.HasLocationService m r,
        HasKafkaProducer r,
        ClickhouseFlow m r
      ) =>
      Maybe Bool ->
      m [Id Driver]
    getPreviousBatchesDrivers mbOnRide = do
      batches <- SDP.previouslyAttemptedDrivers searchTry.id mbOnRide
      return $ fst <$> batches

    prepareDriverPoolBatch' previousBatchesDrivers batchNum merchantOpCityId txnId isValueAddNP = withLogTag ("BatchNum - " <> show batchNum <> " and txnId:- " <> show txnId) $ do
      transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigDoesNotExist merchantOpCityId.getId)
      airportEntryFee <- AirportEntryFee.requiredEntryFeeForBooking (fromMaybe False transporterConfig.airportEntryFeeEnabled) searchReq.pickupGateId
      isAirportRequest <- AirportEntryFee.isAirportPickupArea searchReq.area
      blockListedDriversForSearch <- Redis.withCrossAppRedis $ Redis.getList (mkBlockListedDriversKey searchReq.id)
      blockListedDriversForRider <- maybe (pure []) (Redis.withCrossAppRedis . Redis.getList . mkBlockListedDriversForRiderKey) searchReq.riderId
      let blockListedDrivers = blockListedDriversForSearch <> blockListedDriversForRider
      -- Blocklisted drivers are excluded at LTS-level inside calculateDriverPoolWithActualDist;
      -- previously-attempted drivers are sorted to the tail of LTS candidates (chunking only
      -- pulls them in if fresher drivers run out — replaces the old fillBatch backfill).
      (allDriversNotOnRide', allOnRideDriverPoolResults) <- withTimeAPI "driverPooling" "calcDriverPool" $ calcDriverPool NormalPool transporterConfig blockListedDrivers previousBatchesDrivers airportEntryFee isAirportRequest
      favDrivers <- maybe (pure []) (`QFavDrivers.findFavDriversForRider` True) searchReq.riderId
      let newFilteredDriversWithFavourites = assignTagsToDrivers (favDrivers <&> (.driverId)) FavouriteDriver allDriversNotOnRide'
      (driverPoolNotOnRide, driverPoolOnRide) <- do
        case batchNum of
          -1 -> do
            gateTaggedDrivers <- assignDriverGateTags searchReq newFilteredDriversWithFavourites
            goHomeTaggedDrivers <- assignDriverGoHomeTags gateTaggedDrivers searchReq searchTry tripQuoteDetails driverPoolCfg merchant goHomeConfig merchantOpCityId isValueAddNP transporterConfig paymentMethodInfo
            logDebug $ "GoHomeDriverPool and GateTaggedPool-" <> show goHomeTaggedDrivers
            withTimeAPI "driverPooling" "calculateNormalBatchGoHome" $ calculateNormalBatch merchantOpCityId transporterConfig (bookAnyFilters transporterConfig goHomeTaggedDrivers) txnId allOnRideDriverPoolResults airportEntryFee isAirportRequest
          _ -> do
            allNearbyNonGoHomeDrivers <- withTimeAPI "driverPooling" "filterM getDriverGoHomeRequestInfo" $ filterM (\dpr -> (CQDGR.getDriverGoHomeRequestInfo dpr.driverPoolResult.driverId merchantOpCityId (Just goHomeConfig)) <&> (/= Just DDGR.ACTIVE) . (.status)) newFilteredDriversWithFavourites
            logDebug $ "Calculating Normal Batch for the pool " <> show allNearbyNonGoHomeDrivers
            withTimeAPI "driverPooling" "calculateNormalBatch" $ calculateNormalBatch merchantOpCityId transporterConfig (bookAnyFilters transporterConfig allNearbyNonGoHomeDrivers) txnId allOnRideDriverPoolResults airportEntryFee isAirportRequest
      cacheBatch driverPoolNotOnRide Nothing
      cacheBatch driverPoolOnRide (Just True)
      let (poolNotOnRide, poolOnRide) =
            ( addDistanceSplitConfigBasedDelaysForDriversWithinBatch driverPoolNotOnRide,
              addDistanceSplitConfigBasedDelaysForOnRideDriversWithinBatch driverPoolOnRide
            )
      (poolWithSpecialZoneInfoNotOnRide, poolWithSpecialZoneInfoOnRide) <-
        if isJust searchReq.specialLocationTag
          then (,) <$> addSpecialZonePickupInfo poolNotOnRide <*> addSpecialZonePickupInfo poolOnRide
          else pure (poolNotOnRide, poolOnRide)
      pure $ SDP.PrepareDriverPoolBatchEntity poolWithSpecialZoneInfoNotOnRide NormalPool Nothing poolWithSpecialZoneInfoOnRide
      where
        addSpecialZonePickupInfo pool = do
          (driversFromGate, restDrivers) <- splitDriverFromGateAndRest pool
          pure $ restDrivers <> addSpecialZoneInfo searchReq.driverDefaultExtraFee driversFromGate

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

        calcDriverPool poolType transporterConfig excludeDriverIds prevAttemptedDriverIds airportEntryFee isAirportRequest = do
          now <- getCurrentTime
          let serviceTiers = tripQuoteDetails <&> (.vehicleServiceTier)
              merchantId = searchReq.providerId
              pickupLoc = searchReq.fromLocation
              pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
              dropLocation = searchReq.toLocation <&> (\loc -> LatLong loc.lat loc.lon)
              routeDistance = searchReq.estimatedDistance
              currentSearchInfo = DTS.CurrentSearchInfo {..}
              govtCharges = listToMaybe tripQuoteDetails >>= (.govtCharges)
              tollCharges_ = listToMaybe tripQuoteDetails >>= (.tollCharges)
              parkingCharge = listToMaybe tripQuoteDetails >>= (.driverParkingCharge)
              currentRideTripCategoryValidForForwardBatching = driverPoolCfg.currentRideTripCategoryValidForForwardBatching
              driverPoolReq =
                CalculateDriverPoolReq
                  { poolStage = DriverSelection,
                    pickup = pickupLatLong,
                    merchantOperatingCityId = merchantOpCityId,
                    isRental = isRentalTrip searchTry.tripCategory,
                    isInterCity = isInterCityTrip searchTry.tripCategory,
                    onlinePayment = merchant.onlinePayment,
                    rideFare = Just searchTry.baseFare,
                    tollCharges = tollCharges_,
                    paymentInstrument = fmap (.paymentInstrument) paymentMethodInfo,
                    paymentMode = searchReq.paymentMode,
                    excludeDriverIds = excludeDriverIds,
                    prevAttemptedDriverIds = prevAttemptedDriverIds,
                    ..
                  }
          calculateDriverPoolWithActualDist driverPoolReq poolType currentSearchInfo batchNum

        calculateNormalBatch mOCityId transporterConfig onlyNewNormalDrivers txnId' onRidePoolResults airportEntryFee isAirportRequest = do
          logDebug $ "calculateNormalBatch txnId " <> show txnId'
          (normalBatchNotOnRide, _, _) <- withTimeAPI "driverPooling" "getDriverPoolNotOnRide" $ getDriverPoolNotOnRide mOCityId transporterConfig onlyNewNormalDrivers
          logDebug $ "NormalBatchNotOnRide-" <> show normalBatchNotOnRide <> " and txnId " <> show txnId'
          normalBatchOnRide <- getDriverPoolOnRide mOCityId transporterConfig NormalPool onRidePoolResults airportEntryFee isAirportRequest
          pure (normalBatchNotOnRide, normalBatchOnRide)

        getDriverPoolNotOnRide mOCityId transporterConfig onlyNewNormalDrivers = do
          (_, normalDriverPoolBatch) <- withTimeAPI "driverPooling" "mkDriverPoolBatch" $ mkDriverPoolBatch mOCityId onlyNewNormalDrivers transporterConfig batchSize False
          pure (normalDriverPoolBatch, [], Nothing)

        filtersForNormalBatch mOCityId transporterConfig normalDriverPool = do
          allNearbyNonGoHomeDrivers <- filterM (\dpr -> (CQDGR.getDriverGoHomeRequestInfo dpr.driverPoolResult.driverId mOCityId (Just goHomeConfig)) <&> (/= Just DDGR.ACTIVE) . (.status)) normalDriverPool
          pure $ bookAnyFilters transporterConfig allNearbyNonGoHomeDrivers

        getDriverPoolOnRide mOCityId transporterConfig poolType allDriverPoolResults airportEntryFee isAirportRequest = do
          if poolType == NormalPool && driverPoolCfg.enableForwardBatching && searchTry.isAdvancedBookingEnabled
            then do
              previousDriverOnRide <- getPreviousBatchesDrivers (Just True)
              allNearbyDriversCurrentlyOnRide <- calcDriverCurrentlyOnRidePool poolType transporterConfig batchNum allDriverPoolResults airportEntryFee isAirportRequest
              logDebug $ "NormalDriverPoolBatchOnRideCurrentlyOnRide-" <> show allNearbyDriversCurrentlyOnRide
              onlyNewNormalDriversOnRide <- filtersForNormalBatch mOCityId transporterConfig allNearbyDriversCurrentlyOnRide
              (_, normalDriverPoolBatchOnRide) <- withTimeAPI "driverPooling" "mkDriverPoolBatchOnRide" $ mkDriverPoolBatch mOCityId onlyNewNormalDriversOnRide transporterConfig batchSizeOnRide True
              validDriversFromPreviousBatch <-
                filterM
                  ( \dpr -> do
                      isHasValidRequests <- SDP.checkRequestCount searchTry.id (SDP.isBookAny $ tripQuoteDetails <&> (.vehicleServiceTier)) dpr.driverPoolResult.driverId dpr.driverPoolResult.serviceTier dpr.driverPoolResult.serviceTierDowngradeLevel driverPoolCfg
                      let isPreviousBatchDriver = dpr.driverPoolResult.driverId `elem` previousDriverOnRide
                      return $ isHasValidRequests && isPreviousBatchDriver
                  )
                  allNearbyDriversCurrentlyOnRide
              logDebug $ "NormalDriverPoolBatchOnRide-" <> show normalDriverPoolBatchOnRide
              logDebug $ "ValidDriversFromPreviousBatchOnRide-" <> show validDriversFromPreviousBatch
              let finalBatchOnRide = take batchSizeOnRide $ normalDriverPoolBatchOnRide <> validDriversFromPreviousBatch
              pure finalBatchOnRide
            else pure []

        bookAnyFilters transporterConfig allNearbyDrivers = do
          if SDP.isBookAny (tripQuoteDetails <&> (.vehicleServiceTier))
            then do selectMinDowngrade transporterConfig.bookAnyVehicleDowngradeLevel allNearbyDrivers
            else do allNearbyDrivers

        -- This function takes a list of DriverPoolWithActualDistResult and returns a list with unique driverId entries with the minimum serviceTierDowngradeLevel.
        selectMinDowngrade :: Int -> [DriverPoolWithActualDistResult] -> [DriverPoolWithActualDistResult]
        selectMinDowngrade config results = Map.elems $ foldr insertOrUpdate Map.empty filtered
          where
            insertOrUpdate :: DriverPoolWithActualDistResult -> Map.Map Text DriverPoolWithActualDistResult -> Map.Map Text DriverPoolWithActualDistResult
            insertOrUpdate result currentMap =
              let driver = result.driverPoolResult
                  key = driver.driverId.getId
               in Map.insertWith minByDowngradeLevel key result currentMap

            minByDowngradeLevel :: DriverPoolWithActualDistResult -> DriverPoolWithActualDistResult -> DriverPoolWithActualDistResult
            minByDowngradeLevel new old =
              if new.driverPoolResult.serviceTierDowngradeLevel < old.driverPoolResult.serviceTierDowngradeLevel
                then new
                else old

            filtered = filter (\d -> d.driverPoolResult.serviceTierDowngradeLevel >= config) results

        mkDriverPoolBatch mOCityId onlyNewDrivers transporterConfig batchSize' isOnRidePool = withTimeAPI "driverPooling" "makeTaggedDriverPool" $ SDP.makeTaggedDriverPool mOCityId transporterConfig.timeDiffFromUtc searchReq onlyNewDrivers batchSize' isOnRidePool searchReq.customerNammaTags searchReq.poolingLogicVersion batchNum driverPoolCfg searchTry.id

        addDistanceSplitConfigBasedDelaysForDriversWithinBatch =
          addDelaysWithPrioritySplit driverPoolCfg.distanceBasedBatchSplit

        addDistanceSplitConfigBasedDelaysForOnRideDriversWithinBatch =
          addDelaysWithPrioritySplit driverPoolCfg.onRideBatchSplitConfig

        addDelaysWithPrioritySplit ::
          ( HasField "batchSplitSize" split Int,
            HasField "batchSplitDelay" split Seconds
          ) =>
          [split] ->
          [DriverPoolWithActualDistResult] ->
          [DriverPoolWithActualDistResult]
        addDelaysWithPrioritySplit splits pool =
          if not enablePriorityTagSplit' || null priorityTagNames || null priorityDrivers
            then applyDelays splits pool
            else firstSplitWithDelay <> restWithDelay
          where
            enablePriorityTagSplit' = fromMaybe False driverPoolCfg.enablePriorityTagSplit
            priorityTagNames = extractPriorityDriverTags searchReq.searchTags
            (priorityDrivers, nonPriorityDrivers) = DL.partition (hasAnyPriorityTag priorityTagNames) pool
            mbFirstSplit = listToMaybe splits
            firstSplitDelay = maybe (Seconds 0) (.batchSplitDelay) mbFirstSplit
            firstSplitSize = maybe 0 (.batchSplitSize) mbFirstSplit
            backfillCount = max 0 (firstSplitSize - length priorityDrivers)
            restSplits = drop 1 splits
            firstSplitWithDelay = map (\d -> d {keepHiddenForSeconds = firstSplitDelay}) priorityDrivers
            lastIdx = length restSplits - 1
            restWithDelay =
              fst $
                foldl'
                  ( \(finalBatch, (restBatch, idx)) splitConfig ->
                      let extraSize = if idx == lastIdx then backfillCount else 0
                          (splitToAddDelay, newRestBatch) = splitAt (splitConfig.batchSplitSize + extraSize) restBatch
                          splitWithDelay = map (\d -> d {keepHiddenForSeconds = splitConfig.batchSplitDelay}) splitToAddDelay
                       in (finalBatch <> splitWithDelay, (newRestBatch, idx + 1))
                  )
                  ([], (nonPriorityDrivers, 0 :: Int))
                  restSplits

            applyDelays splits' pool' =
              fst $
                foldl'
                  ( \(finalBatch, restBatch) splitConfig ->
                      let (splitToAddDelay, newRestBatch) = splitAt splitConfig.batchSplitSize restBatch
                          splitWithDelay = map (\d -> d {keepHiddenForSeconds = splitConfig.batchSplitDelay}) splitToAddDelay
                       in (finalBatch <> splitWithDelay, newRestBatch)
                  )
                  ([], pool')
                  splits'

        calcDriverCurrentlyOnRidePool poolType transporterConfig _batchNum allDriverPoolResults airportEntryFee isAirportRequest = do
          let merchantId = searchReq.providerId
          now <- getCurrentTime
          if transporterConfig.includeDriverCurrentlyOnRide && driverPoolCfg.enableForwardBatching
            then do
              let onRideDrivers = filterOnRideDriversFromPool allDriverPoolResults
              let serviceTiers = tripQuoteDetails <&> (.vehicleServiceTier)
              let pickupLoc = searchReq.fromLocation
              let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
              let dropLocation = searchReq.toLocation <&> (\loc -> LatLong loc.lat loc.lon)
                  routeDistance = searchReq.estimatedDistance
              let currentSearchInfo = DTS.CurrentSearchInfo {..}
              let govtCharges = listToMaybe tripQuoteDetails >>= (.govtCharges)
                  tollCharges_ = listToMaybe tripQuoteDetails >>= (.tollCharges)
                  parkingCharge = listToMaybe tripQuoteDetails >>= (.driverParkingCharge)
              let currentRideTripCategoryValidForForwardBatching = driverPoolCfg.currentRideTripCategoryValidForForwardBatching
              let driverPoolReq =
                    CalculateDriverPoolReq
                      { poolStage = DriverSelection,
                        pickup = pickupLatLong,
                        merchantOperatingCityId = merchantOpCityId,
                        isRental = isRentalTrip searchTry.tripCategory,
                        isInterCity = isInterCityTrip searchTry.tripCategory,
                        onlinePayment = merchant.onlinePayment,
                        rideFare = Just searchTry.baseFare,
                        tollCharges = tollCharges_,
                        paymentInstrument = fmap (.paymentInstrument) paymentMethodInfo,
                        paymentMode = searchReq.paymentMode,
                        excludeDriverIds = [],
                        prevAttemptedDriverIds = [],
                        ..
                      }
              calculateDriverCurrentlyOnRideWithActualDist driverPoolReq onRideDrivers poolType currentSearchInfo
            else pure []

        cacheBatch batch consideOnRideDrivers = do
          logDebug $ "Caching batch-" <> show batch
          batches <- SDP.previouslyAttemptedDrivers searchTry.id consideOnRideDrivers
          let minimalBatch = (\dp -> (dp.driverPoolResult.driverId, dp.driverPoolResult.serviceTier)) <$> batch
          Redis.withCrossAppRedis $ Redis.setExp (SDP.previouslyAttemptedDriversKey searchTry.id consideOnRideDrivers) (batches <> minimalBatch) (60 * 30)

        batchSize = getBatchSize driverPoolCfg.dynamicBatchSize batchNum driverPoolCfg.driverBatchSize
        batchSizeOnRide = driverPoolCfg.batchSizeOnRide

assignDriverGateTags ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    LT.HasLocationService m r
  ) =>
  DSR.SearchRequest ->
  [DriverPoolWithActualDistResult] ->
  m [DriverPoolWithActualDistResult]
assignDriverGateTags searchReq pool = do
  case searchReq.pickupZoneGateId of
    Just pickupZoneGateId -> do
      (onGateDrivers', outSideDrivers) <-
        partitionM
          ( \dd -> do
              mbDriverGate <- findGateInfoIfDriverInsideGatePickupZone (LatLong dd.driverPoolResult.lat dd.driverPoolResult.lon)
              pure $ case mbDriverGate of
                Just driverGate -> driverGate.id.getId == pickupZoneGateId
                Nothing -> False
          )
          pool
      let onGateDrivers'' =
            map
              ( \(dp :: DriverPoolWithActualDistResult) ->
                  let dpResult = dp.driverPoolResult
                      updatedTags = insertInObject dpResult.driverTags [SpecialZoneQueueDriver]
                      dprWithTags = (dpResult :: DriverPoolResult) {driverTags = updatedTags}
                   in dp {driverPoolResult = dprWithTags}
              )
              onGateDrivers'
          onGateDrivers = addSpecialZoneInfo searchReq.driverDefaultExtraFee onGateDrivers''
      return $ onGateDrivers <> outSideDrivers
    Nothing -> return pool

addSpecialZoneInfo :: Maybe HighPrecMoney -> [DriverPoolWithActualDistResult] -> [DriverPoolWithActualDistResult]
addSpecialZoneInfo driverDefaultExtraFee = map (\driverWithDistance -> driverWithDistance {pickupZone = True, specialZoneExtraTip = driverDefaultExtraFee})

assignDriverGoHomeTags ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    LT.HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    Redis.HedisLTSFlowEnv r
  ) =>
  [DriverPoolWithActualDistResult] ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  [TripQuoteDetail] ->
  DriverPoolConfig ->
  DM.Merchant ->
  GoHomeConfig ->
  Id MerchantOperatingCity ->
  Bool ->
  DTC.TransporterConfig ->
  Maybe DMPM.PaymentMethodInfo ->
  m [DriverPoolWithActualDistResult]
assignDriverGoHomeTags pool searchReq searchTry tripQuoteDetails driverPoolCfg merchant goHomeConfig merchantOpCityId isValueAddNP transporterConfig paymentMethodInfo = do
  (goHomeDriversInQueue, goHomeDriversNotToDestination) <-
    case searchReq.toLocation of
      Just toLoc | isGoHomeAvailable searchTry.tripCategory -> do
        let dropLocation = searchReq.toLocation <&> (\loc -> LatLong loc.lat loc.lon)
            routeDistance = searchReq.estimatedDistance
        let currentSearchInfo = DTS.CurrentSearchInfo {..}
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
                  isInterCity = isInterCityTrip searchTry.tripCategory,
                  onlinePayment = merchant.onlinePayment,
                  configsInExperimentVersions = searchReq.configInExperimentVersions,
                  rideFare = Just searchTry.baseFare,
                  govtCharges = listToMaybe tripQuoteDetails >>= (.govtCharges),
                  tollCharges = listToMaybe tripQuoteDetails >>= (.tollCharges),
                  parkingCharge = listToMaybe tripQuoteDetails >>= (.driverParkingCharge),
                  paymentInstrument = fmap (.paymentInstrument) paymentMethodInfo,
                  paymentMode = searchReq.paymentMode,
                  ..
                }
        filterOutGoHomeDriversAccordingToHomeLocation (map (convertDriverPoolWithActualDistResultToNearestGoHomeDriversResult False True) pool) goHomeReq merchantOpCityId
      _ -> pure ([], [])
  let goHomeDriversToDestionation = map (\dp -> dp.driverPoolResult.driverId) goHomeDriversInQueue
      goHomePool' = filter (\dp -> dp.driverPoolResult.driverId `notElem` goHomeDriversToDestionation) pool <> assignGoHomeTags goHomeDriversToDestionation GoHomeDriverToDestination False goHomeDriversInQueue
  return $ assignGoHomeTags goHomeDriversNotToDestination GoHomeDriverNotToDestination True goHomePool'
  where
    assignGoHomeTags goHomeDrivers driverTag checkNeeded =
      map
        ( \dp ->
            if not checkNeeded || dp.driverPoolResult.driverId `elem` goHomeDrivers
              then
                dp
                  { driverPoolResult =
                      (driverPoolResult dp :: DriverPoolResult)
                        { driverTags =
                            insertInObject
                              (((driverTags :: DriverPoolResult -> Value) . driverPoolResult) dp)
                              [driverTag]
                        }
                  }
              else dp
        )

insertInObject :: Value -> [DriverPoolTags] -> Value
insertInObject obj tags =
  case obj of
    Object keymap -> Object $ DL.foldl' (\acc key -> AKM.insert ((AK.fromString . show) key) (toJSON True) acc) keymap tags
    _ -> Object $ DL.foldl' (\acc key -> AKM.insert ((AK.fromString . show) key) (toJSON True) acc) AKM.empty tags

hasAnyPriorityTag :: [Text] -> DriverPoolWithActualDistResult -> Bool
hasAnyPriorityTag tagNames dp = case dp.driverPoolResult.driverTags of
  Object keymap -> any (\name -> AKM.member (AK.fromText name) keymap) tagNames
  _ -> False

priorityDriverTagPrefix :: Text
priorityDriverTagPrefix = "priorityDriverTag#"

extractPriorityDriverTags :: Maybe [LYT.TagNameValue] -> [Text]
extractPriorityDriverTags = maybe [] (mapMaybe extractTag)
  where
    extractTag (LYT.TagNameValue raw) = case T.stripPrefix priorityDriverTagPrefix raw of
      Just name | not (T.null name) -> Just name
      _ -> Nothing
