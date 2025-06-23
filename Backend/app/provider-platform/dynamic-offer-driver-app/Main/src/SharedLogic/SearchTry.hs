{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.SearchTry where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Data.Text.Encoding as TE
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import Domain.Types.ConditionalCharges as DAC
import Domain.Types.DriverPoolConfig
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.Allocator
import qualified SharedLogic.Booking as SBooking
import SharedLogic.DriverPool.Types
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FarePolicy
import SharedLogic.GoogleTranslate (TranslateFlow)
import SharedLogic.Pricing
import Storage.Cac.DriverPoolConfig (getDriverPoolConfig)
import qualified Storage.Cac.GoHomeConfig as CGHC
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.VehicleServiceTier as CQDVST
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import qualified Tools.Metrics as Metrics
import qualified Tools.SharedRedisKeys as SharedRedisKeys
import TransactionLogs.Types
import Utils.Common.Cac.KeyNameConstants

getNextScheduleTime ::
  ( MonadFlow m,
    Metrics.CoreMetrics m,
    CacheFlow m r,
    MonadReader r m
  ) =>
  DriverPoolConfig ->
  DSR.SearchRequest ->
  UTCTime ->
  m (Maybe NominalDiffTime)
getNextScheduleTime driverPoolConfig searchRequest now = do
  mbScheduleTryTimes <- getKey
  scheduleTryTimes <-
    case mbScheduleTryTimes of
      Just scheduleTryTimes' -> pure scheduleTryTimes'
      Nothing -> do
        let origTryTimes = secondsToNominalDiffTime . Seconds <$> driverPoolConfig.scheduleTryTimes
        case origTryTimes of
          [] -> throwError $ InternalError "Non-emptiness of scheduleTryTime is guaranteed."
          (x : xs) -> pure (x : xs)
  case scheduleTryTimes of
    [] -> return Nothing
    (scheduleTryTime : rest) -> do
      if diffUTCTime searchRequest.startTime now <= scheduleTryTime
        then do
          setKey rest
          case rest of
            [] -> do
              void $ Redis.withCrossAppRedis $ Redis.del scheduleSearchKey
              return Nothing
            (next : _) -> return $ Just $ max 2 (searchRequest.startTime `diffUTCTime` (next `addUTCTime` now))
        else return $ Just $ max 2 (searchRequest.startTime `diffUTCTime` (scheduleTryTime `addUTCTime` now))
  where
    scheduleSearchKey = "ScheduleSearch-" <> searchRequest.id.getId
    setKey scheduleTryTimes = Redis.withCrossAppRedis $ Redis.setExp scheduleSearchKey scheduleTryTimes 432000
    getKey = Redis.withCrossAppRedis $ Redis.safeGet scheduleSearchKey

initiateDriverSearchBatch ::
  ( EncFlow m r,
    TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Log m,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasShortDurationRetryCfg r c
  ) =>
  DriverSearchBatchInput m ->
  m ()
initiateDriverSearchBatch searchBatchInput@DriverSearchBatchInput {..} = do
  searchTry <- createNewSearchTry
  try @_ @SomeException
    ( do
        driverPoolConfig <- getDriverPoolConfig searchReq.merchantOperatingCityId searchTry.vehicleServiceTier searchTry.tripCategory (fromMaybe SL.Default searchReq.area) searchReq.estimatedDistance searchTry.searchRepeatType searchTry.searchRepeatCounter (Just (TransactionId (Id searchReq.transactionId))) searchReq
        goHomeCfg <- CGHC.findByMerchantOpCityId searchReq.merchantOperatingCityId (Just (TransactionId (Id searchReq.transactionId)))
        singleBatchProcessingTempDelay <- asks (.singleBatchProcessingTempDelay)
        now <- getCurrentTime
        let batchTime = fromIntegral driverPoolConfig.singleBatchProcessTime + singleBatchProcessingTempDelay
        let totalBatchTime = fromIntegral driverPoolConfig.maxNumberOfBatches * batchTime
        let scheduleTryTimes = secondsToNominalDiffTime . Seconds <$> driverPoolConfig.scheduleTryTimes
            instantReallocation = maybe True (\scheduleTryTime -> diffUTCTime searchReq.startTime now <= scheduleTryTime) (listToMaybe scheduleTryTimes)
        if not searchTry.isScheduled || (instantReallocation && isRepeatSearch)
          then do
            (res, _, mbNewScheduleTimeIn) <- sendSearchRequestToDrivers driverPoolConfig searchTry searchBatchInput goHomeCfg
            let inTime = singleBatchProcessingTempDelay + maybe (fromIntegral driverPoolConfig.singleBatchProcessTime) fromIntegral mbNewScheduleTimeIn
            case res of
              (ReSchedule _) -> scheduleBatching searchTry inTime
              _ -> return ()
            SharedRedisKeys.setBatchConfig searchReq.transactionId $
              SharedRedisKeys.BatchConfig
                { totalBatches = driverPoolConfig.maxNumberOfBatches,
                  batchTime = nominalDiffTimeToSeconds batchTime,
                  batchingStartedAt = now,
                  batchingExpireAt = totalBatchTime `addUTCTime` now
                }
            logInfo $ "initiateDriverSearchBatch: " <> show (totalBatchTime `addUTCTime` now)
          else do
            mbScheduleTime <- getNextScheduleTime driverPoolConfig searchReq now
            case mbScheduleTime of
              Just scheduleTime -> do
                scheduleBatching searchTry scheduleTime
                let batchingStartedAt = scheduleTime `addUTCTime` now
                SharedRedisKeys.setBatchConfig searchReq.transactionId $
                  SharedRedisKeys.BatchConfig
                    { totalBatches = driverPoolConfig.maxNumberOfBatches,
                      batchTime = nominalDiffTimeToSeconds batchTime,
                      batchingStartedAt,
                      batchingExpireAt = totalBatchTime `addUTCTime` batchingStartedAt
                    }
                logInfo $ "initiateDriverSearchBatch: " <> show (totalBatchTime `addUTCTime` batchingStartedAt)
              Nothing -> do
                booking <- QRB.findByQuoteId searchTry.estimateId >>= fromMaybeM (BookingDoesNotExist searchTry.estimateId)
                QST.updateStatus DST.CANCELLED searchTry.id
                SBooking.cancelBooking booking Nothing merchant
    )
    >>= \case
      Left e -> do
        logError $ "Error in initiateDriverSearchBatch: " <> show e
        return ()
      Right _ -> return ()
  where
    scheduleBatching searchTry inTime = do
      JC.createJobIn @_ @'SendSearchRequestToDriver (Just searchReq.providerId) (Just searchReq.merchantOperatingCityId) inTime $
        SendSearchRequestToDriverJobData
          { searchTryId = searchTry.id,
            estimatedRideDistance = searchReq.estimatedDistance
          }

    createNewSearchTry = do
      mbLastSearchTry <- QST.findLastByRequestId searchReq.id
      case tripQuoteDetails of
        [] -> throwError $ InternalError "No trip quote details found"
        (firstQuoteDetail : _) -> do
          let estimatedFare = firstQuoteDetail.baseFare
          let tripCategory = firstQuoteDetail.tripCategory -- for fallback case
          let serviceTier = firstQuoteDetail.vehicleServiceTier -- for fallback case
          let estOrQuoteId = firstQuoteDetail.estimateOrQuoteId -- for fallback case
          let estimateOrQuoteIds = tripQuoteDetails <&> (.estimateOrQuoteId)
          let estimateOrQuoteServiceTierNames = tripQuoteDetails <&> (.vehicleServiceTierName)
          searchTry <- case mbLastSearchTry of
            Nothing -> do
              searchTry <- buildSearchTry merchant.id searchReq estimateOrQuoteIds estOrQuoteId estimatedFare 0 DST.INITIAL tripCategory customerExtraFee firstQuoteDetail.petCharges messageId estimateOrQuoteServiceTierNames serviceTier
              _ <- QST.create searchTry
              return searchTry
            Just oldSearchTry -> do
              let searchRepeatType
                    | isRepeatSearch = DST.REALLOCATION
                    | oldSearchTry.status == DST.ACTIVE = DST.CANCELLED_AND_RETRIED
                    | otherwise = DST.RETRIED
              -- TODO : Fix this
              -- unless (pureEstimatedFare == oldSearchTry.baseFare - fromMaybe 0 oldSearchTry.customerExtraFee) $
              --   throwError SearchTryEstimatedFareChanged
              searchTry <- buildSearchTry merchant.id searchReq estimateOrQuoteIds estOrQuoteId estimatedFare (oldSearchTry.searchRepeatCounter + 1) searchRepeatType tripCategory customerExtraFee firstQuoteDetail.petCharges messageId estimateOrQuoteServiceTierNames serviceTier
              when (oldSearchTry.status == DST.ACTIVE) $ do
                QST.updateStatus DST.CANCELLED oldSearchTry.id
                void $ QDQ.setInactiveBySTId oldSearchTry.id
              _ <- QST.create searchTry
              return searchTry

          logDebug $
            "search try id=" <> show searchTry.id
              <> "; estimated distance = "
              <> show searchReq.estimatedDistance
              <> "; estimated base fare:"
              <> show estimatedFare
          return searchTry

buildSearchTry ::
  ( MonadFlow m,
    CacheFlow m r,
    Metrics.CoreMetrics m,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  DSR.SearchRequest ->
  [Text] ->
  Text ->
  HighPrecMoney ->
  Int ->
  DST.SearchRepeatType ->
  DTC.TripCategory ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Text ->
  [Text] ->
  DVST.ServiceTierType ->
  m DST.SearchTry
buildSearchTry merchantId searchReq estimateOrQuoteIds estOrQuoteId baseFare searchRepeatCounter searchRepeatType tripCategory customerExtraFee petCharges messageId estimateOrQuoteServTierNames serviceTier = do
  now <- getCurrentTime
  id_ <- Id <$> generateGUID
  vehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityIdInRideFlow serviceTier searchReq.merchantOperatingCityId searchReq.configInExperimentVersions >>= fromMaybeM (VehicleServiceTierNotFound (show serviceTier))
  transporterConfig <- CTC.findByMerchantOpCityId searchReq.merchantOperatingCityId (Just (TransactionId (Id searchReq.transactionId))) >>= fromMaybeM (TransporterConfigNotFound searchReq.merchantOperatingCityId.getId)
  if tripCategory == DTC.OneWay DTC.OneWayOnDemandDynamicOffer && transporterConfig.isDynamicPricingQARCalEnabled == Just True
    then do
      void $ Redis.withCrossAppRedis $ Redis.geoAdd (mkDemandVehicleCategoryWithDistanceBin now vehicleServiceTierItem.vehicleCategory ((.getMeters) <$> searchReq.estimatedDistance)) [(searchReq.fromLocation.lon, searchReq.fromLocation.lat, TE.encodeUtf8 (id_.getId))]
      void $ Redis.withCrossAppRedis $ Redis.expire (mkDemandVehicleCategoryWithDistanceBin now vehicleServiceTierItem.vehicleCategory ((.getMeters) <$> searchReq.estimatedDistance)) 3600
      void $ Redis.withCrossAppRedis $ Redis.geoAdd (mkDemandVehicleCategory now vehicleServiceTierItem.vehicleCategory) [(searchReq.fromLocation.lon, searchReq.fromLocation.lat, TE.encodeUtf8 (id_.getId))]
      void $ Redis.withCrossAppRedis $ Redis.expire (mkDemandVehicleCategory now vehicleServiceTierItem.vehicleCategory) 3600
      void $ Redis.withCrossAppRedis $ Redis.incr (mkDemandVehicleCategoryCity now vehicleServiceTierItem.vehicleCategory searchReq.merchantOperatingCityId.getId)
      void $ Redis.withCrossAppRedis $ Redis.expire (mkDemandVehicleCategoryCity now vehicleServiceTierItem.vehicleCategory searchReq.merchantOperatingCityId.getId) 3600
    else pure ()
  pure $
    DST.SearchTry
      { id = id_,
        vehicleServiceTier = serviceTier,
        vehicleServiceTierName = vehicleServiceTierItem.name,
        requestId = searchReq.id,
        vehicleCategory = vehicleServiceTierItem.vehicleCategory,
        estimateIds = estimateOrQuoteIds,
        estimateId = estOrQuoteId,
        merchantId = Just merchantId,
        merchantOperatingCityId = searchReq.merchantOperatingCityId,
        messageId = messageId,
        startTime = searchReq.startTime,
        isScheduled = searchReq.isScheduled,
        validTill = searchReq.validTill,
        status = DST.ACTIVE,
        createdAt = now,
        updatedAt = now,
        currency = searchReq.currency,
        isAdvancedBookingEnabled = searchReq.isAdvanceBookingEnabled,
        serviceTierArray = estimateOrQuoteServTierNames,
        preferSafetyPlus = searchReq.preferSafetyPlus,
        ..
      }

buildTripQuoteDetail ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  DSR.SearchRequest ->
  DTC.TripCategory ->
  DVST.ServiceTierType ->
  Maybe Text ->
  HighPrecMoney ->
  Maybe Bool ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Text ->
  [DAC.ConditionalCharges] ->
  Bool ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  m TripQuoteDetail
buildTripQuoteDetail searchReq tripCategory vehicleServiceTier mbVehicleServiceTierName baseFare isDashboardRequest mbDriverMinFee mbDriverMaxFee mbStepFee mbDefaultStepFee mDriverPickUpCharge mbDriverParkingCharge estimateOrQuoteId conditionalCharges eligibleForUpgrade congestionCharges petCharges = do
  vehicleServiceTierName <-
    case mbVehicleServiceTierName of
      Just name -> return name
      _ -> do
        item <- CQDVST.findByServiceTierTypeAndCityIdInRideFlow vehicleServiceTier searchReq.merchantOperatingCityId searchReq.configInExperimentVersions >>= fromMaybeM (VehicleServiceTierNotFound $ show vehicleServiceTier)
        return item.name
  (driverParkingCharge, driverPickUpCharge, driverMinFee, driverMaxFee, driverStepFee, driverDefaultStepFee) <-
    case (mbDriverParkingCharge, mDriverPickUpCharge, mbDriverMinFee, mbDriverMaxFee, mbStepFee, mbDefaultStepFee) of
      (Just parkingCharge, Just charge, Just minFee, Just maxFee, Just stepFee, Just defaultStepFee) -> return (Just parkingCharge, Just charge, Just minFee, Just maxFee, Just stepFee, Just defaultStepFee)
      _ -> do
        farePolicy <- getFarePolicyByEstOrQuoteId (Just $ getCoordinates searchReq.fromLocation) searchReq.fromLocGeohash searchReq.toLocGeohash searchReq.estimatedDistance searchReq.estimatedDuration searchReq.merchantOperatingCityId tripCategory vehicleServiceTier searchReq.area estimateOrQuoteId Nothing isDashboardRequest searchReq.dynamicPricingLogicVersion (Just (TransactionId (Id searchReq.transactionId))) searchReq.configInExperimentVersions
        let mbDriverExtraFeeBounds = DFP.findDriverExtraFeeBoundsByDistance (fromMaybe 0 searchReq.estimatedDistance) <$> farePolicy.driverExtraFeeBounds
        return $
          ( farePolicy.parkingCharge,
            USRD.extractDriverPickupCharges farePolicy.farePolicyDetails,
            mbDriverExtraFeeBounds <&> (.minFee),
            mbDriverExtraFeeBounds <&> (.maxFee),
            mbDriverExtraFeeBounds <&> (.stepFee),
            mbDriverExtraFeeBounds <&> (.defaultStepFee)
          )
  return $ TripQuoteDetail {..}
