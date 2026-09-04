{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.SearchTry where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import Domain.Types.ConditionalCharges as DAC
import Domain.Types.DriverPoolConfig
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.TransporterConfig as DTTC
import qualified EulerHS.Language as L
import Kernel.Beam.Types (TxnIdKey (..))
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.Allocator
import qualified SharedLogic.Booking as SBooking
import SharedLogic.DriverPool.Types
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FarePolicy
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified SharedLogic.MetricsLabels as SML
import SharedLogic.Pricing
import qualified SharedLogic.Type as SLT
import Storage.Cac.DriverPoolConfig (getDriverPoolConfig)
import qualified Storage.CachedQueries.VehicleServiceTier as CQDVST
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Storage.ConfigPilot.Config.GoHomeConfig (GoHomeConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
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
    Metrics.HasBPPMetrics m r,
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
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
    HasShortDurationRetryCfg r c,
    HasField "blackListedJobs" r [Text],
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r,
    Redis.HedisLTSFlowEnv r,
    Finance.HasActorInfo m r
  ) =>
  DriverSearchBatchInput m ->
  m DST.SearchTry
initiateDriverSearchBatch searchBatchInput@DriverSearchBatchInput {..} = do
  L.setOptionLocal TxnIdKey searchReq.transactionId
  searchTry <- createNewSearchTry
  withTryCatch
    "initiateDriverSearchBatch"
    ( do
        driverPoolConfig <- getDriverPoolConfig searchReq.merchantOperatingCityId searchTry.vehicleServiceTier searchTry.tripCategory (fromMaybe SL.Default searchReq.area) searchReq.estimatedDistance searchTry.searchRepeatType searchTry.searchRepeatCounter (Just (TransactionId (Id searchReq.transactionId))) searchReq
        goHomeCfg <- getConfig (GoHomeConfigDimensions {merchantOperatingCityId = searchReq.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (InvalidRequest $ "GoHome Config not found for MerchantOperatingCity: " <> searchReq.merchantOperatingCityId.getId)
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
        return searchTry
      Right _ -> return searchTry
  where
    scheduleBatching searchTry inTime = do
      let jobData =
            SendSearchRequestToDriverJobData
              { searchTryId = searchTry.id,
                estimatedRideDistance = searchReq.estimatedDistance,
                batchEpoch = Nothing -- start of the chain; early advances bump it from here
              }
      if searchTry.isScheduled
        then JC.createJobIn @_ @'SendScheduledSearchRequestToDriver (Just searchReq.providerId) (Just searchReq.merchantOperatingCityId) inTime jobData
        else JC.createJobIn @_ @'SendSearchRequestToDriver (Just searchReq.providerId) (Just searchReq.merchantOperatingCityId) inTime jobData

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
          -- Read once here and thread it down: buildSearchTry needs it, and so does the
          -- search-try counter's distance bucket, and both run on every search try.
          transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = searchReq.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound searchReq.merchantOperatingCityId.getId)
          searchTry <- case mbLastSearchTry of
            Nothing -> do
              searchTry <- buildSearchTry merchant.id searchReq estimateOrQuoteIds estOrQuoteId estimatedFare 0 DST.INITIAL tripCategory billingCategory customerExtraFee firstQuoteDetail.petCharges messageId estimateOrQuoteServiceTierNames serviceTier emailDomain searchBatchInput.businessEmailDomain driverPreference ((.paymentInstrument) <$> paymentMethodInfo) transporterConfig
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
              searchTry <- buildSearchTry merchant.id searchReq estimateOrQuoteIds estOrQuoteId estimatedFare (oldSearchTry.searchRepeatCounter + 1) searchRepeatType tripCategory billingCategory customerExtraFee firstQuoteDetail.petCharges messageId estimateOrQuoteServiceTierNames serviceTier emailDomain searchBatchInput.businessEmailDomain driverPreference ((.paymentInstrument) <$> paymentMethodInfo) transporterConfig
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
          cityLabel <- SML.getCityLabel searchReq.merchantOperatingCityId
          let (pickupZone, dropZone) = SML.specialZoneLabels searchReq.area
          Metrics.incrementSearchTryCount merchant.shortId.getShortId cityLabel (show searchTry.vehicleServiceTier) (show searchTry.searchRepeatType) (SML.distanceBucketLabel (SML.distanceBucketEdges transporterConfig) searchReq.estimatedDistance) pickupZone dropZone
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
  SLT.BillingCategory ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Text ->
  [Text] ->
  DVST.ServiceTierType ->
  Maybe Text ->
  Maybe Text ->
  Maybe [Text] ->
  Maybe DMPM.PaymentInstrument ->
  DTTC.TransporterConfig ->
  m DST.SearchTry
buildSearchTry merchantId searchReq estimateOrQuoteIds estOrQuoteId baseFare searchRepeatCounter searchRepeatType tripCategory billingCategory customerExtraFee petCharges messageId estimateOrQuoteServTierNames serviceTier emailDomain businessEmailDomain driverPreference mbPaymentInstrument transporterConfig = do
  now <- getCurrentTime
  id_ <- Id <$> generateGUID
  vehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityIdInRideFlow serviceTier searchReq.merchantOperatingCityId (searchReq.area >>= SL.pickupSpecialZoneIdFromArea) >>= fromMaybeM (VehicleServiceTierNotFound (show serviceTier))
  if tripCategory == DTC.OneWay DTC.OneWayOnDemandDynamicOffer && transporterConfig.isDynamicPricingQARCalEnabled == Just True
    then
      fork "updateDynamicPricingDemandCounters" $
        geoAddDynamicPricingCounter mkDemandVehicleCategoryWithDistanceBin mkDemandVehicleCategory mkDemandVehicleCategoryCity now vehicleServiceTierItem.vehicleCategory searchReq.fromLocation.lat searchReq.fromLocation.lon id_.getId ((.getMeters) <$> searchReq.estimatedDistance) searchReq.merchantOperatingCityId.getId
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
        driverPreference = driverPreference,
        businessEmailDomain = businessEmailDomain,
        paymentInstrument = mbPaymentInstrument,
        ..
      }

buildTripQuoteDetail ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r
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
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe Bool ->
  m TripQuoteDetail
buildTripQuoteDetail searchReq tripCategory vehicleServiceTier mbVehicleServiceTierName baseFare isDashboardRequest mbDriverMinFee mbDriverMaxFee mbStepFee mbDefaultStepFee mDriverPickUpCharge mbDriverParkingCharge estimateOrQuoteId conditionalCharges eligibleForUpgrade congestionCharges petCharges priorityCharges commissionCharges mbTollCharges govtCharges mbDriverCancellationNotAllowed = do
  vehicleServiceTierName <-
    case mbVehicleServiceTierName of
      Just name -> return name
      _ -> do
        item <- CQDVST.findByServiceTierTypeAndCityIdInRideFlow vehicleServiceTier searchReq.merchantOperatingCityId (searchReq.area >>= SL.pickupSpecialZoneIdFromArea) >>= fromMaybeM (VehicleServiceTierNotFound $ show vehicleServiceTier)
        return item.name
  (driverParkingCharge, tollCharges, driverPickUpCharge, driverMinFee, driverMaxFee, driverStepFee, driverDefaultStepFee, driverCancellationNotAllowed) <-
    case (mbDriverParkingCharge, mbTollCharges, mDriverPickUpCharge, mbDriverMinFee, mbDriverMaxFee, mbStepFee, mbDefaultStepFee) of
      (Just parkingCharge, Just tollCharges', Just charge, Just minFee, Just maxFee, Just stepFee, Just defaultStepFee) ->
        return (Just parkingCharge, Just tollCharges', Just charge, Just minFee, Just maxFee, Just stepFee, Just defaultStepFee, mbDriverCancellationNotAllowed)
      _ -> do
        farePolicy <- getFarePolicyByEstOrQuoteId (Just $ getCoordinates searchReq.fromLocation) (Just . getCoordinates =<< searchReq.toLocation) searchReq.fromLocGeohash searchReq.toLocGeohash searchReq.estimatedDistance searchReq.estimatedDuration searchReq.merchantOperatingCityId tripCategory vehicleServiceTier searchReq.area estimateOrQuoteId Nothing isDashboardRequest searchReq.dynamicPricingLogicVersion (Just (TransactionId (Id searchReq.transactionId))) searchReq.configInExperimentVersions searchReq.specialLocationName
        let mbDriverExtraFeeBounds = DFP.findDriverExtraFeeBoundsByDistance (fromMaybe 0 searchReq.estimatedDistance) <$> farePolicy.driverExtraFeeBounds
            -- Parking already EDC-collected at the booth for these settlement types isn't the
            -- driver's cash to hold - don't factor it into the driver's cash-wallet eligibility check.
            edcCollectsParking = SL.edcCollectsParking farePolicy.fareSettlementType
            driverFacingParkingCharge = if edcCollectsParking then Nothing else farePolicy.parkingCharge
        return $
          ( driverFacingParkingCharge,
            farePolicy.tollCharges,
            USRD.extractDriverPickupCharges farePolicy.farePolicyDetails,
            mbDriverExtraFeeBounds <&> (.minFee),
            mbDriverExtraFeeBounds <&> (.maxFee),
            mbDriverExtraFeeBounds <&> (.stepFee),
            mbDriverExtraFeeBounds <&> (.defaultStepFee),
            mbDriverCancellationNotAllowed <|> farePolicy.driverCancellationNotAllowed
          )
  return $ TripQuoteDetail {..}
