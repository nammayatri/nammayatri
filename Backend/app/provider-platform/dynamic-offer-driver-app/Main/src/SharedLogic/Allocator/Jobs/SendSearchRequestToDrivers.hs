{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import Domain.Types as DTC
import Domain.Types.Booking (BookingStatus (..))
import qualified Domain.Types.ConditionalCharges as DAC
import Domain.Types.DriverPoolConfig
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.GoHomeConfig (GoHomeConfig)
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SearchTry (SearchTry)
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude hiding (handle)
import Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto as Esq
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle (Handle (..), MetricsHandle (..), handler)
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal as I
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config as DriverPoolConfig
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPoolUnified as UI
import qualified SharedLogic.Booking as SBooking
import SharedLogic.CallBAPInternal
import qualified SharedLogic.CallInternalMLPricing as ML
import SharedLogic.DriverPool hiding (getDriverPoolConfig)
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified SharedLogic.SearchTry as SST
import qualified SharedLogic.Type as SLT
import Storage.Cac.DriverPoolConfig (getDriverPoolConfig)
import qualified Storage.Cac.GoHomeConfig as CGHC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import qualified Tools.Metrics as Metrics
import Tools.Utils
import TransactionLogs.Types
import Utils.Common.Cac.KeyNameConstants

sendSearchRequestToDrivers ::
  ( EncFlow m r,
    TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Log m,
    MonadFlow m,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasShortDurationRetryCfg r c,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal],
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'SendSearchRequestToDriver ->
  m ExecutionResult
sendSearchRequestToDrivers Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
  let searchTryId = jobData.searchTryId
  searchTry <- B.runInReplica $ QST.findById searchTryId >>= fromMaybeM (SearchTryNotFound searchTryId.getId)
  searchReq <- B.runInReplica $ QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
  merchant <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound (searchReq.providerId.getId))
  driverPoolConfig <- getDriverPoolConfig searchReq.merchantOperatingCityId searchTry.vehicleServiceTier searchTry.tripCategory (fromMaybe SL.Default searchReq.area) jobData.estimatedRideDistance searchTry.searchRepeatType searchTry.searchRepeatCounter (Just (TransactionId (Id searchReq.transactionId))) searchReq
  goHomeCfg <- CGHC.findByMerchantOpCityId searchReq.merchantOperatingCityId (Just (TransactionId (Id searchReq.transactionId)))
  tripQuoteDetailsWithoutUpgrades <- do
    let estimateIds = if length searchTry.estimateIds == 0 then [searchTry.estimateId] else searchTry.estimateIds
    estimateIds `forM` \estimateId -> do
      if DTC.isDynamicOfferTrip searchTry.tripCategory
        then do
          estimate <- B.runInReplica $ QEst.findById (Id estimateId) >>= fromMaybeM (EstimateNotFound estimateId)
          buildEstimateTripQuoteDetails searchTry searchReq estimate
        else do
          quote <- B.runInReplica $ QQuote.findById (Id estimateId) >>= fromMaybeM (QuoteNotFound estimateId)
          let mbDriverExtraFeeBounds = ((,) <$> searchReq.estimatedDistance <*> (join $ (.driverExtraFeeBounds) <$> quote.farePolicy)) <&> \(dist, driverExtraFeeBounds) -> DFP.findDriverExtraFeeBoundsByDistance dist driverExtraFeeBounds
              driverPickUpCharge = join $ USRD.extractDriverPickupCharges <$> ((.farePolicyDetails) <$> quote.farePolicy)
              driverParkingCharge = join $ (.parkingCharge) <$> quote.farePolicy
              businessDiscount = if searchTry.billingCategory == SLT.BUSINESS then fromMaybe 0.0 quote.fareParams.businessDiscount else 0.0
              personalDiscount = if searchTry.billingCategory == SLT.PERSONAL then fromMaybe 0.0 quote.fareParams.personalDiscount else 0.0
          SST.buildTripQuoteDetail searchReq quote.tripCategory quote.vehicleServiceTier quote.vehicleServiceTierName (quote.estimatedFare + fromMaybe 0 searchTry.customerExtraFee + fromMaybe 0 searchTry.petCharges - businessDiscount - personalDiscount) Nothing (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge driverParkingCharge quote.id.getId [] False quote.fareParams.congestionCharge searchTry.petCharges quote.fareParams.priorityCharges Nothing

  tripQuoteDetails <-
    case tripQuoteDetailsWithoutUpgrades of
      [tripQuoteDetail] ->
        -- allow upgrade for one way regular rides only if Auto is selected and rider is eligible for upgrade
        case (tripQuoteDetail.tripCategory, tripQuoteDetail.vehicleServiceTier, isRiderEligibleForCabUpgrade searchReq) of
          (OneWay OneWayOnDemandDynamicOffer, AUTO_RICKSHAW, True) -> do
            upgradeEstimates <- QEst.findEligibleForCabUpgrade searchReq.id True
            upgradeTripQuoteDetails <- upgradeEstimates `forM` buildEstimateTripQuoteDetails searchTry searchReq
            return $ tripQuoteDetailsWithoutUpgrades <> upgradeTripQuoteDetails
          _ -> return tripQuoteDetailsWithoutUpgrades
      _ -> return tripQuoteDetailsWithoutUpgrades

  let driverSearchBatchInput =
        DriverSearchBatchInput
          { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
            merchant,
            searchReq,
            tripQuoteDetails,
            customerExtraFee = searchTry.customerExtraFee,
            messageId = searchTry.messageId,
            isRepeatSearch = False,
            isAllocatorBatch = True,
            billingCategory = searchTry.billingCategory,
            paymentMethodInfo = Nothing
          }
  (res, _, _) <- sendSearchRequestToDrivers' driverPoolConfig searchTry driverSearchBatchInput goHomeCfg
  return res
  where
    buildEstimateTripQuoteDetails ::
      ( CacheFlow m r,
        EsqDBFlow m r,
        EsqDBReplicaFlow m r,
        HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
        HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
      ) =>
      SearchTry ->
      DSR.SearchRequest ->
      DEst.Estimate ->
      m TripQuoteDetail
    buildEstimateTripQuoteDetails searchTry searchReq estimate = do
      let mbDriverExtraFeeBounds = if isJust estimate.driverExtraFeeBounds then estimate.driverExtraFeeBounds else ((,) <$> estimate.estimatedDistance <*> (join $ (.driverExtraFeeBounds) <$> estimate.farePolicy)) <&> \(dist, driverExtraFeeBounds) -> DFP.findDriverExtraFeeBoundsByDistance dist driverExtraFeeBounds
          driverPickUpCharge = join $ USRD.extractDriverPickupCharges <$> ((.farePolicyDetails) <$> estimate.farePolicy)
          driverParkingCharge = join $ (.parkingCharge) <$> estimate.farePolicy
          driverAdditionalCharges = filterChargesByApplicability (fromMaybe [] $ (.conditionalCharges) <$> estimate.farePolicy) searchReq
          businessDiscount = if searchTry.billingCategory == SLT.BUSINESS then fromMaybe 0.0 estimate.businessDiscount else 0.0
          personalDiscount = if searchTry.billingCategory == SLT.PERSONAL then fromMaybe 0.0 estimate.personalDiscount else 0.0
      SST.buildTripQuoteDetail searchReq estimate.tripCategory estimate.vehicleServiceTier estimate.vehicleServiceTierName (estimate.minFare + fromMaybe 0 searchTry.customerExtraFee + fromMaybe 0 searchTry.petCharges - businessDiscount - personalDiscount) Nothing (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge driverParkingCharge estimate.id.getId driverAdditionalCharges estimate.eligibleForUpgrade ((.congestionCharge) =<< estimate.fareParams) searchTry.petCharges (estimate.fareParams >>= (.priorityCharges)) estimate.commissionCharges
    filterChargesByApplicability conditionalCharges sReq = do
      let safetyCharges = if sReq.preferSafetyPlus then find (\ac -> (ac.chargeCategory) == DAC.SAFETY_PLUS_CHARGES) conditionalCharges else Nothing
          nyregularCharges = if fromMaybe False sReq.isReserveRide then find (\ac -> (ac.chargeCategory) == DAC.NYREGULAR_SUBSCRIPTION_CHARGE) conditionalCharges else Nothing
      catMaybes $ [safetyCharges, nyregularCharges]

sendSearchRequestToDrivers' ::
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
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasShortDurationRetryCfg r c,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal],
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
    HasField "blackListedJobs" r [Text]
  ) =>
  DriverPoolConfig ->
  SearchTry ->
  DriverSearchBatchInput m ->
  GoHomeConfig ->
  m (ExecutionResult, PoolType, Maybe Seconds)
sendSearchRequestToDrivers' driverPoolConfig searchTry driverSearchBatchInput goHomeCfg = do
  -- In case of static offer flow we will have booking created before driver ride request is sent
  mbBooking <- if DTC.isDynamicOfferTrip searchTry.tripCategory then pure Nothing else QRB.findByQuoteId searchTry.estimateId
  handler (handle mbBooking) goHomeCfg driverSearchBatchInput.searchReq.transactionId
  where
    handle mbBooking =
      Handle
        { isBatchNumExceedLimit = I.isBatchNumExceedLimit driverPoolConfig searchTry.id,
          isReceivedMaxDriverQuotes = I.isReceivedMaxDriverQuotes driverPoolConfig searchTry.id,
          getNextDriverPoolBatch = (if driverPoolConfig.poolSortingType == DriverPoolConfig.Tagged && driverPoolConfig.enableUnifiedPooling == Just True then UI.getNextDriverPoolBatch else I.getNextDriverPoolBatch) driverPoolConfig driverSearchBatchInput.searchReq searchTry driverSearchBatchInput.tripQuoteDetails driverSearchBatchInput.paymentMethodInfo,
          sendSearchRequestToDrivers = I.sendSearchRequestToDrivers driverSearchBatchInput.isAllocatorBatch driverSearchBatchInput.tripQuoteDetails driverSearchBatchInput.searchReq searchTry driverPoolConfig,
          getRescheduleTime = I.getRescheduleTime driverPoolConfig.singleBatchProcessTime,
          metrics =
            MetricsHandle
              { incrementTaskCounter = Metrics.incrementTaskCounter driverSearchBatchInput.merchant.name,
                incrementFailedTaskCounter = Metrics.incrementFailedTaskCounter driverSearchBatchInput.merchant.name,
                putTaskDuration = Metrics.putTaskDuration driverSearchBatchInput.merchant.name
              },
          isSearchTryValid = I.isSearchTryValid searchTry.id,
          initiateDriverSearchBatch = SST.initiateDriverSearchBatch driverSearchBatchInput,
          isScheduledBooking = searchTry.isScheduled,
          cancelSearchTry = I.cancelSearchTry searchTry.id,
          isBookingValid = do
            case mbBooking of
              Just booking -> booking.status `notElem` [COMPLETED, CANCELLED]
              Nothing -> True,
          cancelBookingIfApplies = do
            whenJust mbBooking $ \booking -> do
              SBooking.cancelBooking booking Nothing driverSearchBatchInput.merchant
        }
