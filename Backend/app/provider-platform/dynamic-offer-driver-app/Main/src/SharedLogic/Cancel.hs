{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Cancel where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import qualified Domain.Types as DTC
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.ConditionalCharges as DCC
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DMerc
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import Domain.Types.TransporterConfig (TransporterConfig)
import qualified Domain.Types.Vehicle as DVeh
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer, KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics (DeploymentVersion)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler (SchedulerType)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import SharedLogic.Booking
import qualified SharedLogic.CallBAP as BP
import SharedLogic.CallBAPInternal
import qualified SharedLogic.CallInternalMLPricing as ML
import SharedLogic.DriverPool
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.DriverPool.Types as SDT
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FarePolicy
import SharedLogic.GoogleTranslate (TranslateFlow)
import SharedLogic.MerchantPaymentMethod
import SharedLogic.Ride (multipleRouteKey, searchRequestKey)
import SharedLogic.SearchTry
import qualified SharedLogic.Type as SLT
import qualified Storage.Cac.TransporterConfig as QTC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as QMPM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import qualified Tools.Metrics as Metrics
import TransactionLogs.Types
import Utils.Common.Cac.KeyNameConstants

reAllocateBookingIfPossible ::
  ( MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime,
    HasField "version" r DeploymentVersion,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    TranslateFlow m r,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasShortDurationRetryCfg r c,
    Redis.HedisFlow m r,
    HasKafkaProducer r,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal],
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal]
  ) =>
  Bool ->
  Bool ->
  DMerc.Merchant ->
  SRB.Booking ->
  DRide.Ride ->
  DP.Person ->
  DVeh.Vehicle ->
  SBCR.BookingCancellationReason ->
  Bool ->
  m Bool
reAllocateBookingIfPossible isValueAddNP userReallocationEnabled merchant booking ride driver vehicle bookingCReason isForceReallocation = do
  case booking.tripCategory of
    DTC.OneWay DTC.OneWayOnDemandDynamicOffer -> reallocateDynamicOffer
    DTC.Ambulance DTC.OneWayOnDemandDynamicOffer -> reallocateDynamicOffer
    DTC.Rental DTC.OnDemandStaticOffer -> reallocateStaticOffer
    DTC.InterCity DTC.OneWayOnDemandStaticOffer _ -> reallocateStaticOffer
    DTC.Delivery DTC.OneWayOnDemandDynamicOffer -> reallocateDynamicOffer
    DTC.OneWay DTC.OneWayOnDemandStaticOffer -> reallocateStaticOffer
    _ -> cancelRideTransactionForNonReallocation Nothing Nothing
  where
    reallocateDynamicOffer = do
      now <- getCurrentTime
      driverQuote <- QDQ.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
      searchTry <- QST.findById driverQuote.searchTryId >>= fromMaybeM (SearchTryNotFound driverQuote.searchTryId.getId)
      searchReq <- QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
      transporterConfig <- QTC.findByMerchantOpCityId booking.merchantOperatingCityId (Just (TransactionId $ Id booking.transactionId)) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
      isRepeatSearch <- checkIfRepeatSearch searchTry ride.driverArrivalTime searchReq.isReallocationEnabled now booking.isScheduled transporterConfig
      if isRepeatSearch
        then performDynamicOfferReallocation driverQuote searchReq searchTry
        else cancelRideTransactionForNonReallocation Nothing (Just searchTry.estimateId)

    reallocateStaticOffer = do
      now <- getCurrentTime
      quote <- QQuote.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
      searchReq <- QSR.findById quote.searchRequestId >>= fromMaybeM (SearchRequestNotFound quote.searchRequestId.getId)
      searchTry <- QST.findLastByRequestId quote.searchRequestId >>= fromMaybeM (SearchTryNotFound quote.searchRequestId.getId)
      transporterConfig <- QTC.findByMerchantOpCityId booking.merchantOperatingCityId (Just (TransactionId $ Id booking.transactionId)) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
      isRepeatSearch <- checkIfRepeatSearch searchTry ride.driverArrivalTime searchReq.isReallocationEnabled now searchReq.isScheduled transporterConfig
      if isRepeatSearch || isForceReallocation
        then performStaticOfferReallocation quote searchReq searchTry transporterConfig now isRepeatSearch
        else cancelRideTransactionForNonReallocation Nothing Nothing

    performDynamicOfferReallocation driverQuote searchReq searchTry = do
      DP.addDriverToSearchCancelledList searchReq.id ride.driverId
      let conditionalCharges = driverQuote.fareParams.conditionalCharges
      tripQuoteDetails <- createTripQuoteDetails searchReq searchTry driverQuote.estimateId conditionalCharges
      merchantPaymentMethod <- maybe (return Nothing) QMPM.findById booking.paymentMethodId
      let paymentMethodInfo = mkPaymentMethodInfo <$> merchantPaymentMethod
      let driverSearchBatchInput =
            DriverSearchBatchInput
              { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
                merchant,
                searchReq,
                tripQuoteDetails = tripQuoteDetails,
                customerExtraFee = searchTry.customerExtraFee,
                messageId = booking.id.getId,
                isRepeatSearch = True,
                isAllocatorBatch = False,
                billingCategory = searchTry.billingCategory,
                paymentMethodInfo = paymentMethodInfo
              }
      handleDriverSearchBatch driverSearchBatchInput booking searchTry.estimateId False

    performStaticOfferReallocation quote searchReq searchTry transporterConfig now isRepeatSearch = do
      DP.addDriverToSearchCancelledList searchReq.id ride.driverId
      (newBooking, newQuote) <- createNewBookingAndQuote quote transporterConfig now searchReq
      let mbDriverExtraFeeBounds = ((,) <$> searchReq.estimatedDistance <*> ((.driverExtraFeeBounds) =<< (quote.farePolicy))) <&> uncurry DFP.findDriverExtraFeeBoundsByDistance
          driverPickUpCharge = USRD.extractDriverPickupCharges . (.farePolicyDetails) =<< (quote.farePolicy)
          driverParkingCharge = (.parkingCharge) =<< (quote.farePolicy)
      tripQuoteDetail <- buildTripQuoteDetail searchReq booking.tripCategory booking.vehicleServiceTier quote.vehicleServiceTierName booking.estimatedFare (Just booking.isDashboardRequest) (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge driverParkingCharge newQuote.id.getId [] False booking.fareParams.congestionCharge booking.fareParams.petCharges booking.fareParams.priorityCharges
      void $ clearCachedFarePolicyByEstOrQuoteId booking.quoteId
      QQuote.create newQuote
      QRB.createBooking newBooking
      when newBooking.isScheduled $ void $ addScheduledBookingInRedis newBooking
      merchantPaymentMethod <- maybe (return Nothing) QMPM.findById booking.paymentMethodId
      let paymentMethodInfo = mkPaymentMethodInfo <$> merchantPaymentMethod
      let driverSearchBatchInput =
            DriverSearchBatchInput
              { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
                merchant,
                searchReq,
                tripQuoteDetails = [tripQuoteDetail],
                customerExtraFee = searchTry.customerExtraFee,
                messageId = booking.id.getId,
                isRepeatSearch,
                isAllocatorBatch = False,
                billingCategory = searchTry.billingCategory,
                paymentMethodInfo = paymentMethodInfo
              }
      handleDriverSearchBatch driverSearchBatchInput newBooking searchTry.estimateId True

    handleDriverSearchBatch driverSearchBatchInput newBooking estimateId isStatic = do
      result <- withTryCatch "initiateDriverSearchBatch:handleDriverSearchBatch" (initiateDriverSearchBatch driverSearchBatchInput)
      case result of
        Right _ ->
          if isValueAddNP
            then do
              if isStatic then BP.sendQuoteRepetitionUpdateToBAP booking ride newBooking.id bookingCReason.source driver vehicle else BP.sendEstimateRepetitionUpdateToBAP booking ride (Id estimateId) bookingCReason.source driver vehicle
              return True
            else cancelRideTransactionForNonReallocation Nothing (Just estimateId)
        Left _ -> cancelRideTransactionForNonReallocation Nothing (Just estimateId)

    createTripQuoteDetails ::
      ( MonadFlow m,
        CacheFlow m r,
        EsqDBFlow m r,
        EsqDBReplicaFlow m r,
        HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
        HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
      ) =>
      DSR.SearchRequest ->
      DST.SearchTry ->
      Id DEst.Estimate ->
      [DCC.ConditionalCharges] ->
      m [SDT.TripQuoteDetail]
    createTripQuoteDetails searchReq searchTry estimateId conditionalCharges = do
      if length searchTry.estimateIds > 1
        then traverse (createQuoteDetails searchReq searchTry conditionalCharges) searchTry.estimateIds
        else do
          quoteDetail <- createQuoteDetails searchReq searchTry conditionalCharges estimateId.getId
          return [quoteDetail]

    createNewBookingAndQuote quote transporterConfig now searchReq = do
      bookingId <- generateGUID
      quoteId <- generateGUID
      fareParamsId <- generateGUID
      searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
      let newIsScheduled = booking.isScheduled && transporterConfig.scheduleRideBufferTime `addUTCTime` now < searchReq.startTime
          newFareParams = quote.fareParams{id = fareParamsId, updatedAt = now}
          newQuote = quote{id = Id quoteId, fareParams = newFareParams, validTill = searchRequestExpirationSeconds `addUTCTime` now, isScheduled = booking.isScheduled}
          newBooking = booking{id = bookingId, quoteId = quoteId, status = SRB.NEW, isScheduled = newIsScheduled, startTime = max now booking.startTime, createdAt = now, updatedAt = now}
      return (newBooking, newQuote)

    createQuoteDetails ::
      ( MonadFlow m,
        CacheFlow m r,
        EsqDBFlow m r,
        EsqDBReplicaFlow m r,
        HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
        HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
      ) =>
      DSR.SearchRequest ->
      DST.SearchTry ->
      [DCC.ConditionalCharges] ->
      Text ->
      m SDT.TripQuoteDetail
    createQuoteDetails searchReq searchTry conditionalCharges estimateId = do
      estimate <- QEst.findById (Id estimateId) >>= fromMaybeM (EstimateNotFound estimateId)
      let mbDriverExtraFeeBounds = if isJust estimate.driverExtraFeeBounds then estimate.driverExtraFeeBounds else ((,) <$> estimate.estimatedDistance <*> (join $ (.driverExtraFeeBounds) <$> estimate.farePolicy)) <&> \(dist, driverExtraFeeBounds) -> DFP.findDriverExtraFeeBoundsByDistance dist driverExtraFeeBounds
          driverPickUpCharge = join $ USRD.extractDriverPickupCharges <$> ((.farePolicyDetails) <$> estimate.farePolicy)
          driverParkingCharge = join $ (.parkingCharge) <$> estimate.farePolicy
          businessDiscount = if searchTry.billingCategory == SLT.BUSINESS then fromMaybe 0.0 estimate.businessDiscount else 0.0
      buildTripQuoteDetail searchReq estimate.tripCategory estimate.vehicleServiceTier estimate.vehicleServiceTierName (estimate.minFare + fromMaybe 0 searchTry.customerExtraFee + fromMaybe 0 searchTry.petCharges - businessDiscount) (Just booking.isDashboardRequest) (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge driverParkingCharge estimate.id.getId conditionalCharges False ((.congestionCharge) =<< estimate.fareParams) searchTry.petCharges (estimate.fareParams >>= (.priorityCharges))
    cancelRideTransactionForNonReallocation ::
      ( MonadFlow m,
        Redis.HedisFlow m r,
        EsqDBReplicaFlow m r,
        CacheFlow m r,
        EsqDBFlow m r
      ) =>
      Maybe SRB.Booking ->
      Maybe Text ->
      m Bool
    cancelRideTransactionForNonReallocation mbNewBooking mbEstimateId = do
      Redis.del $ multipleRouteKey booking.transactionId
      Redis.del $ searchRequestKey booking.transactionId
      whenJust mbEstimateId $ \estimateId ->
        void $ clearCachedFarePolicyByEstOrQuoteId estimateId
      whenJust mbNewBooking $ \newBooking -> do
        bookingCancellationReason <- buildBookingCancellationReason newBooking
        QBCR.upsert bookingCancellationReason
        QRB.updateStatus newBooking.id SRB.CANCELLED
      void $ clearCachedFarePolicyByEstOrQuoteId booking.quoteId -- shouldn't be required for new booking
      return False
    checkIfRepeatSearch :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DST.SearchTry -> Maybe UTCTime -> Maybe Bool -> UTCTime -> Bool -> TransporterConfig -> m Bool
    checkIfRepeatSearch searchTry driverArrivalTime isReallocationEnabled now isScheduled transporterConfig = do
      let searchRepeatLimit = if isScheduled then transporterConfig.scheduledRideSearchRepeatLimit else transporterConfig.searchRepeatLimit
          isSearchTryValid = searchTry.validTill > now
          arrivedPickupThreshold = highPrecMetersToMeters transporterConfig.arrivedPickupThreshold
          driverHasNotArrived = isNothing driverArrivalTime || maybe True (> arrivedPickupThreshold) bookingCReason.driverDistToPickup
      return $
        searchTry.searchRepeatCounter < searchRepeatLimit
          && (bookingCReason.source == SBCR.ByDriver || (bookingCReason.source == SBCR.ByUser && userReallocationEnabled))
          && (isSearchTryValid || isScheduled)
          && fromMaybe False isReallocationEnabled
          && driverHasNotArrived

    buildBookingCancellationReason newBooking = do
      return $
        SBCR.BookingCancellationReason
          { bookingId = newBooking.id,
            rideId = Nothing,
            merchantId = Just newBooking.providerId,
            source = SBCR.ByApplication,
            reasonCode = Nothing,
            driverId = Nothing,
            additionalInfo = Just "Reallocation Failed",
            driverCancellationLocation = Nothing,
            driverDistToPickup = Nothing,
            distanceUnit = newBooking.distanceUnit,
            merchantOperatingCityId = Just newBooking.merchantOperatingCityId,
            ..
          }

mkCancelSearchInitLockKey :: Text -> Text
mkCancelSearchInitLockKey transactionId = "cancelSearchInit:" <> transactionId
