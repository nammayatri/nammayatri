module SharedLogic.FRFSStatus where

import API.Types.UI.FRFSTicketService
import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Control.Monad.Extra hiding (fromMaybeM)
import qualified Data.Text as Text
import qualified Domain.Action.Beckn.FRFS.Common as FRFSCommon
import qualified Domain.Action.Beckn.FRFS.OnConfirm as DACFOC
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSQuoteCategory as FRFSQuoteCategory
import Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBooking
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.JourneyLeg as DJL
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person
import qualified Domain.Types.Person as DP
import EulerHS.Prelude hiding (all, and, any, concatMap, elem, find, foldr, forM_, fromList, groupBy, hoistMaybe, id, length, map, mapM_, maximum, null, readMaybe, toList, whenJust)
import qualified ExternalBPP.CallAPI as CallExternalBPP
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Payment.Interface
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude hiding (whenJust)
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.Common as Common
import Kernel.Types.Id
import Kernel.Utils.Common hiding (mkPrice)
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QPaymentTransaction
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.FRFSUtils
import SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.FRFSUtils as Utils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import SharedLogic.JobScheduler as JobScheduler
import SharedLogic.Offer as SOffer
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Metrics.BAPMetrics (HasBAPMetrics)
import qualified Tools.Payment as Payment
import qualified UrlShortner.Common as UrlShortner

-- pass isMultiModalBooking = True in case of multimodal flow
frfsBookingStatus ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    CallFRFSBPP.BecknAPICallFlow m r,
    HasFlowEnv m r '["googleSAPrivateKey" ::: String],
    HasBAPMetrics m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig]
  ) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) ->
  Bool ->
  (((DFRFSTicketBookingPayment.FRFSTicketBookingPayment, DPaymentOrder.PaymentOrder, Maybe DPayment.PaymentStatusResp) -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes) -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes) ->
  DFRFSTicketBooking.FRFSTicketBooking ->
  DP.Person ->
  (DJL.JourneyLeg -> Id DFRFSQuote.FRFSQuote -> m ()) ->
  m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
frfsBookingStatus (personId, merchantId_) isMultiModalBooking withPaymentStatusResponseHandler booking' person switchFRFSQuoteTier = do
  logInfo $ "frfsBookingStatus for booking: " <> show booking'
  let bookingId = booking'.id
  merchant <- CQM.findById merchantId_ >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback booking'.merchantOperatingCityId merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory booking'.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
  unless (personId == booking'.riderId) $ throwError AccessDenied
  now <- getCurrentTime
  let validTillWithBuffer = addUTCTime 5 booking'.validTill
  when (booking'.status /= DFRFSTicketBooking.CONFIRMED && booking'.status /= DFRFSTicketBooking.FAILED && booking'.status /= DFRFSTicketBooking.CANCELLED && validTillWithBuffer < now) $
    void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
  booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId Nothing Nothing booking.quoteId
  merchantOperatingCity <- getMerchantOperatingCityFromBooking booking
  let commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person person.id
  logInfo $ "Booking status: " <> show booking.status
  case booking.status of
    DFRFSTicketBooking.NEW -> buildFRFSTicketBookingStatusAPIRes booking quoteCategories Nothing
    DFRFSTicketBooking.FAILED -> do
      withPaymentStatusResponseHandler $ \(paymentBooking, paymentOrder, paymentStatusResp) -> do
        logInfo $ "payment status resp: " <> show paymentStatusResp
        let paymentBookingStatus = maybe FRFSTicketService.NEW makeTicketBookingPaymentAPIStatus (paymentStatusResp <&> (.status))
        logInfo $ "payment booking status: " <> show paymentBookingStatus
        when (paymentBookingStatus == FRFSTicketService.FAILURE) do
          void $ QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.FAILED paymentBooking.id
          let mPrice = Common.mkPrice (Just booking'.totalPrice.currency) (HighPrecMoney $ toRational (0 :: Int))
          void $ QFRFSRecon.updateTOrderValueAndSettlementAmountById mPrice mPrice booking.id
        when (paymentBookingStatus == FRFSTicketService.SUCCESS) do
          void $ markJourneyPaymentSuccess booking paymentOrder paymentBooking
        when (paymentBookingStatus == FRFSTicketService.PENDING) do
          void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.PAYMENT_PENDING bookingId
          void $ QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.PENDING paymentBooking.id
        let paymentStatusAPI =
              case paymentBooking.status of
                DFRFSTicketBookingPayment.REFUND_PENDING -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.REFUND_PENDING
                DFRFSTicketBookingPayment.REFUND_INITIATED -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.REFUND_INITIATED
                DFRFSTicketBookingPayment.REFUND_FAILED -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.REFUND_FAILED
                DFRFSTicketBookingPayment.REFUNDED -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.REFUNDED
                _ ->
                  case paymentBookingStatus of
                    FRFSTicketService.REFUNDED -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.REFUNDED
                    FRFSTicketService.FAILURE -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.FAILED
                    FRFSTicketService.SUCCESS -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.FAILED
                    FRFSTicketService.PENDING -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.PENDING
                    _ -> Nothing
        logInfo $ "payment status api: " <> show paymentStatusAPI
        let mbPaymentObj = paymentStatusAPI <&> \status -> FRFSTicketService.FRFSBookingPaymentAPI {status, paymentOrder = Nothing, transactionId = Nothing}
        buildFRFSTicketBookingStatusAPIRes booking quoteCategories mbPaymentObj
    DFRFSTicketBooking.CONFIRMING -> do
      if addUTCTime 5 booking.validTill < now
        then do
          logInfo $ "booking is expired in confirming: " <> show booking
          void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
          let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
          buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories paymentFailed
        else do
          buildFRFSTicketBookingStatusAPIRes booking quoteCategories paymentSuccess
    DFRFSTicketBooking.CONFIRMED -> do
      void $ addPaymentoffersTags booking'.totalPrice person
      fork "FRFS Booking Status" $ CallExternalBPP.status merchant.id merchantOperatingCity bapConfig booking
      buildFRFSTicketBookingStatusAPIRes booking quoteCategories paymentSuccess
    DFRFSTicketBooking.APPROVED -> do
      withPaymentStatusResponseHandler $ \(paymentBooking, paymentOrder, paymentStatusResp) -> do
        logInfo $ "payment status response: " <> show paymentStatusResp
        let paymentBookingStatus = maybe FRFSTicketService.NEW makeTicketBookingPaymentAPIStatus (paymentStatusResp <&> (.status))
        if paymentBookingStatus == FRFSTicketService.FAILURE
          then do
            logInfo $ "payment failed in approved: " <> show booking
            QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
            QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.FAILED paymentBooking.id
            let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
            buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories paymentFailed
          else
            if (paymentBookingStatus == FRFSTicketService.SUCCESS) && (booking.validTill < now)
              then do
                logInfo $ "booking is expired in approved: " <> show booking
                void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED booking.id
                let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
                buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories paymentFailed
              else do
                txn <- QPaymentTransaction.findNewTransactionByOrderId paymentOrder.id
                let paymentStatus_ = if isNothing txn then FRFSTicketService.NEW else paymentBookingStatus
                void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.PAYMENT_PENDING bookingId
                let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.PAYMENT_PENDING Nothing Nothing
                paymentOrder_ <- buildCreateOrderResp paymentOrder commonPersonId merchantOperatingCity.id booking
                let paymentObj =
                      Just $
                        FRFSTicketService.FRFSBookingPaymentAPI
                          { status = paymentStatus_,
                            paymentOrder = paymentOrder_,
                            transactionId = Nothing
                          }
                buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories paymentObj
    DFRFSTicketBooking.PAYMENT_PENDING -> do
      withPaymentStatusResponseHandler $ \(paymentBooking, paymentOrder, paymentStatusResp) -> do
        logInfo $ "paymentStatusResp: " <> show paymentStatusResp
        let paymentBookingStatus = maybe FRFSTicketService.NEW makeTicketBookingPaymentAPIStatus (paymentStatusResp <&> (.status))
        logInfo $ "paymentBookingStatus: " <> show paymentBookingStatus
        bookingApiResp <-
          if paymentBookingStatus == FRFSTicketService.FAILURE
            then do
              logInfo $ "payment failed in payment pending: " <> show booking <> ", status: " <> show paymentBookingStatus
              QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
              QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.FAILED paymentBooking.id
              let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
              buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories paymentFailed
            else
              if (paymentBookingStatus == FRFSTicketService.SUCCESS) && (booking.validTill < now)
                then do
                  logInfo $ "booking is expired in payment success and booking is expired: " <> show booking
                  void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED booking.id
                  let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
                  void $ markJourneyPaymentSuccess updatedBooking paymentOrder paymentBooking
                  buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories paymentFailed
                else do
                  (mbJourneyId, _) <- getAllJourneyFrfsBookings booking
                  if paymentBookingStatus == FRFSTicketService.SUCCESS && (not isMultiModalBooking || isJust mbJourneyId)
                    then do
                      -- Add default TTL of 1 min or the value provided in the config
                      let updatedTTL = addUTCTime (maybe 60 intToNominalDiffTime bapConfig.confirmTTLSec) now
                      transactions <- QPaymentTransaction.findAllByOrderId paymentOrder.id
                      txnId <- getSuccessTransactionId transactions
                      isLockAcquired <- Hedis.tryLockRedis (mkPaymentSuccessLockKey bookingId) 60
                      if isLockAcquired
                        then do
                          latestBookingPayment <- QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id
                          let isPaymentBookingLatest = maybe True (\lpb -> lpb.id == paymentBooking.id) latestBookingPayment
                          if isPaymentBookingLatest
                            then do
                              void $ QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.SUCCESS paymentBooking.id
                              void $ QFRFSTicketBooking.updateStatusValidTillAndPaymentTxnById DFRFSTicketBooking.CONFIRMING updatedTTL (Just txnId.getId) booking.id
                              mbJourneyLeg <- markJourneyPaymentSuccess booking paymentOrder paymentBooking
                              quoteUpdatedBooking <- maybeM (pure booking) pure (QFRFSTicketBooking.findById bookingId)
                              let mRiderName = person.firstName <&> (\fName -> person.lastName & maybe fName (\lName -> fName <> " " <> lName))
                              mRiderNumber <- mapM decrypt person.mobileNumber
                              whenJust paymentStatusResp $ \statusResp -> do
                                void $ QFRFSTicketBooking.insertPayerVpaIfNotPresent statusResp.payerVpa bookingId
                              mbJourney <- case mbJourneyLeg of
                                Just journeyLeg -> do
                                  QJourney.findByPrimaryKey journeyLeg.journeyId
                                Nothing -> pure Nothing
                              let mbIsSingleMode = mbJourney >>= (.isSingleMode)
                              void $ CallExternalBPP.confirm processOnConfirm merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) quoteUpdatedBooking quoteCategories mbIsSingleMode
                              void $ addPaymentoffersTags quoteUpdatedBooking.totalPrice person
                              let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.CONFIRMING (Just updatedTTL) (Just txnId.getId)
                              buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories paymentSuccess
                            else do
                              logError $ "booking payment for which order was charged is older: " <> show booking
                              void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED booking.id
                              let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
                              void $ markJourneyPaymentSuccess updatedBooking paymentOrder paymentBooking
                              buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories paymentFailed
                        else buildFRFSTicketBookingStatusAPIRes booking quoteCategories paymentSuccess
                    else do
                      if paymentBookingStatus == FRFSTicketService.REFUNDED
                        then do
                          logInfo $ "payment failed in payment pending: " <> show booking <> ", status: " <> show paymentBookingStatus
                          QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
                          let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
                          QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.REFUNDED paymentBooking.id
                          void $ markJourneyPaymentSuccess updatedBooking paymentOrder paymentBooking
                          let paymentStatusAPI = Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.REFUNDED
                          let mbPaymentObj = paymentStatusAPI <&> \status -> FRFSTicketService.FRFSBookingPaymentAPI {status, paymentOrder = Nothing, transactionId = Nothing}
                          buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories mbPaymentObj
                        else do
                          logInfo $ "payment success in payment pending: " <> show booking
                          paymentOrder_ <- buildCreateOrderResp paymentOrder commonPersonId merchantOperatingCity.id booking
                          txn <- QPaymentTransaction.findNewTransactionByOrderId paymentOrder.id
                          let paymentStatus_ = if isNothing txn then FRFSTicketService.NEW else paymentBookingStatus
                              paymentObj =
                                Just
                                  FRFSTicketService.FRFSBookingPaymentAPI
                                    { status = paymentStatus_,
                                      paymentOrder = paymentOrder_,
                                      transactionId = Nothing
                                    }
                          buildFRFSTicketBookingStatusAPIRes booking quoteCategories paymentObj
        when (isMultiModalBooking && paymentBookingStatus == FRFSTicketService.SUCCESS) $ do
          riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCity.id Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCity.id.getId)
          becknConfigs <- CQBC.findByMerchantIdDomainandMerchantOperatingCityId merchantId_ "FRFS" merchantOperatingCity.id
          let initTTLs = map (.initTTLSec) becknConfigs
          let maxInitTTL = intToNominalDiffTime $ case catMaybes initTTLs of
                [] -> 0 -- 30 minutes in seconds if all are Nothing
                ttlList -> maximum ttlList
          let bufferTime = case riderConfig.refundBufferTTLSec of
                Just secs -> secs.getSeconds
                Nothing -> 2 * 60
          let scheduleAfter = maxInitTTL + (intToNominalDiffTime bufferTime) -- schedule job (maxInitTTL + bufferTime) after calling confirm
              jobData = JobScheduler.CheckMultimodalConfirmFailJobData {JobScheduler.bookingId = bookingId}
          createJobIn @_ @'CheckMultimodalConfirmFail (Just merchantId_) (Just merchantOperatingCity.id) scheduleAfter (jobData :: CheckMultimodalConfirmFailJobData)
        return bookingApiResp
    DFRFSTicketBooking.CANCELLED -> do
      FRFSUtils.updateTotalOrderValueAndSettlementAmount booking quoteCategories bapConfig
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id
      let mbPaymentObj = paymentBooking <&> \tbp -> FRFSTicketService.FRFSBookingPaymentAPI {status = Utils.mkTBPStatusAPI tbp.status, paymentOrder = Nothing, transactionId = Nothing}
      buildFRFSTicketBookingStatusAPIRes booking quoteCategories mbPaymentObj
    DFRFSTicketBooking.COUNTER_CANCELLED -> do
      FRFSUtils.updateTotalOrderValueAndSettlementAmount booking quoteCategories bapConfig
      buildFRFSTicketBookingStatusAPIRes booking quoteCategories Nothing
    DFRFSTicketBooking.CANCEL_INITIATED -> do
      buildFRFSTicketBookingStatusAPIRes booking quoteCategories Nothing
    DFRFSTicketBooking.TECHNICAL_CANCEL_REJECTED -> do
      buildFRFSTicketBookingStatusAPIRes booking quoteCategories Nothing
  where
    markJourneyPaymentSuccess booking paymentOrder paymentBooking = do
      mbJourneyLeg <- QJourneyLeg.findByLegSearchId (Just booking.searchId.getId)
      whenJust mbJourneyLeg $ \journeyLeg -> do
        let journeyId = journeyLeg.journeyId
        whenJust paymentBooking.frfsQuoteId $ \paymentBookingQuoteId -> do
          when (booking.quoteId /= paymentBookingQuoteId) $ do
            switchFRFSQuoteTier journeyLeg paymentBookingQuoteId
        void $ QJourney.updatePaymentOrderShortId (Just paymentOrder.shortId) (Just True) journeyId
        void $ QJourney.updateStatus (if booking.status == DFRFSTicketBooking.FAILED then DJ.FAILED else DJ.INPROGRESS) journeyId
      pure mbJourneyLeg

    paymentSuccess =
      Just $
        FRFSTicketService.FRFSBookingPaymentAPI
          { status = FRFSTicketService.SUCCESS,
            paymentOrder = Nothing,
            transactionId = Nothing
          }

    mkPaymentSuccessLockKey :: Kernel.Types.Id.Id DFRFSTicketBooking.FRFSTicketBooking -> Text
    mkPaymentSuccessLockKey bookingId = "frfsPaymentSuccess:" <> bookingId.getId

    processOnConfirm ::
      ( CacheFlow m r,
        EsqDBFlow m r,
        EsqDBReplicaFlow m r,
        MonadFlow m,
        EncFlow m r,
        SchedulerFlow r,
        HasBAPMetrics m r,
        HasFlowEnv m r '["smsCfg" ::: SmsConfig],
        HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
        HasKafkaProducer r,
        HasLongDurationRetryCfg r c,
        HasShortDurationRetryCfg r c,
        CallFRFSBPP.BecknAPICallFlow m r,
        HasFlowEnv m r '["googleSAPrivateKey" ::: String]
      ) =>
      FRFSCommon.DOrder ->
      m ()
    processOnConfirm onConfirmReq = do
      (merchant', booking'', quoteCategories') <- DACFOC.validateRequest onConfirmReq
      DACFOC.onConfirm merchant' booking'' quoteCategories' onConfirmReq

    paymentFailed =
      Just $
        FRFSTicketService.FRFSBookingPaymentAPI
          { status = FRFSTicketService.FAILURE,
            paymentOrder = Nothing,
            transactionId = Nothing
          }

    buildCreateOrderResp paymentOrder commonPersonId merchantOperatingCityId booking = do
      personEmail <- mapM decrypt person.email
      personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
      isSplitEnabled_ <- Payment.getIsSplitEnabled merchantId_ merchantOperatingCityId Nothing (getPaymentType isMultiModalBooking booking.vehicleType)
      isPercentageSplitEnabled <- Payment.getIsPercentageSplit merchantId_ merchantOperatingCityId Nothing (getPaymentType isMultiModalBooking booking.vehicleType)
      let isSingleMode = fromMaybe False booking.isSingleMode
      splitSettlementDetails <- Payment.mkSplitSettlementDetails isSplitEnabled_ paymentOrder.amount [] isPercentageSplitEnabled isSingleMode
      let createOrderReq =
            Payment.CreateOrderReq
              { orderId = paymentOrder.id.getId,
                orderShortId = paymentOrder.shortId.getShortId,
                amount = paymentOrder.amount,
                customerId = person.id.getId,
                customerEmail = fromMaybe "growth@nammayatri.in" personEmail,
                customerPhone = personPhone,
                customerFirstName = person.firstName,
                customerLastName = person.lastName,
                createMandate = Nothing,
                mandateMaxAmount = Nothing,
                mandateFrequency = Nothing,
                mandateEndDate = Nothing,
                mandateStartDate = Nothing,
                optionsGetUpiDeepLinks = Nothing,
                metadataExpiryInMins = Nothing,
                metadataGatewayReferenceId = Nothing, --- assigned in shared kernel
                splitSettlementDetails = splitSettlementDetails,
                basket = Nothing
              }
      mbPaymentOrderValidTill <- Payment.getPaymentOrderValidity merchantId_ merchantOperatingCityId Nothing (getPaymentType isMultiModalBooking booking.vehicleType)
      DPayment.createOrderService commonMerchantId (Just $ cast merchantOperatingCityId) commonPersonId mbPaymentOrderValidTill Nothing (getPaymentType isMultiModalBooking booking.vehicleType) createOrderReq (createOrderCall merchantOperatingCityId booking (Just person.id.getId) person.clientSdkVersion)

    createOrderCall merchantOperatingCityId booking mRoutingId sdkVersion = Payment.createOrder merchantId_ merchantOperatingCityId Nothing (getPaymentType isMultiModalBooking booking.vehicleType) mRoutingId sdkVersion

    commonMerchantId = Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant merchantId_

    makeUpdatedBooking DFRFSTicketBooking.FRFSTicketBooking {..} updatedStatus mTTL transactionId =
      let validTill' = mTTL & fromMaybe validTill
          newPaymentTxnId = transactionId <|> paymentTxnId
       in DFRFSTicketBooking.FRFSTicketBooking {status = updatedStatus, validTill = validTill', paymentTxnId = newPaymentTxnId, ..}
    getSuccessTransactionId transactions = do
      let successTransactions = filter (\transaction -> transaction.status == Payment.CHARGED) transactions
      case successTransactions of
        [] -> throwError $ InvalidRequest "No successful transaction found"
        [transaction] -> return transaction.id
        _ -> throwError $ InvalidRequest "Multiple successful transactions found"

getMerchantOperatingCityFromBooking :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DFRFSTicketBooking.FRFSTicketBooking -> m DMOC.MerchantOperatingCity
getMerchantOperatingCityFromBooking tBooking = do
  let moCityId = tBooking.merchantOperatingCityId
  CQMOC.findById moCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show moCityId)

buildFRFSTicketBookingStatusAPIRes ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    SchedulerFlow r,
    HasShortDurationRetryCfg r c
  ) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  [FRFSQuoteCategory.FRFSQuoteCategory] ->
  Maybe FRFSTicketService.FRFSBookingPaymentAPI ->
  m FRFSTicketService.FRFSTicketBookingStatusAPIRes
buildFRFSTicketBookingStatusAPIRes booking quoteCategories payment = do
  integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  merchantOperatingCity <- getMerchantOperatingCityFromBooking booking
  tickets' <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
  let tickets =
        map
          ( \DFRFSTicket.FRFSTicket {..} ->
              FRFSTicketService.FRFSTicketAPI {..}
          )
          tickets'
      fareParameters = FRFSUtils.mkFareParameters (mkCategoryPriceItemFromQuoteCategories quoteCategories)
  (routeStations :: Maybe [FRFSRouteStationsAPI], stations :: Maybe [FRFSStationAPI]) <- do
    if integratedBppConfig.platformType == DIBC.MULTIMODAL
      then return (Nothing, Nothing)
      else do
        let stationsJson :: Maybe [FRFSStationAPI] = decodeFromText booking.stationsJson
        case stationsJson of
          Just stations -> do
            proccessedStations <- mapM (Utils.mkPOrgStationAPI booking.partnerOrgId integratedBppConfig) stations
            let routeStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< booking.routeStationsJson
            return (routeStations, Just proccessedStations)
          Nothing -> do
            let routeStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< booking.routeStationsJson
            return (routeStations, Nothing)
  return $
    FRFSTicketService.FRFSTicketBookingStatusAPIRes
      { bookingId = booking.id,
        city = merchantOperatingCity.city,
        updatedAt = booking.updatedAt,
        createdAt = booking.createdAt,
        _type = booking._type,
        quoteCategories = map mkFRFSQuoteCategoryAPIEntity quoteCategories,
        price = Just $ booking.totalPrice.amount,
        priceWithCurrency = Just $ mkPriceAPIEntity booking.totalPrice,
        quantity = find (\category -> category.categoryType == ADULT) fareParameters.priceItems <&> (.quantity),
        validTill = booking.validTill,
        vehicleType = booking.vehicleType,
        status = booking.status,
        discountedTickets = booking.discountedTickets,
        eventDiscountAmount = booking.eventDiscountAmount,
        payment = payment <&> (\p -> p {transactionId = booking.paymentTxnId}),
        isFareChanged = booking.isFareChanged,
        googleWalletJWTUrl = booking.googleWalletJWTUrl,
        integratedBppConfigId = booking.integratedBppConfigId,
        stations = fromMaybe [] stations,
        ..
      }

makeTicketBookingPaymentAPIStatus :: Payment.TransactionStatus -> FRFSTicketService.FRFSBookingPaymentStatusAPI
makeTicketBookingPaymentAPIStatus Payment.NEW = FRFSTicketService.NEW
makeTicketBookingPaymentAPIStatus PENDING_VBV = FRFSTicketService.PENDING
makeTicketBookingPaymentAPIStatus CHARGED = FRFSTicketService.SUCCESS
makeTicketBookingPaymentAPIStatus AUTHENTICATION_FAILED = FRFSTicketService.PENDING -- FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus AUTHORIZATION_FAILED = FRFSTicketService.PENDING -- FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus JUSPAY_DECLINED = FRFSTicketService.PENDING -- FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus AUTHORIZING = FRFSTicketService.PENDING
makeTicketBookingPaymentAPIStatus COD_INITIATED = FRFSTicketService.REFUNDED
makeTicketBookingPaymentAPIStatus STARTED = FRFSTicketService.PENDING
makeTicketBookingPaymentAPIStatus AUTO_REFUNDED = FRFSTicketService.REFUNDED
makeTicketBookingPaymentAPIStatus CLIENT_AUTH_TOKEN_EXPIRED = FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus Payment.CANCELLED = FRFSTicketService.FAILURE

addPaymentoffersTags ::
  ( MonadFlow m,
    CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasKafkaProducer r,
    ServiceFlow m r
  ) =>
  Price ->
  DP.Person ->
  m ()
addPaymentoffersTags totalPrice person = do
  logInfo $ "Add payment offer tag, personId: " <> person.id.getId <> ", totalPrice: " <> show totalPrice
  withTryCatch "addPaymentoffersTags:offerListCache" (SOffer.offerListCache person.merchantId person.id person.merchantOperatingCityId DPaymentOrder.Normal totalPrice)
    >>= \case
      Left err -> do
        logError $ "Error fetching offers for payment tags: " <> show err
        pure ()
      Right offerListResp -> do
        let offerTags = createPaymentOfferTags offerListResp.offerResp
        unless (null offerTags) $ do
          let currentTags = fromMaybe [] person.customerNammaTags
              -- Extract offer codes from new tags (format: "offerCode#STATUS") before wrapping
              newOfferCodes = mapMaybe extractOfferCode offerTags
              -- Remove existing tags that match any of the new offer codes
              filteredCurrentTags = filter (not . hasMatchingOfferCode newOfferCodes) currentTags
              -- Create new tags and add them after removing old ones
              newTags = map LYT.TagNameValueExpiry offerTags
              updatedTags = filteredCurrentTags <> newTags
          QP.updateCustomerTags (Just updatedTags) person.id
          logInfo $ "Added payment offer tags for person: " <> person.id.getId <> ", tags: " <> show offerTags
  where
    createPaymentOfferTags :: [Payment.OfferResp] -> [Text]
    createPaymentOfferTags offers = do
      let tags = mapMaybe createTagFromOffer offers
      tags
      where
        createTagFromOffer :: Payment.OfferResp -> Maybe Text
        createTagFromOffer offer = do
          let offerCode = offer.offerCode
              statusStr = case offer.status of
                Payment.ELIGIBLE -> "ELIGIBLE"
                Payment.INELIGIBLE -> "INELIGIBLE"
          pure $ offerCode <> "#" <> statusStr

    -- Extract offer code from tag string (format: "offerCode#STATUS")
    extractOfferCode :: Text -> Maybe Text
    extractOfferCode tagStr = case Text.splitOn "#" tagStr of
      (offerCode : _) -> Just offerCode
      _ -> Nothing

    -- Check if a tag has a matching offer code
    hasMatchingOfferCode :: [Text] -> LYT.TagNameValueExpiry -> Bool
    hasMatchingOfferCode offerCodes (LYT.TagNameValueExpiry tagStr) =
      case extractOfferCode tagStr of
        Just offerCode -> offerCode `elem` offerCodes
        Nothing -> False

mkFRFSQuoteCategoryAPIEntity :: FRFSQuoteCategory.FRFSQuoteCategory -> FRFSTicketService.FRFSQuoteCategoryAPIEntity
mkFRFSQuoteCategoryAPIEntity FRFSQuoteCategory.FRFSQuoteCategory {..} =
  FRFSTicketService.FRFSQuoteCategoryAPIEntity {categoryMetadata = mkCategoryMetadataAPIEntity <$> categoryMeta, price = mkPriceAPIEntity price, offeredPrice = mkPriceAPIEntity offeredPrice, finalPrice = mkPriceAPIEntity <$> finalPrice, ..}
  where
    mkCategoryMetadataAPIEntity FRFSQuoteCategory.QuoteCategoryMetadata {..} = FRFSTicketService.FRFSTicketCategoryMetadataAPIEntity {..}
