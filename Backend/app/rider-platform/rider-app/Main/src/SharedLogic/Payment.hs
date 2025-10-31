module SharedLogic.Payment where

import qualified Beckn.ACL.Cancel as ACL
import qualified Data.HashMap.Strict as HM
import Domain.Action.UI.BBPS as BBPS
import qualified Domain.Action.UI.Cancel as DCancel
import Domain.Action.UI.FRFSTicketService as FRFSTicketService
import Domain.Action.UI.Pass as Pass
import qualified Domain.Types.Booking as Booking
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import qualified Domain.Types.Ride as Ride
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer, KafkaProducerTools)
import Kernel.Types.CacheFlow
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JMU
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.Refunds as DRefunds
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.Refunds as QRefunds
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.JobScheduler
import qualified SharedLogic.JobScheduler as JobScheduler
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PurchasedPass as QPurchasedPass
import qualified Storage.Queries.PurchasedPassPayment as QPurchasedPassPayment
import Tools.Error
import Tools.Metrics.BAPMetrics
import qualified Tools.Payment as TPayment
import TransactionLogs.Types
import qualified UrlShortner.Common as UrlShortner

makePaymentIntent ::
  ( MonadFlow m,
    EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r
  ) =>
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id Person.Person ->
  Ride.Ride ->
  Payment.CreatePaymentIntentReq ->
  m Payment.CreatePaymentIntentResp
makePaymentIntent merchantId merchantOpCityId personId ride createPaymentIntentReq = do
  let commonMerchantId = cast @Merchant.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @Person.Person @DPayment.Person personId
      commonRideId = cast @Ride.Ride @DPayment.Ride ride.id
      createPaymentIntentCall = TPayment.createPaymentIntent merchantId merchantOpCityId
      updatePaymentIntentAmountCall = TPayment.updateAmountInPaymentIntent merchantId merchantOpCityId
      capturePaymentIntentCall = TPayment.capturePaymentIntent merchantId merchantOpCityId
      getPaymentIntentCall = TPayment.getPaymentIntent merchantId merchantOpCityId
  DPayment.createPaymentIntentService commonMerchantId (Just $ cast merchantOpCityId) commonPersonId commonRideId ride.shortId.getShortId createPaymentIntentReq createPaymentIntentCall updatePaymentIntentAmountCall capturePaymentIntentCall getPaymentIntentCall

cancelPaymentIntent ::
  ( MonadFlow m,
    EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r
  ) =>
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id Ride.Ride ->
  m ()
cancelPaymentIntent merchantId merchantOpCityId rideId = do
  let cancelPaymentIntentCall = TPayment.cancelPaymentIntent merchantId merchantOpCityId
  DPayment.cancelPaymentIntentService (cast @Ride.Ride @DPayment.Ride rideId) cancelPaymentIntentCall

chargePaymentIntent ::
  ( MonadFlow m,
    EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r
  ) =>
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Payment.PaymentIntentId ->
  m Bool
chargePaymentIntent merchantId merchantOpCityId paymentIntentId = do
  let capturePaymentIntentCall = TPayment.capturePaymentIntent merchantId merchantOpCityId
      getPaymentIntentCall = TPayment.getPaymentIntent merchantId merchantOpCityId
  DPayment.chargePaymentIntentService paymentIntentId capturePaymentIntentCall getPaymentIntentCall

paymentErrorHandler ::
  ( EncFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r
  ) =>
  Booking.Booking ->
  SomeException ->
  m ()
paymentErrorHandler booking exec = do
  let err = fromException @Payment.StripeError exec
  cancelBooking err
  where
    cancelBooking err = do
      let req =
            DCancel.CancelReq
              { reasonCode = SCR.CancellationReasonCode (maybe "UNKOWN_ERROR" toErrorCode err),
                reasonStage = SCR.OnAssign,
                additionalInfo = err >>= toMessage,
                reallocate = Nothing,
                blockOnCancellationRate = Nothing
              }
      dCancelRes <- DCancel.cancel booking Nothing req SBCR.ByApplication
      void $ withShortRetry $ CallBPP.cancelV2 booking.merchantId dCancelRes.bppUrl =<< ACL.buildCancelReqV2 dCancelRes req.reallocate

makeCxCancellationPayment ::
  ( MonadFlow m,
    EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r
  ) =>
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Payment.PaymentIntentId ->
  HighPrecMoney ->
  m Bool
makeCxCancellationPayment merchantId merchantOpCityId paymentIntentId cancellationAmount = do
  let capturePaymentIntentCall = TPayment.capturePaymentIntent merchantId merchantOpCityId
      getPaymentIntentCall = TPayment.getPaymentIntent merchantId merchantOpCityId
  DPayment.updateForCXCancelPaymentIntentService paymentIntentId capturePaymentIntentCall getPaymentIntentCall cancellationAmount

orderStatusHandler ::
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
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasField "ltsHedisEnv" r Redis.HedisEnv
  ) =>
  DOrder.PaymentServiceType ->
  DOrder.PaymentOrder ->
  DPayment.PaymentStatusResp ->
  m DPayment.PaymentStatusResp
orderStatusHandler paymentService paymentOrder paymentStatusResponse = do
  refundStatusHandler paymentOrder paymentStatusResponse.refunds paymentService
  eitherPaymentFullfillmentStatusWithEntityId <-
    try @_ @SomeException $
      ( do
          case paymentService of
            DOrder.FRFSBooking -> FRFSTicketService.frfsOrderStatusHandler (cast paymentOrder.merchantId) paymentStatusResponse JMU.switchFRFSQuoteTierUtil
            DOrder.FRFSBusBooking -> FRFSTicketService.frfsOrderStatusHandler (cast paymentOrder.merchantId) paymentStatusResponse JMU.switchFRFSQuoteTierUtil
            DOrder.FRFSMultiModalBooking -> FRFSTicketService.frfsOrderStatusHandler (cast paymentOrder.merchantId) paymentStatusResponse JMU.switchFRFSQuoteTierUtil
            DOrder.FRFSPassPurchase -> do
              status <- DPayment.getTransactionStatus paymentStatusResponse
              Pass.passOrderStatusHandler paymentOrder.id (cast paymentOrder.merchantId) status
            DOrder.BBPS -> do
              paymentFulfillStatus <- BBPS.bbpsOrderStatusHandler (cast paymentOrder.merchantId) paymentStatusResponse
              return (paymentFulfillStatus, Nothing)
            _ -> return (DPayment.FulfillmentPending, Nothing)
      )
  finalPaymentStatusResponse <-
    case paymentStatusResponse.status of
      Payment.CHARGED -> do
        case eitherPaymentFullfillmentStatusWithEntityId of
          Right paymentFulfillmentStatus ->
            case paymentFulfillmentStatus of
              (DPayment.FulfillmentFailed, _) -> initiateRefundWithPaymentStatusRespSync (cast paymentOrder.personId) paymentOrder.id
              (DPayment.FulfillmentPending, _) -> do
                now <- getCurrentTime
                case paymentOrder.validTill of
                  Just orderValidTill -> do
                    if now > orderValidTill
                      then initiateRefundWithPaymentStatusRespSync (cast paymentOrder.personId) paymentOrder.id
                      else return paymentStatusResponse
                  _ -> return paymentStatusResponse
              _ -> return paymentStatusResponse
          Left err -> do
            logError $ "Error in payment fullfillment status handler: " <> show err
            -- TODO :: Handle refund if some retry thresholds passed.
            -- initiateRefundWithPaymentStatusRespSync (cast paymentOrder.personId) paymentOrder.id
            return paymentStatusResponse
      _ -> return paymentStatusResponse
  let paymentStatusResponseWithFulfillmentStatus =
        case eitherPaymentFullfillmentStatusWithEntityId of
          Right (paymentFulfillmentStatus', domainEntityId') ->
            case finalPaymentStatusResponse of
              DPayment.PaymentStatus {..} -> DPayment.PaymentStatus {DPayment.paymentFulfillmentStatus = Just paymentFulfillmentStatus', DPayment.domainEntityId = domainEntityId', ..}
              _ -> finalPaymentStatusResponse
          Left _ -> finalPaymentStatusResponse
  return paymentStatusResponseWithFulfillmentStatus

refundStatusHandler ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    ServiceFlow m r
  ) =>
  DOrder.PaymentOrder ->
  [Payment.RefundsData] ->
  DOrder.PaymentServiceType ->
  m ()
refundStatusHandler paymentOrder refunds paymentServiceType = do
  mapM_
    ( \refund -> do
        refundEntry <- QRefunds.findById (Id refund.requestId) >>= fromMaybeM (InvalidRequest "Refund entry not found")
        case paymentServiceType of
          DOrder.FRFSBooking -> bookingsRefundStatusHandler refundEntry
          DOrder.FRFSBusBooking -> bookingsRefundStatusHandler refundEntry
          DOrder.FRFSMultiModalBooking -> bookingsRefundStatusHandler refundEntry
          DOrder.FRFSPassPurchase -> passesRefundStatusHandler refundEntry
          _ -> pure ()
    )
    refunds
  where
    bookingsRefundStatusHandler ::
      ( EsqDBFlow m r,
        CacheFlow m r,
        MonadFlow m,
        EsqDBReplicaFlow m r,
        ServiceFlow m r,
        EncFlow m r
      ) =>
      DRefunds.Refunds ->
      m ()
    bookingsRefundStatusHandler refund = do
      let refundStatus = refund.status
          isRefundApiCallSuccess = refund.isApiCallSuccess
      bookingPayment <- QFRFSTicketBookingPayment.findAllByOrderId paymentOrder.id
      let bookingPaymentIds = bookingPayment <&> (.id)
      mapM_
        ( \bookingPaymentId ->
            case refundStatus of
              Payment.REFUND_SUCCESS -> QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.REFUNDED bookingPaymentId
              Payment.REFUND_FAILURE -> QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.REFUND_FAILED bookingPaymentId
              _ ->
                case isRefundApiCallSuccess of
                  Nothing -> QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.REFUND_PENDING bookingPaymentId
                  Just True -> QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.REFUND_INITIATED bookingPaymentId
                  Just False -> QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.REFUND_FAILED bookingPaymentId
        )
        bookingPaymentIds

    passesRefundStatusHandler ::
      ( EsqDBFlow m r,
        CacheFlow m r,
        MonadFlow m,
        EsqDBReplicaFlow m r,
        ServiceFlow m r,
        EncFlow m r
      ) =>
      DRefunds.Refunds ->
      m ()
    passesRefundStatusHandler refund = do
      mbPurchasedPassPayment <- QPurchasedPassPayment.findOneByPaymentOrderId paymentOrder.id
      purchasedPass <- maybe (pure Nothing) (QPurchasedPass.findById . (.purchasedPassId)) mbPurchasedPassPayment >>= fromMaybeM (PurchasedPassNotFound paymentOrder.id.getId)
      case refund.status of
        Payment.REFUND_SUCCESS -> do
          QPurchasedPass.updateStatusById DPurchasedPass.Refunded purchasedPass.id
        Payment.REFUND_FAILURE -> do
          QPurchasedPass.updateStatusById DPurchasedPass.RefundFailed purchasedPass.id
        _ ->
          case refund.isApiCallSuccess of
            Nothing -> QPurchasedPass.updateStatusById DPurchasedPass.RefundPending purchasedPass.id
            Just True -> QPurchasedPass.updateStatusById DPurchasedPass.RefundInitiated purchasedPass.id
            Just False -> QPurchasedPass.updateStatusById DPurchasedPass.RefundFailed purchasedPass.id

initiateRefundWithPaymentStatusRespSync ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    EncFlow m r,
    SchedulerFlow r
  ) =>
  Id Person.Person ->
  Id DOrder.PaymentOrder ->
  m DPayment.PaymentStatusResp
initiateRefundWithPaymentStatusRespSync personId paymentOrderId = do
  paymentOrder <- QPaymentOrder.findById paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found")
  paymentServiceType <- paymentOrder.paymentServiceType & fromMaybeM (InvalidRequest "Payment service type not found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  processRefund person paymentOrder paymentServiceType
  let merchanOperatingCityId = fromMaybe person.merchantOperatingCityId (cast <$> paymentOrder.merchantOperatingCityId)
      orderStatusCall = TPayment.orderStatus (cast paymentOrder.merchantId) merchanOperatingCityId Nothing paymentServiceType (Just person.id.getId) person.clientSdkVersion
  paymentStatusResp <- DPayment.orderStatusService paymentOrder.personId paymentOrder.id orderStatusCall
  refundStatusHandler paymentOrder paymentStatusResp.refunds paymentServiceType
  return paymentStatusResp
  where
    processRefund ::
      ( EsqDBFlow m r,
        CacheFlow m r,
        MonadFlow m,
        EsqDBReplicaFlow m r,
        ServiceFlow m r,
        EncFlow m r,
        SchedulerFlow r
      ) =>
      Person.Person ->
      DOrder.PaymentOrder ->
      DOrder.PaymentServiceType ->
      m ()
    processRefund person paymentOrder paymentServiceType = do
      let merchanOperatingCityId = fromMaybe person.merchantOperatingCityId (cast <$> paymentOrder.merchantOperatingCityId)
      riderConfig <- QRC.findByMerchantOperatingCityId merchanOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchanOperatingCityId.getId)
      let refundsOrderCall = TPayment.refundOrder (cast paymentOrder.merchantId) merchanOperatingCityId Nothing paymentServiceType (Just person.id.getId) person.clientSdkVersion
      mbRefundResp <- DPayment.createRefundService paymentOrder.shortId refundsOrderCall
      whenJust mbRefundResp $ \refundResp -> do
        let refundRequestId = (listToMaybe refundResp.refunds) <&> (.requestId) -- TODO :: When will refunds be more than one ? even if more than 1 there requestId would be same right ?
        whenJust refundRequestId $ \refundId -> do
          let scheduleAfter = riderConfig.refundStatusUpdateInterval -- Schedule for 24 hours later
              jobData =
                JobScheduler.CheckRefundStatusJobData
                  { JobScheduler.refundId = refundId,
                    JobScheduler.numberOfRetries = 0
                  }
          createJobIn @_ @'CheckRefundStatus (Just person.merchantId) (Just merchanOperatingCityId) scheduleAfter (jobData :: JobScheduler.CheckRefundStatusJobData)
          logInfo $ "Scheduled refund status check job for " <> refundId <> " in 24 hours (initial check)"
