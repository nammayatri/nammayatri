module SharedLogic.Payment where

import qualified Beckn.ACL.Cancel as ACL
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Types.Booking as Booking
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.FRFSRecon as Recon
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBooking
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.ParkingTransaction as DPT
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import qualified Domain.Types.Ride as Ride
import qualified Kernel.External.Notification as Notification
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
import SharedLogic.Offer
import Storage.Beam.Payment ()
import Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.FRFSRecon as QRecon
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.ParkingTransaction as QPT
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PurchasedPass as QPurchasedPass
import qualified Storage.Queries.PurchasedPassPayment as QPurchasedPassPayment
import qualified Storage.Queries.TicketBooking as QTB
import Tools.Error
import Tools.Metrics.BAPMetrics
import qualified Tools.Notifications as TNotifications
import qualified Tools.Payment as TPayment
import qualified Tools.Wallet as TWallet
import TransactionLogs.Types
import qualified UrlShortner.Common as UrlShortner

-------------------------------------------------------------------------------------------------------
----------------------------------- Payment Order Status Handler --------------------------------------
-------------------------------------------------------------------------------------------------------

-- | Type alias for fulfillment status handler function
-- This allows callers to pass the appropriate handler for their payment service type
type FulfillmentStatusHandler m =
  DPayment.PaymentStatusResp ->
  m (DPayment.PaymentFulfillmentStatus, Maybe Text, Maybe Text)

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
    HasField "ltsHedisEnv" r Redis.HedisEnv,
    HasField "isMetroTestTransaction" r Bool,
    HasField "blackListedJobs" r [Text]
  ) =>
  FulfillmentStatusHandler m ->
  DOrder.PaymentServiceType ->
  DOrder.PaymentOrder ->
  (Payment.OrderStatusReq -> m Payment.OrderStatusResp) ->
  m DPayment.PaymentStatusResp
orderStatusHandler fulfillmentHandler paymentService paymentOrder orderStatusCall = do
  Redis.withWaitAndLockCrossAppRedis
    makePaymentOrderStatusHandlerLockKey
    60
    100
    ( do
        let walletPostingCall = case paymentOrder.merchantOperatingCityId of
              Just merchantOperatingCityId -> Just $ TWallet.walletPosting (cast paymentOrder.merchantId) (cast merchantOperatingCityId)
              Nothing -> Nothing
        orderStatusResponse <- DPayment.orderStatusService paymentOrder.personId paymentOrder.id orderStatusCall walletPostingCall
        mbUpdatedPaymentOrder <- QPaymentOrder.findById paymentOrder.id
        let updatedPaymentOrder = fromMaybe paymentOrder mbUpdatedPaymentOrder
        orderStatusHandlerWithRefunds fulfillmentHandler paymentService paymentOrder updatedPaymentOrder orderStatusResponse
    )
  where
    makePaymentOrderStatusHandlerLockKey :: Text
    makePaymentOrderStatusHandlerLockKey = "orderStatusHandler:paymentOrder:" <> paymentOrder.id.getId

orderStatusHandlerWithRefunds ::
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
    HasField "ltsHedisEnv" r Redis.HedisEnv,
    HasField "isMetroTestTransaction" r Bool,
    HasField "blackListedJobs" r [Text]
  ) =>
  FulfillmentStatusHandler m ->
  DOrder.PaymentServiceType ->
  DOrder.PaymentOrder ->
  DOrder.PaymentOrder ->
  DPayment.PaymentStatusResp ->
  m DPayment.PaymentStatusResp
orderStatusHandlerWithRefunds fulfillmentHandler paymentService paymentOrder updatedPaymentOrder paymentStatusResponse = do
  refundStatusHandler paymentOrder paymentService
  eitherPaymentFullfillmentStatusWithEntityIdAndTransactionId <-
    withTryCatch "orderStatusHandler:orderStatusHandler" $
      fulfillmentHandler paymentStatusResponse
  finalPaymentStatusResponse <-
    case paymentStatusResponse.status of
      Payment.CHARGED -> do
        case eitherPaymentFullfillmentStatusWithEntityIdAndTransactionId of
          Right paymentFullfillmentStatusWithEntityIdAndTransactionId ->
            case paymentFullfillmentStatusWithEntityIdAndTransactionId of
              (DPayment.FulfillmentFailed, domainEntityId, _) -> do
                paymentStatusRespWithRefund <- initiateRefundWithPaymentStatusRespSync (cast paymentOrder.personId) paymentOrder.id
                return $ mkPaymentStatusResp paymentStatusRespWithRefund (Just DPayment.FulfillmentFailed) domainEntityId
              (DPayment.FulfillmentRefundPending, domainEntityId, _) -> do
                paymentStatusRespWithRefund <- initiateRefundWithPaymentStatusRespSync (cast paymentOrder.personId) paymentOrder.id
                return $ mkPaymentStatusResp paymentStatusRespWithRefund (Just DPayment.FulfillmentRefundPending) domainEntityId
              -- If Payment Charged after the Order Validity, then initiate the Refund for the Customer
              (DPayment.FulfillmentPending, domainEntityId, _) -> do
                now <- getCurrentTime
                case paymentOrder.validTill of
                  Just orderValidTill -> do
                    if now > orderValidTill
                      then do
                        paymentStatusRespWithRefund <- initiateRefundWithPaymentStatusRespSync (cast paymentOrder.personId) paymentOrder.id
                        return $ mkPaymentStatusResp paymentStatusRespWithRefund (Just DPayment.FulfillmentPending) domainEntityId
                      else return $ mkPaymentStatusResp paymentStatusResponse (Just DPayment.FulfillmentPending) domainEntityId
                  _ -> return $ mkPaymentStatusResp paymentStatusResponse (Just DPayment.FulfillmentPending) domainEntityId
              (paymentFulfillmentStatus, domainEntityId, _) -> return $ mkPaymentStatusResp paymentStatusResponse (Just paymentFulfillmentStatus) domainEntityId
          Left err -> do
            logError $ "Error in payment fullfillment status handler: " <> show err
            return $ mkPaymentStatusResp paymentStatusResponse paymentOrder.paymentFulfillmentStatus paymentOrder.domainEntityId
      Payment.AUTO_REFUNDED -> do
        return $ mkPaymentStatusResp paymentStatusResponse (Just DPayment.FulfillmentRefundInitiated) Nothing
      _ -> return $ mkPaymentStatusResp paymentStatusResponse paymentOrder.paymentFulfillmentStatus paymentOrder.domainEntityId
  -- Create the Recon Entry and Trigger the Refund Notifications
  case eitherPaymentFullfillmentStatusWithEntityIdAndTransactionId of
    Right (newPaymentFulfillmentStatus, _, mbDomainTransactionId) -> do
      whenJust paymentOrder.paymentFulfillmentStatus $ \oldPaymentFulfillmentStatus -> do
        let personId = cast @DPayment.Person @Person.Person paymentOrder.personId
        when (newPaymentFulfillmentStatus /= oldPaymentFulfillmentStatus) $ do
          -- Recon Entry
          whenJust mbDomainTransactionId $ \domainTransactionId -> do
            void $
              withTryCatch "createReconEntry" $ do
                when (newPaymentFulfillmentStatus == DPayment.FulfillmentSucceeded) $ do
                  case paymentService of
                    DOrder.FRFSPassPurchase -> createPassReconEntry domainTransactionId
                    _ -> pure ()
          -- Refund Notify
          fork "Process Refunds Notifications" $ do
            case newPaymentFulfillmentStatus of
              DPayment.FulfillmentRefundInitiated -> TNotifications.notifyPaymentFulfillment Notification.REFUND_PENDING paymentOrder.id personId paymentService
              DPayment.FulfillmentRefundFailed -> TNotifications.notifyPaymentFulfillment Notification.REFUND_FAILED paymentOrder.id personId paymentService
              DPayment.FulfillmentRefunded -> TNotifications.notifyPaymentFulfillment Notification.REFUND_SUCCESS paymentOrder.id personId paymentService
              DPayment.FulfillmentPending -> do
                when (paymentOrder.status /= updatedPaymentOrder.status && updatedPaymentOrder.status == Payment.CHARGED) $ do
                  TNotifications.notifyPaymentFulfillment Notification.FULFILLMENT_PENDING paymentOrder.id personId paymentService
              DPayment.FulfillmentSucceeded -> TNotifications.notifyPaymentFulfillment Notification.FULFILLMENT_SUCCESS paymentOrder.id personId paymentService
              _ -> pure ()
        -- Invalidate the Offer List Cache
        case newPaymentFulfillmentStatus of
          DPayment.FulfillmentSucceeded ->
            fork "Invalidate Offer List Cache" $ do
              person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
              let merchantOperatingCityId = maybe person.merchantOperatingCityId (cast @DPayment.MerchantOperatingCity @DMOC.MerchantOperatingCity) paymentOrder.merchantOperatingCityId
              invalidateOfferListCache person merchantOperatingCityId paymentService (mkPrice (Just paymentOrder.currency) paymentOrder.amount)
          _ -> pure ()
    _ -> pure ()
  -- Update the Payment Order with the new payment fulfillment status, domain entity id and domain transaction id
  case eitherPaymentFullfillmentStatusWithEntityIdAndTransactionId of
    Right (_, _, domainTransactionId) -> do
      QPaymentOrder.updatePaymentFulfillmentStatus paymentOrder.id finalPaymentStatusResponse.paymentFulfillmentStatus finalPaymentStatusResponse.domainEntityId domainTransactionId
    _ -> pure ()
  return finalPaymentStatusResponse
  where
    mkPaymentStatusResp :: DPayment.PaymentStatusResp -> Maybe DPayment.PaymentFulfillmentStatus -> Maybe Text -> DPayment.PaymentStatusResp
    mkPaymentStatusResp paymentStatusResponse' paymentFulfillmentStatus' domainEntityId' = do
      case paymentStatusResponse' of
        DPayment.PaymentStatus {..} -> DPayment.PaymentStatus {DPayment.paymentFulfillmentStatus = paymentFulfillmentStatus', DPayment.domainEntityId = domainEntityId', ..}
        _ -> paymentStatusResponse'

    createPassReconEntry ::
      ( EsqDBFlow m r,
        CacheFlow m r,
        MonadFlow m,
        EsqDBReplicaFlow m r
      ) =>
      Text ->
      m ()
    createPassReconEntry transactionId = do
      case paymentStatusResponse.status of
        Payment.CHARGED -> do
          purchasedPassPayment <- QPurchasedPassPayment.findByPrimaryKey (Id transactionId) >>= fromMaybeM (InvalidRequest $ "Purchase pass payment not found for id: " <> transactionId)
          bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback purchasedPassPayment.merchantOperatingCityId purchasedPassPayment.merchantId (show Spec.FRFS) (Utils.frfsVehicleCategoryToBecknVehicleCategory Spec.BUS) >>= fromMaybeM (InternalError "Beckn Config not found")
          mkPassReconEntry bapConfig purchasedPassPayment
        _ -> return ()
      where
        mkPassReconEntry bapConfig purchasedPassPayment = do
          let finderFee :: Price = mkPrice Nothing $ fromMaybe 0 $ (readMaybe . T.unpack) =<< bapConfig.buyerFinderFee
          now <- getCurrentTime
          reconId <- generateGUID
          let reconEntry =
                Recon.FRFSRecon
                  { Recon.id = reconId,
                    Recon.frfsTicketBookingId = Id purchasedPassPayment.id.getId,
                    Recon.networkOrderId = purchasedPassPayment.id.getId,
                    Recon.collectorSubscriberId = bapConfig.subscriberId,
                    Recon.receiverSubscriberId = "MTC bus pass",
                    Recon.date = show now,
                    Recon.time = show now,
                    Recon.mobileNumber = Nothing,
                    Recon.sourceStationCode = Nothing,
                    Recon.destinationStationCode = Nothing,
                    Recon.ticketQty = Nothing,
                    Recon.ticketNumber = Nothing,
                    Recon.transactionRefNumber = Nothing,
                    Recon.transactionUUID = paymentStatusResponse.txnUUID,
                    Recon.txnId = paymentStatusResponse.txnId,
                    Recon.fare = mkPrice Nothing purchasedPassPayment.amount,
                    Recon.buyerFinderFee = finderFee,
                    Recon.totalOrderValue = mkPrice Nothing purchasedPassPayment.amount,
                    Recon.settlementAmount = mkPrice Nothing purchasedPassPayment.amount,
                    Recon.beneficiaryIFSC = Nothing,
                    Recon.beneficiaryBankAccount = Nothing,
                    Recon.collectorIFSC = bapConfig.bapIFSC,
                    Recon.settlementReferenceNumber = Nothing,
                    Recon.settlementDate = Nothing,
                    Recon.differenceAmount = Nothing,
                    Recon.message = Nothing,
                    Recon.ticketStatus = Nothing,
                    Recon.providerId = "MTC Bus Pass",
                    Recon.providerName = "MTC Bus Pass Provider",
                    Recon.entityType = Just Recon.BUS_PASS,
                    Recon.reconStatus = Just Recon.PENDING,
                    Recon.paymentGateway = Nothing,
                    Recon.merchantId = Just purchasedPassPayment.merchantId,
                    Recon.merchantOperatingCityId = Just purchasedPassPayment.merchantOperatingCityId,
                    Recon.createdAt = now,
                    Recon.updatedAt = now
                  }
          void $ QRecon.create reconEntry

refundStatusHandler ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    ServiceFlow m r
  ) =>
  DOrder.PaymentOrder ->
  DOrder.PaymentServiceType ->
  m ()
refundStatusHandler paymentOrder paymentServiceType = do
  refundsEntry <- QRefunds.findAllByOrderId paymentOrder.shortId
  mapM_
    ( \refundEntry -> do
        case paymentServiceType of
          DOrder.FRFSBooking -> bookingsRefundStatusHandler refundEntry
          DOrder.FRFSBusBooking -> bookingsRefundStatusHandler refundEntry
          DOrder.FRFSMultiModalBooking -> bookingsRefundStatusHandler refundEntry
          DOrder.FRFSPassPurchase -> passesRefundStatusHandler refundEntry
          DOrder.ParkingBooking -> parkingBookingRefundStatusHandler refundEntry
          _ -> pure ()
    )
    refundsEntry
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
      let isRefundApiCallSuccess = refund.isApiCallSuccess
      bookingPayments <- QFRFSTicketBookingPayment.findAllByOrderId paymentOrder.id
      mapM_
        ( \bookingPayment -> do
            let bookingPaymentId = bookingPayment.id
                bookingId = bookingPayment.frfsTicketBookingId
            mbBooking <- QFRFSTicketBooking.findById bookingId
            case mbBooking of
              Nothing -> pure ()
              Just booking -> do
                when (booking.status `elem` [DFRFSTicketBooking.NEW, DFRFSTicketBooking.APPROVED, DFRFSTicketBooking.PAYMENT_PENDING]) $ do
                  QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
            case refund.status of
              Payment.REFUND_SUCCESS -> QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.REFUNDED bookingPaymentId
              Payment.REFUND_FAILURE -> QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.REFUND_FAILED bookingPaymentId
              _ -> do
                case isRefundApiCallSuccess of
                  Nothing -> QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.REFUND_PENDING bookingPaymentId
                  Just True -> QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.REFUND_INITIATED bookingPaymentId
                  Just False -> QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.REFUND_FAILED bookingPaymentId
        )
        bookingPayments

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
      purchasedPassPayment <- QPurchasedPassPayment.findOneByPaymentOrderId paymentOrder.id >>= fromMaybeM (PurchasedPassPaymentNotFound paymentOrder.id.getId)
      purchasedPass <- QPurchasedPass.findById purchasedPassPayment.purchasedPassId >>= fromMaybeM (PurchasedPassNotFound purchasedPassPayment.purchasedPassId.getId)
      case refund.status of
        Payment.REFUND_SUCCESS -> do
          QPurchasedPassPayment.updateStatusByOrderId DPurchasedPass.Refunded paymentOrder.id
          when (purchasedPass.status == DPurchasedPass.Pending && purchasedPass.startDate == purchasedPassPayment.startDate && purchasedPass.endDate == purchasedPassPayment.endDate) $ do
            QPurchasedPass.updateStatusById DPurchasedPass.Refunded purchasedPass.id
        Payment.REFUND_FAILURE -> do
          QPurchasedPassPayment.updateStatusByOrderId DPurchasedPass.RefundFailed paymentOrder.id
          when (purchasedPass.status == DPurchasedPass.Pending && purchasedPass.startDate == purchasedPassPayment.startDate && purchasedPass.endDate == purchasedPassPayment.endDate) $ do
            QPurchasedPass.updateStatusById DPurchasedPass.RefundFailed purchasedPass.id
        _ ->
          case refund.isApiCallSuccess of
            Nothing -> do
              QPurchasedPassPayment.updateStatusByOrderId DPurchasedPass.RefundPending paymentOrder.id
              when (purchasedPass.status == DPurchasedPass.Pending && purchasedPass.startDate == purchasedPassPayment.startDate && purchasedPass.endDate == purchasedPassPayment.endDate) $ do
                QPurchasedPass.updateStatusById DPurchasedPass.RefundPending purchasedPass.id
            Just True -> do
              QPurchasedPassPayment.updateStatusByOrderId DPurchasedPass.RefundInitiated paymentOrder.id
              when (purchasedPass.status == DPurchasedPass.Pending && purchasedPass.startDate == purchasedPassPayment.startDate && purchasedPass.endDate == purchasedPassPayment.endDate) $ do
                QPurchasedPass.updateStatusById DPurchasedPass.RefundInitiated purchasedPass.id
            Just False -> do
              QPurchasedPassPayment.updateStatusByOrderId DPurchasedPass.RefundFailed paymentOrder.id
              when (purchasedPass.status == DPurchasedPass.Pending && purchasedPass.startDate == purchasedPassPayment.startDate && purchasedPass.endDate == purchasedPassPayment.endDate) $ do
                QPurchasedPass.updateStatusById DPurchasedPass.RefundFailed purchasedPass.id

    parkingBookingRefundStatusHandler ::
      ( EsqDBFlow m r,
        CacheFlow m r,
        MonadFlow m,
        EsqDBReplicaFlow m r,
        ServiceFlow m r,
        EncFlow m r
      ) =>
      DRefunds.Refunds ->
      m ()
    parkingBookingRefundStatusHandler refund = do
      parkingTransaction <- QPT.findByPaymentOrderId paymentOrder.id >>= fromMaybeM (InvalidRequest "Parking transaction not found")
      case refund.status of
        Payment.REFUND_SUCCESS -> do
          QPT.updateStatusById DPT.Refunded parkingTransaction.id
        Payment.REFUND_FAILURE -> do
          QPT.updateStatusById DPT.RefundFailed parkingTransaction.id
        _ ->
          case refund.isApiCallSuccess of
            Nothing -> QPT.updateStatusById DPT.RefundPending parkingTransaction.id
            Just True -> QPT.updateStatusById DPT.RefundInitiated parkingTransaction.id
            Just False -> QPT.updateStatusById DPT.RefundFailed parkingTransaction.id

initiateRefundWithPaymentStatusRespSync ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    EncFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Id Person.Person ->
  Id DOrder.PaymentOrder ->
  m DPayment.PaymentStatusResp
initiateRefundWithPaymentStatusRespSync personId paymentOrderId = do
  paymentOrder <- QPaymentOrder.findById paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found")
  paymentServiceType <- paymentOrder.paymentServiceType & fromMaybeM (InvalidRequest "Payment service type not found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  processRefund person paymentOrder paymentServiceType
  let merchantOperatingCityId = fromMaybe person.merchantOperatingCityId (cast <$> paymentOrder.merchantOperatingCityId)
      orderStatusCall = TPayment.orderStatus (cast paymentOrder.merchantId) merchantOperatingCityId Nothing paymentServiceType (Just person.id.getId) person.clientSdkVersion paymentOrder.isMockPayment
      walletPostingCall = TWallet.walletPosting (cast paymentOrder.merchantId) merchantOperatingCityId
  paymentStatusResp <- DPayment.orderStatusService paymentOrder.personId paymentOrder.id orderStatusCall (Just walletPostingCall)
  refundStatusHandler paymentOrder paymentServiceType
  return paymentStatusResp
  where
    processRefund ::
      ( EsqDBFlow m r,
        CacheFlow m r,
        MonadFlow m,
        EsqDBReplicaFlow m r,
        ServiceFlow m r,
        EncFlow m r,
        SchedulerFlow r,
        HasField "blackListedJobs" r [Text]
      ) =>
      Person.Person ->
      DOrder.PaymentOrder ->
      DOrder.PaymentServiceType ->
      m ()
    processRefund person paymentOrder paymentServiceType = do
      let merchantOperatingCityId = fromMaybe person.merchantOperatingCityId (cast <$> paymentOrder.merchantOperatingCityId)
      riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
      let refundsOrderCall = TPayment.refundOrder (cast paymentOrder.merchantId) merchantOperatingCityId Nothing paymentServiceType (Just person.id.getId) person.clientSdkVersion
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
          createJobIn @_ @'CheckRefundStatus (Just person.merchantId) (Just merchantOperatingCityId) scheduleAfter (jobData :: JobScheduler.CheckRefundStatusJobData)
          logInfo $ "Scheduled refund status check job for " <> refundId <> " in 24 hours (initial check)"

markRefundPendingAndSyncOrderStatus ::
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
    HasField "ltsHedisEnv" r Redis.HedisEnv,
    HasField "isMetroTestTransaction" r Bool,
    HasField "blackListedJobs" r [Text]
  ) =>
  Id Merchant.Merchant ->
  Id Person.Person ->
  Id DOrder.PaymentOrder ->
  m DPayment.PaymentStatusResp
markRefundPendingAndSyncOrderStatus merchantId personId orderId = do
  paymentOrder <- QPaymentOrder.findById orderId >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
  let paymentServiceType = fromMaybe DOrder.Normal paymentOrder.paymentServiceType
  case paymentServiceType of
    DOrder.FRFSBooking -> markBookingsRefundPending paymentOrder
    DOrder.FRFSBusBooking -> markBookingsRefundPending paymentOrder
    DOrder.FRFSMultiModalBooking -> markBookingsRefundPending paymentOrder
    DOrder.FRFSPassPurchase -> markPassesRefundPending paymentOrder
    DOrder.ParkingBooking -> markParkingRefundPending paymentOrder
    _ -> pure ()
  -- Hardcoded refund handler since this is only used for refund scenarios
  let refundFulfillmentHandler _ = pure (DPayment.FulfillmentRefundPending, Nothing, Nothing)
  syncOrderStatus refundFulfillmentHandler merchantId personId paymentOrder
  where
    markBookingsRefundPending paymentOrder = do
      bookingPayments <- QFRFSTicketBookingPayment.findAllByOrderId paymentOrder.id
      mapM_ (\bookingPayment -> QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.REFUND_PENDING bookingPayment.id) bookingPayments

    markPassesRefundPending paymentOrder = do
      QPurchasedPassPayment.updateStatusByOrderId DPurchasedPass.RefundPending paymentOrder.id

    markParkingRefundPending paymentOrder = do
      mbParkingTransaction <- QPT.findByPaymentOrderId paymentOrder.id
      whenJust mbParkingTransaction $ \parkingTransaction -> do
        QPT.updateStatusById DPT.RefundPending parkingTransaction.id

syncOrderStatus ::
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
    HasField "ltsHedisEnv" r Redis.HedisEnv,
    HasField "isMetroTestTransaction" r Bool,
    HasField "blackListedJobs" r [Text]
  ) =>
  FulfillmentStatusHandler m ->
  Id Merchant.Merchant ->
  Id Person.Person ->
  DOrder.PaymentOrder ->
  m DPayment.PaymentStatusResp
syncOrderStatus fulfillmentHandler merchantId personId paymentOrder = do
  person <- QPerson.findById personId >>= fromMaybeM (InvalidRequest "Person not found")
  mocId <- paymentOrder.merchantOperatingCityId & fromMaybeM (InternalError "MerchantOperatingCityId not found in payment order")
  let paymentServiceType = fromMaybe DOrder.Normal paymentOrder.paymentServiceType
  ticketPlaceId <-
    case paymentServiceType of
      DOrder.Normal -> do
        ticketBooking <- QTB.findById (cast paymentOrder.id)
        return $ ticketBooking <&> (.ticketPlaceId)
      _ -> return Nothing
  let orderStatusCall = TPayment.orderStatus merchantId (cast mocId) ticketPlaceId paymentServiceType (Just person.id.getId) person.clientSdkVersion paymentOrder.isMockPayment
  orderStatusHandler fulfillmentHandler paymentServiceType paymentOrder orderStatusCall

-------------------------------------------------------------------------------------------------------
------------------------------------- Payment Utility Functions ---------------------------------------
-------------------------------------------------------------------------------------------------------

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
  Maybe DMPM.PaymentMode ->
  Id Person.Person ->
  Ride.Ride ->
  Payment.CreatePaymentIntentReq ->
  m Payment.CreatePaymentIntentResp
makePaymentIntent merchantId merchantOpCityId paymentMode personId ride createPaymentIntentReq = do
  let commonMerchantId = cast @Merchant.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @Person.Person @DPayment.Person personId
      commonRideId = cast @Ride.Ride @DPayment.Ride ride.id
      createPaymentIntentCall = TPayment.createPaymentIntent merchantId merchantOpCityId paymentMode
      cancelPaymentIntentCall = TPayment.cancelPaymentIntent merchantId merchantOpCityId paymentMode
  DPayment.createPaymentIntentService commonMerchantId (Just $ cast merchantOpCityId) commonPersonId commonRideId ride.shortId.getShortId createPaymentIntentReq createPaymentIntentCall cancelPaymentIntentCall

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
  Maybe DMPM.PaymentMode ->
  Id Ride.Ride ->
  m ()
cancelPaymentIntent merchantId merchantOpCityId paymentMode rideId = do
  let cancelPaymentIntentCall = TPayment.cancelPaymentIntent merchantId merchantOpCityId paymentMode
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
  Maybe DMPM.PaymentMode ->
  Payment.PaymentIntentId ->
  m Bool
chargePaymentIntent merchantId merchantOpCityId paymentMode paymentIntentId = do
  let capturePaymentIntentCall = TPayment.capturePaymentIntent merchantId merchantOpCityId paymentMode
      getPaymentIntentCall = TPayment.getPaymentIntent merchantId merchantOpCityId paymentMode
  DPayment.chargePaymentIntentService paymentIntentId capturePaymentIntentCall getPaymentIntentCall

makeStripeRefund ::
  ( MonadFlow m,
    EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r
  ) =>
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe DMPM.PaymentMode ->
  DPayment.InitiateStripeRefundReq ->
  m (Either Text DPayment.InitiateStripeRefundResp)
makeStripeRefund merchantId merchantOpCityId paymentMode initiateRefundReq = do
  let createRefundsCall = TPayment.createRefund merchantId merchantOpCityId paymentMode
  let getRefundsCall = TPayment.getRefund merchantId merchantOpCityId paymentMode
  DPayment.initiateStripeRefundService initiateRefundReq createRefundsCall getRefundsCall

refreshStripeRefund ::
  ( MonadFlow m,
    EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r
  ) =>
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe DMPM.PaymentMode ->
  DPayment.RefreshStripeRefundReq ->
  m DPayment.RefreshStripeRefundResp
refreshStripeRefund merchantId merchantOpCityId paymentMode refreshRefundReq = do
  let getRefundsCall = TPayment.getRefund merchantId merchantOpCityId paymentMode
  DPayment.refreshStripeRefundService refreshRefundReq getRefundsCall

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
  Maybe DMPM.PaymentMode ->
  Payment.PaymentIntentId ->
  HighPrecMoney ->
  m Bool
makeCxCancellationPayment merchantId merchantOpCityId paymentMode paymentIntentId cancellationAmount = do
  let capturePaymentIntentCall = TPayment.capturePaymentIntent merchantId merchantOpCityId paymentMode
      getPaymentIntentCall = TPayment.getPaymentIntent merchantId merchantOpCityId paymentMode
  DPayment.updateForCXCancelPaymentIntentService paymentIntentId capturePaymentIntentCall getPaymentIntentCall cancellationAmount

validatePaymentInstrument :: (MonadThrow m, Log m) => Merchant.Merchant -> Maybe DMPM.PaymentInstrument -> Maybe Payment.PaymentMethodId -> m ()
validatePaymentInstrument merchant mbPaymentInstrument mbPaymentMethodId = do
  if merchant.onlinePayment
    then do
      let paymentInstrument = fromMaybe (DMPM.Card DMPM.DefaultCardType) mbPaymentInstrument
      case paymentInstrument of
        DMPM.Card _ -> when (isNothing mbPaymentMethodId) $ throwError PaymentMethodRequired
        DMPM.Cash -> pure ()
        _ -> throwError (InvalidRequest "Only Card and Cash payment instruments supported")
    else do
      let paymentInstrument = fromMaybe DMPM.Cash mbPaymentInstrument
      case paymentInstrument of
        DMPM.Cash -> pure ()
        DMPM.BoothOnline -> pure ()
        _ -> throwError (InvalidRequest "Only Cash and BoothOnline payment instruments supported")

isOnlinePayment :: Maybe Merchant.Merchant -> Booking.Booking -> Bool
isOnlinePayment mbMerchant booking = maybe False (.onlinePayment) mbMerchant && booking.paymentInstrument /= Just DMPM.Cash

-- in case if person.paymentMode was already changed, we use booking.paymentMode for old completed rides
getCustomerAndPaymentMethod :: (MonadThrow m, Log m) => Booking.Booking -> Person.Person -> m (Payment.CustomerId, Payment.PaymentMethodId)
getCustomerAndPaymentMethod booking person = do
  let paymentMode = fromMaybe DMPM.LIVE booking.paymentMode
  case paymentMode of
    DMPM.LIVE -> do
      customerPaymentId <- person.customerPaymentId & fromMaybeM (PersonFieldNotPresent "customerPaymentId")
      paymentMethodId <- person.defaultPaymentMethodId & fromMaybeM (PersonFieldNotPresent "defaultPaymentMethodId")
      pure (customerPaymentId, paymentMethodId)
    DMPM.TEST -> do
      customerPaymentId <- person.customerTestPaymentId & fromMaybeM (PersonFieldNotPresent "customerTestPaymentId")
      paymentMethodId <- person.defaultTestPaymentMethodId & fromMaybeM (PersonFieldNotPresent "defaultTestPaymentMethodId")
      pure (customerPaymentId, paymentMethodId)

updateDefaultPersonPaymentMethodId ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Person.Person ->
  Maybe Payment.PaymentMethodId ->
  m ()
updateDefaultPersonPaymentMethodId person paymentMethodId = do
  let paymentMode = fromMaybe DMPM.LIVE person.paymentMode
  case paymentMode of
    DMPM.LIVE -> QPerson.updateDefaultPaymentMethodId paymentMethodId person.id
    DMPM.TEST -> QPerson.updateDefaultTestPaymentMethodId paymentMethodId person.id
