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
import qualified Kernel.External.Payment.Interface.Types as PInterface
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
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import Lib.Finance.FinanceM (FinanceCtx (..))
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.Offer as DOffer
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.Refunds as DRefunds
import qualified Lib.Payment.Storage.HistoryQueries.Refunds as HQRefunds
import qualified Lib.Payment.Storage.Queries.Offer as QOffer
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance
import SharedLogic.JobScheduler
import qualified SharedLogic.JobScheduler as JobScheduler
import SharedLogic.Offer (invalidateOfferListCache)
import Storage.Beam.Payment ()
import Storage.ConfigPilot.Config.BecknConfig (BecknConfigDimensions (..))
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Storage.Queries.FRFSRecon as QRecon
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.ParkingTransaction as QPT
import qualified Storage.Queries.PaymentCustomer as QPaymentCustomer
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PurchasedPass as QPurchasedPass
import qualified Storage.Queries.PurchasedPassPayment as QPurchasedPassPayment
import qualified Storage.Queries.Ride as QRide
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
    ServiceFlow m r,
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
  Id DMOC.MerchantOperatingCity ->
  FulfillmentStatusHandler m ->
  DOrder.PaymentServiceType ->
  DOrder.PaymentOrder ->
  (Payment.OrderStatusReq -> m Payment.OrderStatusResp) ->
  m DPayment.PaymentStatusResp
orderStatusHandler merchantOpCityId fulfillmentHandler paymentService paymentOrder orderStatusCall = do
  Redis.withWaitAndLockCrossAppRedis
    makePaymentOrderStatusHandlerLockKey
    60
    100
    ( do
        let walletPostingCall = case paymentOrder.merchantOperatingCityId of
              Just merchantOperatingCityId -> Just $ TWallet.walletPosting (cast paymentOrder.merchantId) (cast merchantOperatingCityId)
              Nothing -> Nothing
            commonMerchantOperatingCityId = cast @DMOC.MerchantOperatingCity @DPayment.MerchantOperatingCity merchantOpCityId
        orderStatusResponse <- DPayment.orderStatusService commonMerchantOperatingCityId paymentOrder.personId paymentOrder.id orderStatusCall walletPostingCall
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
    ServiceFlow m r,
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
          bapConfig <- getOneConfig (BecknConfigDimensions {merchantOperatingCityId = purchasedPassPayment.merchantOperatingCityId.getId, merchantId = purchasedPassPayment.merchantId.getId, domain = Just (show Spec.FRFS), vehicleCategory = Just (Utils.frfsVehicleCategoryToBecknVehicleCategory Spec.BUS)}) >>= fromMaybeM (InternalError "Beckn Config not found")
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
  refundsEntry <- HQRefunds.findAllByOrderId paymentOrder.shortId
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
      commonMerchantOperatingCityId = cast @DMOC.MerchantOperatingCity @DPayment.MerchantOperatingCity merchantOperatingCityId
  paymentStatusResp <- DPayment.orderStatusService commonMerchantOperatingCityId paymentOrder.personId paymentOrder.id orderStatusCall (Just walletPostingCall)
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
      riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
      let refundsOrderCall = TPayment.refundOrder (cast paymentOrder.merchantId) merchantOperatingCityId Nothing paymentServiceType (Just person.id.getId) person.clientSdkVersion
      let commonMerchantOperatingCityId = cast @DMOC.MerchantOperatingCity @DPayment.MerchantOperatingCity merchantOperatingCityId
      mbRefundResp <- DPayment.createRefundService commonMerchantOperatingCityId paymentOrder.shortId refundsOrderCall
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
  commonMerchantOperatingCityId <- paymentOrder.merchantOperatingCityId & fromMaybeM (InternalError "MerchantOperatingCityId not found in payment order")
  let paymentServiceType = fromMaybe DOrder.Normal paymentOrder.paymentServiceType
      mocId = cast @DPayment.MerchantOperatingCity @DMOC.MerchantOperatingCity commonMerchantOperatingCityId
  ticketPlaceId <-
    case paymentServiceType of
      DOrder.Normal -> do
        ticketBooking <- QTB.findById (cast paymentOrder.id)
        return $ ticketBooking <&> (.ticketPlaceId)
      DOrder.RideBooking -> return Nothing
      _ -> return Nothing
  let orderStatusCall = TPayment.orderStatus merchantId mocId ticketPlaceId paymentServiceType (Just person.id.getId) person.clientSdkVersion paymentOrder.isMockPayment
  orderStatusHandler mocId fulfillmentHandler paymentServiceType paymentOrder orderStatusCall

-------------------------------------------------------------------------------------------------------
------------------------------------- Payment Utility Functions ---------------------------------------
-------------------------------------------------------------------------------------------------------

-- | Optional ledger info for ride payments. When provided, ledger entries are
--   created/settled/voided alongside the payment intent operations.
data RidePaymentLedgerInfo = RidePaymentLedgerInfo
  { rideFare :: HighPrecMoney, -- fare without GST
    gstAmount :: HighPrecMoney,
    platformFee :: HighPrecMoney, -- application fee / platform commission
    offerDiscountAmount :: HighPrecMoney, -- discount absorbed by marketplace (0 for CASHBACK or no offer)
    cashbackPayoutAmount :: HighPrecMoney, -- cashback to pay rider (0 for DISCOUNT or no offer)
    financeCtx :: FinanceCtx
  }

-- | Retrieve the ride payment order for a rideId (domainEntityId = rideId).
--   Returns only the ride PI order, NOT the tip PI order (which uses "tip:<rideId>").
getOrderIdForRide :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Ride.Ride -> m (Maybe (Id DOrder.PaymentOrder))
getOrderIdForRide rideId = do
  mbOrder <- QPaymentOrder.findByDomainEntityId rideId.getId
  pure $ (.id) <$> mbOrder

-- | Retrieve ALL payment orders for a ride (ride PI + tip PI).
--   Useful for capture, refund, and status operations that need to act on all orders.
getAllOrdersForRide :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Ride.Ride -> m [DOrder.PaymentOrder]
getAllOrdersForRide rideId = do
  mbRideOrder <- QPaymentOrder.findByDomainEntityId rideId.getId
  mbTipOrder <- QPaymentOrder.findByDomainEntityId ("tip:" <> rideId.getId)
  pure $ catMaybes [mbRideOrder, mbTipOrder]

type MakePaymentIntentConstraints m r c =
  ( MonadFlow m,
    EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    ServiceFlow m r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    FinanceBeamFlow.BeamFlow m r
  )

makePaymentIntent ::
  MakePaymentIntentConstraints m r c =>
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe DMPM.PaymentMode ->
  Id Person.Person ->
  Maybe (Id Ride.Ride) ->
  Maybe (Id DOrder.PaymentOrder) ->
  DOrder.PaymentServiceType ->
  DPayment.CreatePaymentIntentServiceReq ->
  Maybe RidePaymentLedgerInfo ->
  m (Maybe DPayment.CreatePaymentIntentServiceResp)
makePaymentIntent merchantId merchantOpCityId paymentMode personId mbRideId mbExistingOrderId paymentServiceType req mbLedgerInfo = do
  let effectiveAmount = req.amount - req.discountAmount
  -- If offer fully covers the fare, skip payment service (treat as cash/free ride flow)
  if effectiveAmount <= 0
    then do
      logInfo $ "Post-offer amount <= 0, skipping payment intent creation for ride: " <> show ((.getId) <$> mbRideId)
      pure Nothing
    else do
      let commonMerchantId = cast @Merchant.Merchant @DPayment.Merchant merchantId
          commonMerchantOperatingCityId = cast @DMOC.MerchantOperatingCity @DPayment.MerchantOperatingCity merchantOpCityId
          commonPersonId = cast @Person.Person @DPayment.Person personId
          createPaymentCall = TPayment.createPayment merchantId merchantOpCityId paymentMode Nothing
          cancelPaymentCall piId = do
            cancelResp <- TPayment.cancelPaymentIntent merchantId merchantOpCityId paymentMode piId
            pure
              PInterface.CreatePaymentResp
                { paymentServiceOrderId = cancelResp.paymentIntentId,
                  clientSecret = cancelResp.clientSecret,
                  status = Payment.castToTransactionStatus cancelResp.status,
                  sdkPayload = Nothing,
                  paymentLinks = Nothing
                }
          incrementAuthCall piId amount applicationFeeAmount =
            TPayment.updateAmountInPaymentIntent merchantId merchantOpCityId paymentMode piId amount applicationFeeAmount
          serviceReq =
            DPayment.CreatePaymentServiceReq
              { amount = effectiveAmount,
                currency = req.currency,
                customerId = req.customer,
                customerEmail = fromMaybe "" req.receiptEmail,
                customerPhone = "",
                customerFirstName = Nothing,
                customerLastName = Nothing,
                paymentMethodId = Just req.paymentMethod,
                driverAccountId = Just req.driverAccountId,
                applicationFeeAmount = Just req.applicationFeeAmount,
                receiptEmail = req.receiptEmail,
                splitSettlementDetails = Nothing,
                createMandate = Nothing,
                mandateMaxAmount = Nothing,
                mandateFrequency = Nothing,
                mandateStartDate = Nothing,
                mandateEndDate = Nothing,
                metadataGatewayReferenceId = Nothing,
                optionsGetUpiDeepLinks = Nothing,
                metadataExpiryInMins = Nothing,
                basket = Nothing,
                offerId = req.offerId <&> (.getId),
                discountAmount = Just req.discountAmount,
                payoutAmount = Nothing,
                domainEntityId = (.getId) <$> mbRideId
              }
      mbServiceResp <- DPayment.createPaymentService commonMerchantId (Just commonMerchantOperatingCityId) commonPersonId mbExistingOrderId Nothing paymentServiceType serviceReq createPaymentCall cancelPaymentCall (Just incrementAuthCall)
      serviceResp <- mbServiceResp & fromMaybeM (InternalError "Payment order expired, please try again")
      let resp =
            DPayment.CreatePaymentIntentServiceResp
              { paymentIntentId = serviceResp.paymentServiceOrderId,
                orderId = serviceResp.orderId
              }
      -- Create or update PENDING core ride ledger entries (RideFare, GST, PlatformFee) after successful payment creation.
      -- Tip and cancellation entries are managed separately — not touched here.
      whenJust mbLedgerInfo $ \ledgerInfo -> do
        existingEntries <- RidePaymentFinance.findRidePaymentEntries ledgerInfo.financeCtx.referenceId
        let coreEntries = filter (\e -> e.referenceType `elem` RidePaymentFinance.coreRidePaymentRefTypes) existingEntries
            pendingCoreEntries = filter (\e -> e.status == LE.PENDING) coreEntries
            newTotal = ledgerInfo.rideFare + ledgerInfo.gstAmount + ledgerInfo.platformFee
            oldTotal = sum $ map (.amount) pendingCoreEntries
        if null coreEntries
          then do
            -- First time: create core ride ledger entries
            result <- RidePaymentFinance.createRidePaymentLedger ledgerInfo.financeCtx ledgerInfo.rideFare ledgerInfo.gstAmount ledgerInfo.platformFee ledgerInfo.offerDiscountAmount ledgerInfo.cashbackPayoutAmount
            case result of
              Right _ -> logInfo $ "Created PENDING ride payment ledger entries for order: " <> resp.orderId.getId
              Left err -> logError $ "Failed to create ride payment ledger entries: " <> show err
          else
            if not (null pendingCoreEntries) && newTotal /= oldTotal
              then do
                -- Fare recomputed: void old PENDING core entries, create new ones with updated amounts
                let entryIds = map (.id) pendingCoreEntries
                RidePaymentFinance.voidRidePaymentLedger entryIds
                logInfo $ "Voided " <> show (length entryIds) <> " stale PENDING entries (old=" <> show oldTotal <> " new=" <> show newTotal <> ") for ride: " <> ledgerInfo.financeCtx.referenceId
                result <- RidePaymentFinance.createRidePaymentLedger ledgerInfo.financeCtx ledgerInfo.rideFare ledgerInfo.gstAmount ledgerInfo.platformFee ledgerInfo.offerDiscountAmount ledgerInfo.cashbackPayoutAmount
                case result of
                  Right _ -> logInfo $ "Created updated PENDING ledger entries for recomputed fare"
                  Left err -> logError $ "Failed to create updated ledger entries: " <> show err
              else logInfo $ "Skipping ledger creation — core entries already exist for ride: " <> ledgerInfo.financeCtx.referenceId
      pure (Just resp)

cancelPaymentIntent ::
  ( MonadFlow m,
    EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    FinanceBeamFlow.BeamFlow m r
  ) =>
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe DMPM.PaymentMode ->
  Id Ride.Ride ->
  m ()
cancelPaymentIntent merchantId merchantOpCityId paymentMode rideId = do
  let cancelPaymentIntentCall = TPayment.cancelPaymentIntent merchantId merchantOpCityId paymentMode
      commonMerchantOperatingCityId = cast @DMOC.MerchantOperatingCity @DPayment.MerchantOperatingCity merchantOpCityId
  -- Cancel at Stripe — tolerate failures (PI may already be cancelled/captured/expired)
  handle (\(e :: SomeException) -> logError $ "Cancel payment intent failed for ride " <> rideId.getId <> ": " <> show e) $
    DPayment.cancelPaymentIntentService commonMerchantOperatingCityId (cast @Ride.Ride @DPayment.Ride rideId) cancelPaymentIntentCall
  -- Void unsettled ledger entries (PENDING or DUE) regardless of Stripe cancel result
  pendingEntries <- RidePaymentFinance.findUnsettledRidePaymentEntries rideId.getId
  unless (null pendingEntries) $ do
    let entryIds = map (.id) pendingEntries
    RidePaymentFinance.voidRidePaymentLedger entryIds
    logInfo $ "Voided " <> show (length entryIds) <> " pending ledger entries for cancelled ride: " <> rideId.getId

chargePaymentIntent ::
  ( MonadFlow m,
    EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    FinanceBeamFlow.BeamFlow m r
  ) =>
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe DMPM.PaymentMode ->
  DOrder.PaymentServiceType ->
  Payment.PaymentIntentId ->
  Id Ride.Ride -> -- rideId for looking up pending ledger entries
  Text -> -- settlement reason (e.g. "RidePaymentCaptured", "TipPaymentCaptured")
  m Bool
chargePaymentIntent merchantId merchantOpCityId paymentMode paymentServiceType paymentIntentId rideId settledReason = do
  let capturePaymentIntentCall = TPayment.capturePaymentIntent merchantId merchantOpCityId paymentMode
      getPaymentIntentCall = TPayment.getPaymentIntent merchantId merchantOpCityId paymentMode
      commonMerchantOperatingCityId = cast @DMOC.MerchantOperatingCity @DPayment.MerchantOperatingCity merchantOpCityId
  charged <- DPayment.chargePaymentIntentService commonMerchantOperatingCityId paymentServiceType paymentIntentId capturePaymentIntentCall getPaymentIntentCall
  -- Find all unsettled ledger entries (PENDING or DUE) for this ride
  unsettledEntries <- RidePaymentFinance.findUnsettledRidePaymentEntries rideId.getId
  unless (null unsettledEntries) $ do
    let entryIds = map (.id) unsettledEntries
    if charged
      then do
        -- Settle all unsettled entries on successful capture
        let ctx =
              RidePaymentFinance.buildRiderFinanceCtx
                merchantId.getId
                merchantOpCityId.getId
                (Kernel.Prelude.head unsettledEntries).currency
                ""
                rideId.getId
                Nothing
                Nothing
        result <- RidePaymentFinance.settleRidePaymentLedger ctx entryIds settledReason
        case result of
          Right () -> logInfo $ "Settled " <> show (length entryIds) <> " ledger entries for ride: " <> rideId.getId <> " reason: " <> settledReason
          Left err -> logError $ "Failed to settle ledger entries for ride " <> rideId.getId <> ": " <> show err
      else do
        -- Mark entries as DUE on failed capture so getDueAmount picks them up
        RidePaymentFinance.markEntriesAsDue entryIds
        logWarning $ "Marked " <> show (length entryIds) <> " ledger entries as DUE for ride: " <> rideId.getId <> " (capture failed)"
  pure charged

-- | Unified refund wrapper. Uses refundPaymentService under the hood.
makeRefundPayment ::
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
  DPayment.RefundPaymentServiceReq ->
  m (Maybe Payment.RefundPaymentResp)
makeRefundPayment merchantId merchantOpCityId paymentMode refundReq = do
  let refundCall = TPayment.refundPayment merchantId merchantOpCityId paymentMode Nothing
  DPayment.refundPaymentService refundReq refundCall

-- | Unified refund status check. Replaces refreshStripeRefund.
getRefundStatusForOrder ::
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
  Id DOrder.PaymentOrder ->
  m (Maybe Payment.RefundPaymentResp)
getRefundStatusForOrder merchantId merchantOpCityId paymentMode orderId = do
  let getRefundStatusCall = TPayment.getRefundStatus merchantId merchantOpCityId paymentMode
      commonMerchantOperatingCityId = cast @DMOC.MerchantOperatingCity @DPayment.MerchantOperatingCity merchantOpCityId
  DPayment.getRefundStatusService orderId commonMerchantOperatingCityId getRefundStatusCall

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
  DOrder.PaymentServiceType ->
  Payment.PaymentIntentId ->
  HighPrecMoney ->
  m Bool
makeCxCancellationPayment merchantId merchantOpCityId paymentMode paymentServiceType paymentIntentId cancellationAmount = do
  let capturePaymentIntentCall = TPayment.capturePaymentIntent merchantId merchantOpCityId paymentMode
      getPaymentIntentCall = TPayment.getPaymentIntent merchantId merchantOpCityId paymentMode
      commonMerchantOperatingCityId = cast @DMOC.MerchantOperatingCity @DPayment.MerchantOperatingCity merchantOpCityId
  DPayment.updateForCXCancelPaymentIntentService commonMerchantOperatingCityId paymentServiceType paymentIntentId capturePaymentIntentCall getPaymentIntentCall cancellationAmount

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
getCustomerAndPaymentMethod :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Booking.Booking -> Person.Person -> m (Payment.CustomerId, Payment.PaymentMethodId)
getCustomerAndPaymentMethod booking person = do
  let paymentMode = fromMaybe DMPM.LIVE booking.paymentMode
  logDebug $ "getCustomerAndPaymentMethod: " <> show booking.id.getId <> " " <> show person.id.getId <> " " <> show paymentMode
  paymentCustomer <- QPaymentCustomer.findByPersonIdAndPaymentMode (Just person.id) (Just paymentMode) >>= fromMaybeM (PersonFieldNotPresent "paymentCustomer")
  logDebug $ "paymentCustomer: " <> show paymentCustomer
  paymentMethodId <- paymentCustomer.defaultPaymentMethodId & fromMaybeM (PersonFieldNotPresent "defaultPaymentMethodId")
  pure (paymentCustomer.customerId, paymentMethodId)

updateDefaultPersonPaymentMethodId ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Person.Person ->
  Payment.PaymentMethodId ->
  m ()
updateDefaultPersonPaymentMethodId person paymentMethodId = do
  let paymentMode = fromMaybe DMPM.LIVE person.paymentMode
  QPaymentCustomer.updateDefaultPaymentMethodId (Just paymentMethodId) (Just person.id) (Just paymentMode)

getOfferAmount :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DOffer.Offer -> HighPrecMoney -> m (Maybe DPayment.ComputedOfferAmount)
getOfferAmount offerId amount = do
  mbOffer <- QOffer.findById offerId
  case mbOffer of
    Just offer -> do
      let computed = DPayment.computeOfferAmount offer amount
      logInfo $ "Offer " <> offerId.getId <> " applied: amount=" <> show amount <> " postOffer=" <> show computed.postOfferAmount <> " discount=" <> show computed.discountAmount
      pure $ Just computed
    Nothing -> do
      logError $ "Offer not found: " <> offerId.getId <> ", proceeding with full amount"
      pure Nothing

-------------------------------------------------------------------------------------------------------
------------------------------------- Pending Payment Validation --------------------------------------
-------------------------------------------------------------------------------------------------------

paymentJobExecLockKey :: Text -> Text
paymentJobExecLockKey rideId = "PaymentJobExec:RideId-" <> rideId

-- | Capture pending payment before allowing new ride.
-- Finds rides with uncaptured payment (Initiated or NotInitiated), attempts capture.
-- Success -> allow new ride. Failure -> block ride; user can retry via Get Dues / Clear Dues.
-- | Check for PENDING ledger entries on most recent ride and block new ride if found.
--   Uses finance-kernel LedgerEntry instead of PaymentInvoice.
capturePendingPaymentIfExists ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasKafkaProducer r,
    FinanceBeamFlow.BeamFlow m r
  ) =>
  Person.Person ->
  Id DMOC.MerchantOperatingCity ->
  m ()
capturePendingPaymentIfExists person merchantOperatingCityId = do
  -- Find most recent ride for this rider
  mbLatestRideBooking <- QRide.findMostRecentRideForRider person.id
  case mbLatestRideBooking of
    Nothing -> pure () -- No rides, allow new ride
    Just (ride, booking) ->
      Redis.withWaitOnLockRedisWithExpiry (paymentJobExecLockKey ride.id.getId) 10 20 $ do
        -- 1. If there are already DUE entries (previous capture failed), block immediately
        existingDues <- RidePaymentFinance.findDueRidePaymentEntries ride.id.getId
        unless (null existingDues) $ do
          let dueAmount = sum $ map (.amount) existingDues
          logError $ "Blocking new ride: rider has " <> show (length existingDues) <> " DUE entries totaling " <> show dueAmount <> " for ride " <> ride.id.getId
          throwError $ InvalidRequest "You have pending dues from a previous ride. Please clear your dues before booking a new ride."

        -- 2. If there are PENDING entries, attempt capture now
        pendingEntries <- RidePaymentFinance.findPendingRidePaymentEntries ride.id.getId
        unless (null pendingEntries) $ do
          let totalPending = sum $ map (.amount) pendingEntries
          logInfo $ "Found " <> show (length pendingEntries) <> " PENDING ledger entries totaling " <> show totalPending <> " for ride " <> ride.id.getId <> ", attempting capture"

          mbOrderId <- getOrderIdForRide ride.id
          whenJust mbOrderId $ \orderId -> do
            mbOrder <- QPaymentOrder.findById orderId
            whenJust mbOrder $ \order -> do
              let paymentIntentId = order.paymentServiceOrderId
              -- chargePaymentIntent will mark entries as DUE on failure, SETTLED on success
              paymentCharged <- chargePaymentIntent booking.merchantId merchantOperatingCityId booking.paymentMode DOrder.OnlineRideHailing paymentIntentId ride.id RidePaymentFinance.settledReasonRidePayment
              if paymentCharged
                then do
                  logInfo $ "Successfully captured payment for ride " <> ride.id.getId
                  QRide.markPaymentStatus Ride.Completed ride.id
                else do
                  logError $ "Failed to capture payment for ride " <> ride.id.getId
                  QRide.markPaymentStatus Ride.Failed ride.id
                  -- chargePaymentIntent already marked entries as DUE, now block
                  throwError $ InvalidRequest "You have pending dues from a previous ride. Please clear your dues before booking a new ride."

zeroEffectivePaymentDueToOffer ::
  (MonadFlow m, EncFlow m r, CacheFlow m r, EsqDBFlow m r, FinanceBeamFlow.BeamFlow m r) =>
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id Ride.Ride ->
  Id Person.Person ->
  Maybe Text ->
  Currency ->
  Maybe RidePaymentLedgerInfo ->
  m ()
zeroEffectivePaymentDueToOffer merchantId merchantOperatingCityId rideId personId mbOfferId currency ledgerInfo = do
  -- Offer fully covers fare, no payment needed — create SETTLED ledger entries
  logInfo $ "Post-offer amount <= 0, skipping charge for ride: " <> rideId.getId
  whenJust mbOfferId $ \offerId -> do
    whenJust ledgerInfo $ \li -> do
      result <-
        RidePaymentFinance.createFullyDiscountedRidePaymentLedger
          li.financeCtx
          li.rideFare
          li.gstAmount
          li.platformFee
          li.offerDiscountAmount
      case result of
        Right _ -> logInfo $ "Created SETTLED ledger for fully discounted ride: " <> rideId.getId
        Left err -> logError $ "Failed to create fully discounted ledger: " <> show err
      let discountAmount = li.offerDiscountAmount
      void $
        withTryCatch "applyOfferWithoutPayment:executeJob" $
          DPayment.applyOfferWithoutPaymentService rideId.getId offerId personId.getId (Just discountAmount) Nothing currency merchantId.getId merchantOperatingCityId.getId
  QRide.markPaymentStatus Ride.Completed rideId
