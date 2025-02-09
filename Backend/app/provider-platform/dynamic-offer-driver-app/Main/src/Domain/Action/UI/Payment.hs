{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Payment
  ( DPayment.PaymentStatusResp (..),
    createOrder,
    getStatus,
    getOrder,
    juspayWebhookHandler,
    pdnNotificationStatus,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Tuple.Extra as Tuple
import qualified Domain.Action.UI.Driver as DADriver
import qualified Domain.Action.UI.Payout as PayoutA
import qualified Domain.Action.UI.Plan as ADPlan
import Domain.Action.UI.Ride.EndRide.Internal
import qualified Domain.Action.WebhookHandler as AWebhook
import Domain.Types.DriverFee
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Mandate as DM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Domain.Types.Notification (Notification)
import qualified Domain.Types.Notification as DNTF
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DP
import qualified Domain.Types.SubscriptionConfig as DSC
import qualified Domain.Types.WebhookExtra as WT
import Environment
import Kernel.Beam.Functions (runInMasterDb)
import Kernel.Beam.Functions as B (runInReplica)
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as DPayments
import qualified Kernel.External.Payment.Interface.Juspay as Juspay
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Payment.Juspay.Types as Juspay
import qualified Kernel.External.Payment.Types as Payment
import Kernel.External.Types (SchedulerType, ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow, Transactionable)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import Lib.Scheduler.Environment (JobCreatorEnv)
import Lib.Scheduler.JobStorageType.SchedulerType
import Lib.SessionizerMetrics.Types.Event
import qualified Lib.Webhook.Storage.Queries.Webhook as QWeb
import qualified Lib.Webhook.Types.Webhook as DW
import Servant (BasicAuthData)
import SharedLogic.Allocator
import qualified SharedLogic.DriverFee as SLDriverFee
import qualified SharedLogic.EventTracking as SEVT
import SharedLogic.Merchant
import qualified SharedLogic.Merchant as SMerchant
import qualified SharedLogic.Payment as SPayment
import Storage.Beam.Webhook ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import Storage.Queries.DriverPlan (findByDriverIdWithServiceName)
import qualified Storage.Queries.DriverPlan as QDP
import qualified Storage.Queries.Invoice as QIN
import qualified Storage.Queries.Mandate as QM
import qualified Storage.Queries.Notification as QNTF
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Vehicle as QVeh
import qualified Storage.Queries.VendorFeeExtra as QVF
import Tools.Error
import Tools.Notifications
import qualified Tools.Payment as Payment
import qualified Tools.PaymentNudge as PaymentNudge
import Utils.Common.Cac.KeyNameConstants

-- create order -----------------------------------------------------
createOrder :: (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id INV.Invoice -> Flow Payment.CreateOrderResp
createOrder (driverId, merchantId, opCityId) invoiceId = do
  invoices <- B.runInReplica $ QIN.findAllByInvoiceId invoiceId
  driverFees <- (B.runInReplica . QDF.findById . (.driverFeeId)) `mapM` invoices
  let mbServiceName = listToMaybe invoices <&> (.serviceName)
  let serviceName = fromMaybe DP.YATRI_SUBSCRIPTION mbServiceName
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName opCityId serviceName
      >>= fromMaybeM (NoSubscriptionConfigForService opCityId.getId $ show serviceName)
  let paymentServiceName = subscriptionConfig.paymentServiceName
      splitEnabled = subscriptionConfig.isVendorSplitEnabled == Just True
  vendorFees' <- if splitEnabled then concat <$> mapM (QVF.findAllByDriverFeeId . Domain.Types.DriverFee.id) (catMaybes driverFees) else pure []
  let vendorFees = map SPayment.roundVendorFee vendorFees'
  (createOrderResp, _) <- SPayment.createOrder (driverId, merchantId, opCityId) paymentServiceName (catMaybes driverFees, []) Nothing INV.MANUAL_INVOICE (getIdAndShortId <$> listToMaybe invoices) vendorFees Nothing splitEnabled
  return createOrderResp
  where
    getIdAndShortId inv = (inv.id, inv.invoiceShortId)

getOrder :: (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id DOrder.PaymentOrder -> Flow DOrder.PaymentOrderAPIEntity
getOrder (personId, _, _) orderId = do
  order <- QOrder.findById orderId >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
  unless (order.personId == cast personId) $ throwError NotAnExecutor
  mkOrderAPIEntity order

mkOrderAPIEntity :: EncFlow m r => DOrder.PaymentOrder -> m DOrder.PaymentOrderAPIEntity
mkOrderAPIEntity DOrder.PaymentOrder {..} = do
  clientAuthToken_ <- decrypt `mapM` clientAuthToken
  return $ DOrder.PaymentOrderAPIEntity {clientAuthToken = clientAuthToken_, ..}

-- order status -----------------------------------------------------

getStatus ::
  ( ServiceFlow m r,
    Transactionable m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    EventStreamFlow m r,
    MonadFlow m,
    JobCreatorEnv r,
    HasField "schedulerType" r SchedulerType
  ) =>
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Id DOrder.PaymentOrder ->
  m DPayment.PaymentStatusResp
getStatus (personId, merchantId, merchantOperatingCityId) orderId = do
  let commonPersonId = cast @DP.Person @DPayment.Person personId
      orderStatusCall = Payment.orderStatus merchantId merchantOperatingCityId -- api call
  order <- QOrder.findById orderId >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
  now <- getCurrentTime
  invoices <- QIN.findById (cast orderId)
  let firstInvoice = listToMaybe invoices
  let mbServiceName = firstInvoice <&> (.serviceName)
  if order.status == Payment.CHARGED -- Consider CHARGED status as terminal status
    then do
      return $
        DPayment.PaymentStatus
          { status = order.status,
            bankErrorCode = firstInvoice >>= (.bankErrorCode),
            bankErrorMessage = firstInvoice >>= (.bankErrorMessage),
            isRetried = Just $ order.isRetried,
            isRetargeted = Just $ order.isRetargeted,
            retargetLink = order.retargetLink,
            refunds = [],
            payerVpa = Nothing,
            card = Nothing,
            paymentMethodType = Nothing,
            authIdCode = Nothing,
            txnUUID = Nothing
          }
    else do
      let serviceName = fromMaybe DP.YATRI_SUBSCRIPTION mbServiceName
      serviceConfig <-
        CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOperatingCityId serviceName
          >>= fromMaybeM (NoSubscriptionConfigForService merchantOperatingCityId.getId $ show serviceName)
      paymentStatus <- DPayment.orderStatusService commonPersonId orderId (orderStatusCall serviceConfig.paymentServiceName)
      driver <- B.runInReplica $ QP.findById (cast order.personId) >>= fromMaybeM (PersonDoesNotExist order.personId.getId)
      case paymentStatus of
        DPayment.MandatePaymentStatus {..} -> do
          unless (status /= Payment.CHARGED) $ do
            processPayment merchantId driver order.id (shouldSendSuccessNotification mandateStatus) (serviceName, serviceConfig) invoices
          processMandate (serviceName, serviceConfig) (personId, merchantId, merchantOperatingCityId) mandateStatus (Just mandateStartDate) (Just mandateEndDate) (Id mandateId) mandateMaxAmount payerVpa upi order.shortId.getShortId --- needs refactoring ----
          QIN.updateBankErrorsByInvoiceId bankErrorMessage bankErrorCode (Just now) (cast order.id)
          notifyAndUpdateInvoiceStatusIfPaymentFailed personId order.id status Nothing bankErrorCode False (serviceName, serviceConfig)
        DPayment.PaymentStatus {..} -> do
          logDebug $ "Payment Status: " <> show status <> " Payer Vpa: " <> show payerVpa <> " OrderId: " <> order.id.getId
          logDebug $ "Invoices: " <> show invoices
          let isOneTimeSecurityInvoice = any (\inv -> inv.paymentMode == INV.ONE_TIME_SECURITY_INVOICE && inv.invoiceStatus == INV.ACTIVE_INVOICE) invoices
          let isPayoutRegistrationInvoice = any (\inv -> inv.paymentMode == INV.PAYOUT_REGISTRATION_INVOICE && inv.invoiceStatus == INV.ACTIVE_INVOICE) invoices
          when ((isOneTimeSecurityInvoice || isPayoutRegistrationInvoice) && status == Payment.CHARGED) do
            whenJust payerVpa $ \vpa -> QDI.updatePayoutVpaAndStatus (Just vpa) (Just DI.VIA_WEBHOOK) (cast order.personId)
            logDebug $ "Updating Payout (Via getStatus) And Process Previous Payout For Person: " <> show order.personId <> " with Vpa: " <> show payerVpa
            when (isJust payerVpa && isPayoutRegistrationInvoice) $ fork ("processing backlog payout for driver " <> order.personId.getId) $ PayoutA.processPreviousPayoutAmount (cast order.personId) payerVpa merchantOperatingCityId
          unless (status /= Payment.CHARGED) $ do
            processPayment merchantId driver order.id True (serviceName, serviceConfig) invoices
          QIN.updateBankErrorsByInvoiceId bankErrorMessage bankErrorCode (Just now) (cast order.id)
          notifyAndUpdateInvoiceStatusIfPaymentFailed personId order.id status Nothing Nothing False (serviceName, serviceConfig)
        DPayment.PDNNotificationStatusResp {..} -> do
          notification <- QNTF.findByShortId notificationId >>= fromMaybeM (InternalError "notification not found")
          let driverFeeId = notification.driverFeeId
          driverFee <- QDF.findById driverFeeId >>= fromMaybeM (DriverFeeNotFound driverFeeId.getId)
          processNotification driver.merchantOperatingCityId notification notificationStatus responseCode responseMessage driverFee driver False
      return paymentStatus

-- webhook ----------------------------------------------------------

juspayWebhookHandler ::
  ShortId DM.Merchant ->
  Maybe Context.City ->
  Maybe DP.ServiceNames ->
  BasicAuthData ->
  Value ->
  Flow AckResponse
juspayWebhookHandler merchantShortId mbOpCity mbServiceName authData value = do
  merchant <- findMerchantByShortId merchantShortId
  now <- getCurrentTime
  merchanOperatingCityId <- CQMOC.getMerchantOpCityId Nothing merchant mbOpCity
  let merchantId = merchant.id
      serviceName' = fromMaybe DP.YATRI_SUBSCRIPTION mbServiceName
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchanOperatingCityId serviceName'
      >>= fromMaybeM (NoSubscriptionConfigForService merchanOperatingCityId.getId $ show serviceName')
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity (subscriptionConfig.paymentServiceName) merchanOperatingCityId
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show Payment.Juspay))
  psc <- case merchantServiceConfig.serviceConfig of
    DMSC.PaymentServiceConfig psc' -> pure psc'
    DMSC.RentalPaymentServiceConfig psc' -> pure psc'
    DMSC.CautioPaymentServiceConfig psc' -> pure psc'
    _ -> throwError $ InternalError "Unknown Service Config"
  orderStatusResp <- Juspay.orderStatusWebhook psc DPayment.juspayWebhookService authData value
  osr <- case orderStatusResp of
    Nothing -> throwError $ InternalError "Order Contents not found."
    Just osr' -> pure osr'
  case osr of
    Payment.OrderStatusResp {..} -> do
      order <- QOrder.findByShortId (ShortId orderShortId) >>= fromMaybeM (PaymentOrderNotFound orderShortId)
      (invoices, serviceName, serviceConfig, driver) <- getInvoicesAndServiceWithServiceConfigByOrderId order
      logDebug $ "Webhook Response Status: " <> show transactionStatus <> " Payer Vpa: " <> show payerVpa <> " OrderId: " <> show orderShortId
      logDebug $ "Invoices: " <> show invoices
      when (any (\inv -> inv.paymentMode == INV.PAYOUT_REGISTRATION_INVOICE && inv.invoiceStatus == INV.ACTIVE_INVOICE) invoices && transactionStatus == Payment.CHARGED) do
        let mbVpa = payerVpa <|> ((.payerVpa) =<< upi)
        whenJust mbVpa $ \vpa -> QDI.updatePayoutVpaAndStatus (Just vpa) (Just DI.VIA_WEBHOOK) (cast order.personId)
        logDebug $ "Updating Payout And Process Previous Payout For Person: " <> show order.personId <> " with Vpa: " <> show mbVpa
        when (isJust mbVpa) $ fork ("processing backlog payout for driver " <> order.personId.getId) $ PayoutA.processPreviousPayoutAmount (cast order.personId) mbVpa merchanOperatingCityId
      when (order.status /= Payment.CHARGED || order.status == transactionStatus) $ do
        unless (transactionStatus /= Payment.CHARGED) $ do
          processPayment merchantId driver order.id True (serviceName, serviceConfig) invoices
        notifyAndUpdateInvoiceStatusIfPaymentFailed (cast order.personId) order.id transactionStatus eventName bankErrorCode True (serviceName, serviceConfig)
        QIN.updateBankErrorsByInvoiceId bankErrorMessage bankErrorCode (Just now) (cast order.id)
    Payment.MandateOrderStatusResp {..} -> do
      order <- QOrder.findByShortId (ShortId orderShortId) >>= fromMaybeM (PaymentOrderNotFound orderShortId)
      (invoices, serviceName, serviceConfig, driver) <- getInvoicesAndServiceWithServiceConfigByOrderId order
      when (order.status /= Payment.CHARGED || order.status == transactionStatus) $ do
        unless (transactionStatus /= Payment.CHARGED) $ do
          processPayment merchantId driver order.id (shouldSendSuccessNotification mandateStatus) (serviceName, serviceConfig) invoices
        processMandate (serviceName, serviceConfig) (cast order.personId, merchantId, driver.merchantOperatingCityId) mandateStatus mandateStartDate mandateEndDate (Id mandateId) mandateMaxAmount payerVpa upi order.shortId.getShortId
        notifyAndUpdateInvoiceStatusIfPaymentFailed (cast order.personId) order.id transactionStatus eventName bankErrorCode True (serviceName, serviceConfig)
        QIN.updateBankErrorsByInvoiceId bankErrorMessage bankErrorCode (Just now) (cast order.id)
    Payment.MandateStatusResp {..} -> do
      order <- QOrder.findByShortId (ShortId orderShortId) >>= fromMaybeM (PaymentOrderNotFound orderShortId)
      (_, serviceName, serviceConfig, driver) <- getInvoicesAndServiceWithServiceConfigByOrderId order
      processMandate (serviceName, serviceConfig) (cast order.personId, merchantId, driver.merchantOperatingCityId) status mandateStartDate mandateEndDate (Id mandateId) mandateMaxAmount Nothing Nothing order.shortId.getShortId
    Payment.PDNNotificationStatusResp {..} -> do
      notification <- QNTF.findByShortId notificationId >>= fromMaybeM (InternalError "notification not found")
      let driverFeeId = notification.driverFeeId
      driverFee <- QDF.findById driverFeeId >>= fromMaybeM (DriverFeeNotFound driverFeeId.getId)
      driver <- B.runInReplica $ QP.findById driverFee.driverId >>= fromMaybeM (PersonDoesNotExist driverFee.driverId.getId)
      processNotification driver.merchantOperatingCityId notification notificationStatus responseCode responseMessage driverFee driver True
    Payment.BadStatusResp -> pure ()
  pure Ack
  where
    getInvoicesAndServiceWithServiceConfigByOrderId ::
      (MonadFlow m, CacheFlow m r, EsqDBReplicaFlow m r, EsqDBFlow m r) =>
      DOrder.PaymentOrder ->
      m ([INV.Invoice], DP.ServiceNames, DSC.SubscriptionConfig, DP.Driver)
    getInvoicesAndServiceWithServiceConfigByOrderId order = do
      invoices' <- QIN.findById (cast order.id)
      let firstInvoice = listToMaybe invoices'
      let mbServiceName' = firstInvoice <&> (.serviceName)
      let serviceName' = fromMaybe DP.YATRI_SUBSCRIPTION mbServiceName'
      driver <- B.runInReplica $ QP.findById (cast order.personId) >>= fromMaybeM (PersonDoesNotExist order.personId.getId)
      serviceConfig <-
        CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName driver.merchantOperatingCityId serviceName'
          >>= fromMaybeM (InternalError $ "No subscription config found" <> show serviceName')
      return (invoices', serviceName', serviceConfig, driver)

processPayment ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  DP.Driver ->
  Id DOrder.PaymentOrder ->
  Bool ->
  (DP.ServiceNames, DSC.SubscriptionConfig) ->
  [INV.Invoice] ->
  m ()
processPayment _ driver orderId sendNotification (serviceName, subsConfig) invoices = do
  transporterConfig <- SCTC.findByMerchantOpCityId driver.merchantOperatingCityId (Just (DriverId (cast driver.id))) >>= fromMaybeM (TransporterConfigNotFound driver.merchantOperatingCityId.getId)
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let invoice = listToMaybe invoices
  let driverFeeIds = (.driverFeeId) <$> invoices
  Redis.whenWithLockRedis (paymentProcessingLockKey driver.id.getId) 60 $ do
    when ((invoice <&> (.paymentMode)) == Just INV.AUTOPAY_INVOICE && (invoice <&> (.invoiceStatus)) == Just INV.ACTIVE_INVOICE) $ do
      maybe (pure ()) (QDF.updateAutopayPaymentStageById (Just EXECUTION_SUCCESS) (Just now)) (invoice <&> (.driverFeeId))
    Redis.whenWithLockRedis (DADriver.mkPayoutLockKeyByDriverAndService driver.id serviceName) 60 $
      QDF.updateStatusByIds CLEARED driverFeeIds now
    QIN.updateInvoiceStatusByInvoiceId INV.SUCCESS (cast orderId)
    updatePaymentStatus driver.id driver.merchantOperatingCityId serviceName
    when (sendNotification && subsConfig.sendInAppFcmNotifications) $ notifyPaymentSuccessIfNotNotified driver orderId

updatePaymentStatus ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  DP.ServiceNames ->
  m ()
updatePaymentStatus driverId merchantOpCityId serviceName = do
  dueInvoices <- runInMasterDb $ QDF.findAllPendingAndDueDriverFeeByDriverIdForServiceName (cast driverId) serviceName
  let totalDue = sum $ calcDueAmount dueInvoices
  when (totalDue <= 0) $ QDI.updatePendingPayment False (cast driverId)
  mbDriverPlan <- findByDriverIdWithServiceName (cast driverId) serviceName -- what if its changed? needed inside lock?
  plan <- getPlan mbDriverPlan serviceName merchantOpCityId Nothing (mbDriverPlan >>= (.vehicleCategory))
  case plan of
    Nothing -> QDI.updateSubscription True (cast driverId)
    Just plan_ -> when (totalDue < plan_.maxCreditLimit && plan_.subscribedFlagToggleAllowed) $ QDI.updateSubscription True (cast driverId)
  where
    calcDueAmount =
      map
        ( \dueInvoice ->
            SLDriverFee.roundToHalf dueInvoice.currency $
              dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst
        )

notifyPaymentSuccessIfNotNotified :: (CacheFlow m r, EsqDBFlow m r) => DP.Person -> Id DOrder.PaymentOrder -> m ()
notifyPaymentSuccessIfNotNotified driver orderId = do
  let key = "driver-offer:SuccessNotif-" <> orderId.getId
  sendNotificationIfNotSent key 86400 $ do
    notifyPaymentSuccess driver.merchantOperatingCityId driver orderId

shouldSendSuccessNotification :: Payment.MandateStatus -> Bool
shouldSendSuccessNotification mandateStatus = mandateStatus `notElem` [Payment.REVOKED, Payment.FAILURE, Payment.EXPIRED, Payment.PAUSED]

notifyAndUpdateInvoiceStatusIfPaymentFailed ::
  (MonadFlow m, CacheFlow m r, EsqDBReplicaFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Id DOrder.PaymentOrder ->
  Payment.TransactionStatus ->
  Maybe Juspay.PaymentStatus ->
  Maybe Text ->
  Bool ->
  (DP.ServiceNames, DSC.SubscriptionConfig) ->
  m ()
notifyAndUpdateInvoiceStatusIfPaymentFailed driverId orderId orderStatus eventName mbBankErrorCode fromWebhook (serviceName, subsConfig) = do
  activeExecutionInvoice <- QIN.findByIdWithPaymenModeAndStatus (cast orderId) INV.AUTOPAY_INVOICE INV.ACTIVE_INVOICE
  now <- getCurrentTime
  let paymentMode = if isJust activeExecutionInvoice then DP.AUTOPAY else DP.MANUAL
  let (notifyFailure, updateFailure) = toNotifyFailure (isJust activeExecutionInvoice) eventName orderStatus
  when (updateFailure || (not fromWebhook && notifyFailure)) $ do
    QIN.updateInvoiceStatusByInvoiceId INV.FAILED (cast orderId)
    case activeExecutionInvoice of
      Just invoice' -> do
        QDF.updateAutoPayToManual invoice'.driverFeeId
        QDF.updateAutopayPaymentStageById (Just EXECUTION_FAILED) (Just now) invoice'.driverFeeId
      Nothing -> pure ()
    when (subsConfig.sendInAppFcmNotifications) $ do
      notifyPaymentFailureIfNotNotified paymentMode
  let toNotify = notifyFailure && isJust mbBankErrorCode && subsConfig.sendInAppFcmNotifications
  when toNotify $ notifyPaymentFailureIfNotNotified paymentMode
  where
    notifyPaymentFailureIfNotNotified paymentMode = do
      let key = "driver-offer:FailedNotif-" <> orderId.getId
      sendNotificationIfNotSent key 3600 $ fork "Sending payment failure notification" (PaymentNudge.notifyPaymentFailure driverId paymentMode mbBankErrorCode serviceName)

    toNotifyFailure isActiveExecutionInvoice_ eventName_ orderStatus_ = do
      let validStatus = orderStatus_ `elem` [Payment.AUTHENTICATION_FAILED, Payment.AUTHORIZATION_FAILED, Payment.JUSPAY_DECLINED]
      case (isActiveExecutionInvoice_, eventName_ == Just Juspay.ORDER_FAILED) of
        (True, False) -> (validStatus, False)
        (_, _) -> (validStatus, validStatus)

sendNotificationIfNotSent :: (MonadFlow m, CacheFlow m r) => Text -> Int -> m () -> m ()
sendNotificationIfNotSent key expiry actions = do
  isNotificationSent <- fromMaybe False <$> Hedis.get key
  unless isNotificationSent $ do
    Hedis.setExp key True expiry -- 24 hours
    actions

pdnNotificationStatus ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    MonadFlow m,
    HasShortDurationRetryCfg r c,
    ServiceFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Id Notification ->
  m DPayments.NotificationStatusResp
pdnNotificationStatus (_, merchantId, opCity) notificationId = do
  pdnNotification <- QNTF.findById notificationId >>= fromMaybeM (InternalError $ "No Notification Sent With Id" <> notificationId.getId)
  let driverFeeId = pdnNotification.driverFeeId
  driverFee <- QDF.findById driverFeeId >>= fromMaybeM (DriverFeeNotFound driverFeeId.getId)
  driver <- B.runInReplica $ QP.findById driverFee.driverId >>= fromMaybeM (PersonDoesNotExist driverFee.driverId.getId)
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName opCity driverFee.serviceName
      >>= fromMaybeM (NoSubscriptionConfigForService opCity.getId $ show driverFee.serviceName)
  resp <- Payment.mandateNotificationStatus merchantId opCity subscriptionConfig.paymentServiceName (mkNotificationRequest pdnNotification.shortId)
  let (responseCode, reponseMessage) = Tuple.both (\func -> func =<< resp.providerResponse) ((.responseCode), (.responseMessage))
  processNotification opCity pdnNotification resp.status responseCode reponseMessage driverFee driver False
  return resp
  where
    mkNotificationRequest shortNotificationId =
      DPayments.NotificationStatusReq
        { notificationId = shortNotificationId
        }

processNotification ::
  (CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  DNTF.Notification ->
  Payment.NotificationStatus ->
  Maybe Text ->
  Maybe Text ->
  DriverFee ->
  DP.Person ->
  Bool ->
  m ()
processNotification merchantOpCityId notification notificationStatus respCode respMessage driverFee driver fromWebhook = do
  let driverFeeId = driverFee.id
  now <- getCurrentTime
  unless (notification.status == Juspay.SUCCESS) $ do
    transporterConfig <- SCTC.findByMerchantOpCityId driver.merchantOperatingCityId (Just (DriverId (cast driver.id))) >>= fromMaybeM (TransporterConfigNotFound driver.merchantOperatingCityId.getId)
    case notificationStatus of
      Juspay.NOTIFICATION_FAILURE -> do
        --- here based on notification status failed update driver fee to payment_overdue and reccuring invoice----
        mbIsNotificationSchedulerRunning <- SLDriverFee.isNotificationSchedulerRunningKey driverFee.startTime driverFee.endTime merchantOpCityId driverFee.serviceName
        let isRetryEligibleError = case (mbIsNotificationSchedulerRunning, respCode) of
              (Just True, Just err) -> err `elem` transporterConfig.notificationRetryEligibleErrorCodes
              (_, _) -> False
        unless (driverFee.status == CLEARED) $ do
          if driverFee.notificationRetryCount < transporterConfig.notificationRetryCountThreshold && fromWebhook && isRetryEligibleError
            then do
              QIN.updateInvoiceStatusByDriverFeeIdsAndMbPaymentMode INV.ACTIVE_INVOICE [driverFeeId] (Just INV.AUTOPAY_INVOICE)
              QDF.updateManualToAutoPay driverFeeId
              QDF.updateAutopayPaymentStageById (Just NOTIFICATION_SCHEDULED) (Just now) driverFeeId
              QDF.updateNotificationRetryCountById (driverFee.notificationRetryCount + 1) driverFeeId
            else do
              QDF.updateAutoPayToManual driverFeeId
              QIN.updateInvoiceStatusByDriverFeeIdsAndMbPaymentMode INV.INACTIVE [driverFeeId] (Just INV.AUTOPAY_INVOICE)
      Juspay.SUCCESS -> do
        --- based on notification status Success udpate driver fee autoPayPaymentStage to Execution scheduled -----
        unless (driverFee.status == CLEARED) $ do
          QIN.updateInvoiceStatusByDriverFeeIdsAndMbPaymentMode INV.ACTIVE_INVOICE [driverFeeId] (Just INV.AUTOPAY_INVOICE)
          QDF.updateManualToAutoPay driverFeeId
        QDF.updateAutopayPaymentStageById (Just EXECUTION_SCHEDULED) (Just now) driverFeeId
      _ -> pure ()
    QNTF.updateNotificationStatusAndResponseInfoById notificationStatus respCode respMessage notification.id

processMandate ::
  (MonadFlow m, CacheFlow m r, EsqDBReplicaFlow m r, EsqDBFlow m r, EventStreamFlow m r, JobCreatorEnv r, HasField "schedulerType" r SchedulerType, EncFlow m r) =>
  (DP.ServiceNames, DSC.SubscriptionConfig) ->
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Payment.MandateStatus ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Id DM.Mandate ->
  HighPrecMoney ->
  Maybe Text ->
  Maybe Payment.Upi ->
  Text ->
  m ()
processMandate (serviceName, subsConfig) (driverId, merchantId, merchantOpCityId) mandateStatus startDate endDate mandateId maxAmount payerVpa upiDetails orderId = do
  let payerApp = upiDetails >>= (.payerApp)
      payerAppName = upiDetails >>= (.payerAppName)
      mandatePaymentFlow = upiDetails >>= (.txnFlowType)
  mbExistingMandate <- QM.findById mandateId
  currency <- SMerchant.getCurrencyByMerchantOpCity merchantOpCityId
  now <- getCurrentTime
  mandateData <- do
    case mbExistingMandate of
      Just mandate -> return mandate
      Nothing -> do
        mData <- mkMandate currency payerApp payerAppName mandatePaymentFlow
        QM.create mData
        return mData
  driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  let isWebhookEventEnabled = WT.MANDATE `elem` subsConfig.eventsEnabledForWebhook
  when (mandateStatus == Payment.ACTIVE) $ do
    Redis.withWaitOnLockRedisWithExpiry (mandateProcessingLockKey driverId.getId) 60 60 $ do
      --- do not update payer vpa from euler for older active mandates also we update only when autopayStatus not suspended because on suspend we make the mandate inactive in table
      (autoPayStatus, mbDriverPlan) <- ADPlan.getSubcriptionStatusWithPlan serviceName driverId
      let toUpdatePayerVpa = checkToUpdatePayerVpa mbExistingMandate autoPayStatus
      let payerVpa' = if toUpdatePayerVpa then payerVpa else Nothing
      QDP.updateMandateIdByDriverIdAndServiceName (Just mandateId) driverId serviceName
      QM.updateMandateDetails mandateId DM.ACTIVE payerVpa' payerApp payerAppName mandatePaymentFlow
      QDP.updatePaymentModeByDriverIdAndServiceName DP.AUTOPAY (cast driverId) serviceName
      QDP.updateMandateSetupDateByDriverIdAndServiceName (Just now) (cast driverId) serviceName
      mbPlan <- getPlan mbDriverPlan serviceName merchantOpCityId Nothing (mbDriverPlan >>= (.vehicleCategory))
      let subcribeToggleAllowed = maybe False (.subscribedFlagToggleAllowed) mbPlan
      when subcribeToggleAllowed $ QDI.updateSubscription True (cast driverId)
      ADPlan.updateSubscriptionStatus serviceName (driverId, merchantId, merchantOpCityId) (castAutoPayStatus mandateStatus) payerVpa'
      maybe (pure ()) (\plan -> fork "track autopay status" $ SEVT.trackAutoPayStatusChange plan $ show (castAutoPayStatus mandateStatus)) mbDriverPlan
      when (serviceName == DP.YATRI_SUBSCRIPTION) $ QDI.updatPayerVpa payerVpa' (cast driverId)
      when (isWebhookEventEnabled) $ callWebhookInFork driver mandateData
  when (mandateStatus `elem` [Payment.REVOKED, Payment.FAILURE, Payment.EXPIRED, Payment.PAUSED]) $ do
    Redis.withWaitOnLockRedisWithExpiry (mandateProcessingLockKey driverId.getId) 60 60 $ do
      QM.updateMandateDetails mandateId DM.INACTIVE Nothing payerApp Nothing mandatePaymentFlow --- should we store driver Id in mandate table ?
      mbDriverPlan <- QDP.findByMandateIdAndServiceName (Just mandateId) serviceName
      case mbDriverPlan of
        Just driverPlan -> do
          ADPlan.updateSubscriptionStatus serviceName (driverId, merchantId, merchantOpCityId) (castAutoPayStatus mandateStatus) Nothing
          QDP.updatePaymentModeByDriverIdAndServiceName DP.MANUAL (cast driverPlan.driverId) serviceName
          when (mandateStatus == Payment.PAUSED) $ do
            QDF.updateAllExecutionPendingToManualOverdueByDriverIdForServiceName (cast driver.id) serviceName
            QIN.inActivateAllAutopayActiveInvoices (cast driver.id) serviceName
            when (subsConfig.sendInAppFcmNotifications) $ do
              PaymentNudge.notifyMandatePaused driver
          when (mandateStatus == Payment.REVOKED) $ do
            QDF.updateAllExecutionPendingToManualOverdueByDriverIdForServiceName (cast driver.id) serviceName
            QIN.inActivateAllAutopayActiveInvoices (cast driver.id) serviceName
            when (subsConfig.sendInAppFcmNotifications) $ do
              PaymentNudge.notifyMandateCancelled driver
          fork "track autopay status" $ SEVT.trackAutoPayStatusChange driverPlan $ show (castAutoPayStatus mandateStatus)
          when (isWebhookEventEnabled) $ callWebhookInFork driver mandateData
        Nothing -> do
          (autoPayStatus, mbDriverPlanByDriverId) <- ADPlan.getSubcriptionStatusWithPlan serviceName driverId
          let currentMandateId = mbDriverPlanByDriverId >>= (.mandateId)
          when (isNothing currentMandateId || (currentMandateId /= Just mandateId) && notElem autoPayStatus [Just DI.ACTIVE, Just DI.SUSPENDED]) $ do
            ADPlan.updateSubscriptionStatus serviceName (driverId, merchantId, merchantOpCityId) (castAutoPayStatus mandateStatus) Nothing
          maybe (pure ()) (\plan -> fork "track autopay status" $ SEVT.trackAutoPayStatusChange plan $ show (castAutoPayStatus mandateStatus)) mbDriverPlanByDriverId
  where
    castAutoPayStatus = \case
      Payment.CREATED -> Just DI.PENDING
      Payment.ACTIVE -> Just DI.ACTIVE
      Payment.REVOKED -> Just DI.CANCELLED_PSP
      Payment.PAUSED -> Just DI.PAUSED_PSP
      Payment.FAILURE -> Just DI.MANDATE_FAILED
      Payment.EXPIRED -> Just DI.MANDATE_EXPIRED
    mkMandate currency payerApp payerAppName mandatePaymentFlow = do
      now <- getCurrentTime
      return $
        DM.Mandate
          { id = mandateId,
            status = DM.INACTIVE,
            createdAt = now,
            updatedAt = now,
            payerApp,
            payerAppName,
            mandatePaymentFlow,
            startDate = fromMaybe now startDate,
            endDate = fromMaybe now endDate,
            merchantOperatingCityId = Just merchantOpCityId,
            merchantId = Just merchantId,
            ..
          }
    checkToUpdatePayerVpa existingMandateEntry autoPayStatus =
      case existingMandateEntry of
        Just mandateEntry -> (mandateEntry.status /= DM.ACTIVE && autoPayStatus /= Just DI.SUSPENDED) || (mandateEntry.status == DM.ACTIVE && isNothing (mandateEntry.payerVpa))
        Nothing -> True
    mkWebhookData webhookDataEntity = do
      now <- getCurrentTime
      id <- generateGUID
      shortId <- generateShortId
      batchId <- getBatchId WT.MANDATE
      case subsConfig.webhookConfig of
        Just webhookConfig -> do
          webhookData <- do
            return $
              DW.Webhook
                { batchId = batchId,
                  city = merchantOpCityId.getId,
                  createdAt = now,
                  eventName = WT.MANDATE,
                  extMerchantName = "",
                  id = id,
                  lastTriedAt = now,
                  merchantId = merchantId.getId,
                  mode = webhookConfig.webhookDeliveryMode,
                  responseCode = Nothing,
                  responseMessage = Nothing,
                  retryCount = 0,
                  shortId = shortId,
                  status = WT.PENDING,
                  webhookData = toJSON webhookDataEntity,
                  updatedAt = now
                }
          QWeb.create webhookData
          return $ Just webhookData
        Nothing -> return Nothing

    makeWebhookData person mandate = do
      veh <- QVeh.findById person.id >>= fromMaybeM (VehicleNotFound $ "driverId:-" <> person.id.getId)
      unencryptedMobileNumber <- mapM decrypt person.mobileNumber
      now <- getCurrentTime
      return $
        AWebhook.WebhookDataEntity
          { driver_id = person.id.getId,
            mandate_id = mandate.id.getId,
            vehicle_number = veh.registrationNo,
            phone_number = fromMaybe "6666666666" unencryptedMobileNumber,
            name = person.firstName <> maybe "" (" " <>) person.middleName <> maybe "" (" " <>) person.lastName,
            org_id = "",
            mandate_created_at = mandate.createdAt,
            order_id = orderId,
            mandate_status = show mandate.status,
            event_name = show WT.MANDATE,
            last_sent_at = show now
          }
    callWebhookInFork driver mandateData = do
      fork "send wehook" $ do
        dataWebhook <- makeWebhookData driver mandateData
        mbWebhook <- mkWebhookData dataWebhook
        whenJust mbWebhook $ \webhook -> do
          createWebhookJob subsConfig webhook

createWebhookJob :: (MonadFlow m, CacheFlow m r, EsqDBReplicaFlow m r, EsqDBFlow m r, JobCreatorEnv r, HasField "schedulerType" r SchedulerType) => DSC.SubscriptionConfig -> DW.Webhook -> m ()
createWebhookJob subsConfig webhook = do
  whenJust subsConfig.webhookConfig $ \webhookConfig -> do
    case webhookConfig.webhookDeliveryMode of
      WT.BATCHING -> mkJob Nothing webhook.batchId webhookConfig
      WT.REAL_TIME -> mkJob (Just webhook.id) webhook.id.getId webhookConfig
  where
    mkJob webhookId keyId webhookConfig = do
      whenJust subsConfig.extWebhookConfigs $ \extWebhookConfigs -> do
        jobKeyExists :: Maybe Bool <- Hedis.get (AWebhook.mkWebhookRealTimeKey keyId)
        unless (isJust jobKeyExists) $ do
          let jobData =
                AWebhook.WebhookJobInfo
                  { mode = webhookConfig.webhookDeliveryMode,
                    webhookId = webhookId,
                    statusToCheck = [WT.FAILED, WT.PENDING],
                    retryCount = Just 0,
                    retryLimit = webhookConfig.retryLimit,
                    event = Just WT.MANDATE,
                    limit = webhookConfig.batchSize,
                    webhookConfig = extWebhookConfigs,
                    nextJobScheduleTimeThreshold = webhookConfig.nextJobScheduleTimeThreshold,
                    rescheduleTimeThreshold = webhookConfig.rescheduleTimeThreshold,
                    batchId = Just webhook.batchId
                  }
          createJobIn @_ @'SendWebhookToExternal Nothing Nothing (secondsToNominalDiffTime $ Seconds webhookConfig.nextJobScheduleTimeThreshold) $
            SendWebhookToExternalJobData
              { webhookData = jobData
              }
          Hedis.setExp (AWebhook.mkWebhookRealTimeKey keyId) True (24 * 3600)

mkBatchIdKey :: WT.WebhookEvent -> Text
mkBatchIdKey event = "BatchId:Webhook:Event: " <> show event

getBatchId :: (MonadFlow m, CacheFlow m r) => WT.WebhookEvent -> m Text
getBatchId event = do
  batchId :: Maybe Text <- Hedis.get $ mkBatchIdKey event
  case batchId of
    Just batchId' -> return batchId'
    Nothing -> do
      shortId <- generateShortId
      Hedis.setExp (mkBatchIdKey event) shortId.getShortId (12 * 3600)
      return shortId.getShortId
