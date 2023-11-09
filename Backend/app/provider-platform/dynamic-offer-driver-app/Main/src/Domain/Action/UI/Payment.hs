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

import Domain.Action.UI.Ride.EndRide.Internal
import Domain.Types.DriverFee
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Mandate as DM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Domain.Types.Notification (Notification)
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DP
import Environment
import Kernel.Beam.Functions as B (runInReplica)
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as DPayments
import qualified Kernel.External.Payment.Interface.Juspay as Juspay
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Payment.Juspay.Types as Juspay
import qualified Kernel.External.Payment.Types as Payment
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow, Transactionable)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import Servant (BasicAuthData)
import qualified SharedLogic.DriverFee as SLDriverFee
import SharedLogic.Merchant
import qualified SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import Storage.Queries.DriverPlan (findByDriverId)
import qualified Storage.Queries.DriverPlan as QDP
import qualified Storage.Queries.Invoice as QIN
import qualified Storage.Queries.Mandate as QM
import qualified Storage.Queries.Notification as QNTF
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Notifications
import qualified Tools.Payment as Payment
import qualified Tools.PaymentNudge as PaymentNudge

-- create order -----------------------------------------------------
createOrder :: (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id INV.Invoice -> Flow Payment.CreateOrderResp
createOrder (driverId, merchantId, _) invoiceId = do
  invoices <- B.runInReplica $ QIN.findAllByInvoiceId invoiceId
  driverFees <- (B.runInReplica . QDF.findById . (.driverFeeId)) `mapM` invoices
  (createOrderResp, _) <- SPayment.createOrder (driverId, merchantId) (catMaybes driverFees, []) Nothing INV.MANUAL_INVOICE (getIdAndShortId <$> listToMaybe invoices)
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

getStatus :: (ServiceFlow m r, Transactionable m, EncFlow m r, EsqDBReplicaFlow m r, EsqDBFlow m r, CacheFlow m r, MonadFlow m) => (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id DOrder.PaymentOrder -> m DPayment.PaymentStatusResp
getStatus (personId, merchantId, _) orderId = do
  let commonPersonId = cast @DP.Person @DPayment.Person personId
      orderStatusCall = Payment.orderStatus merchantId -- api call
  order <- QOrder.findById orderId >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
  if order.status == Payment.CHARGED -- Consider CHARGED status as terminal status
    then do
      invoice <- listToMaybe <$> QIN.findById (cast orderId)
      return $ DPayment.PaymentStatus {status = order.status, bankErrorCode = invoice >>= (.bankErrorCode), bankErrorMessage = invoice >>= (.bankErrorMessage)}
    else do
      paymentStatus <- DPayment.orderStatusService commonPersonId orderId orderStatusCall
      case paymentStatus of
        DPayment.MandatePaymentStatus {..} -> do
          unless (status /= Payment.CHARGED) $ do
            processPayment merchantId (cast order.personId) order.id (shouldSendSuccessNotification mandateStatus)
          processMandate (cast order.personId) mandateStatus (Just mandateStartDate) (Just mandateEndDate) (Id mandateId) mandateMaxAmount payerVpa upi --- needs refactoring ----
          QIN.updateBankErrorsByInvoiceId bankErrorMessage bankErrorCode (cast order.id)
          notifyAndUpdateInvoiceStatusIfPaymentFailed personId order.id status Nothing bankErrorCode False
        DPayment.PaymentStatus {..} -> do
          unless (status /= Payment.CHARGED) $ do
            processPayment merchantId (cast order.personId) order.id True
          QIN.updateBankErrorsByInvoiceId bankErrorMessage bankErrorCode (cast order.id)
          notifyAndUpdateInvoiceStatusIfPaymentFailed personId order.id status Nothing Nothing False
        DPayment.PDNNotificationStatusResp {..} ->
          processNotification notificationId notificationStatus
      return paymentStatus

-- webhook ----------------------------------------------------------

juspayWebhookHandler ::
  ShortId DM.Merchant ->
  BasicAuthData ->
  Value ->
  Flow AckResponse
juspayWebhookHandler merchantShortId authData value = do
  merchant <- findMerchantByShortId merchantShortId
  let merchantId = merchant.id
  merchantServiceConfig <-
    CQMSC.findByMerchantIdAndService merchantId (DMSC.PaymentService Payment.Juspay)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show Payment.Juspay))
  case merchantServiceConfig.serviceConfig of
    DMSC.PaymentServiceConfig psc -> do
      orderStatusResp <- Juspay.orderStatusWebhook psc DPayment.juspayWebhookService authData value
      case orderStatusResp of
        Nothing -> throwError $ InternalError "Order Contents not found."
        Just osr -> do
          case osr of
            Payment.OrderStatusResp {..} -> do
              order <- QOrder.findByShortId (ShortId orderShortId) >>= fromMaybeM (PaymentOrderNotFound orderShortId)
              when (order.status /= Payment.CHARGED || order.status == transactionStatus) $ do
                unless (transactionStatus /= Payment.CHARGED) $ do
                  processPayment merchantId (cast order.personId) order.id True
                notifyAndUpdateInvoiceStatusIfPaymentFailed (cast order.personId) order.id transactionStatus eventName bankErrorCode True
                QIN.updateBankErrorsByInvoiceId bankErrorMessage bankErrorCode (cast order.id)
            Payment.MandateOrderStatusResp {..} -> do
              order <- QOrder.findByShortId (ShortId orderShortId) >>= fromMaybeM (PaymentOrderNotFound orderShortId)
              when (order.status /= Payment.CHARGED || order.status == transactionStatus) $ do
                unless (transactionStatus /= Payment.CHARGED) $ do
                  processPayment merchantId (cast order.personId) order.id (shouldSendSuccessNotification mandateStatus)
                processMandate (cast order.personId) mandateStatus mandateStartDate mandateEndDate (Id mandateId) mandateMaxAmount payerVpa upi
                notifyAndUpdateInvoiceStatusIfPaymentFailed (cast order.personId) order.id transactionStatus eventName bankErrorCode True
                QIN.updateBankErrorsByInvoiceId bankErrorMessage bankErrorCode (cast order.id)
            ---- to do add FCM for insufficient funds  -----
            Payment.MandateStatusResp {..} -> do
              order <- QOrder.findByShortId (ShortId orderShortId) >>= fromMaybeM (PaymentOrderNotFound orderShortId)
              processMandate (cast order.personId) status mandateStartDate mandateEndDate (Id mandateId) mandateMaxAmount Nothing Nothing
            Payment.PDNNotificationStatusResp {..} ->
              processNotification notificationId notificationStatus
            Payment.BadStatusResp -> pure ()
      pure Ack
    _ -> throwError $ InternalError "Unknown Service Config"

processPayment :: (MonadFlow m, CacheFlow m r, EsqDBReplicaFlow m r, EsqDBFlow m r) => Id DM.Merchant -> Id DP.Person -> Id DOrder.PaymentOrder -> Bool -> m ()
processPayment merchantId driverId orderId sendNotification = do
  driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  transporterConfig <- SCT.findByMerchantOpCityId driver.merchantOperatingCityId >>= fromMaybeM (TransporterConfigNotFound driver.merchantOperatingCityId.getId)
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  invoices <- QIN.findAllByInvoiceId (cast orderId)
  let invoice = listToMaybe invoices
  when ((invoice <&> (.paymentMode)) == Just INV.AUTOPAY_INVOICE && (invoice <&> (.invoiceStatus)) == Just INV.ACTIVE_INVOICE) $ do
    maybe (pure ()) (QDF.updateAutopayPaymentStageById (Just EXECUTION_SUCCESS)) (invoice <&> (.driverFeeId))
  let driverFeeIds = (.driverFeeId) <$> invoices
  Redis.whenWithLockRedis (paymentProcessingLockKey driverId.getId) 60 $ do
    QDF.updateStatusByIds CLEARED driverFeeIds now
    QIN.updateInvoiceStatusByInvoiceId INV.SUCCESS (cast orderId)
    updatePaymentStatus driverId merchantId
    when sendNotification $ notifyPaymentSuccessIfNotNotified driver orderId

updatePaymentStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> Id DM.Merchant -> m ()
updatePaymentStatus driverId merchantId = do
  dueInvoices <- QDF.findAllPendingAndDueDriverFeeByDriverId (cast driverId)
  let totalDue = sum $ map (\dueInvoice -> SLDriverFee.roundToHalf $ fromIntegral dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst) dueInvoices
  when (totalDue <= 0) $ QDI.updatePendingPayment False (cast driverId)
  mbDriverPlan <- findByDriverId (cast driverId) -- what if its changed? needed inside lock?
  plan <- getPlan mbDriverPlan merchantId
  case plan of
    Nothing -> QDI.updateSubscription True (cast driverId)
    Just plan_ -> when (totalDue < plan_.maxCreditLimit) $ QDI.updateSubscription True (cast driverId)

notifyPaymentSuccessIfNotNotified :: (CacheFlow m r, EsqDBFlow m r) => DP.Person -> Id DOrder.PaymentOrder -> m ()
notifyPaymentSuccessIfNotNotified driver orderId = do
  let key = "driver-offer:SuccessNotif-" <> orderId.getId
  sendNotificationIfNotSent key 86400 $ do
    notifyPaymentSuccess driver.merchantOperatingCityId driver.id driver.deviceToken orderId

shouldSendSuccessNotification :: Payment.MandateStatus -> Bool
shouldSendSuccessNotification mandateStatus = mandateStatus `notElem` [Payment.REVOKED, Payment.FAILURE, Payment.EXPIRED, Payment.PAUSED]

notifyAndUpdateInvoiceStatusIfPaymentFailed :: (MonadFlow m, CacheFlow m r, EsqDBReplicaFlow m r, EsqDBFlow m r) => Id DP.Person -> Id DOrder.PaymentOrder -> Payment.TransactionStatus -> Maybe Juspay.PaymentStatus -> Maybe Text -> Bool -> m ()
notifyAndUpdateInvoiceStatusIfPaymentFailed driverId orderId orderStatus eventName mbBankErrorCode fromWebhook = do
  activeExecutionInvoice <- QIN.findByIdWithPaymenModeAndStatus (cast orderId) INV.AUTOPAY_INVOICE INV.ACTIVE_INVOICE
  let paymentMode = if isJust activeExecutionInvoice then DP.AUTOPAY else DP.MANUAL
  let (notifyFailure, updateFailure) = toNotifyFailure (isJust activeExecutionInvoice) eventName orderStatus
  when (updateFailure || (not fromWebhook && notifyFailure)) $ do
    QIN.updateInvoiceStatusByInvoiceId INV.FAILED (cast orderId)
    case activeExecutionInvoice of
      Just invoice' -> do
        QDF.updateAutoPayToManual invoice'.driverFeeId
        QDF.updateAutopayPaymentStageById (Just EXECUTION_FAILED) invoice'.driverFeeId
      Nothing -> pure ()
    nofityPaymentFailureIfNotNotified paymentMode

  when (notifyFailure && isJust mbBankErrorCode) $
    nofityPaymentFailureIfNotNotified paymentMode
  where
    nofityPaymentFailureIfNotNotified paymentMode = do
      let key = "driver-offer:FailedNotif-" <> orderId.getId
      sendNotificationIfNotSent key 3600 $ fork "Sending payment failure notification" (PaymentNudge.notifyPaymentFailure driverId paymentMode mbBankErrorCode)

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
pdnNotificationStatus (_, merchantId, _) notificationId = do
  pdnNotification <- QNTF.findById notificationId >>= fromMaybeM (InternalError $ "No Notification Sent With Id" <> notificationId.getId)
  resp <- Payment.mandateNotificationStatus merchantId (mkNotificationRequest pdnNotification.shortId)
  processNotification pdnNotification.shortId resp.status
  return resp
  where
    mkNotificationRequest shortNotificationId =
      DPayments.NotificationStatusReq
        { notificationId = shortNotificationId
        }

processNotification :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Text -> Payment.NotificationStatus -> m ()
processNotification notificationId notificationStatus = do
  notification <- QNTF.findByShortId notificationId >>= fromMaybeM (InternalError "notification not found")
  let driverFeeId = notification.driverFeeId
  unless (notification.status == Juspay.SUCCESS) $ do
    driverFee <- QDF.findById driverFeeId
    case notificationStatus of
      Juspay.NOTIFICATION_FAILURE -> do
        --- here based on notification status failed update driver fee to payment_overdue and reccuring invoice----
        QDF.updateAutoPayToManual driverFeeId
        QIN.updateInvoiceStatusByDriverFeeIds INV.INACTIVE [driverFeeId]
      Juspay.SUCCESS -> do
        --- based on notification status Success udpate driver fee autoPayPaymentStage to Execution scheduled -----
        unless ((driverFee <&> (.status)) == Just CLEARED) $ do
          QIN.updateInvoiceStatusByDriverFeeIds INV.ACTIVE_INVOICE [driverFeeId]
          QDF.updateManualToAutoPay driverFeeId
        QDF.updateAutopayPaymentStageById (Just EXECUTION_SCHEDULED) driverFeeId
      _ -> pure ()
    QNTF.updateNotificationStatusById notification.id notificationStatus

processMandate :: (MonadFlow m, CacheFlow m r, EsqDBReplicaFlow m r, EsqDBFlow m r) => Id DP.Person -> Payment.MandateStatus -> Maybe UTCTime -> Maybe UTCTime -> Id DM.Mandate -> HighPrecMoney -> Maybe Text -> Maybe Payment.Upi -> m ()
processMandate driverId mandateStatus startDate endDate mandateId maxAmount payerVpa upiDetails = do
  let payerApp = upiDetails >>= (.payerApp)
      payerAppName = upiDetails >>= (.payerAppName)
      mandatePaymentFlow = upiDetails >>= (.txnFlowType)
  mbExistingMandate <- QM.findById mandateId
  when (isNothing mbExistingMandate) $ QM.create =<< mkMandate payerApp payerAppName mandatePaymentFlow
  when (mandateStatus == Payment.ACTIVE) $ do
    driverPlan <- QDP.findByMandateId mandateId >>= fromMaybeM (NoDriverPlanForMandate mandateId.getId)
    Redis.withWaitOnLockRedisWithExpiry (mandateProcessingLockKey driverId.getId) 60 60 $ do
      --- do not update payer vpa from euler for older active mandates also we update only when autopayStatus not suspended because on suspend we make the mandate inactive in table
      driverInfo <- QDI.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
      let toUpdatePayerVpa = checkToUpdatePayerVpa mbExistingMandate (driverInfo.autoPayStatus)
      let payerVpa' = if toUpdatePayerVpa then payerVpa else Nothing
      QDP.updateMandateIdByDriverId driverId mandateId
      QM.updateMandateDetails mandateId DM.ACTIVE payerVpa' payerApp payerAppName mandatePaymentFlow
      QDP.updatePaymentModeByDriverId (cast driverPlan.driverId) DP.AUTOPAY
      QDP.updateMandateSetupDateByDriverId (cast driverPlan.driverId)
      QDI.updateSubscription True (cast driverPlan.driverId)
      QDI.updateAutoPayStatusAndPayerVpa (castAutoPayStatus mandateStatus) payerVpa' (cast driverPlan.driverId)
  when (mandateStatus `elem` [Payment.REVOKED, Payment.FAILURE, Payment.EXPIRED, Payment.PAUSED]) $ do
    driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
    Redis.withWaitOnLockRedisWithExpiry (mandateProcessingLockKey driverId.getId) 60 60 $ do
      QM.updateMandateDetails mandateId DM.INACTIVE Nothing payerApp Nothing mandatePaymentFlow --- should we store driver Id in mandate table ?
      mbDriverPlan <- QDP.findByMandateId mandateId
      case mbDriverPlan of
        Just driverPlan -> do
          QDI.updateAutoPayStatusAndPayerVpa (castAutoPayStatus mandateStatus) Nothing (cast driverId)
          QDP.updatePaymentModeByDriverId (cast driverPlan.driverId) DP.MANUAL
          when (mandateStatus == Payment.PAUSED) $ do
            QDF.updateAllExecutionPendingToManualOverdueByDriverId (cast driver.id)
            QIN.inActivateAllAutopayActiveInvoices (cast driver.id)
            PaymentNudge.notifyMandatePaused driver.id driver.merchantId driver.deviceToken driver.language
          when (mandateStatus == Payment.REVOKED) $ do
            QDF.updateAllExecutionPendingToManualOverdueByDriverId (cast driver.id)
            QIN.inActivateAllAutopayActiveInvoices (cast driver.id)
            PaymentNudge.notifyMandateCancelled driver.id driver.merchantId driver.deviceToken driver.language
        Nothing -> do
          driverInfo <- QDI.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
          mbDriverPlanByDriverId <- QDP.findByDriverId driverId
          let currentMandateId = mbDriverPlanByDriverId >>= (.mandateId)
              autoPayStatus = driverInfo.autoPayStatus
          when (isNothing currentMandateId || (currentMandateId /= Just mandateId) && notElem autoPayStatus [Just DI.ACTIVE, Just DI.SUSPENDED]) $ do
            QDI.updateAutoPayStatusAndPayerVpa (castAutoPayStatus mandateStatus) Nothing (cast driverId)
  where
    castAutoPayStatus = \case
      Payment.CREATED -> Just DI.PENDING
      Payment.ACTIVE -> Just DI.ACTIVE
      Payment.REVOKED -> Just DI.CANCELLED_PSP
      Payment.PAUSED -> Just DI.PAUSED_PSP
      Payment.FAILURE -> Just DI.MANDATE_FAILED
      Payment.EXPIRED -> Just DI.MANDATE_EXPIRED
    mkMandate payerApp payerAppName mandatePaymentFlow = do
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
            ..
          }
    checkToUpdatePayerVpa existingMandateEntry autoPayStatus =
      case existingMandateEntry of
        Just mandateEntry -> (mandateEntry.status /= DM.ACTIVE && autoPayStatus /= Just DI.SUSPENDED) || (mandateEntry.status == DM.ACTIVE && isNothing (mandateEntry.payerVpa))
        Nothing -> True
