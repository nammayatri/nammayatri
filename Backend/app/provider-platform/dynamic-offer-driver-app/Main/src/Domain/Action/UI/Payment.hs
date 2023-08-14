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
  )
where

import Domain.Types.DriverFee
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Mandate as DM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DP
import Environment
-- import qualified EulerHS.Language as L

import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Juspay as Juspay
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Payment.Types as Payment
import Kernel.Prelude
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
import SharedLogic.Merchant
import qualified SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.DriverInformation as CDI
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverPlan as QDP
import qualified Storage.Queries.Invoice as QIN
import qualified Storage.Queries.Mandate as QM
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Notifications
import qualified Tools.Payment as Payment

-- create order -----------------------------------------------------
createOrder :: (Id DP.Person, Id DM.Merchant) -> Id DriverFee -> Flow Payment.CreateOrderResp
createOrder (driverId, merchantId) driverFeeId = do
  driverFee <- B.runInReplica $ QDF.findById driverFeeId >>= fromMaybeM (DriverFeeNotFound $ getId driverFeeId)
  mbInvoice <- B.runInReplica $ QIN.findByDriverFeeId driverFee.id
  (createOrderResp, _) <- SPayment.createOrder (driverId, merchantId) [driverFee] Nothing (getIdAndShortId <$> mbInvoice) -- To do rectify the last param based on invoice flow --
  return createOrderResp
  where
    getIdAndShortId inv = (inv.id, inv.invoiceShortId)

getOrder :: (Id DP.Person, Id DM.Merchant) -> Id DOrder.PaymentOrder -> Flow DOrder.PaymentOrderAPIEntity
getOrder (personId, _) orderId = do
  order <- QOrder.findById orderId >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
  -- order <- QOrder.findById orderId >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
  unless (order.personId == cast personId) $ throwError NotAnExecutor
  mkOrderAPIEntity order

mkOrderAPIEntity :: DOrder.PaymentOrder -> Flow DOrder.PaymentOrderAPIEntity
mkOrderAPIEntity DOrder.PaymentOrder {..} = do
  clientAuthToken_ <- decrypt clientAuthToken
  return $ DOrder.PaymentOrderAPIEntity {clientAuthToken = clientAuthToken_, ..}

-- order status -----------------------------------------------------

getStatus :: (Id DP.Person, Id DM.Merchant) -> Id DOrder.PaymentOrder -> Flow DPayment.PaymentStatusResp
getStatus (personId, merchantId) orderId = do
  let commonPersonId = cast @DP.Person @DPayment.Person personId
      orderStatusCall = Payment.orderStatus merchantId -- api call
  mOrder <- QOrder.findById orderId
  order <-
    case mOrder of -- handling backward compatibility case of jatri saathi
      Just _order -> return _order
      Nothing -> do
        invoice <- QIN.findByDriverFeeId (cast orderId) >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
        QOrder.findById (cast invoice.id) >>= fromMaybeM (PaymentOrderNotFound invoice.id.getId)

  paymentStatus <- DPayment.orderStatusService commonPersonId order.id orderStatusCall

  unless (order.status /= Payment.CHARGED) $ do
    case paymentStatus of
      DPayment.MandatePaymentStatus {..} -> do
        processPayment merchantId (cast order.personId) order.id (shouldSendSuccessNotification mandateStatus)
        processMandate (cast order.personId) DM.INACTIVE mandateStartDate mandateEndDate (Id mandateId) mandateMaxAmount payerVpa
        processMandateStatus mandateStatus mandateId
      DPayment.PaymentStatus _ -> do
        processPayment merchantId (cast order.personId) order.id True
  notifyAndUpdateInvoiceStatusIfPaymentFailed personId order.id order.status
  pure paymentStatus

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
              unless (transactionStatus /= Payment.CHARGED) $ do
                processPayment merchantId (cast order.personId) order.id True
              notifyAndUpdateInvoiceStatusIfPaymentFailed (cast order.personId) order.id transactionStatus
            Payment.MandateOrderStatusResp {..} -> do
              order <- QOrder.findByShortId (ShortId orderShortId) >>= fromMaybeM (PaymentOrderNotFound orderShortId)
              unless (transactionStatus /= Payment.CHARGED) $ do
                processPayment merchantId (cast order.personId) order.id (shouldSendSuccessNotification mandateStatus)
                processMandate (cast order.personId) DM.INACTIVE mandateStartDate mandateEndDate (Id mandateId) mandateMaxAmount payerVpa
                processMandateStatus mandateStatus mandateId
              notifyAndUpdateInvoiceStatusIfPaymentFailed (cast order.personId) order.id transactionStatus
            Payment.MandateStatusResp {..} -> do
              processMandateStatus status mandateId
            Payment.BadStatusResp -> pure ()
      pure Ack
    _ -> throwError $ InternalError "Unknown Service Config"

processPayment :: Id DM.Merchant -> Id DP.Person -> Id DOrder.PaymentOrder -> Bool -> Flow ()
processPayment merchantId driverId orderId sendNotification = do
  driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  driverInfo <- CDI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  transporterConfig <- SCT.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  invoices <- QIN.findAllByInvoiceId (cast orderId)
  let driverFeeIds = (.driverFeeId) <$> invoices
  Redis.whenWithLockRedis (paymentProcessingLockKey driverId.getId) 60 $ do
    CDI.updatePendingPayment False (cast driverId)
    CDI.updateSubscription True (cast driverId)
    QDF.updateStatusByIds CLEARED driverFeeIds now
    QDFS.clearPaymentStatus driverId driverInfo.active
    QIN.updateInvoiceStatusByInvoiceId (cast orderId) INV.SUCCESS
    when sendNotification $ notifyPaymentSuccessIfNotNotified driver orderId

notifyPaymentSuccessIfNotNotified :: DP.Person -> Id DOrder.PaymentOrder -> Flow ()
notifyPaymentSuccessIfNotNotified driver orderId = do
  let key = "driver-offer:SuccessNotif-" <> orderId.getId
  sendNotificationIfNotSent key $ do
    notifyPaymentSuccess driver.merchantId driver.id driver.deviceToken orderId

shouldSendSuccessNotification :: Payment.MandateStatus -> Bool
shouldSendSuccessNotification mandateStatus = mandateStatus `notElem` [Payment.REVOKED, Payment.FAILURE, Payment.EXPIRED, Payment.PAUSED]

notifyAndUpdateInvoiceStatusIfPaymentFailed :: Id DP.Person -> Id DOrder.PaymentOrder -> Payment.TransactionStatus -> Flow ()
notifyAndUpdateInvoiceStatusIfPaymentFailed driverId orderId orderStatus = do
  when (orderStatus `elem` [Payment.AUTHENTICATION_FAILED, Payment.AUTHORIZATION_FAILED, Payment.JUSPAY_DECLINED]) $ do
    QIN.updateInvoiceStatusByInvoiceId (cast orderId) INV.FAILED
    let key = "driver-offer:FailedNotif-" <> orderId.getId
    sendNotificationIfNotSent key $ do
      driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      notifyPaymentFailed driver.merchantId driver.id driver.deviceToken orderId

sendNotificationIfNotSent :: Text -> Flow () -> Flow ()
sendNotificationIfNotSent key actions = do
  isNotificationSent <- fromMaybe False <$> Hedis.get key
  unless isNotificationSent $ do
    Hedis.setExp key True 86400 -- 24 hours
    actions

processMandate :: Id DP.Person -> DM.MandateStatus -> UTCTime -> UTCTime -> Id DM.Mandate -> HighPrecMoney -> Maybe Text -> Flow ()
processMandate driverId mandateStatus startDate endDate mandateId maxAmount payerVpa = do
  Redis.whenWithLockRedis (paymentProcessingLockKey driverId.getId) 60 $ do
    mbExistingMandate <- QM.findById mandateId
    case mbExistingMandate of
      Just mandate -> QM.updateStatus mandate.id mandateStatus
      Nothing -> QM.create =<< mkMandate
    QDP.updateMandateIdByDriverId driverId mandateId
  where
    mkMandate = do
      now <- getCurrentTime
      return $
        DM.Mandate
          { id = mandateId,
            status = mandateStatus,
            createdAt = now,
            updatedAt = now,
            payerVpa = payerVpa,
            ..
          }

processMandateStatus :: Payment.MandateStatus -> Text -> Flow ()
processMandateStatus mandateStatus mandateId = do
  when (mandateStatus == Payment.ACTIVE) $ do
    driverPlan <- QDP.findByMandateId (Id mandateId) >>= fromMaybeM (NoCurrentPlanForDriver mandateId)
    Redis.whenWithLockRedis (paymentProcessingLockKey driverPlan.driverId.getId) 60 $ do
      QM.updateStatus (Id mandateId) DM.ACTIVE
      QDP.updatePaymentModeByDriverId (cast driverPlan.driverId) DP.AUTOPAY
      CDI.updateSubscription True (cast driverPlan.driverId)
      CDI.updateAutoPayStatus (castAutoPayStatus mandateStatus) (cast driverPlan.driverId)
  when (mandateStatus `elem` [Payment.REVOKED, Payment.FAILURE, Payment.EXPIRED, Payment.PAUSED]) $ do
    driverPlan <- QDP.findByMandateId (Id mandateId) >>= fromMaybeM (NoCurrentPlanForDriver mandateId)
    driver <- B.runInReplica $ QP.findById driverPlan.driverId >>= fromMaybeM (PersonDoesNotExist driverPlan.driverId.getId)
    Redis.whenWithLockRedis (paymentProcessingLockKey driverPlan.driverId.getId) 60 $ do
      QM.updateStatus (Id mandateId) DM.INACTIVE
      QDP.updatePaymentModeByDriverId (cast driverPlan.driverId) DP.MANUAL
      CDI.updateAutoPayStatus (castAutoPayStatus mandateStatus) (cast driverPlan.driverId)
      when (mandateStatus == Payment.PAUSED) $
        notifyPaymentModeManualOnPause driver.merchantId driver.id driver.deviceToken
      when (mandateStatus == Payment.REVOKED) $
        notifyPaymentModeManualOnCancel driver.merchantId driver.id driver.deviceToken
  where
    castAutoPayStatus = \case
      Payment.CREATED -> Just DI.PENDING
      Payment.ACTIVE -> Just DI.ACTIVE
      Payment.REVOKED -> Just DI.CANCELLED_PSP
      Payment.FAILURE -> Nothing
      Payment.PAUSED -> Just DI.PAUSED_PSP
      Payment.EXPIRED -> Nothing
