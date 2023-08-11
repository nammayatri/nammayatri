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
import qualified Domain.Types.Mandate as DM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DP
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Juspay as Juspay
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Payment.Types as Payment
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (Value)
import qualified Kernel.Storage.Hedis as Redis
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
import Tools.Error
import qualified Tools.Payment as Payment

-- create order -----------------------------------------------------
createOrder :: (Id DP.Person, Id DM.Merchant) -> Id DriverFee -> Flow Payment.CreateOrderResp
createOrder (driverId, merchantId) driverFeeId = do
  driverFee <- runInReplica $ QDF.findById driverFeeId >>= fromMaybeM (DriverFeeNotFound $ getId driverFeeId)
  (createOrderResp, _) <- SPayment.createOrder (driverId, merchantId) [driverFee] Nothing
  return createOrderResp

getOrder :: (Id DP.Person, Id DM.Merchant) -> Id DOrder.PaymentOrder -> Flow DOrder.PaymentOrderAPIEntity
getOrder (personId, _) orderId = do
  order <- runInReplica $ QOrder.findById orderId >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
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
  paymentStatus <- DPayment.orderStatusService commonPersonId orderId orderStatusCall
  order <- runInReplica $ QOrder.findById orderId >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
  processPayment merchantId (cast order.personId) orderId
  unless (order.status /= Payment.CHARGED) $ do
    case paymentStatus of
      DPayment.MandatePaymentStatus {..} -> processMandate (cast order.personId) DM.ACTIVE mandateStartDate mandateEndDate mandateId mandateMaxAmount
      _ -> pure ()
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
              unless (transactionStatus /= Payment.CHARGED) $ do
                order <- runInReplica $ QOrder.findByShortId (ShortId orderShortId) >>= fromMaybeM (PaymentOrderNotFound orderShortId)
                processPayment merchantId (cast order.personId) order.id
            Payment.MandateOrderStatusResp {..} -> do
              unless (transactionStatus /= Payment.CHARGED) $ do
                order <- runInReplica $ QOrder.findByShortId (ShortId orderShortId) >>= fromMaybeM (PaymentOrderNotFound orderShortId)
                processPayment merchantId (cast order.personId) order.id
                processMandate (cast order.personId) DM.INACTIVE mandateStartDate mandateEndDate mandateId mandateMaxAmount
                processMandateStatus mandateStatus mandateId
            Payment.MandateStatusResp {..} -> do
              processMandateStatus status mandateId
            Payment.BadStatusResp -> pure ()
      pure Ack
    _ -> throwError $ InternalError "Unknown Service Config"

processPayment :: Id DM.Merchant -> Id DP.Person -> Id DOrder.PaymentOrder -> Flow ()
processPayment merchantId driverId orderId = do
  driverInfo <- CDI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  transporterConfig <- SCT.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  invoices <- runInReplica $ QIN.findAllByInvoiceId (cast orderId)
  let driverFeeIds = (.driverFeeId) <$> invoices
  Redis.whenWithLockRedis (paymentProcessingLockKey driverId.getId) 60 $ do
    CDI.updatePendingPayment False (cast driverId)
    CDI.updateSubscription True (cast driverId)
    Esq.runTransaction $ do
      QDF.updateStatusByIds CLEARED driverFeeIds now
      QDFS.clearPaymentStatus driverId driverInfo.active

processMandate :: Id DP.Person -> DM.MandateStatus -> UTCTime -> UTCTime -> Text -> HighPrecMoney -> Flow ()
processMandate driverId mandateStatus startDate endDate mandateId maxAmount = do
  Redis.whenWithLockRedis (paymentProcessingLockKey mandateId) 60 $ do
    mbExistingMandate <- runInReplica $ QM.findById (Id mandateId)
    case mbExistingMandate of
      Just mandate -> Esq.runNoTransaction $ QM.updateStatus mandate.id mandateStatus
      Nothing -> Esq.runTransaction $ QM.create =<< mkMandate
  Redis.whenWithLockRedis (paymentProcessingLockKey driverId.getId) 60 $ do
    Esq.runTransaction $ QDP.updateMandateIdByDriverId driverId (Id mandateId)
  where
    mkMandate = do
      now <- getCurrentTime
      return $
        DM.Mandate
          { id = Id mandateId,
            status = mandateStatus,
            createdAt = now,
            updatedAt = now,
            ..
          }

processMandateStatus :: Payment.MandateStatus -> Text -> Flow ()
processMandateStatus mandateStatus mandateId = do
  when (mandateStatus == Payment.ACTIVE) $ do
    driverPlan <- runInReplica $ QDP.findByMandateId (Id mandateId) >>= fromMaybeM (NoCurrentPlanForDriver mandateId)
    Esq.runTransaction $ do
      QM.updateStatus (Id mandateId) DM.ACTIVE
      QDP.updatePaymentModeByDriverId (cast driverPlan.driverId) DP.AUTOPAY
    CDI.updateSubscription True (cast driverPlan.driverId)
    CDI.updateAutoPayStatus (castAutoPayStatus mandateStatus) (cast driverPlan.driverId)
  when (mandateStatus `elem` [Payment.REVOKED, Payment.FAILURE, Payment.EXPIRED, Payment.PAUSED]) $ do
    driverPlan <- runInReplica $ QDP.findByMandateId (Id mandateId) >>= fromMaybeM (NoCurrentPlanForDriver mandateId)
    Esq.runTransaction $ do
      QM.updateStatus (Id mandateId) DM.INACTIVE
      QDP.updatePaymentModeByDriverId (cast driverPlan.driverId) DP.MANUAL
    CDI.updateAutoPayStatus (castAutoPayStatus mandateStatus) (cast driverPlan.driverId)
  where
    castAutoPayStatus = \case
      Payment.CREATED -> Nothing
      Payment.ACTIVE -> Just DI.ACTIVE
      Payment.REVOKED -> Just DI.CANCELLED_PSP
      Payment.FAILURE -> Nothing
      Payment.PAUSED -> Just DI.PAUSED_PSP
      Payment.EXPIRED -> Nothing
