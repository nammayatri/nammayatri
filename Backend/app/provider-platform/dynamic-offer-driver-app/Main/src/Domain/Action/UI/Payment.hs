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
    juspayWebhookHandler,
  )
where

import Domain.Types.DriverFee
import qualified Domain.Types.DriverFee as DF
import Domain.Types.DriverInformation (DriverInformation)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import Environment
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Juspay as Juspay
import qualified Kernel.External.Payment.Juspay.Types as Juspay
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (Value)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Servant (BasicAuthData)
import SharedLogic.Merchant
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.DriverInformation as CDI
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Metrics
import qualified Tools.Payment as Payment

-- create order -----------------------------------------------------

createOrder ::
  (Id DP.Person, Id DM.Merchant) ->
  Id DriverFee ->
  Flow Payment.CreateOrderResp
createOrder (driverId, merchantId) driverFeeId = do
  driverFee <- runInReplica $ QDF.findById driverFeeId >>= fromMaybeM (DriverFeeNotFound $ getId driverFeeId)
  when (driverFee.status `elem` [CLEARED, EXEMPTED]) $ throwError (DriverFeeAlreadySettled $ getId driverFeeId)
  when (driverFee.status `elem` [INACTIVE, ONGOING]) $ throwError (DriverFeeNotInUse $ getId driverFeeId)
  -- driver <- runInReplica $ QP.findById (cast driverFee.driverId) >>= fromMaybeM (PersonNotFound $ getId driverFee.driverId)
  driver <- QP.findById (cast driverFee.driverId) >>= fromMaybeM (PersonNotFound $ getId driverFee.driverId)
  unless (driver.id == driverId) $ throwError NotAnExecutor
  driverPhone <- driver.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  let driverEmail = fromMaybe "test@juspay.in" driver.email
  pendingFees <- QDF.findPendingFeesByDriverFeeId driverFee.id >>= fromMaybeM (DriverFeeNotFound $ getId driverFeeId)
  let pendingAmount = fromIntegral pendingFees.govtCharges + fromIntegral pendingFees.platformFee.fee + pendingFees.platformFee.cgst + pendingFees.platformFee.sgst
  let createOrderReq =
        Payment.CreateOrderReq
          { orderShortId = driverFee.shortId.getShortId,
            amount = round pendingAmount,
            customerId = driver.id.getId,
            customerEmail = driverEmail,
            customerPhone = driverPhone,
            paymentPageClientId = driver.id.getId,
            customerFirstName = Just driver.firstName,
            customerLastName = driver.lastName
          }

  let commonMerchantId = cast @DM.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @DP.Person @DPayment.Person driver.id
      orderId = cast @DriverFee @DOrder.PaymentOrder driverFee.id
      createOrderCall = Payment.createOrder merchantId -- api call
  DPayment.createOrderService commonMerchantId commonPersonId orderId createOrderReq createOrderCall

-- order status -----------------------------------------------------

getStatus ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  Id DOrder.PaymentOrder ->
  m DPayment.PaymentStatusResp
getStatus (personId, merchantId) orderId = do
  let commonPersonId = cast @DP.Person @DPayment.Person personId
      orderStatusCall = Payment.orderStatus merchantId -- api call
  paymentStatus <- DPayment.orderStatusService commonPersonId orderId orderStatusCall
  driverInfo <- QDI.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  transporterConfig <- SCT.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  case paymentStatus.status of
    Juspay.CHARGED -> do
      CDI.updatePendingPayment False (cast personId)
      CDI.updateSubscription True (cast personId)
      -- Esq.runTransaction $ processPaymentTransactions (cast personId) (cast orderId) driverInfo transporterConfig.timeDiffFromUtc
      processPaymentTransactions (cast personId) (cast orderId) driverInfo transporterConfig.timeDiffFromUtc
    _ -> pure ()
  pure paymentStatus

-- webhook ----------------------------------------------------------

processPaymentTransactions :: (L.MonadFlow m, MonadTime m) => Id DP.Driver -> Id DriverFee -> DriverInformation -> Seconds -> m ()
processPaymentTransactions driverId driverFeeId driverInfo timeDiff = do
  now <- getLocalCurrentTime timeDiff
  QDF.updateStatus DF.CLEARED driverFeeId now
  QDFS.clearPaymentStatus (cast driverId) driverInfo.active

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
      orderStatusContent <- Juspay.orderStatusWebhook psc DPayment.juspayWebhookService authData value
      case orderStatusContent of
        Nothing -> throwError $ InternalError "Order Contents not found."
        Just osc -> do
          let driverFeeId = Id osc.order.order_id
          driverFee <- QDF.findById driverFeeId >>= fromMaybeM (DriverFeeNotFound driverFeeId.getId)
          driverInfo <- QDI.findById (cast driverFee.driverId) >>= fromMaybeM (PersonNotFound driverFee.driverId.getId)
          transporterConfig <- SCT.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
          unless (osc.order.status /= Juspay.CHARGED) $ do
            CDI.updatePendingPayment False driverFee.driverId
            CDI.updateSubscription True driverFee.driverId
            -- Esq.runTransaction $ processPaymentTransactions driverFee.driverId driverFee.id driverInfo transporterConfig.timeDiffFromUtc
            processPaymentTransactions driverFee.driverId driverFee.id driverInfo transporterConfig.timeDiffFromUtc
          pure Ack
    _ -> throwError $ InternalError "Unknown Service Config"
