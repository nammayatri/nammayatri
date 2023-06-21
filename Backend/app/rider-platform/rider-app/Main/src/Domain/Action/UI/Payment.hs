{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Payment
  ( PaymentStatusResp (..),
    createOrder,
    getStatus,
    juspayWebhookHandler,
  )
where

import Data.Coerce (coerce)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Domain.Types.Payment.PaymentOrder as DOrder
import qualified Domain.Types.Payment.PaymentTransaction as DTransaction
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Juspay as Juspay
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (Value)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant (BasicAuthData)
import SharedLogic.Merchant
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.Queries.Payment.PaymentOrder as QOrder
import qualified Storage.Queries.Payment.PaymentTransaction as QTransaction
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Metrics
import qualified Tools.Payment as Payment

-- newtype CreateOrderRequest = CreateOrderRequest
--   {}
--   deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype PaymentStatusResp = PaymentStatusResp
  { status :: Payment.TransactionStatus
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

---------------------------------------------------------------------
createOrder ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  Id DRide.Ride ->
  -- CreateOrderRequest ->
  m Payment.CreateOrderResp
createOrder (personId, merchantId) rideId = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.status == DRide.COMPLETED) $ throwError (RideInvalidStatus $ show ride.status)
  totalFare <- ride.totalFare & fromMaybeM (RideFieldNotPresent "totalFare")
  person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)
  riderId <- runInReplica $ QRide.findRiderIdByRideId ride.id >>= fromMaybeM (InternalError "riderId not found")
  unless (person.id == riderId) $ throwError (InvalidRequest "Invalid riderId")
  customerEmail <- person.email & fromMaybeM (PersonFieldNotPresent "email") >>= decrypt
  customerPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  existingOrder <- runInReplica $ QOrder.findById $ cast @DRide.Ride @DOrder.PaymentOrder rideId
  whenJust existingOrder \_ -> throwError (InvalidRequest "Payment order already exists")
  let createOrderReq =
        Payment.CreateOrderReq
          { orderShortId = ride.shortId.getShortId, -- should be Alphanumeric with character length less than 18.
            amount = totalFare,
            customerId = person.id.getId,
            customerEmail,
            customerPhone,
            paymentPageClientId = person.id.getId, -- FIXME
            customerFirstName = person.firstName,
            customerLastName = person.lastName
          }
  createOrderResp <- Payment.createOrder merchantId createOrderReq
  paymentOrder <- buildPaymentOrder merchantId personId totalFare ride createOrderResp
  Esq.runTransaction $
    QOrder.create paymentOrder
  pure createOrderResp

buildPaymentOrder :: EncFlow m r => Id DM.Merchant -> Id DP.Person -> Money -> DRide.Ride -> Payment.CreateOrderResp -> m DOrder.PaymentOrder
buildPaymentOrder merchantId personId totalFare ride resp = do
  now <- getCurrentTime
  clientAuthToken <- encrypt resp.sdk_payload.payload.clientAuthToken
  pure
    DOrder.PaymentOrder
      { id = cast @DRide.Ride @DOrder.PaymentOrder ride.id,
        shortId = coerce @(ShortId DRide.Ride) @(ShortId DOrder.PaymentOrder) ride.shortId,
        customerId = personId,
        merchantId = merchantId,
        amount = totalFare, -- FIXME resp.sdk_payload.payload.amount
        currency = resp.sdk_payload.payload.currency,
        status = resp.status,
        paymentLinks = fromMaybe (Payment.PaymentLinks Nothing Nothing Nothing) resp.payment_links,
        clientAuthToken,
        clientAuthTokenExpiry = resp.sdk_payload.payload.clientAuthTokenExpiry,
        getUpiDeepLinksOption = resp.sdk_payload.payload.options_getUpiDeepLinks,
        environment = resp.sdk_payload.payload.environment,
        createdAt = now,
        updatedAt = now
      }

---------------------------------------------------------------------
getStatus ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  Id DOrder.PaymentOrder ->
  m PaymentStatusResp
getStatus (personId, merchantId) orderId = do
  order <- runInReplica $ QOrder.findById orderId >>= fromMaybeM (PaymentOrderDoesNotExist orderId.getId)
  unless (personId == order.customerId) $ throwError (InvalidRequest "Invalid riderId")
  let orderStatusReq = Payment.OrderStatusReq {orderShortId = order.shortId.getShortId}
  orderStatusResp <- Payment.orderStatus merchantId orderStatusReq
  updateOrderTransaction order orderStatusResp Nothing
  return $ PaymentStatusResp {status = orderStatusResp.transactionStatus}

buildPaymentTransaction :: MonadFlow m => DOrder.PaymentOrder -> Payment.OrderStatusResp -> Maybe Text -> m DTransaction.PaymentTransaction
buildPaymentTransaction order Payment.OrderStatusResp {..} respDump = do
  uuid <- generateGUID
  now <- getCurrentTime
  pure
    DTransaction.PaymentTransaction
      { id = uuid,
        orderId = order.id,
        merchantId = order.merchantId,
        txnUUID = transactionUUID,
        statusId = transactionStatusId,
        status = transactionStatus,
        createdAt = now,
        updatedAt = now,
        juspayResponse = respDump,
        ..
      }

updateOrderTransaction ::
  ( EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CoreMetrics m
  ) =>
  DOrder.PaymentOrder ->
  Payment.OrderStatusResp ->
  Maybe Text ->
  m ()
updateOrderTransaction order resp respDump = do
  mbTransaction <- runInReplica $ QTransaction.findByTxnUUID resp.transactionUUID
  let updOrder = order{status = resp.transactionStatus}
  case mbTransaction of
    Nothing -> do
      transaction <- buildPaymentTransaction order resp respDump
      Esq.runTransaction $ do
        QTransaction.create transaction
        when (order.status /= updOrder.status) $ QOrder.updateStatus updOrder
    Just transaction -> do
      let updTransaction =
            transaction{statusId = resp.transactionStatusId,
                        status = resp.transactionStatus,
                        paymentMethodType = resp.paymentMethod,
                        paymentMethod = resp.paymentMethodType,
                        respMessage = resp.respMessage,
                        respCode = resp.respCode,
                        gatewayReferenceId = resp.gatewayReferenceId,
                        amount = resp.amount,
                        currency = resp.currency,
                        juspayResponse = respDump
                       }
      Esq.runTransaction $ do
        QTransaction.updateMultiple updTransaction
        when (order.status /= updOrder.status) $ QOrder.updateStatus updOrder

---------------------------------------------------------------------
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
      >>= fromMaybeM (InternalError $ "No payment service provider configured for the merchant, merchantId:" <> merchantId.getId)
  case merchantServiceConfig.serviceConfig of
    DMSC.PaymentServiceConfig psc -> do
      Juspay.orderStatusWebhook psc orderStatus authData value
    _ -> throwError $ InternalError "Unknown Service Config"

orderStatus ::
  ( EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CoreMetrics m
  ) =>
  Payment.OrderStatusResp ->
  Text ->
  m AckResponse
orderStatus resp respDump = do
  let orderShortId = ShortId resp.orderShortId
  order <- runInReplica $ QOrder.findByShortId orderShortId >>= fromMaybeM (PaymentOrderNotFound resp.orderShortId)
  updateOrderTransaction order resp $ Just respDump
  return Ack
