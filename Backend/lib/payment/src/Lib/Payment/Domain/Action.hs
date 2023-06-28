{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Payment.Domain.Action
  ( PaymentStatusResp (..),
    createOrderService,
    orderStatusService,
    juspayWebhookService,
  )
where

import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Payment.Juspay.Types as Juspay
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (Value)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Payment.Domain.Types.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.PaymentTransaction as DTransaction
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QTransaction

newtype PaymentStatusResp = PaymentStatusResp
  { status :: Payment.TransactionStatus
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- create order -----------------------------------------------------

createOrderService ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Id DOrder.PaymentOrder ->
  Payment.CreateOrderReq ->
  (Payment.CreateOrderReq -> m Payment.CreateOrderResp) ->
  m Payment.CreateOrderResp
createOrderService merchantId personId orderId createOrderReq createOrderCall = do
  mbExistingOrder <- runInReplica $ QOrder.findById orderId
  case mbExistingOrder of
    Nothing -> do
      createOrderResp <- createOrderCall createOrderReq -- api call
      paymentOrder <- buildPaymentOrder merchantId personId orderId createOrderReq createOrderResp
      Esq.runTransaction $
        QOrder.create paymentOrder
      pure createOrderResp
    Just existingOrder -> do
      sdk_payload <- buildSDKPayload createOrderReq existingOrder
      pure
        Payment.CreateOrderResp
          { status = existingOrder.status,
            id = "ordeh_xxxxxxxxxxxxxxxxxxxx", -- not saved in db
            order_id = existingOrder.shortId.getShortId,
            payment_links = Just existingOrder.paymentLinks,
            sdk_payload
          }

buildSDKPayload :: EncFlow m r => Payment.CreateOrderReq -> DOrder.PaymentOrder -> m Juspay.SDKPayload
buildSDKPayload req order = do
  payload <- buildSDKPayloadDetails req order
  pure
    Juspay.SDKPayload
      { requestId = "12398b5571d74c3388a74004bc24370c", -- not saved in db
        service = "in.juspay.hyperpay", -- not saved in db
        payload
      }

buildSDKPayloadDetails :: EncFlow m r => Payment.CreateOrderReq -> DOrder.PaymentOrder -> m Juspay.SDKPayloadDetails
buildSDKPayloadDetails req order = do
  clientAuthToken <- decrypt order.clientAuthToken
  pure
    Juspay.SDKPayloadDetails
      { clientId = Nothing,
        amount = show order.amount,
        merchantId = Nothing,
        clientAuthToken,
        clientAuthTokenExpiry = order.clientAuthTokenExpiry,
        environment = order.environment,
        options_getUpiDeepLinks = order.getUpiDeepLinksOption,
        lastName = req.customerLastName,
        action = Nothing,
        customerId = Just order.personId.getId,
        returnUrl = Nothing,
        currency = order.currency,
        firstName = req.customerFirstName,
        customerPhone = Just req.customerPhone,
        customerEmail = Just req.customerEmail,
        orderId = Just order.shortId.getShortId,
        description = Nothing
      }

buildPaymentOrder ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Id DOrder.PaymentOrder ->
  Payment.CreateOrderReq ->
  Payment.CreateOrderResp ->
  m DOrder.PaymentOrder
buildPaymentOrder merchantId personId orderId req resp = do
  now <- getCurrentTime
  clientAuthToken <- encrypt resp.sdk_payload.payload.clientAuthToken
  pure
    DOrder.PaymentOrder
      { id = orderId,
        shortId = ShortId req.orderShortId,
        personId,
        merchantId,
        amount = req.amount, -- FIXME resp.sdk_payload.payload.amount
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

-- order status -----------------------------------------------------

orderStatusService ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  Id Person ->
  Id DOrder.PaymentOrder ->
  (Payment.OrderStatusReq -> m Payment.OrderStatusResp) ->
  m PaymentStatusResp
orderStatusService personId orderId orderStatusCall = do
  order <- runInReplica $ QOrder.findById orderId >>= fromMaybeM (PaymentOrderDoesNotExist orderId.getId)
  unless (personId == order.personId) $ throwError NotAnExecutor
  let orderStatusReq = Payment.OrderStatusReq {orderShortId = order.shortId.getShortId}
  orderStatusResp <- orderStatusCall orderStatusReq -- api call
  updateOrderTransaction order orderStatusResp Nothing
  return $ PaymentStatusResp {status = orderStatusResp.transactionStatus}

updateOrderTransaction ::
  ( EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  DOrder.PaymentOrder ->
  Payment.OrderStatusResp ->
  Maybe Text ->
  m ()
updateOrderTransaction order resp respDump = do
  mbTransaction <- do
    case resp.transactionUUID of
      Just transactionUUID -> runInReplica $ QTransaction.findByTxnUUID transactionUUID
      Nothing -> runInReplica $ QTransaction.findNewTransactionByOrderId order.id
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

-- webhook ----------------------------------------------------------

juspayWebhookService ::
  ( EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  Payment.OrderStatusResp ->
  Text ->
  m AckResponse
juspayWebhookService resp respDump = do
  let orderShortId = ShortId resp.orderShortId
  order <- runInReplica $ QOrder.findByShortId orderShortId >>= fromMaybeM (PaymentOrderNotFound resp.orderShortId)
  updateOrderTransaction order resp $ Just respDump
  return Ack
