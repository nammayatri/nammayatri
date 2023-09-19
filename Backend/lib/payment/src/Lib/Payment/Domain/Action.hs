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
    createNotificationService,
    createExecutionService,
  )
where

import qualified Data.Text as T
import Data.Time.Clock.POSIX hiding (getCurrentTime)
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

data PaymentStatusResp
  = PaymentStatus
      { status :: Payment.TransactionStatus
      }
  | MandatePaymentStatus
      { status :: Payment.TransactionStatus,
        mandateStatus :: Payment.MandateStatus,
        mandateStartDate :: UTCTime,
        mandateEndDate :: UTCTime,
        mandateId :: Text,
        mandateMaxAmount :: HighPrecMoney,
        payerVpa :: Maybe Text,
        bankErrorMessage :: Maybe Text,
        bankErrorCode :: Maybe Text,
        upi :: Maybe Payment.Upi
      }
  | PDNNotificationStatusResp
      { eventName :: Maybe Payment.PaymentStatus,
        notificationStatus :: Payment.NotificationStatus,
        sourceObject :: Text,
        endDate :: Text,
        sourceInfo :: Payment.SourceInfo,
        notificationType :: Text,
        juspayProviedId :: Text,
        notificationId :: Text
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
  Payment.CreateOrderReq ->
  (Payment.CreateOrderReq -> m Payment.CreateOrderResp) ->
  m (Maybe Payment.CreateOrderResp)
createOrderService merchantId personId createOrderReq createOrderCall = do
  mbExistingOrder <- QOrder.findById (Id createOrderReq.orderId)
  case mbExistingOrder of
    Nothing -> do
      createOrderResp <- createOrderCall createOrderReq -- api call
      paymentOrder <- buildPaymentOrder merchantId personId createOrderReq createOrderResp
      Esq.runTransaction $
        QOrder.create paymentOrder
      return $ Just createOrderResp
    Just existingOrder -> do
      isOrderExpired <- maybe (pure True) checkIfExpired existingOrder.clientAuthTokenExpiry
      if isOrderExpired
        then do
          Esq.runTransaction $ QOrder.updateStatusToExpired existingOrder.id
          return Nothing
        else do
          sdkPayload <- buildSDKPayload createOrderReq existingOrder
          case sdkPayload of
            Just sdk_payload -> do
              return $
                Just $
                  Payment.CreateOrderResp
                    { status = existingOrder.status,
                      id = existingOrder.paymentServiceOrderId,
                      order_id = existingOrder.shortId.getShortId,
                      payment_links = Just existingOrder.paymentLinks,
                      sdk_payload
                    }
            Nothing -> return Nothing
  where
    checkIfExpired expiry = do
      now <- getCurrentTime
      let buffer = secondsToNominalDiffTime 150 -- 2.5 mins of buffer
      return (expiry < addUTCTime buffer now)

buildSDKPayload :: EncFlow m r => Payment.CreateOrderReq -> DOrder.PaymentOrder -> m (Maybe Juspay.SDKPayload)
buildSDKPayload req order = do
  payload <- buildSDKPayloadDetails req order
  case payload of
    Just sdkPayload -> do
      return $
        Just
          Juspay.SDKPayload
            { requestId = order.requestId,
              service = order.service,
              payload = sdkPayload
            }
    Nothing -> return Nothing

buildSDKPayloadDetails :: EncFlow m r => Payment.CreateOrderReq -> DOrder.PaymentOrder -> m (Maybe Juspay.SDKPayloadDetails)
buildSDKPayloadDetails req order = do
  case (order.clientAuthToken, order.clientAuthTokenExpiry) of
    (Just token, Just clientAuthTokenExpiry) -> do
      clientAuthToken <- decrypt token
      return $
        Just
          Juspay.SDKPayloadDetails
            { clientId = order.clientId,
              amount = show order.amount,
              merchantId = order.paymentMerchantId,
              clientAuthToken,
              clientAuthTokenExpiry = clientAuthTokenExpiry,
              environment = order.environment,
              options_getUpiDeepLinks = order.getUpiDeepLinksOption,
              lastName = req.customerLastName,
              action = order.action,
              customerId = Just order.personId.getId,
              returnUrl = order.returnUrl,
              currency = order.currency,
              firstName = req.customerFirstName,
              customerPhone = Just req.customerPhone,
              customerEmail = Just req.customerEmail,
              orderId = Just order.shortId.getShortId,
              description = order.description,
              createMandate = order.createMandate,
              mandateMaxAmount = show <$> order.mandateMaxAmount,
              mandateStartDate = show . utcTimeToPOSIXSeconds <$> (order.mandateStartDate),
              mandateEndDate = show . utcTimeToPOSIXSeconds <$> order.mandateEndDate
            }
    (_, _) -> return Nothing

buildPaymentOrder ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Payment.CreateOrderReq ->
  Payment.CreateOrderResp ->
  m DOrder.PaymentOrder
buildPaymentOrder merchantId personId req resp = do
  now <- getCurrentTime
  clientAuthToken <- encrypt resp.sdk_payload.payload.clientAuthToken
  pure
    DOrder.PaymentOrder
      { id = Id req.orderId,
        shortId = ShortId req.orderShortId,
        paymentServiceOrderId = resp.id,
        requestId = resp.sdk_payload.requestId,
        service = resp.sdk_payload.service,
        clientId = resp.sdk_payload.payload.clientId,
        description = resp.sdk_payload.payload.description,
        returnUrl = resp.sdk_payload.payload.returnUrl,
        action = resp.sdk_payload.payload.action,
        personId,
        merchantId,
        paymentMerchantId = resp.sdk_payload.payload.merchantId,
        amount = round req.amount,
        currency = resp.sdk_payload.payload.currency,
        status = resp.status,
        paymentLinks = fromMaybe (Payment.PaymentLinks Nothing Nothing Nothing) resp.payment_links,
        clientAuthToken = Just clientAuthToken,
        clientAuthTokenExpiry = Just resp.sdk_payload.payload.clientAuthTokenExpiry,
        getUpiDeepLinksOption = resp.sdk_payload.payload.options_getUpiDeepLinks,
        environment = resp.sdk_payload.payload.environment,
        createMandate = resp.sdk_payload.payload.createMandate,
        mandateMaxAmount = read . T.unpack <$> resp.sdk_payload.payload.mandateMaxAmount,
        mandateStartDate = posixSecondsToUTCTime . read . T.unpack <$> (resp.sdk_payload.payload.mandateStartDate),
        mandateEndDate = posixSecondsToUTCTime . read . T.unpack <$> resp.sdk_payload.payload.mandateEndDate,
        bankErrorCode = Nothing,
        bankErrorMessage = Nothing,
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
  -- order <- runInReplica $ QOrder.findById orderId >>= fromMaybeM (PaymentOrderDoesNotExist orderId.getId)
  order <- QOrder.findById orderId >>= fromMaybeM (PaymentOrderDoesNotExist orderId.getId)
  unless (personId == order.personId) $ throwError NotAnExecutor
  let orderStatusReq = Payment.OrderStatusReq {orderShortId = order.shortId.getShortId}
  orderStatusResp <- orderStatusCall orderStatusReq -- api call
  case orderStatusResp of
    Payment.MandateOrderStatusResp {..} -> do
      let orderTxn =
            OrderTxn
              { mandateStartDate = Just mandateStartDate,
                mandateEndDate = Just mandateEndDate,
                mandateId = Just mandateId,
                mandateFrequency = Just mandateFrequency,
                mandateMaxAmount = Just mandateMaxAmount,
                mandateStatus = Just mandateStatus,
                ..
              }
      updateOrderTransaction order orderTxn Nothing
      return $ MandatePaymentStatus {status = orderStatusResp.transactionStatus, upi = orderStatusResp.upi, ..}
    Payment.OrderStatusResp {..} -> do
      let orderTxn =
            OrderTxn
              { mandateStartDate = Nothing,
                mandateEndDate = Nothing,
                mandateId = Nothing,
                mandateFrequency = Nothing,
                mandateMaxAmount = Nothing,
                mandateStatus = Nothing,
                ..
              }
      updateOrderTransaction order orderTxn Nothing
      return $ PaymentStatus {status = transactionStatus, ..}
    _ -> throwError $ InternalError "Unexpected Order Status Response."

data OrderTxn = OrderTxn
  { transactionUUID :: Maybe Text,
    transactionStatusId :: Int,
    transactionStatus :: Payment.TransactionStatus,
    paymentMethodType :: Maybe Text,
    paymentMethod :: Maybe Text,
    respMessage :: Maybe Text,
    respCode :: Maybe Text,
    gatewayReferenceId :: Maybe Text,
    amount :: HighPrecMoney,
    bankErrorMessage :: Maybe Text,
    bankErrorCode :: Maybe Text,
    currency :: Payment.Currency,
    dateCreated :: Maybe UTCTime,
    mandateStatus :: Maybe Payment.MandateStatus,
    mandateStartDate :: Maybe UTCTime,
    mandateEndDate :: Maybe UTCTime,
    mandateId :: Maybe Text,
    mandateFrequency :: Maybe Payment.MandateFrequency,
    mandateMaxAmount :: Maybe HighPrecMoney
  }

updateOrderTransaction ::
  ( EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  DOrder.PaymentOrder ->
  OrderTxn ->
  Maybe Text ->
  m ()
updateOrderTransaction order resp respDump = do
  let errorMessage = resp.bankErrorMessage
      errorCode = resp.bankErrorCode
  mbTransaction <- do
    case resp.transactionUUID of
      -- Just transactionUUID -> runInReplica $ QTransaction.findByTxnUUID transactionUUID
      Just transactionUUID -> QTransaction.findByTxnUUID transactionUUID
      -- Nothing -> runInReplica $ QTransaction.findNewTransactionByOrderId order.id
      Nothing -> QTransaction.findNewTransactionByOrderId order.id
  let updOrder = order{status = resp.transactionStatus}
  case mbTransaction of
    Nothing -> do
      transaction <- buildPaymentTransaction order resp respDump
      Esq.runTransaction $ do
        QTransaction.create transaction
        when (order.status /= updOrder.status && order.status /= Payment.CHARGED) $ QOrder.updateStatusAndError updOrder errorMessage errorCode
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
                        mandateStatus = resp.mandateStatus,
                        mandateStartDate = resp.mandateStartDate,
                        mandateEndDate = resp.mandateEndDate,
                        mandateId = resp.mandateId,
                        mandateFrequency = resp.mandateFrequency,
                        mandateMaxAmount = resp.mandateMaxAmount,
                        juspayResponse = respDump
                       }
      Esq.runTransaction $ do
        -- Avoid updating status if already in CHARGED state to handle race conditions
        when (transaction.status /= Payment.CHARGED) $ QTransaction.updateMultiple updTransaction
        when (order.status /= updOrder.status && order.status /= Payment.CHARGED) $ QOrder.updateStatusAndError updOrder errorMessage errorCode

buildPaymentTransaction :: MonadFlow m => DOrder.PaymentOrder -> OrderTxn -> Maybe Text -> m DTransaction.PaymentTransaction
buildPaymentTransaction order OrderTxn {..} respDump = do
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
        bankErrorCode,
        bankErrorMessage,
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
  logWarning $ "Webhook response dump: " <> respDump --- want this for now that's why changed it to warning
  logWarning $ "Webhook response: " <> show resp
  case resp of
    Payment.MandateOrderStatusResp {..} -> do
      order <- QOrder.findByShortId (ShortId orderShortId) >>= fromMaybeM (PaymentOrderNotFound orderShortId)
      let orderTxn =
            OrderTxn
              { mandateStartDate = Just mandateStartDate,
                mandateEndDate = Just mandateEndDate,
                mandateId = Just mandateId,
                mandateStatus = Just mandateStatus,
                mandateFrequency = Just mandateFrequency,
                mandateMaxAmount = Just mandateMaxAmount,
                ..
              }
      updateOrderTransaction order orderTxn $ Just respDump
    Payment.OrderStatusResp {..} -> do
      order <- QOrder.findByShortId (ShortId orderShortId) >>= fromMaybeM (PaymentOrderNotFound orderShortId)
      let orderTxn =
            OrderTxn
              { mandateStartDate = Nothing,
                mandateEndDate = Nothing,
                mandateId = Nothing,
                mandateStatus = Nothing,
                mandateFrequency = Nothing,
                mandateMaxAmount = Nothing,
                ..
              }
      updateOrderTransaction order orderTxn $ Just respDump
    _ -> return ()
  return Ack

--- notification api ----------

createNotificationService ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  Payment.MandateNotificationReq ->
  (Payment.MandateNotificationReq -> m Payment.MandateNotificationRes) ->
  m Payment.MandateNotificationRes
createNotificationService req notificationCall = do
  notificationCall req

----- execution api --------

createExecutionService ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  (Payment.MandateExecutionReq, Text) ->
  Id Merchant ->
  (Payment.MandateExecutionReq -> m Payment.MandateExecutionRes) ->
  m Payment.MandateExecutionRes
createExecutionService (request, orderId) merchantId executionCall = do
  executionResp <- executionCall request
  executionOrder <- mkExecutionOrder request executionResp
  Esq.runTransaction $ QOrder.create executionOrder
  return executionResp
  where
    mkExecutionOrder req resp = do
      now <- getCurrentTime
      return
        DOrder.PaymentOrder
          { id = Id orderId,
            shortId = ShortId req.orderId, ---- to check --------
            paymentServiceOrderId = resp.orderId,
            requestId = Nothing,
            service = Nothing,
            clientId = Nothing,
            description = Nothing,
            returnUrl = Nothing,
            action = Nothing,
            personId = Id req.customerId,
            merchantId = merchantId,
            paymentMerchantId = Nothing,
            amount = round req.amount,
            currency = Juspay.INR,
            status = resp.status,
            paymentLinks = Payment.PaymentLinks Nothing Nothing Nothing,
            clientAuthToken = Nothing,
            clientAuthTokenExpiry = Nothing,
            getUpiDeepLinksOption = Nothing,
            environment = Nothing,
            createMandate = Nothing,
            mandateMaxAmount = Nothing,
            mandateStartDate = Nothing,
            mandateEndDate = Nothing,
            bankErrorMessage = Nothing,
            bankErrorCode = Nothing,
            createdAt = now,
            updatedAt = now
          }
