{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=ambiguous-fields #-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Lib.Payment.Domain.Action
  ( PaymentStatusResp (..),
    createOrderService,
    orderStatusService,
    juspayWebhookService,
    createNotificationService,
    createExecutionService,
    buildSDKPayload,
    refundService,
    createPaymentIntentService,
    chargePaymentIntentService,
    createPayoutService,
    payoutStatusService,
    payoutStatusUpdates,
    cancelPaymentIntentService,
  )
where

import Control.Applicative ((<|>))
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time.Clock.POSIX hiding (getCurrentTime)
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Payment.Interface.Types as PInterface
import Kernel.External.Payment.Juspay.Types (RefundStatus (REFUND_PENDING))
import qualified Kernel.External.Payment.Juspay.Types as Juspay
import qualified Kernel.External.Payout.Interface as PT
import qualified Kernel.External.Payout.Interface.Types as Payout
import qualified Kernel.External.Payout.Juspay.Types as Juspay
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (Value, isNothing)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Payment.Domain.Types.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.PaymentTransaction as DTransaction
import qualified Lib.Payment.Domain.Types.PayoutOrder as Payment
import qualified Lib.Payment.Domain.Types.PayoutTransaction as PT
import Lib.Payment.Domain.Types.Refunds (Refunds (..))
import Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QTransaction
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutTransaction as QPayoutTransaction
import qualified Lib.Payment.Storage.Queries.Refunds as QRefunds

data PaymentStatusResp
  = PaymentStatus
      { status :: Payment.TransactionStatus,
        bankErrorMessage :: Maybe Text,
        bankErrorCode :: Maybe Text,
        isRetried :: Maybe Bool,
        isRetargeted :: Maybe Bool,
        retargetLink :: Maybe Text,
        refunds :: [Payment.RefundsData],
        payerVpa :: Maybe Text
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
        sourceObject :: Maybe Text,
        sourceInfo :: Payment.SourceInfo,
        notificationType :: Maybe Text,
        juspayProviedId :: Text,
        responseCode :: Maybe Text,
        responseMessage :: Maybe Text,
        notificationId :: Text
      }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data PayoutPaymentStatus = PayoutPaymentStatus
  { status :: Payout.PayoutOrderStatus,
    orderId :: Text,
    accountDetailsType :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- create payment intent --------------------------------------------

createPaymentIntentService ::
  forall m r c.
  ( EncFlow m r,
    BeamFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  Id Merchant ->
  Id Person ->
  Id Ride ->
  Text ->
  Payment.CreatePaymentIntentReq ->
  (Payment.CreatePaymentIntentReq -> m Payment.CreatePaymentIntentResp) ->
  (Payment.PaymentIntentId -> HighPrecMoney -> HighPrecMoney -> m ()) ->
  (Payment.PaymentIntentId -> HighPrecMoney -> HighPrecMoney -> m ()) ->
  (Payment.PaymentIntentId -> m Payment.CreatePaymentIntentResp) ->
  m Payment.CreatePaymentIntentResp
createPaymentIntentService merchantId personId rideId rideShortIdText createPaymentIntentReq createPaymentIntentCall updatePaymentIntentAmountCall capturePaymentIntentCall getPaymentIntentCall = do
  let rideShortId = ShortId rideShortIdText
  let cardFixedCharges = HighPrecMoney 0.3 -- 30 cents, we have to fix it later properly
  mbExistingOrder <- QOrder.findById (cast rideId)
  case mbExistingOrder of
    Nothing -> do
      createPaymentIntentResp <- createPaymentIntentCall createPaymentIntentReq -- api call
      paymentOrder <- buildPaymentOrder_ createPaymentIntentResp rideShortId
      transaction <- buildTransaction paymentOrder createPaymentIntentResp
      QOrder.create paymentOrder
      QTransaction.create transaction
      return createPaymentIntentResp
    Just existingOrder -> do
      transactions <- QTransaction.findAllByOrderId existingOrder.id
      let mbInProgressTransaction = find (isInProgress . (.status)) transactions
      case mbInProgressTransaction of
        Nothing -> createNewTransaction existingOrder -- if previous all payment intents are already charged or cancelled, then create a new payment intent
        Just transaction -> do
          paymentIntentId <- transaction.txnId & fromMaybeM (InternalError "Transaction doesn't have txnId") -- should never happen
          let newTransactionAmount = transaction.amount + createPaymentIntentReq.amount
          let newApplicationFeeAmount = transaction.applicationFeeAmount + createPaymentIntentReq.applicationFeeAmount - cardFixedCharges -- card fixed charges already included in application fee
          if newTransactionAmount > transaction.amount
            then do
              resp <- try @_ @SomeException (updatePaymentIntentAmountCall paymentIntentId newTransactionAmount newApplicationFeeAmount)
              case resp of
                Left err -> do
                  logError $ "Failed to update payment intent amount for paymentIntentId: " <> paymentIntentId <> " err: " <> show err
                  chargePaymentIntentService paymentIntentId capturePaymentIntentCall getPaymentIntentCall -- charge older payment intent
                  createNewTransaction existingOrder -- create new payment intent
                Right _ -> updateOldTransaction paymentIntentId newTransactionAmount newApplicationFeeAmount existingOrder transaction
            else updateOldTransaction paymentIntentId newTransactionAmount newApplicationFeeAmount existingOrder transaction
  where
    isInProgress = (`elem` [Payment.NEW, Payment.PENDING_VBV, Payment.STARTED, Payment.AUTHORIZING])

    createNewTransaction :: (EncFlow m r, BeamFlow m r) => DOrder.PaymentOrder -> m Payment.CreatePaymentIntentResp
    createNewTransaction existingOrder = do
      createPaymentIntentResp <- createPaymentIntentCall createPaymentIntentReq -- api call
      let newOrderAmount = existingOrder.amount + createPaymentIntentReq.amount
      transaction <- buildTransaction existingOrder createPaymentIntentResp
      QOrder.updateAmountAndPaymentIntentId existingOrder.id newOrderAmount createPaymentIntentResp.paymentIntentId
      QTransaction.create transaction
      return createPaymentIntentResp

    updateOldTransaction paymentIntentId newTransactionAmount newApplicationFeeAmount existingOrder transaction = do
      mbClientSecret <- mapM decrypt existingOrder.clientAuthToken
      clientSecret <- mbClientSecret & fromMaybeM (InternalError "Client secret not found") -- should never happen
      let newOrderAmount = existingOrder.amount + createPaymentIntentReq.amount
      QOrder.updateAmountAndPaymentIntentId existingOrder.id newOrderAmount paymentIntentId
      QTransaction.updateAmount transaction.id newTransactionAmount newApplicationFeeAmount
      let paymentIntentStatus = Payment.caseToPaymentIntentStatus transaction.status
      return $ Payment.CreatePaymentIntentResp paymentIntentId clientSecret paymentIntentStatus

    buildPaymentOrder_ ::
      ( EncFlow m r,
        BeamFlow m r
      ) =>
      Payment.CreatePaymentIntentResp ->
      ShortId DOrder.PaymentOrder ->
      m DOrder.PaymentOrder
    buildPaymentOrder_ createPaymentIntentResp rideShortId = do
      now <- getCurrentTime
      clientAuthToken <- encrypt createPaymentIntentResp.clientSecret
      let clientAuthTokenExpiry = Time.UTCTime (Time.fromGregorian 2099 1 1) 0 -- setting infinite expiry for now
      pure
        DOrder.PaymentOrder
          { id = cast rideId,
            shortId = rideShortId,
            paymentServiceOrderId = createPaymentIntentResp.paymentIntentId,
            requestId = Nothing,
            service = Nothing,
            clientId = Nothing,
            description = Nothing,
            returnUrl = Nothing,
            action = Nothing,
            personId,
            merchantId,
            paymentMerchantId = Nothing,
            amount = createPaymentIntentReq.amount,
            currency = createPaymentIntentReq.currency,
            status = Payment.castToTransactionStatus createPaymentIntentResp.status,
            paymentLinks = Payment.PaymentLinks Nothing Nothing Nothing Nothing,
            clientAuthToken = Just clientAuthToken,
            clientAuthTokenExpiry = Just clientAuthTokenExpiry,
            getUpiDeepLinksOption = Nothing,
            environment = Nothing,
            createMandate = Nothing,
            mandateMaxAmount = Nothing,
            mandateStartDate = Nothing,
            mandateEndDate = Nothing,
            serviceProvider = Payment.Stripe, -- fix it later
            bankErrorCode = Nothing,
            bankErrorMessage = Nothing,
            isRetried = False,
            isRetargeted = False,
            retargetLink = Nothing,
            createdAt = now,
            updatedAt = now
          }

    buildTransaction ::
      ( EncFlow m r,
        BeamFlow m r
      ) =>
      DOrder.PaymentOrder ->
      Payment.CreatePaymentIntentResp ->
      m DTransaction.PaymentTransaction
    buildTransaction order resp = do
      uuid <- generateGUID
      now <- getCurrentTime
      pure
        DTransaction.PaymentTransaction
          { id = uuid,
            txnUUID = Nothing,
            txnId = Just resp.paymentIntentId,
            paymentMethodType = Nothing, -- fix it later
            paymentMethod = Nothing, -- fix it later
            respMessage = Nothing,
            respCode = Nothing,
            gatewayReferenceId = Nothing,
            orderId = order.id,
            merchantId = order.merchantId,
            amount = createPaymentIntentReq.amount,
            applicationFeeAmount = createPaymentIntentReq.applicationFeeAmount,
            retryCount = 0,
            currency = createPaymentIntentReq.currency,
            dateCreated = Nothing,
            statusId = 0, -- not used for stripe
            status = Payment.castToTransactionStatus resp.status,
            juspayResponse = Nothing,
            mandateStatus = Nothing,
            mandateStartDate = Nothing,
            mandateEndDate = Nothing,
            mandateId = Nothing,
            bankErrorMessage = Nothing,
            bankErrorCode = Nothing,
            mandateFrequency = Nothing,
            mandateMaxAmount = Nothing,
            splitSettlementResponse = Nothing,
            createdAt = now,
            updatedAt = now
          }

cancelPaymentIntentService ::
  ( EncFlow m r,
    BeamFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  Id Ride ->
  (Payment.PaymentIntentId -> m Payment.CreatePaymentIntentResp) ->
  m ()
cancelPaymentIntentService rideId cancelPaymentIntentCall = do
  mbExistingOrder <- QOrder.findById (cast rideId)
  case mbExistingOrder of
    Nothing -> logError $ "In cancel Payment Intent no order found for rideId : " <> rideId.getId
    Just existingOrder -> do
      resp <- try @_ @SomeException $ cancelPaymentIntentCall existingOrder.paymentServiceOrderId
      case resp of
        Left exec -> do
          let err = fromException @Payment.StripeError exec
              errorCode = err <&> toErrorCode
              errorMessage = err >>= toMessage
          logError $ "Error while cancelling payment intent : " <> show err <> "error code : " <> show errorCode <> "error message : " <> show errorMessage
        Right paymentIntentResp -> do
          transaction <- QTransaction.findByTxnId existingOrder.paymentServiceOrderId >>= fromMaybeM (InternalError $ "No transaction found: " <> existingOrder.paymentServiceOrderId)
          QOrder.updateStatus existingOrder.id existingOrder.paymentServiceOrderId (Payment.castToTransactionStatus paymentIntentResp.status)
          QTransaction.updateStatusAndError transaction.id (Payment.castToTransactionStatus paymentIntentResp.status) Nothing Nothing

chargePaymentIntentService ::
  forall m r c.
  ( EncFlow m r,
    BeamFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  Payment.PaymentIntentId ->
  (Payment.PaymentIntentId -> HighPrecMoney -> HighPrecMoney -> m ()) ->
  (Payment.PaymentIntentId -> m Payment.CreatePaymentIntentResp) ->
  m ()
chargePaymentIntentService paymentIntentId capturePaymentIntentCall getPaymentIntentCall = do
  transaction <- QTransaction.findByTxnId paymentIntentId >>= fromMaybeM (InternalError $ "No transaction found: " <> paymentIntentId)
  if transaction.status `notElem` [Payment.CHARGED, Payment.CANCELLED, Payment.AUTO_REFUNDED] -- if not already charged or cancelled or auto refunded
    then do
      resp <- try @_ @SomeException $ withShortRetry $ capturePaymentIntentCall paymentIntentId transaction.amount transaction.applicationFeeAmount
      case resp of
        Left exec -> do
          let err = fromException @Payment.StripeError exec
              errorCode = err <&> toErrorCode
              errorMessage = err >>= toMessage
          logError $ "Error while charging payment intent: " <> show err
          if transaction.retryCount >= 2 -- already 3 retries
            then do
              logError "Max retries reached, cancelling payment intent"
              -- should we cancel the payment intent from stripe?
              QTransaction.updateStatusAndError transaction.id Payment.CANCELLED errorCode errorMessage
            else do
              -- should we retry on basis of error?
              QTransaction.updateRetryCountAndError transaction.id (transaction.retryCount + 1) errorCode errorMessage -- retry
        Right () -> do
          paymentIntentResp <- getPaymentIntentCall paymentIntentId
          QTransaction.updateStatusAndError transaction.id (Payment.castToTransactionStatus paymentIntentResp.status) Nothing Nothing
          QOrder.updateStatus transaction.orderId paymentIntentId (Payment.castToTransactionStatus paymentIntentResp.status)
    else pure () -- if already charged or cancelled or auto refunded no need to charge again

-- create order -----------------------------------------------------

createOrderService ::
  ( EncFlow m r,
    BeamFlow m r
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
      QOrder.create paymentOrder
      return $ Just createOrderResp
    Just existingOrder -> do
      isOrderExpired <- maybe (pure True) checkIfExpired existingOrder.clientAuthTokenExpiry
      if isOrderExpired
        then do
          QOrder.updateStatusToExpired existingOrder.id
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
    BeamFlow m r
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
        amount = req.amount,
        currency = resp.sdk_payload.payload.currency,
        status = resp.status,
        paymentLinks = fromMaybe (Payment.PaymentLinks Nothing Nothing Nothing Nothing) resp.payment_links,
        clientAuthToken = Just clientAuthToken,
        clientAuthTokenExpiry = Just resp.sdk_payload.payload.clientAuthTokenExpiry,
        getUpiDeepLinksOption = resp.sdk_payload.payload.options_getUpiDeepLinks,
        environment = resp.sdk_payload.payload.environment,
        createMandate = resp.sdk_payload.payload.createMandate,
        mandateMaxAmount = read . T.unpack <$> resp.sdk_payload.payload.mandateMaxAmount,
        mandateStartDate = posixSecondsToUTCTime . read . T.unpack <$> (resp.sdk_payload.payload.mandateStartDate),
        mandateEndDate = posixSecondsToUTCTime . read . T.unpack <$> resp.sdk_payload.payload.mandateEndDate,
        serviceProvider = Payment.Juspay, -- fix it later
        bankErrorCode = Nothing,
        bankErrorMessage = Nothing,
        isRetried = False,
        isRetargeted = False,
        retargetLink = Nothing,
        createdAt = now,
        updatedAt = now
      }

-- order status -----------------------------------------------------

orderStatusService ::
  ( EncFlow m r,
    BeamFlow m r
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
  now <- getCurrentTime
  orderStatusResp <- orderStatusCall orderStatusReq -- api call
  case orderStatusResp of
    Payment.MandateOrderStatusResp {..} -> do
      let orderTxn =
            OrderTxn
              { mandateStartDate = mandateStartDate,
                mandateEndDate = mandateEndDate,
                mandateId = Just mandateId,
                mandateFrequency = Just mandateFrequency,
                mandateMaxAmount = Just mandateMaxAmount,
                mandateStatus = Just mandateStatus,
                isRetried = Nothing,
                isRetargeted = Nothing,
                retargetLink = Nothing,
                splitSettlementResponse = Nothing,
                ..
              }
      maybe
        (updateOrderTransaction order orderTxn Nothing)
        ( \transactionUUID' ->
            Redis.whenWithLockRedis (txnProccessingKey transactionUUID') 60 $ updateOrderTransaction order orderTxn Nothing
        )
        transactionUUID -- should we put it in fork ?
      return $
        MandatePaymentStatus
          { status = orderStatusResp.transactionStatus,
            upi = orderStatusResp.upi,
            mandateStartDate = fromMaybe now mandateStartDate,
            mandateEndDate = fromMaybe now mandateEndDate,
            ..
          }
    Payment.OrderStatusResp {..} -> do
      let orderTxn =
            OrderTxn
              { mandateStartDate = Nothing,
                mandateEndDate = Nothing,
                mandateId = Nothing,
                mandateFrequency = Nothing,
                mandateMaxAmount = Nothing,
                mandateStatus = Nothing,
                isRetried = isRetriedOrder,
                isRetargeted = isRetargetedOrder,
                retargetLink = retargetPaymentLink,
                ..
              }
      maybe
        (updateOrderTransaction order orderTxn Nothing)
        ( \transactionUUID' ->
            Redis.whenWithLockRedis (txnProccessingKey transactionUUID') 60 $ updateOrderTransaction order orderTxn Nothing
        )
        transactionUUID
      mapM_ updateRefundStatus refunds
      return $ PaymentStatus {status = transactionStatus, bankErrorCode = orderTxn.bankErrorCode, bankErrorMessage = orderTxn.bankErrorMessage, isRetried = isRetriedOrder, isRetargeted = isRetargetedOrder, retargetLink = retargetPaymentLink, refunds = refunds, payerVpa = (payerVpa <|> ((.payerVpa) =<< upi))}
    _ -> throwError $ InternalError "Unexpected Order Status Response."

data OrderTxn = OrderTxn
  { transactionUUID :: Maybe Text,
    txnId :: Maybe Text,
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
    currency :: Currency,
    dateCreated :: Maybe UTCTime,
    mandateStatus :: Maybe Payment.MandateStatus,
    mandateStartDate :: Maybe UTCTime,
    mandateEndDate :: Maybe UTCTime,
    mandateId :: Maybe Text,
    mandateFrequency :: Maybe Payment.MandateFrequency,
    mandateMaxAmount :: Maybe HighPrecMoney,
    isRetried :: Maybe Bool,
    isRetargeted :: Maybe Bool,
    retargetLink :: Maybe Text,
    splitSettlementResponse :: Maybe PInterface.SplitSettlementResponse
  }

updateOrderTransaction ::
  BeamFlow m r =>
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
      Just transactionUUID -> do
        mbTxn <- QTransaction.findByTxnUUID transactionUUID
        when (isNothing mbTxn) $ do
          transaction <- buildPaymentTransaction order resp respDump
          QTransaction.create transaction
        return mbTxn
      Nothing -> QTransaction.findNewTransactionByOrderId order.id
  let updOrder = order{status = resp.transactionStatus, isRetargeted = fromMaybe order.isRetargeted resp.isRetargeted, isRetried = fromMaybe order.isRetried resp.isRetried, retargetLink = resp.retargetLink}
  case mbTransaction of
    Nothing -> when (order.status /= updOrder.status && order.status /= Payment.CHARGED) $ QOrder.updateStatusAndError updOrder errorMessage errorCode
    -- Nothing -> runInReplica $ QTransaction.findNewTransactionByOrderId order.id
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
                        juspayResponse = respDump,
                        txnId = resp.txnId,
                        splitSettlementResponse = resp.splitSettlementResponse
                       }

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
        applicationFeeAmount = 0.0,
        retryCount = 0,
        createdAt = now,
        updatedAt = now,
        juspayResponse = respDump,
        bankErrorCode,
        bankErrorMessage,
        ..
      }

-- webhook ----------------------------------------------------------

juspayWebhookService ::
  BeamFlow m r =>
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
              { mandateStartDate = mandateStartDate,
                mandateEndDate = mandateEndDate,
                mandateId = Just mandateId,
                mandateStatus = Just mandateStatus,
                mandateFrequency = Just mandateFrequency,
                mandateMaxAmount = Just mandateMaxAmount,
                isRetried = Nothing,
                isRetargeted = Nothing,
                retargetLink = Nothing,
                splitSettlementResponse = Nothing,
                ..
              }
      maybe
        (updateOrderTransaction order orderTxn Nothing)
        ( \transactionUUID' ->
            Redis.whenWithLockRedis (txnProccessingKey transactionUUID') 60 $ updateOrderTransaction order orderTxn $ Just respDump
        )
        transactionUUID
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
                isRetried = isRetriedOrder,
                isRetargeted = isRetargetedOrder,
                retargetLink = retargetPaymentLink,
                ..
              }
      maybe
        (updateOrderTransaction order orderTxn Nothing)
        ( \transactionUUID' ->
            Redis.whenWithLockRedis (txnProccessingKey transactionUUID') 60 $ updateOrderTransaction order orderTxn $ Just respDump
        )
        transactionUUID
      mapM_ updateRefundStatus refunds
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
    HasShortDurationRetryCfg r c,
    BeamFlow m r
  ) =>
  (Payment.MandateExecutionReq, Text) ->
  Id Merchant ->
  (Payment.MandateExecutionReq -> m Payment.MandateExecutionRes) ->
  m Payment.MandateExecutionRes
createExecutionService (request, orderId) merchantId executionCall = do
  executionOrder <- mkExecutionOrder request
  QOrder.create executionOrder
  executionResp <- withShortRetry $ executionCall request
  QOrder.updateStatus (Id orderId) executionResp.orderId executionResp.status
  return executionResp
  where
    mkExecutionOrder req = do
      now <- getCurrentTime
      return
        DOrder.PaymentOrder
          { id = Id orderId,
            shortId = ShortId req.orderId, ---- to check --------
            paymentServiceOrderId = "Unkown",
            requestId = Nothing,
            service = Nothing,
            clientId = Nothing,
            description = Nothing,
            returnUrl = Nothing,
            action = Nothing,
            personId = Id req.customerId,
            merchantId = merchantId,
            paymentMerchantId = Nothing,
            amount = req.amount,
            currency = INR,
            status = Payment.NEW,
            paymentLinks = Payment.PaymentLinks Nothing Nothing Nothing Nothing,
            clientAuthToken = Nothing,
            clientAuthTokenExpiry = Nothing,
            serviceProvider = Payment.Juspay, -- fix it later
            getUpiDeepLinksOption = Nothing,
            environment = Nothing,
            createMandate = Nothing,
            mandateMaxAmount = Nothing,
            mandateStartDate = Nothing,
            mandateEndDate = Nothing,
            bankErrorMessage = Nothing,
            bankErrorCode = Nothing,
            isRetried = False,
            isRetargeted = False,
            retargetLink = Nothing,
            createdAt = now,
            updatedAt = now
          }

--- refunds api ----

refundService ::
  ( EncFlow m r,
    BeamFlow m r
  ) =>
  (Payment.AutoRefundReq, Id Refunds) ->
  Id Merchant ->
  (Payment.AutoRefundReq -> m Payment.AutoRefundResp) ->
  m Payment.AutoRefundResp
refundService (request, refundId) merchantId refundsCall = do
  now <- getCurrentTime
  QRefunds.create $ mkRefundsEntry now
  response <- refundsCall request
  mapM_ (\Payment.RefundsData {..} -> QRefunds.updateRefundsEntryByResponse initiatedBy (Just idAssignedByServiceProvider) errorMessage errorCode status (Kernel.Types.Id.Id requestId)) response.refunds
  return response
  where
    mkRefundsEntry now =
      Refunds
        { id = refundId,
          merchantId = merchantId.getId,
          shortId = request.requestId,
          status = REFUND_PENDING,
          orderId = Id request.orderId,
          refundAmount = request.amount,
          errorMessage = Nothing,
          errorCode = Nothing,
          idAssignedByServiceProvider = Nothing,
          initiatedBy = Nothing,
          createdAt = now,
          updatedAt = now
        }

updateRefundStatus :: (BeamFlow m r) => Payment.RefundsData -> m ()
updateRefundStatus Payment.RefundsData {..} = do
  Redis.whenWithLockRedis (refundProccessingKey requestId) 60 $
    QRefunds.updateRefundsEntryByResponse initiatedBy (Just idAssignedByServiceProvider) errorMessage errorCode status (Kernel.Types.Id.Id requestId)

txnProccessingKey :: Text -> Text
txnProccessingKey txnUUid = "Txn:Processing:TxnUuid" <> txnUUid

refundProccessingKey :: Text -> Text
refundProccessingKey refundId = "Refund:Processing:RefundId" <> refundId

-- payout APIs ---

createPayoutService ::
  ( EncFlow m r,
    BeamFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Maybe [Text] ->
  Maybe EntityName ->
  Text ->
  PT.CreatePayoutOrderReq ->
  (PT.CreatePayoutOrderReq -> m PT.CreatePayoutOrderResp) ->
  m (Maybe PT.CreatePayoutOrderResp, Maybe Payment.PayoutOrder)
createPayoutService merchantId _personId mbEntityIds mbEntityName city createPayoutOrderReq createPayoutOrderCall = do
  mbExistingPayoutOrder <- QPayoutOrder.findByOrderId createPayoutOrderReq.orderId
  case mbExistingPayoutOrder of
    Nothing -> do
      createPayoutOrderResp <- createPayoutOrderCall createPayoutOrderReq -- api call
      payoutOrder <- buildPayoutOrder createPayoutOrderReq createPayoutOrderResp
      QPayoutOrder.create payoutOrder
      return (Just createPayoutOrderResp, Just payoutOrder)
    Just existingPayoutOrder -> throwError $ PayoutOrderAlreadyExists (existingPayoutOrder.id.getId)
  where
    buildPayoutOrder req resp = do
      now <- getCurrentTime
      uuid <- generateGUID
      shortId <- generateShortId
      customerEmail <- encrypt req.customerEmail
      mobileNo <- encrypt req.customerPhone
      let txn = listToMaybe <$> sortBy (comparing (.updatedAt)) =<< ((.transactions) =<< listToMaybe =<< resp.fulfillments)
      pure $
        Payment.PayoutOrder
          { id = uuid,
            shortId = Just shortId,
            customerId = req.customerId,
            orderId = req.orderId,
            merchantId = merchantId.getId,
            mobileNo = mobileNo,
            city = city,
            amount = mkPrice Nothing req.amount,
            entityIds = mbEntityIds,
            entityName = mbEntityName,
            status = resp.status,
            responseMessage = (.responseMessage) =<< txn,
            responseCode = (.responseCode) =<< txn,
            accountDetailsType = (.detailsType) =<< (.beneficiaryDetails) =<< listToMaybe =<< resp.fulfillments, --- for now only one fullfillment supported
            vpa = Just req.customerVpa,
            customerEmail = customerEmail,
            lastStatusCheckedAt = Nothing,
            createdAt = now,
            updatedAt = now
          }

payoutStatusService ::
  ( EncFlow m r,
    BeamFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  PT.PayoutOrderStatusReq ->
  (PT.PayoutOrderStatusReq -> m PT.PayoutOrderStatusResp) ->
  m PayoutPaymentStatus
payoutStatusService _merchantId _personId createPayoutOrderStatusReq createPayoutOrderStatusCall = do
  _ <- QPayoutOrder.findByOrderId createPayoutOrderStatusReq.orderId >>= fromMaybeM (PayoutOrderNotFound (createPayoutOrderStatusReq.orderId)) -- validation
  let payoutOrderStatusReq = Payout.PayoutOrderStatusReq {orderId = createPayoutOrderStatusReq.orderId, mbExpand = createPayoutOrderStatusReq.mbExpand}
  statusResp <- createPayoutOrderStatusCall payoutOrderStatusReq -- api call
  payoutStatusUpdates statusResp.status createPayoutOrderStatusReq.orderId (Just statusResp)
  pure $ PayoutPaymentStatus {status = statusResp.status, orderId = statusResp.orderId, accountDetailsType = show <$> ((.detailsType) =<< (.beneficiaryDetails) =<< listToMaybe =<< statusResp.fulfillments)}

payoutStatusUpdates :: (EncFlow m r, BeamFlow m r) => Payout.PayoutOrderStatus -> Text -> Maybe PT.PayoutOrderStatusResp -> m ()
payoutStatusUpdates status_ orderId statusResp = do
  order <- QPayoutOrder.findByOrderId orderId >>= fromMaybeM (PayoutOrderNotFound orderId)
  QPayoutOrder.updatePayoutOrderStatus status_ orderId
  logDebug $ "Payout order Status: " <> show statusResp
  case statusResp of
    Just Payout.CreatePayoutOrderResp {orderId = _orderPayoutId, status = _status, ..} -> do
      let txns = (.transactions) =<< listToMaybe =<< fulfillments
          mbTxn = listToMaybe <$> sortBy (comparing (.updatedAt)) =<< txns
      QPayoutOrder.updatePayoutOrderTxnRespInfo ((.responseCode) =<< mbTxn) ((.responseMessage) =<< mbTxn) orderId
      case mbTxn of
        Just Payout.Transaction {amount = amount_txn, ..} -> do
          findTransaction <- QPayoutTransaction.findByTransactionRef transactionRef
          case findTransaction of
            Just _ -> QPayoutTransaction.updatePayoutTransactionStatus status transactionRef
            Nothing -> do
              uuid <- generateGUID
              now <- getCurrentTime
              let payoutTransaction =
                    PT.PayoutTransaction
                      { id = uuid,
                        merchantId = order.merchantId,
                        payoutOrderId = Id orderId,
                        transactionRef = transactionRef,
                        gateWayRefId = gatewayRefId,
                        fulfillmentMethod = fulfillmentMethod,
                        amount = mkPrice Nothing (realToFrac amount_txn),
                        status = status,
                        createdAt = now,
                        updatedAt = now
                      }
              QPayoutTransaction.create payoutTransaction
        Nothing -> pure ()
    Nothing -> pure ()
