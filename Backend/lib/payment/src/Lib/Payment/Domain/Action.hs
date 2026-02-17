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
    CreatePaymentIntentServiceResp (..),
    CreatePaymentIntentServiceReq (..),
    createOrderService,
    orderStatusService,
    juspayWebhookService,
    stripeWebhookService,
    mkStripeWebhookData,
    StripeWebhookData (..),
    createNotificationService,
    createExecutionService,
    buildSDKPayload,
    createRefundService,
    createPaymentIntentService,
    updateForCXCancelPaymentIntentService,
    chargePaymentIntentService,
    InitiateStripeRefundReq (..),
    InitiateStripeRefundResp (..),
    initiateStripeRefundService,
    refreshStripeRefundService,
    RefreshStripeRefundReq (..),
    RefreshStripeRefundResp,
    createPayoutService,
    payoutStatusService,
    payoutStatusUpdates,
    cancelPaymentIntentService,
    verifyVPAService,
    mkCreatePayoutOrderReq,
    buildPaymentOrder,
    upsertRefundStatus,
    buildOrderOffer,
    getOrderShortId,
    getTransactionStatus,
    createWalletService,
    walletPostingService,
    walletBalanceService,
    walletReversalService,
    walletVerifyTxnService,
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
import qualified Kernel.External.Payment.Interface.Events.Types as PEInterface
import qualified Kernel.External.Payment.Interface.Types as PInterface
import qualified Kernel.External.Payment.Juspay.Types as Juspay
import qualified Kernel.External.Payout.Interface as PT
import qualified Kernel.External.Payout.Interface.Types as Payout
import qualified Kernel.External.Payout.Juspay.Types as Juspay
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import Kernel.External.Wallet as Wallet
-- import qualified Tools.Wallet as TWallet

import qualified Kernel.External.Wallet.Interface as WalletInterface
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (Value, isNothing)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Text as TU
import Lib.Payment.Domain.Types.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.PaymentOrderOffer as DPaymentOrderOffer
import qualified Lib.Payment.Domain.Types.PaymentOrderSplit as DPaymentOrderSplit
import qualified Lib.Payment.Domain.Types.PaymentTransaction as DTransaction
import qualified Lib.Payment.Domain.Types.PayoutOrder as Payment
import qualified Lib.Payment.Domain.Types.PayoutTransaction as PT
import qualified Lib.Payment.Domain.Types.PersonWallet as DPersonWallet
import Lib.Payment.Domain.Types.Refunds (Refunds (..))
import qualified Lib.Payment.Domain.Types.WalletRewardPosting as DWalletRewardPosting
import Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrderOffer as QPaymentOrderOffer
import qualified Lib.Payment.Storage.Queries.PaymentOrderSplit as QPaymentOrderSplit
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QTransaction
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutTransaction as QPayoutTransaction
import qualified Lib.Payment.Storage.Queries.PersonWallet as QPersonWallet
import qualified Lib.Payment.Storage.Queries.Refunds as QRefunds
import qualified Lib.Payment.Storage.Queries.WalletRewardPosting as QWalletRewardPosting

data PaymentStatusResp
  = PaymentStatus
      { orderId :: Id DOrder.PaymentOrder,
        orderShortId :: ShortId DOrder.PaymentOrder,
        status :: Payment.TransactionStatus,
        bankErrorMessage :: Maybe Text,
        bankErrorCode :: Maybe Text,
        isRetried :: Maybe Bool,
        isRetargeted :: Maybe Bool,
        retargetLink :: Maybe Text,
        refunds :: [Payment.RefundsData],
        payerVpa :: Maybe Text,
        card :: Maybe Payment.CardInfo,
        paymentMethodType :: Maybe Text,
        authIdCode :: Maybe Text,
        txnUUID :: Maybe Text,
        txnId :: Maybe Text, -- <mid>-<shortId>-<attemptNo>
        effectAmount :: Maybe HighPrecMoney,
        offers :: Maybe [Payment.Offer],
        paymentServiceType :: Maybe DOrder.PaymentServiceType,
        paymentFulfillmentStatus :: Maybe PaymentFulfillmentStatus,
        domainEntityId :: Maybe Text,
        amount :: HighPrecMoney,
        validTill :: Maybe UTCTime
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
  | WalletPaymentStatusResp
      { walletStatus :: Maybe DWalletRewardPosting.WalletPostingStatus,
        walletPostingId :: Maybe (Id DWalletRewardPosting.WalletRewardPosting)
      }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data PayoutPaymentStatus = PayoutPaymentStatus
  { status :: Payout.PayoutOrderStatus,
    orderId :: Text,
    accountDetailsType :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data CreatePaymentIntentServiceResp = CreatePaymentIntentServiceResp
  { paymentIntentId :: Text,
    orderId :: Id DOrder.PaymentOrder
  }
  deriving (Show, Eq, Generic)

-- Request type for createPaymentIntentService that doesn't require orderShortId
-- The orderShortId is generated internally by the service
data CreatePaymentIntentServiceReq = CreatePaymentIntentServiceReq
  { amount :: HighPrecMoney,
    applicationFeeAmount :: HighPrecMoney,
    currency :: Currency,
    customer :: Payment.CustomerId,
    paymentMethod :: Payment.PaymentMethodId,
    receiptEmail :: Maybe Text,
    driverAccountId :: Payment.AccountId
  }
  deriving (Show, Eq, Generic)

-- create payment intent --------------------------------------------
-- Note: All orders now get new IDs. Link to ride is via PaymentInvoice table, not order.id

createPaymentIntentService ::
  forall m r c.
  ( EncFlow m r,
    BeamFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  Id Merchant ->
  Maybe (Id MerchantOperatingCity) ->
  Id Person ->
  Maybe (Id DOrder.PaymentOrder) -> -- existing order ID (for retry handling)
  CreatePaymentIntentServiceReq ->
  (Payment.CreatePaymentIntentReq -> m Payment.CreatePaymentIntentResp) ->
  (Payment.PaymentIntentId -> m Payment.CreatePaymentIntentResp) ->
  m CreatePaymentIntentServiceResp
createPaymentIntentService merchantId mbMerchantOpCityId personId mbExistingOrderId createPaymentIntentServiceReq createPaymentIntentCall cancelPaymentIntentCall = do
  mbExistingOrder <- case mbExistingOrderId of
    Just orderId -> QOrder.findById orderId
    Nothing -> pure Nothing

  case mbExistingOrder of
    Nothing -> do
      newOrderId <- Id <$> generateGUID
      newOrderShortId <- generateShortId
      let createPaymentIntentReq = mkPaymentIntentReq newOrderShortId createPaymentIntentServiceReq
      createPaymentIntentResp <- createPaymentIntentCall createPaymentIntentReq -- api call
      paymentOrder <- buildPaymentOrder_ createPaymentIntentResp newOrderId newOrderShortId
      transaction <- buildTransaction paymentOrder createPaymentIntentResp
      logInfo $ "Created new order and payment intent: " <> createPaymentIntentResp.paymentIntentId <> "; amount: " <> show createPaymentIntentReq.amount <> "; applicationFeeAmount: " <> show createPaymentIntentReq.applicationFeeAmount
      QOrder.create paymentOrder
      QTransaction.create transaction
      return CreatePaymentIntentServiceResp {paymentIntentId = createPaymentIntentResp.paymentIntentId, orderId = newOrderId}
    Just existingOrder -> do
      transactions <- QTransaction.findAllByOrderId existingOrder.id
      let mbInProgressTransaction = find (isInProgress . (.status)) transactions
      case mbInProgressTransaction of
        Nothing -> createNewTransaction existingOrder -- if previous all payment intents are already charged or cancelled, then create a new payment intent
        Just existingTransaction -> do
          paymentIntentId <- existingTransaction.txnId & fromMaybeM (InternalError "Transaction doesn't have txnId") -- should never happen
          let newTransactionAmount = createPaymentIntentServiceReq.amount -- changing whole amount
          let newApplicationFeeAmount = createPaymentIntentServiceReq.applicationFeeAmount -- changing application fee amount
          if newTransactionAmount > existingTransaction.amount
            then do
              -- currently we don't support incremental authorization, so just cancel and create new intent
              logError $ "As amount increased cancel old payment intent: " <> paymentIntentId <> " and create new one"
              cancelOldTransaction existingTransaction paymentIntentId -- cancel older payment intent
              createNewTransaction existingOrder -- create new payment intent
            else updateOldTransaction paymentIntentId newTransactionAmount newApplicationFeeAmount existingOrder existingTransaction
  where
    mkPaymentIntentReq :: ShortId DOrder.PaymentOrder -> CreatePaymentIntentServiceReq -> Payment.CreatePaymentIntentReq
    mkPaymentIntentReq orderShortId CreatePaymentIntentServiceReq {..} = Payment.CreatePaymentIntentReq {orderShortId = orderShortId.getShortId, ..}

    isInProgress = (`elem` [Payment.NEW, Payment.PENDING_VBV, Payment.STARTED, Payment.AUTHORIZING])

    cancelOldTransaction :: DTransaction.PaymentTransaction -> Payment.PaymentIntentId -> m ()
    cancelOldTransaction transaction paymentIntentId = do
      resp <- withTryCatch "cancelPaymentIntentCall:cancelOldTransaction" $ withShortRetry $ cancelPaymentIntentCall paymentIntentId
      (errorCode', errorMessage') <- case resp of
        Left exec -> do
          let err = fromException @Payment.StripeError exec
              errorCode = err <&> toErrorCode
              errorMessage = err >>= toMessage
          logError $ "Error while cancelling payment intent: paymentIntentId: " <> paymentIntentId <> "; err: " <> show err <> "; error code: " <> show errorCode <> "; error message: " <> show errorMessage
          pure (errorCode, errorMessage)
        Right paymentIntentResp -> do
          unless (paymentIntentResp.status == Payment.Cancelled) $ do
            -- impossible: paymentIntentResp.status will be always canceled, if the cancellation attempt fails, Stripe will throw error instead of PaymentIntent object
            logError $ "Invalid payment intent status: " <> show paymentIntentResp.status <> "; paymentIntentId: " <> paymentIntentId <> "; should be: canceled"
          pure (Nothing, Nothing)
      -- no need to update order status, because we will create new payment intent linked to this order
      QTransaction.updateStatusAndError transaction.id Payment.CANCELLED errorCode' errorMessage'

    createNewTransaction :: (EncFlow m r, BeamFlow m r) => DOrder.PaymentOrder -> m CreatePaymentIntentServiceResp
    createNewTransaction existingOrder = do
      let createPaymentIntentReq = mkPaymentIntentReq existingOrder.shortId createPaymentIntentServiceReq
      createPaymentIntentResp <- createPaymentIntentCall createPaymentIntentReq -- api call
      let newOrderAmount = createPaymentIntentReq.amount
      transaction <- buildTransaction existingOrder createPaymentIntentResp
      logInfo $ "Updated order amount with new payment intent: " <> createPaymentIntentResp.paymentIntentId <> "; amount: " <> show newOrderAmount
      logInfo $ "Created new payment intent: " <> createPaymentIntentResp.paymentIntentId
      QOrder.updateAmountAndPaymentIntentId existingOrder.id newOrderAmount createPaymentIntentResp.paymentIntentId
      QTransaction.create transaction
      return CreatePaymentIntentServiceResp {paymentIntentId = createPaymentIntentResp.paymentIntentId, orderId = existingOrder.id}

    updateOldTransaction paymentIntentId newTransactionAmount newApplicationFeeAmount existingOrder existingTransaction = do
      let newOrderAmount = createPaymentIntentServiceReq.amount
      when (newOrderAmount /= existingOrder.amount || paymentIntentId /= existingOrder.paymentServiceOrderId) $ do
        logInfo $ "Updated order amount: " <> paymentIntentId <> "; amount: " <> show newOrderAmount
        QOrder.updateAmountAndPaymentIntentId existingOrder.id newOrderAmount paymentIntentId
      when (newTransactionAmount /= existingTransaction.amount || newApplicationFeeAmount /= existingTransaction.applicationFeeAmount) $ do
        logInfo $ "Updated transaction amount: " <> paymentIntentId <> "; amount: " <> show newTransactionAmount <> "; applicationFeeAmount: " <> show newApplicationFeeAmount
        QTransaction.updateAmount existingTransaction.id newTransactionAmount newApplicationFeeAmount
      return CreatePaymentIntentServiceResp {paymentIntentId = existingOrder.paymentServiceOrderId, orderId = existingOrder.id}

    buildPaymentOrder_ ::
      ( EncFlow m r,
        BeamFlow m r
      ) =>
      Payment.CreatePaymentIntentResp ->
      Id DOrder.PaymentOrder ->
      ShortId DOrder.PaymentOrder ->
      m DOrder.PaymentOrder
    buildPaymentOrder_ createPaymentIntentResp orderId orderShortId = do
      now <- getCurrentTime
      clientAuthToken <- encrypt createPaymentIntentResp.clientSecret
      let clientAuthTokenExpiry = Time.UTCTime (Time.fromGregorian 2099 1 1) 0 -- setting infinite expiry for now
      pure
        DOrder.PaymentOrder
          { id = orderId,
            shortId = orderShortId,
            paymentServiceOrderId = createPaymentIntentResp.paymentIntentId,
            requestId = Nothing,
            service = Nothing,
            clientId = Nothing,
            description = Nothing,
            returnUrl = Nothing,
            action = Nothing,
            personId,
            merchantId,
            entityName = Nothing,
            paymentServiceType = Nothing,
            paymentMerchantId = Nothing,
            amount = createPaymentIntentServiceReq.amount,
            currency = createPaymentIntentServiceReq.currency,
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
            sdkPayloadDump = Nothing,
            validTill = Nothing,
            createdAt = now,
            updatedAt = now,
            merchantOperatingCityId = mbMerchantOpCityId,
            paymentFulfillmentStatus = Just FulfillmentPending,
            domainEntityId = Nothing,
            domainTransactionId = Nothing,
            effectAmount = Nothing,
            isMockPayment = Just False,
            paytmTid = Nothing,
            groupId = Nothing,
            vpa = Nothing
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
            txnUUID = Just resp.paymentIntentId,
            txnId = Just resp.paymentIntentId,
            paymentMethodType = Nothing, -- fix it later
            paymentMethod = Nothing, -- fix it later
            respMessage = Nothing,
            respCode = Nothing,
            gatewayReferenceId = Nothing,
            orderId = order.id,
            merchantId = order.merchantId,
            amount = createPaymentIntentServiceReq.amount,
            applicationFeeAmount = createPaymentIntentServiceReq.applicationFeeAmount,
            retryCount = 0,
            currency = createPaymentIntentServiceReq.currency,
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
            updatedAt = now,
            merchantOperatingCityId = order.merchantOperatingCityId
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
      resp <- withTryCatch "cancelPaymentIntentCall:cancelPaymentIntent" $ cancelPaymentIntentCall existingOrder.paymentServiceOrderId
      case resp of
        Left exec -> do
          let err = fromException @Payment.StripeError exec
              errorCode = err <&> toErrorCode
              errorMessage = err >>= toMessage
          logError $ "Error while cancelling payment intent : " <> show err <> "error code : " <> show errorCode <> "error message : " <> show errorMessage
        Right paymentIntentResp -> do
          transaction <- QTransaction.findByTxnId existingOrder.paymentServiceOrderId >>= fromMaybeM (InternalError $ "No transaction found while cancel payment intent: " <> existingOrder.paymentServiceOrderId)
          QOrder.updateStatus existingOrder.id existingOrder.paymentServiceOrderId (Payment.castToTransactionStatus paymentIntentResp.status)
          QTransaction.updateStatusAndError transaction.id (Payment.castToTransactionStatus paymentIntentResp.status) Nothing Nothing

updateForCXCancelPaymentIntentService ::
  forall m r c.
  ( EncFlow m r,
    BeamFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  Payment.PaymentIntentId ->
  (Payment.PaymentIntentId -> HighPrecMoney -> HighPrecMoney -> m ()) ->
  (Payment.PaymentIntentId -> m Payment.CreatePaymentIntentResp) ->
  HighPrecMoney ->
  m Bool
updateForCXCancelPaymentIntentService paymentIntentId capturePaymentIntentCall getPaymentIntentCall cancelTransactionAmount = do
  transaction <- QTransaction.findByTxnId paymentIntentId >>= fromMaybeM (InternalError $ "No transaction found while update payment intent: " <> paymentIntentId)
  let newApplicationFeeAmount = transaction.applicationFeeAmount -- not changing application fee amount
  updateOldTransaction newApplicationFeeAmount transaction
  chargePaymentIntentService paymentIntentId capturePaymentIntentCall getPaymentIntentCall
  where
    updateOldTransaction newApplicationFeeAmount transaction = do
      let newOrderAmount = cancelTransactionAmount -- changing whole amount with cancellation
      logInfo $ "Updated order amount on cancel: " <> paymentIntentId <> "; amount: " <> show newOrderAmount
      logInfo $ "Updated transaction amount on cancel: " <> paymentIntentId <> "; amount: " <> show cancelTransactionAmount <> "; applicationFeeAmount: " <> show newApplicationFeeAmount
      QOrder.updateAmountAndPaymentIntentId transaction.orderId newOrderAmount paymentIntentId
      QTransaction.updateAmount transaction.id cancelTransactionAmount newApplicationFeeAmount

chargePaymentIntentService ::
  forall m r c.
  ( EncFlow m r,
    BeamFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  Payment.PaymentIntentId ->
  (Payment.PaymentIntentId -> HighPrecMoney -> HighPrecMoney -> m ()) ->
  (Payment.PaymentIntentId -> m Payment.CreatePaymentIntentResp) ->
  m Bool
chargePaymentIntentService paymentIntentId capturePaymentIntentCall getPaymentIntentCall = do
  transaction <- QTransaction.findByTxnId paymentIntentId >>= fromMaybeM (InternalError $ "No transaction found while charge payment intent: " <> paymentIntentId)
  if transaction.status `notElem` [Payment.CHARGED, Payment.CANCELLED, Payment.AUTO_REFUNDED] -- if not already charged or cancelled or auto refunded
    then do
      logInfo $ "Capture payment intent: " <> paymentIntentId <> "; amount: " <> show transaction.amount <> "; applicationFeeAmount: " <> show transaction.applicationFeeAmount
      resp <- withTryCatch "capturePaymentIntentCall:chargePaymentIntentService" $ withShortRetry $ capturePaymentIntentCall paymentIntentId transaction.amount transaction.applicationFeeAmount
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
          pure False
        Right () -> do
          paymentIntentResp <- getPaymentIntentCall paymentIntentId
          QTransaction.updateStatusAndError transaction.id (Payment.castToTransactionStatus paymentIntentResp.status) Nothing Nothing
          QOrder.updateStatus transaction.orderId paymentIntentId (Payment.castToTransactionStatus paymentIntentResp.status)
          pure True
    else pure False -- if already charged or cancelled or auto refunded no need to charge again

-- create order -----------------------------------------------------

createOrderService ::
  ( EncFlow m r,
    BeamFlow m r
  ) =>
  Id Merchant ->
  Maybe (Id MerchantOperatingCity) ->
  Id Person ->
  Maybe Seconds ->
  Maybe EntityName ->
  DOrder.PaymentServiceType ->
  Bool ->
  Payment.CreateOrderReq ->
  (Payment.CreateOrderReq -> m Payment.CreateOrderResp) ->
  Maybe (Wallet.CreateWalletReq -> m Wallet.CreateWalletResp) ->
  Bool ->
  Maybe Text ->
  m (Maybe Payment.CreateOrderResp)
createOrderService merchantId mbMerchantOpCityId personId mbPaymentOrderValidity mbEntityName paymentServiceType isTestTransaction createOrderRequest createOrderCall mbCreateWalletCall _isMockPayment mbGroupId = do
  logInfo $ "CreateOrderService: "
  -- Apply test- prefix if isTestTransaction is True and not already prefixed (idempotent)
  let updatedOrderShortId =
        if isTestTransaction
          then "test-" <> createOrderRequest.orderShortId
          else createOrderRequest.orderShortId
      createOrderReq = (createOrderRequest :: Payment.CreateOrderReq) {Payment.orderShortId = updatedOrderShortId}
  when (paymentServiceType == DOrder.Wallet) $ do
    case mbCreateWalletCall of
      Just createWalletCall -> do handleWalletOrder createOrderReq createWalletCall
      Nothing -> throwError $ InternalError "Wallet creation call not found"
  mbExistingOrder <- QOrder.findById (Id createOrderReq.orderId)
  case mbExistingOrder of
    Nothing -> do
      createOrderResp <- createOrderCall createOrderReq -- api call
      paymentOrder <- buildPaymentOrder merchantId mbMerchantOpCityId personId mbPaymentOrderValidity mbEntityName paymentServiceType createOrderReq createOrderResp _isMockPayment mbGroupId
      QOrder.create paymentOrder
      return $ Just createOrderResp
    Just existingOrder -> do
      isOrderExpired <- maybe (pure True) (checkIfExpired existingOrder) existingOrder.clientAuthTokenExpiry
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
                      sdk_payload,
                      sdk_payload_json = existingOrder.sdkPayloadDump
                    }
            Nothing -> return Nothing
  where
    checkIfExpired order expiry = do
      now <- getCurrentTime
      let buffer = secondsToNominalDiffTime 150 -- 2.5 mins of buffer
      return (order.status `notElem` [Payment.CHARGED, Payment.AUTO_REFUNDED] && expiry < addUTCTime buffer now)

    handleWalletOrder createOrderReq createWalletCall = do
      now <- getCurrentTime
      mbPersonWallet <- QPersonWallet.findByPersonId personId.getId
      personWallet <-
        case mbPersonWallet of
          Just personWallet -> do
            pure personWallet
          Nothing -> do
            let createWalletReq = Wallet.CreateWalletReq {customerId = personId.getId, createCashAccount = False, createPointsAccount = True, operationId = createOrderReq.orderShortId}
            void $ createWalletService createWalletReq createWalletCall
            personWalletId <- generateGUID
            let personWallet =
                  DPersonWallet.PersonWallet
                    { id = personWalletId,
                      cashAmount = HighPrecMoney {getHighPrecMoney = 0},
                      cashFromPointsRedemption = HighPrecMoney {getHighPrecMoney = 0},
                      expiredBalance = HighPrecMoney {getHighPrecMoney = 0},
                      personId = personId.getId,
                      pointsAmount = createOrderReq.amount,
                      updatedAt = now,
                      usableCashAmount = HighPrecMoney {getHighPrecMoney = 0},
                      usablePointsAmount = HighPrecMoney {getHighPrecMoney = 0},
                      merchantId = merchantId.getId,
                      merchantOperatingCityId = mbMerchantOpCityId <&> (.getId),
                      createdAt = now
                    }
            QPersonWallet.create personWallet
            return personWallet
      let walletRewardPosting =
            DWalletRewardPosting.WalletRewardPosting
              { id = Id createOrderReq.orderId,
                shortId = ShortId createOrderReq.orderShortId,
                walletId = personWallet.id,
                pointsAmount = createOrderReq.amount,
                cashAmount = HighPrecMoney {getHighPrecMoney = 0},
                postingType = WalletInterface.GRANT,
                status = DWalletRewardPosting.NEW,
                createdAt = now,
                updatedAt = now,
                merchantId = merchantId.getId,
                merchantOperatingCityId = mbMerchantOpCityId <&> (.getId)
              }
      QWalletRewardPosting.create walletRewardPosting

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
  logDebug $ "CreateOrderReq called for order: " <> show req
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
              mandateEndDate = show . utcTimeToPOSIXSeconds <$> order.mandateEndDate,
              basket = Just $ TU.encodeToText req.basket
            }
    (_, _) -> return Nothing

buildPaymentOrder ::
  ( EncFlow m r,
    BeamFlow m r
  ) =>
  Id Merchant ->
  Maybe (Id MerchantOperatingCity) ->
  Id Person ->
  Maybe Seconds ->
  Maybe EntityName ->
  DOrder.PaymentServiceType ->
  Payment.CreateOrderReq ->
  Payment.CreateOrderResp ->
  Bool ->
  Maybe Text ->
  m DOrder.PaymentOrder
buildPaymentOrder merchantId mbMerchantOpCityId personId mbPaymentOrderValidity mbEntityName paymentServiceType req resp isMockPayment mbGroupId = do
  now <- getCurrentTime
  clientAuthToken <- encrypt resp.sdk_payload.payload.clientAuthToken
  let paymentOrderValidTill = mbPaymentOrderValidity <&> (\validity -> addUTCTime (intToNominalDiffTime validity.getSeconds) now)
      mkPaymentOrder =
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
            entityName = mbEntityName,
            paymentServiceType = Just paymentServiceType,
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
            sdkPayloadDump = resp.sdk_payload_json,
            validTill = paymentOrderValidTill,
            createdAt = now,
            updatedAt = now,
            merchantOperatingCityId = mbMerchantOpCityId,
            paymentFulfillmentStatus = Just FulfillmentPending,
            domainEntityId = Nothing,
            domainTransactionId = Nothing,
            effectAmount = Nothing,
            isMockPayment = Just isMockPayment,
            paytmTid = Nothing,
            groupId = mbGroupId,
            vpa = Nothing
          }
  buildPaymentSplit req.orderId mkPaymentOrder req.splitSettlementDetails merchantId mbMerchantOpCityId
  pure mkPaymentOrder

mkPaymentOrderSplit :: (EncFlow m r, BeamFlow m r) => Text -> HighPrecMoney -> PInterface.MBY -> HighPrecMoney -> Maybe Text -> Text -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> m DPaymentOrderSplit.PaymentOrderSplit
mkPaymentOrderSplit vendorId amount mdrBorneBy merchantCommission transactionId paymentOrderId merchantId merchantOperatingCityId = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    DPaymentOrderSplit.PaymentOrderSplit
      { id = id,
        vendorId = vendorId,
        transactionId = transactionId,
        merchantId = merchantId.getId,
        amount = mkPrice Nothing amount,
        mdrBorneBy = mdrBorneBy,
        merchantCommission = mkPrice Nothing merchantCommission,
        merchantOperatingCityId = merchantOperatingCityId <&> (.getId),
        paymentOrderId = Id paymentOrderId,
        createdAt = now,
        updatedAt = now
      }

buildPaymentSplit :: (EncFlow m r, BeamFlow m r) => Text -> DOrder.PaymentOrder -> Maybe PInterface.SplitSettlementDetails -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> m ()
buildPaymentSplit paymentOrderId paymentOrder mbSplitSettlementDetails merchantId merchantOperatingCityId = do
  case mbSplitSettlementDetails of
    Just (PInterface.AmountBased splitSettlementDetails) ->
      createSplits splitSettlementDetails.marketplace.amount splitSettlementDetails.mdrBorneBy splitSettlementDetails.vendor.split
    Just (PInterface.PercentageBased splitSettlementDetails) -> do
      let totalAmount = paymentOrder.amount
          marketplaceAmount = (realToFrac splitSettlementDetails.marketplace.amountPercentage / 100.0) * totalAmount
          convertedVendorSplits =
            map
              ( \v ->
                  PInterface.Split
                    { amount = (realToFrac v.amountPercentage / 100.0) * totalAmount,
                      merchantCommission = (realToFrac v.merchantCommissionPercentage / 100.0) * totalAmount,
                      subMid = v.subMid,
                      uniqueSplitId = v.uniqueSplitId
                    }
              )
              splitSettlementDetails.vendor.split
      createSplits marketplaceAmount splitSettlementDetails.mdrBorneBy convertedVendorSplits
    Nothing -> pure ()
  where
    createSplits marketplaceAmount mdrBorneBy vendorSplits = do
      marketPlaceSplit <- mkPaymentOrderSplit "marketPlace" marketplaceAmount mdrBorneBy 0 Nothing paymentOrderId merchantId merchantOperatingCityId
      vendorSplitEntries <- mapM (\vendor -> mkPaymentOrderSplit vendor.subMid vendor.amount mdrBorneBy vendor.merchantCommission (Just vendor.uniqueSplitId) paymentOrderId merchantId merchantOperatingCityId) vendorSplits
      let splits = marketPlaceSplit : vendorSplitEntries
      QPaymentOrderSplit.createMany splits

offerProccessingLockKey :: Text -> Text
offerProccessingLockKey paymentOrderId = "Offer:Processing:PaymentOrderId:" <> paymentOrderId

buildOrderOffer :: (EncFlow m r, BeamFlow m r) => Id DOrder.PaymentOrder -> Maybe [PInterface.Offer] -> Id Merchant -> Maybe (Id MerchantOperatingCity) -> m ()
buildOrderOffer paymentOrderId mbOffers merchantId merchantOperatingCityId = do
  let poIdTxt = paymentOrderId.getId
  logDebug $ "buildOrderOffer called for paymentOrderId: " <> poIdTxt <> " with " <> show (length (fromMaybe [] mbOffers)) <> " offers"
  case mbOffers of
    Nothing -> do
      logInfo $ "No offers to create for paymentOrderId: " <> paymentOrderId.getId
      pure ()
    Just [] -> do
      logInfo $ "No offers to create for paymentOrderId: " <> paymentOrderId.getId
      pure ()
    Just offers -> do
      Redis.whenWithLockRedis (offerProccessingLockKey poIdTxt) 60 $ do
        existingPaymentOffers <- QPaymentOrderOffer.findByPaymentOrder paymentOrderId
        case existingPaymentOffers of
          [] -> do
            now <- getCurrentTime
            paymentOrderOffers <- mapM (createPaymentOrderOffer now) offers
            logInfo $ "Creating " <> show (length paymentOrderOffers) <> " payment order offers for paymentOrderId: " <> paymentOrderId.getId
            QPaymentOrderOffer.createMany paymentOrderOffers
            logInfo $ "Successfully created payment order offers for paymentOrderId: " <> paymentOrderId.getId
          _ -> do
            logInfo $ "Payment order offers already exist for paymentOrderId: " <> paymentOrderId.getId <> ", skipping creation"
            pure ()
  where
    createPaymentOrderOffer now offer = do
      offerId <- generateGUID
      let offerIdText = fromMaybe "" offer.offerId
          offerCodeText = fromMaybe "" offer.offerCode
          statusText = show offer.status
          responseJsonText = encodeToText offer
          merchantIdText = merchantId.getId
          merchantOperatingCityIdText = maybe "" ((.getId) . cast) merchantOperatingCityId
      logDebug $ "Creating payment order offer with offerId: " <> offerIdText <> ", offerCode: " <> offerCodeText
      pure $
        DPaymentOrderOffer.PaymentOrderOffer
          { id = offerId,
            paymentOrderId = paymentOrderId,
            offer_id = offerIdText,
            offer_code = offerCodeText,
            status = statusText,
            responseJSON = responseJsonText,
            merchantId = merchantIdText,
            merchantOperatingCityId = merchantOperatingCityIdText,
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
  Maybe (Wallet.WalletPostingReq -> m Wallet.WalletPostingResp) ->
  m PaymentStatusResp
orderStatusService personId orderId orderStatusCall mbWalletPostingCall = do
  -- order <- runInReplica $ QOrder.findById orderId >>= fromMaybeM (PaymentOrderDoesNotExist orderId.getId)
  order <- QOrder.findById orderId >>= fromMaybeM (PaymentOrderDoesNotExist orderId.getId)
  when (order.paymentServiceType == Just DOrder.Wallet) $ do
    case mbWalletPostingCall of
      Just walletPostingCall -> handleWalletOrderStatus order walletPostingCall
      Nothing -> throwError $ InternalError "Wallet posting call not found"

  unless (personId == order.personId) $ throwError NotAnExecutor
  let orderStatusReq =
        Payment.OrderStatusReq
          { orderShortId = order.shortId.getShortId,
            orderId = order.id.getId,
            transactionDateTime = Just order.createdAt,
            terminalId = order.paytmTid -- Paytm EDC: send terminal ID for status enquiry
          }
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
                splitSettlementResponse = Nothing,
                retargetLink = Nothing,
                applicationFeeAmount = Nothing,
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
                applicationFeeAmount = Nothing,
                ..
              }
      maybe
        (updateOrderTransaction order orderTxn Nothing)
        ( \transactionUUID' ->
            Redis.whenWithLockRedis (txnProccessingKey transactionUUID') 60 $
              updateOrderTransaction order orderTxn Nothing
        )
        transactionUUID
      mapM_ (void . upsertRefundStatus order) refunds
      void $ QOrder.updateEffectiveAmount orderId effectiveAmount
      res <- withTryCatch "buildOrderOffer:processPaymentStatus" $ buildOrderOffer orderId offers order.merchantId order.merchantOperatingCityId
      case res of
        Left e -> logError $ "buildOrderOffer failed for orderId=" <> orderId.getId <> " err=" <> show e
        Right _ -> pure ()
      return $
        PaymentStatus
          { orderId = orderId,
            orderShortId = order.shortId,
            status = transactionStatus,
            bankErrorCode = orderTxn.bankErrorCode,
            bankErrorMessage = orderTxn.bankErrorMessage,
            isRetried = isRetriedOrder,
            isRetargeted = isRetargetedOrder,
            retargetLink = retargetPaymentLink,
            refunds = refunds,
            payerVpa = (payerVpa <|> ((.payerVpa) =<< upi)),
            authIdCode = ((.authIdCode) =<< paymentGatewayResponse),
            txnUUID = transactionUUID,
            txnId = txnId,
            effectAmount = effectiveAmount,
            offers = offers,
            paymentServiceType = order.paymentServiceType,
            validTill = order.validTill,
            paymentFulfillmentStatus = order.paymentFulfillmentStatus,
            domainEntityId = order.domainEntityId, -- To be filled by Domain
            ..
          }
    _ -> throwError $ InternalError "Unexpected Order Status Response."
  where
    handleWalletOrderStatus paymentOrder walletPostingCall = do
      personWallet <- QPersonWallet.findByPersonId personId.getId >>= fromMaybeM (InternalError $ "Person wallet not found for personId: " <> show personId.getId)
      mbWalletRewardPosting <- QWalletRewardPosting.findByWalletIdAndStatus personWallet.id DWalletRewardPosting.NEW
      case paymentOrder.status of
        Payment.CHARGED -> do
          case mbWalletRewardPosting of
            Just walletRewardPosting -> do
              let walletPostingReq = Wallet.WalletPostingReq {customerId = personId.getId, postingType = walletRewardPosting.postingType, operationId = walletRewardPosting.shortId.getShortId, pointsAmount = round walletRewardPosting.pointsAmount.getHighPrecMoney :: Int}
              walletPostingResp <- walletPostingService walletPostingReq walletPostingCall
              case walletPostingResp.success of
                True -> do
                  QWalletRewardPosting.updateByPrimaryKey walletRewardPosting {DWalletRewardPosting.status = DWalletRewardPosting.SUCCESS}
                False -> do
                  QWalletRewardPosting.updateByPrimaryKey walletRewardPosting {DWalletRewardPosting.status = DWalletRewardPosting.FAILED}
            Nothing -> do
              logError $ "Wallet reward posting with status NEW not found for walletId: " <> show personWallet.id.getId
        _ -> do
          logError $ "Unexpected payment order status: " <> show paymentOrder.status

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
    applicationFeeAmount :: Maybe HighPrecMoney,
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
    Nothing -> when (order.status /= updOrder.status && order.status `notElem` [Payment.CHARGED, Payment.AUTO_REFUNDED]) $ QOrder.updateStatusAndError updOrder errorMessage errorCode
    -- Nothing -> runInReplica $ QTransaction.findNewTransactionByOrderId order.id
    Just transaction -> do
      let updTransaction =
            transaction{statusId = resp.transactionStatusId,
                        status = resp.transactionStatus,
                        paymentMethodType = resp.paymentMethod, -- why paymentMethod and paymentMethodType confused?
                        paymentMethod = resp.paymentMethodType,
                        respMessage = resp.respMessage,
                        respCode = resp.respCode,
                        gatewayReferenceId = resp.gatewayReferenceId,
                        amount = resp.amount,
                        applicationFeeAmount = fromMaybe transaction.applicationFeeAmount resp.applicationFeeAmount,
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
      when (transaction.status `notElem` [Payment.CHARGED, Payment.AUTO_REFUNDED]) $ QTransaction.updateMultiple updTransaction
      when (order.status /= updOrder.status && order.status `notElem` [Payment.CHARGED, Payment.AUTO_REFUNDED]) $ QOrder.updateStatusAndError updOrder errorMessage errorCode

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
        merchantOperatingCityId = order.merchantOperatingCityId,
        ..
      }

-- juspay webhook ----------------------------------------------------------

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
                applicationFeeAmount = Nothing,
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
                applicationFeeAmount = Nothing,
                ..
              }
      maybe
        (updateOrderTransaction order orderTxn Nothing)
        ( \transactionUUID' ->
            Redis.whenWithLockRedis (txnProccessingKey transactionUUID') 60 $ updateOrderTransaction order orderTxn $ Just respDump
        )
        transactionUUID
      mapM_ (void . upsertRefundStatus order) refunds
    _ -> return ()
  return Ack

-- stripe webhook ----------------------------------------------------------

data StripeWebhookData
  = OrderTxnWebhookData (Maybe Text, OrderTxn)
  | RefundWebhookData PEInterface.Refund
  | SkipWebhookData

mkStripeWebhookData ::
  PEInterface.EventObject ->
  StripeWebhookData
mkStripeWebhookData = \case
  -- order transaction update
  PEInterface.PaymentIntentSucceededEvent paymentIntent -> OrderTxnWebhookData $ mkPaymentIntentOrderTxn paymentIntent
  PEInterface.PaymentIntentPaymentFailedEvent paymentIntent -> OrderTxnWebhookData $ mkPaymentIntentOrderTxn paymentIntent
  PEInterface.PaymentIntentProcessingEvent paymentIntent -> OrderTxnWebhookData $ mkPaymentIntentOrderTxn paymentIntent
  PEInterface.PaymentIntentCanceledEvent paymentIntent -> OrderTxnWebhookData $ mkPaymentIntentOrderTxn paymentIntent
  PEInterface.PaymentIntentCreatedEvent paymentIntent -> OrderTxnWebhookData $ mkPaymentIntentOrderTxn paymentIntent
  PEInterface.PaymentIntentRequiresActionEvent paymentIntent -> OrderTxnWebhookData $ mkPaymentIntentOrderTxn paymentIntent
  PEInterface.ChargeSucceededEvent charge -> OrderTxnWebhookData $ mkChargeOrderTxn charge
  PEInterface.ChargeFailedEvent charge -> OrderTxnWebhookData $ mkChargeOrderTxn charge
  PEInterface.ChargeRefundedEvent charge -> OrderTxnWebhookData $ mkChargeOrderTxn charge
  PEInterface.ChargeDisputeCreatedEvent charge -> OrderTxnWebhookData $ mkChargeOrderTxn charge
  PEInterface.ChargeDisputeClosedEvent charge -> OrderTxnWebhookData $ mkChargeOrderTxn charge
  -- refunds update
  PEInterface.ChargeRefundUpdatedEvent _refunds -> SkipWebhookData -- redundant event
  PEInterface.RefundCreatedEvent refunds -> RefundWebhookData refunds
  PEInterface.RefundUpdatedEvent refunds -> RefundWebhookData refunds
  PEInterface.RefundFailedEvent refunds -> RefundWebhookData refunds
  -- setup intent, not implemented currently
  PEInterface.SetupIntentSucceededEvent _setupIntent -> SkipWebhookData
  PEInterface.SetupIntentSetupFailedEvent _setupIntent -> SkipWebhookData
  PEInterface.SetupIntentCanceledEvent _setupIntent -> SkipWebhookData
  PEInterface.SetupIntentCreatedEvent _setupIntent -> SkipWebhookData
  PEInterface.SetupIntentRequiresActionEvent _setupIntent -> SkipWebhookData
  PEInterface.CustomEvent _event -> SkipWebhookData

stripeWebhookService ::
  BeamFlow m r =>
  PEInterface.ServiceEventResp ->
  Text ->
  StripeWebhookData ->
  m AckResponse
stripeWebhookService resp respDump stripeWebhookData = do
  logWarning $ "Webhook response dump: " <> respDump
  logWarning $ "Webhook response: " <> show resp

  case stripeWebhookData of
    OrderTxnWebhookData (mbOrderShortId, orderTxn) -> do
      case mbOrderShortId of
        Just orderShortId -> do
          order <- QOrder.findByShortId (ShortId orderShortId) >>= fromMaybeM (PaymentOrderNotFound orderShortId)
          maybe
            (updateOrderTransaction order orderTxn Nothing)
            ( \transactionUUID' ->
                Redis.whenWithLockRedis (txnStripeProccessingKey transactionUUID') 60 $ updateOrderTransaction order orderTxn $ Just respDump
            )
            orderTxn.transactionUUID
        Nothing -> throwError (InvalidRequest $ "orderShortId not found for eventId: " <> resp.id.getId)
    RefundWebhookData refundsInfo -> do
      case refundsInfo.orderShortId of
        Just orderShortId -> do
          Redis.whenWithLockRedis (refundProccessingKey orderShortId) 60 $ updateRefundsByWebhook refundsInfo -- respDump currently does not stored in refunds table
        Nothing -> throwError (InvalidRequest $ "orderShortId not found for eventId: " <> resp.id.getId)
    SkipWebhookData -> logInfo $ "Skip webhook event: " <> show resp.eventType <> "; eventId: " <> resp.id.getId

  pure Ack

txnStripeProccessingKey :: Text -> Text
txnStripeProccessingKey txnId = "Txn:Stripe:Processing:TxnId-" <> txnId

mkDefaultStripeOrderTxn :: Payment.TransactionStatus -> HighPrecMoney -> Currency -> OrderTxn
mkDefaultStripeOrderTxn transactionStatus amount currency =
  OrderTxn
    { transactionUUID = Nothing,
      transactionStatusId = 0, -- not used in stripe
      txnId = Nothing,
      paymentMethodType = Nothing,
      paymentMethod = Nothing,
      respMessage = Nothing,
      respCode = Nothing,
      gatewayReferenceId = Nothing,
      applicationFeeAmount = Nothing,
      bankErrorMessage = Nothing,
      bankErrorCode = Nothing,
      dateCreated = Nothing,
      mandateStatus = Nothing,
      mandateStartDate = Nothing,
      mandateEndDate = Nothing,
      mandateId = Nothing,
      mandateFrequency = Nothing,
      mandateMaxAmount = Nothing,
      isRetried = Nothing,
      isRetargeted = Nothing,
      retargetLink = Nothing,
      splitSettlementResponse = Nothing,
      ..
    }

mkPaymentIntentOrderTxn :: PEInterface.PaymentIntent -> (Maybe Text, OrderTxn)
mkPaymentIntentOrderTxn PEInterface.PaymentIntent {..} = do
  let transactionStatus = Payment.castToTransactionStatus status
      defaultOrderTxn = mkDefaultStripeOrderTxn transactionStatus amount currency
      orderTxn =
        defaultOrderTxn{txnId = Just paymentIntentId,
                        transactionUUID = Just paymentIntentId,
                        paymentMethod = paymentMethod,
                        dateCreated = Just createdAt,
                        applicationFeeAmount = applicationFeeAmount
                       }
  (orderShortId, orderTxn)

mkChargeOrderTxn :: PEInterface.Charge -> (Maybe Text, OrderTxn)
mkChargeOrderTxn PEInterface.Charge {..} = do
  let transactionStatus = PInterface.castChargeToTransactionStatus status
      defaultOrderTxn = mkDefaultStripeOrderTxn transactionStatus amount currency
      orderTxn =
        defaultOrderTxn{txnId = paymentIntentId,
                        transactionUUID = paymentIntentId,
                        paymentMethod = paymentMethod,
                        dateCreated = Just createdAt,
                        applicationFeeAmount = applicationFeeAmount,
                        bankErrorMessage = failureMessage,
                        bankErrorCode = failureCode
                       }
  (orderShortId, orderTxn)

updateRefundsByWebhook ::
  forall m r.
  ( BeamFlow m r
  ) =>
  PEInterface.Refund ->
  m ()
updateRefundsByWebhook refundInfo = do
  refundsId <- (Id @Refunds <$>) $ refundInfo.refundsId & fromMaybeM (InvalidRequest "refundsId not found")
  refunds <- QRefunds.findById refundsId >>= fromMaybeM (InvalidRequest $ "No refunds matches passed data \"" <> refundsId.getId <> "\" not exist.")
  when (refundInfo.errorCode /= refunds.errorCode || refundInfo.status /= refunds.status) $ do
    now <- getCurrentTime
    let newCompletedAt = calculateCompletedAt refundInfo.status now
    QRefunds.updateRefundsEntryByStripeResponse refunds.idAssignedByServiceProvider refundInfo.errorCode refundInfo.status refunds.isApiCallSuccess newCompletedAt refunds.id

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
  Maybe (Id MerchantOperatingCity) ->
  (Payment.MandateExecutionReq -> m Payment.MandateExecutionRes) ->
  m Payment.MandateExecutionRes
createExecutionService (request, orderId) merchantId mbMerchantOpCityId executionCall = do
  executionOrder <- mkExecutionOrder request
  QOrder.create executionOrder
  buildPaymentSplit orderId executionOrder (PInterface.AmountBased <$> request.splitSettlementDetails) merchantId mbMerchantOpCityId
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
            entityName = Nothing,
            paymentServiceType = Nothing,
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
            sdkPayloadDump = Nothing,
            validTill = Nothing,
            createdAt = now,
            updatedAt = now,
            merchantOperatingCityId = mbMerchantOpCityId,
            paymentFulfillmentStatus = Just FulfillmentPending,
            domainEntityId = Nothing,
            domainTransactionId = Nothing,
            effectAmount = Nothing,
            isMockPayment = Just False,
            paytmTid = Nothing,
            groupId = Nothing,
            vpa = Nothing
          }

--- refunds api ----

createRefundService ::
  ( EncFlow m r,
    BeamFlow m r
  ) =>
  ShortId DOrder.PaymentOrder ->
  (Payment.AutoRefundReq -> m Payment.AutoRefundResp) ->
  m (Maybe Payment.AutoRefundResp)
createRefundService orderShortId refundsCall =
  do
    order <- QOrder.findByShortId orderShortId >>= fromMaybeM (PaymentOrderDoesNotExist orderShortId.getShortId)
    logDebug $ "Payment order details - shortId: " <> orderShortId.getShortId <> ", id: " <> order.id.getId <> ", status: " <> show order.status <> ", amount: " <> show order.amount.getHighPrecMoney <> ", currency: " <> show order.currency <> ", paymentServiceType: " <> show order.paymentServiceType <> ", paymentServiceOrderId: " <> order.paymentServiceOrderId
    logDebug $ "Effect amount: " <> show order.effectAmount
    Redis.whenWithLockRedisAndReturnValue (refundProccessingKey orderShortId.getShortId) 60 $ do
      processRefund order
    >>= \case
      Left _ -> return Nothing
      Right response -> return response
  where
    -- processRefund :: DOrder.PaymentOrder -> m (Maybe Payment.AutoRefundResp)
    processRefund order = do
      existingOrderRefunds <- QRefunds.findLatestByOrderId order.shortId
      if isNothing existingOrderRefunds
        then do
          paymentSplits <- QPaymentOrderSplit.findByPaymentOrder order.id
          splitSettlementDetails <-
            case order.effectAmount of
              Just effectAmount | effectAmount /= order.amount -> return Nothing
              _ -> mkSplitSettlementDetails paymentSplits
          refundId <- generateGUID
          let refundAmount = case order.effectAmount of
                Just effectAmount | effectAmount /= order.amount -> effectAmount
                _ -> order.amount
          let refundReq =
                PInterface.AutoRefundReq
                  { orderId = order.shortId.getShortId,
                    requestId = refundId,
                    amount = refundAmount,
                    splitSettlementDetails
                  }
          logDebug $ "Refund request splitSettlementDetails : " <> show splitSettlementDetails
          refundsEntry <- mkRefundsEntry order.merchantId refundReq.requestId order.shortId refundAmount PInterface.REFUND_PENDING
          QRefunds.create refundsEntry
          resp <- withTryCatch "refundsCall:refundService" (refundsCall refundReq)
          case resp of
            Right response -> do
              mapM_ (void . upsertRefundStatus order) response.refunds
              QRefunds.updateIsApiCallSuccess (Just True) (Id refundReq.requestId)
              return $ Just response
            Left err -> do
              logError $ "Refund API Call Failure with Error: " <> show err
              QRefunds.updateIsApiCallSuccess (Just False) (Id refundReq.requestId)
              return Nothing
        else return Nothing
    mkSplitSettlementDetails :: MonadFlow m => [DPaymentOrderSplit.PaymentOrderSplit] -> m (Maybe PInterface.RefundSplitSettlementDetails)
    mkSplitSettlementDetails paymentSplits = do
      if null paymentSplits
        then return Nothing
        else do
          marketPlaceSplit <- find (\split -> split.vendorId == "marketPlace") paymentSplits & fromMaybeM (InternalError "marketPlace Split Detail not Found")
          let vendorSplits =
                map
                  ( \split ->
                      PInterface.RefundSplit
                        { refundAmount = split.amount.amount,
                          subMid = split.vendorId,
                          uniqueSplitId = fromMaybe split.id.getId split.transactionId
                        }
                  )
                  paymentSplits
              mdrBorneBy = marketPlaceSplit.mdrBorneBy
          return $
            Just $
              PInterface.RefundSplitSettlementDetails
                { marketplace = PInterface.RefundMarketplace marketPlaceSplit.amount.amount,
                  mdrBorneBy,
                  vendor = PInterface.RefundVendor vendorSplits
                }

refundProccessingKey :: Text -> Text
refundProccessingKey refundId = "Refund:Processing:RefundId" <> refundId

mkRefundsEntry :: (BeamFlow m r) => Id Merchant -> Text -> ShortId DOrder.PaymentOrder -> HighPrecMoney -> PInterface.RefundStatus -> m Refunds
mkRefundsEntry merchantId requestId orderShortId amount refundStatus = do
  now <- getCurrentTime
  return $
    Refunds
      { id = Id requestId,
        merchantId = merchantId.getId,
        shortId = ShortId requestId,
        status = refundStatus,
        isApiCallSuccess = if refundStatus == PInterface.REFUND_PENDING then Nothing else Just True,
        orderId = orderShortId,
        refundAmount = amount,
        errorMessage = Nothing,
        errorCode = Nothing,
        idAssignedByServiceProvider = Nothing,
        initiatedBy = Nothing,
        createdAt = now,
        updatedAt = now,
        arn = Nothing,
        completedAt = Nothing
      }

upsertRefundStatus :: (BeamFlow m r) => DOrder.PaymentOrder -> Payment.RefundsData -> m (Maybe Refunds)
upsertRefundStatus order Payment.RefundsData {..} =
  do
    now <- getCurrentTime
    let newCompletedAt = calculateCompletedAt status now
    Redis.whenWithLockRedisAndReturnValue upsertRefundProcessingKey 60 $
      ( do
          QRefunds.findById (Id requestId)
            >>= \case
              Just refundEntry -> do
                QRefunds.updateRefundsEntryByResponse initiatedBy idAssignedByServiceProvider errorMessage errorCode status arn newCompletedAt (Id requestId)
                return $ refundEntry {status = status, initiatedBy = initiatedBy, idAssignedByServiceProvider = idAssignedByServiceProvider, errorMessage = errorMessage, errorCode = errorCode, arn = arn, completedAt = newCompletedAt}
              Nothing -> do
                refundEntry <- mkRefundsEntry order.merchantId requestId order.shortId order.amount status
                QRefunds.create refundEntry
                return refundEntry
      )
    >>= \case
      Left _ -> return Nothing
      Right refundEntry -> return $ Just refundEntry
  where
    upsertRefundProcessingKey = "RefundUpsert:Processing:RequestId:" <> requestId

-- | Calculate completedAt timestamp for terminal refund statuses
calculateCompletedAt :: Payment.RefundStatus -> UTCTime -> Maybe UTCTime
calculateCompletedAt status now =
  case status of
    Payment.REFUND_SUCCESS -> Just now
    Payment.REFUND_FAILURE -> Just now
    _ -> Nothing

txnProccessingKey :: Text -> Text
txnProccessingKey txnUUid = "Txn:Processing:TxnUuid" <> txnUUid

data InitiateStripeRefundReq = InitiateStripeRefundReq
  { orderId :: Id DOrder.PaymentOrder,
    transactionId :: Id DTransaction.PaymentTransaction,
    amount :: Maybe HighPrecMoney,
    driverAccountId :: Payment.AccountId,
    email :: Maybe Text,
    retryRefunds :: Bool
  }

data InitiateStripeRefundResp = InitiateStripeRefundResp
  { refundsId :: Id Refunds,
    stripeRefundId :: Payment.RefundId,
    status :: PInterface.RefundStatus,
    errorCode :: Maybe Text
  }

initiateStripeRefundService ::
  forall m r.
  ( EncFlow m r,
    BeamFlow m r
  ) =>
  InitiateStripeRefundReq ->
  (Payment.CreateRefundReq -> m Payment.CreateRefundResp) ->
  (Payment.GetRefundReq -> m Payment.GetRefundResp) ->
  m (Either Text InitiateStripeRefundResp)
initiateStripeRefundService req createRefundsCall getRefundsCall = do
  order <- QOrder.findById req.orderId >>= fromMaybeM (PaymentOrderDoesNotExist req.orderId.getId) -- TODO should we update PaymentFulfillmentStatus inside of lock?
  Redis.whenWithLockRedisAndReturnValue (refundProccessingKey order.shortId.getShortId) 60 (processRefund' order) >>= \case
    Left () -> return . Left $ "Order refund already initiated: " <> req.orderId.getId
    Right (response :: Either Text InitiateStripeRefundResp) -> return response
  where
    processRefund' :: DOrder.PaymentOrder -> m (Either Text InitiateStripeRefundResp)
    processRefund' order =
      try (processRefund order) >>= \case
        Right resp -> pure $ Right resp
        Left err -> pure $ Left (getErrMessage err)

    -- TODO test this
    getErrMessage :: SomeException -> Text
    getErrMessage exc
      | Just (BaseException baseExc) <- fromException @BaseException exc = fromMaybe "Failed to initiate refunds" $ toMessage baseExc
      | otherwise = "Failed to initiate refunds"

    processRefund :: DOrder.PaymentOrder -> m InitiateStripeRefundResp
    processRefund order = do
      unless (order.status == Payment.CHARGED) $
        throwError (InvalidRequest $ "Order status for refund should be CHARGED, but got: " <> show order.status)
      existingOrderRefunds <- QRefunds.findLatestByOrderId order.shortId
      case existingOrderRefunds of
        Nothing -> initiateNewRefund order
        Just latestRefunds ->
          if req.retryRefunds
            then do
              updRefunds <- snd <$> fetchRefundInfo req.driverAccountId latestRefunds getRefundsCall
              unless (updRefunds.status == PInterface.REFUND_FAILURE) $
                throwError $
                  InvalidRequest ("Retry refunds applicable only if previous refunds was failed; latest refunds status: " <> show updRefunds.status)
              -- TODO should we cancel before create new refund?
              initiateNewRefund order
            else throwError . InvalidRequest $ "Refunds already exists for orderId: " <> order.id.getId

    initiateNewRefund :: DOrder.PaymentOrder -> m InitiateStripeRefundResp
    initiateNewRefund order = do
      transaction <-
        QTransaction.findById req.transactionId
          >>= fromMaybeM (InvalidRequest $ "PaymentTransaction with transactionId \"" <> req.transactionId.getId <> "\"not found.") -- FIXME error code
      let updRefundsAmount = fromMaybe transaction.amount req.amount
      when (updRefundsAmount > transaction.amount) $ do
        throwError . InvalidRequest $ "Couldn't refund more than transaction amount: " <> show transaction.amount

      refundId <- Id @Refunds <$> generateGUID
      refundsEntry <- mkRefundsEntry order.merchantId refundId.getId order.shortId updRefundsAmount PInterface.REFUND_PENDING
      QRefunds.create refundsEntry

      let refundReq =
            PInterface.CreateRefundReq
              { orderShortId = order.shortId.getShortId,
                orderId = order.id.getId,
                refundsId = refundId.getId,
                paymentIntentId = order.paymentServiceOrderId,
                amount = req.amount,
                refundApplicationFee = False, -- FIXME handle in proper way
                driverAccountId = req.driverAccountId,
                email = req.email
              }

      resp <- withTryCatch "createRefundsCall:refundService" (createRefundsCall refundReq)
      case resp of
        Right response -> do
          let isApiCallSuccess = Just True
          now <- getCurrentTime
          let newCompletedAt = calculateCompletedAt response.status now
          QRefunds.updateRefundsEntryByStripeResponse (Just response.id.getRefundId) response.errorCode response.status isApiCallSuccess newCompletedAt refundId
          return $ mkInitiateStripeRefundResp refundId response
        Left err -> do
          logError $ "Create Refund API Call Failure with Error: " <> show err
          QRefunds.updateIsApiCallSuccess (Just False) refundId
          throwError $ InvalidRequest "Create Refund API call error"

    mkInitiateStripeRefundResp refundsId Payment.CreateRefundResp {..} = InitiateStripeRefundResp {stripeRefundId = id, ..}

fetchRefundInfo ::
  forall m r.
  ( EncFlow m r,
    BeamFlow m r
  ) =>
  Payment.AccountId ->
  Refunds ->
  (Payment.GetRefundReq -> m Payment.GetRefundResp) ->
  m (InitiateStripeRefundResp, Refunds)
fetchRefundInfo driverAccountId latestRefunds getRefundsCall = do
  idAssignedByServiceProvider <- latestRefunds.idAssignedByServiceProvider & fromMaybeM (InvalidRequest "Could not refresh order, as Stripe refunds object not found. Please retry refunds")
  let refundReq =
        PInterface.GetRefundReq
          { id = PInterface.RefundId idAssignedByServiceProvider,
            driverAccountId = driverAccountId
          }
  resp <- withTryCatch "getRefundsCall:refundService" (getRefundsCall refundReq)
  case resp of
    Right response -> do
      now <- getCurrentTime
      let newCompletedAt = calculateCompletedAt response.status now
      QRefunds.updateRefundsEntryByStripeResponse (Just response.id.getRefundId) response.errorCode response.status latestRefunds.isApiCallSuccess newCompletedAt latestRefunds.id

      let updRefunds =
            latestRefunds{idAssignedByServiceProvider = Just response.id.getRefundId,
                          errorCode = response.errorCode,
                          status = response.status,
                          completedAt = newCompletedAt
                         }
      pure (mkRefreshInitiateStripeRefundResp latestRefunds.id response, updRefunds)
    Left err -> do
      logError $ "Get Refund API Call Failure with Error: " <> show err
      throwError $ InvalidRequest "Get Refund API call error"
  where
    mkRefreshInitiateStripeRefundResp refundsId Payment.GetRefundResp {refundsId = _refundsId, ..} = InitiateStripeRefundResp {stripeRefundId = id, ..}

data RefreshStripeRefundReq = RefreshStripeRefundReq
  { orderId :: Id DOrder.PaymentOrder,
    driverAccountId :: Payment.AccountId
  }

type RefreshStripeRefundResp = InitiateStripeRefundResp

refreshStripeRefundService ::
  forall m r.
  ( EncFlow m r,
    BeamFlow m r
  ) =>
  RefreshStripeRefundReq ->
  (Payment.GetRefundReq -> m Payment.GetRefundResp) ->
  m RefreshStripeRefundResp
refreshStripeRefundService req getRefundsCall = do
  order <- QOrder.findById req.orderId >>= fromMaybeM (PaymentOrderDoesNotExist req.orderId.getId)
  Redis.whenWithLockRedisAndReturnValue (refundProccessingKey order.shortId.getShortId) 60 (processRefund order) >>= \case
    Left () -> throwError (InvalidRequest $ "Order refund locked: " <> req.orderId.getId)
    Right (response :: RefreshStripeRefundResp) -> return response
  where
    processRefund :: DOrder.PaymentOrder -> m RefreshStripeRefundResp
    processRefund order = do
      existingOrderRefunds <- QRefunds.findLatestByOrderId order.shortId
      case existingOrderRefunds of
        Just latestRefunds -> fst <$> fetchRefundInfo req.driverAccountId latestRefunds getRefundsCall
        Nothing -> throwError . InvalidRequest $ "Refunds does not exist for orderId: " <> order.id.getId

-- payout APIs ---

createPayoutService ::
  ( EncFlow m r,
    BeamFlow m r
  ) =>
  Id Merchant ->
  Maybe (Id MerchantOperatingCity) ->
  Id Person ->
  Maybe [Text] ->
  Maybe EntityName ->
  Text ->
  PT.CreatePayoutOrderReq ->
  (PT.CreatePayoutOrderReq -> m PT.CreatePayoutOrderResp) ->
  m (Maybe PT.CreatePayoutOrderResp, Maybe Payment.PayoutOrder)
createPayoutService merchantId mbMerchantOpCityId _personId mbEntityIds mbEntityName city createPayoutOrderReq createPayoutOrderCall = do
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
            retriedOrderId = Nothing,
            accountDetailsType = (.detailsType) =<< (.beneficiaryDetails) =<< listToMaybe =<< resp.fulfillments, --- for now only one fullfillment supported
            vpa = Just req.customerVpa,
            customerEmail = customerEmail,
            lastStatusCheckedAt = Nothing,
            createdAt = now,
            updatedAt = now,
            merchantOperatingCityId = getId <$> mbMerchantOpCityId
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
                        merchantOperatingCityId = order.merchantOperatingCityId,
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

mkCreatePayoutOrderReq :: Text -> HighPrecMoney -> Maybe Text -> Maybe Text -> Text -> Text -> Maybe Text -> Text -> Text -> Bool -> PT.CreatePayoutOrderReq
mkCreatePayoutOrderReq orderId amount mbPhoneNo mbEmail customerId remark mbCustomerName customerVpa orderType isDynamicWebhookRequired =
  PT.CreatePayoutOrderReq
    { customerPhone = fromMaybe "6666666666" mbPhoneNo,
      customerEmail = fromMaybe "growth@nammayatri.in" mbEmail,
      customerName = fromMaybe "Unknown Customer" mbCustomerName,
      isDynamicWebhookRequired = isDynamicWebhookRequired,
      ..
    }

------ verifyVPA api ------

verifyVPAService ::
  ( EncFlow m r,
    BeamFlow m r
  ) =>
  Payment.VerifyVPAReq ->
  (Payment.VerifyVPAReq -> m Payment.VerifyVPAResp) ->
  m Payment.VerifyVPAResp
verifyVPAService verifyVPAReq verifyVPACall = do
  verifyVPACall verifyVPAReq -- api call

getOrderShortId :: MonadFlow m => PaymentStatusResp -> m (ShortId DOrder.PaymentOrder)
getOrderShortId paymentStatusResp = case paymentStatusResp of
  PaymentStatus {..} -> pure orderShortId
  _ -> throwError $ InternalError "Order Id not found in response."

getTransactionStatus :: MonadFlow m => PaymentStatusResp -> m Payment.TransactionStatus
getTransactionStatus paymentStatusResp = case paymentStatusResp of
  PaymentStatus {..} -> pure status
  _ -> throwError $ InternalError "Transaction Status not found in response."

---------------------  Wallet APIs ---------------------

createWalletService :: (BeamFlow m r) => Wallet.CreateWalletReq -> (Wallet.CreateWalletReq -> m Wallet.CreateWalletResp) -> m Wallet.CreateWalletResp
createWalletService createWalletReq createWalletCall = do
  mbCreateWalletResp <- withTryCatch "createWalletService" (createWalletCall createWalletReq)
  case mbCreateWalletResp of
    Right createWalletResp -> do
      case createWalletResp.success of
        True -> do
          return createWalletResp
        False -> throwError $ InternalError $ "createWalletService failed with error: " <> show createWalletResp
    Left err -> throwError $ InternalError $ "createWalletService failed with error: " <> show err

walletPostingService :: (BeamFlow m r) => Wallet.WalletPostingReq -> (Wallet.WalletPostingReq -> m Wallet.WalletPostingResp) -> m Wallet.WalletPostingResp
walletPostingService walletPostingReq walletPostingCall = do
  mbWalletPostingResp <- withTryCatch "walletPostingService" (walletPostingCall walletPostingReq)
  case mbWalletPostingResp of
    Right walletPostingResp -> do
      case walletPostingResp.success of
        True -> do
          return walletPostingResp
        False -> throwError $ InternalError $ "walletPostingService failed with error: " <> show walletPostingResp
    Left err -> throwError $ InternalError $ "walletPostingService failed with error: " <> show err

walletBalanceService :: (BeamFlow m r) => Wallet.WalletBalanceReq -> (Wallet.WalletBalanceReq -> m Wallet.WalletBalanceResp) -> m Wallet.WalletBalanceResp
walletBalanceService walletBalanceReq walletBalanceCall = do
  mbWalletBalanceResp <- withTryCatch "walletBalanceService" (walletBalanceCall walletBalanceReq)
  case mbWalletBalanceResp of
    Right walletBalanceResp -> do
      case walletBalanceResp.success of
        True -> do
          return walletBalanceResp
        False -> throwError $ InternalError $ "walletBalanceService failed with error: " <> show walletBalanceResp
    Left err -> throwError $ InternalError $ "walletBalanceService failed with error: " <> show err

walletReversalService :: (BeamFlow m r) => Wallet.WalletReversalReq -> (Wallet.WalletReversalReq -> m Wallet.WalletReversalResp) -> m Wallet.WalletReversalResp
walletReversalService walletReversalReq walletReversalCall = do
  mbWalletReversalResp <- withTryCatch "walletReversalService" (walletReversalCall walletReversalReq)
  case mbWalletReversalResp of
    Right walletReversalResp -> do
      case walletReversalResp.success of
        True -> do
          return walletReversalResp
        False -> throwError $ InternalError $ "walletReversalService failed with error: " <> show walletReversalResp
    Left err -> throwError $ InternalError $ "walletReversalService failed with error: " <> show err

walletVerifyTxnService :: (BeamFlow m r) => Wallet.WalletVerifyTxnReq -> (Wallet.WalletVerifyTxnReq -> m Wallet.WalletVerifyTxnResp) -> m Wallet.WalletVerifyTxnResp
walletVerifyTxnService walletVerifyTxnReq walletVerifyTxnCall = do
  mbWalletVerifyTxnResp <- withTryCatch "walletVerifyTxnService" (walletVerifyTxnCall walletVerifyTxnReq)
  case mbWalletVerifyTxnResp of
    Right walletVerifyTxnResp -> do
      case walletVerifyTxnResp.success of
        True -> do
          return walletVerifyTxnResp
        False -> throwError $ InternalError $ "walletVerifyTxnService failed with error: " <> show walletVerifyTxnResp
    Left err -> throwError $ InternalError $ "walletVerifyTxnService failed with error: " <> show err
