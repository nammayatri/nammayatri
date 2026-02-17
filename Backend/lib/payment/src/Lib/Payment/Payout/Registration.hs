{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Payment.Payout.Registration
  ( RegistrationResult (..),
    RegistrationStatusResult (..),
    registrationAmount,
    initiateRegistration,
    processRegistrationPayment,
    refundRegistrationAmount,
  )
where

import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Payment.Interface.Types as PInterface
import qualified Kernel.External.Payout.Interface.Types as IPayout
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InternalError, InvalidRequest))
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DCommon
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder

-- | Fixed registration amount — ₹2, no GST
registrationAmount :: HighPrecMoney
registrationAmount = 2.0

-- | Result of initiating registration payment
data RegistrationResult = RegistrationResult
  { orderId :: Id DOrder.PaymentOrder,
    orderResp :: PInterface.CreateOrderResp
  }
  deriving (Show)

-- | Result of processing registration payment status
data RegistrationStatusResult = RegistrationStatusResult
  { paymentStatus :: Payment.TransactionStatus,
    capturedVpa :: Maybe Text,
    refundInitiated :: Bool
  }
  deriving (Show)

-- ---------------------------------------------------------------------------
-- 1. Initiate Registration (create ₹2 payment order)
-- ---------------------------------------------------------------------------

-- | Create a ₹2 PaymentOrder with entityName = PAYOUT_REGISTRATION.
--   Returns the orderId + SDK payload for the frontend.
initiateRegistration ::
  ( EncFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    FinanceBeamFlow.BeamFlow m r
  ) =>
  Id DCommon.Merchant ->
  Maybe (Id DCommon.MerchantOperatingCity) ->
  Id DCommon.Person ->
  (PInterface.CreateOrderReq -> m PInterface.CreateOrderResp) -> -- Juspay create order call
  Text -> -- customerPhone
  Text -> -- customerEmail
  Maybe Text -> -- customerFirstName
  Maybe Text -> -- customerLastName
  m RegistrationResult
initiateRegistration merchantId mbMerchantOpCityId personId createOrderCall customerPhone customerEmail customerFirstName customerLastName = do
  orderId <- generateGUID
  orderShortId <- generateShortId

  let createOrderReq =
        PInterface.CreateOrderReq
          { orderId = orderId,
            orderShortId = orderShortId.getShortId,
            amount = registrationAmount,
            customerId = personId.getId,
            customerEmail = customerEmail,
            customerPhone = customerPhone,
            customerFirstName = customerFirstName,
            customerLastName = customerLastName,
            createMandate = Nothing,
            mandateMaxAmount = Nothing,
            mandateFrequency = Nothing,
            mandateEndDate = Nothing,
            mandateStartDate = Nothing,
            optionsGetUpiDeepLinks = Nothing,
            metadataExpiryInMins = Nothing,
            splitSettlementDetails = Nothing,
            metadataGatewayReferenceId = Nothing,
            basket = Nothing
          }

  logInfo $ "Initiating payout registration for person " <> personId.getId <> " | orderId: " <> orderId <> " | amount: " <> show registrationAmount

  mbCreateOrderResp <-
    DPayment.createOrderService
      merchantId
      mbMerchantOpCityId
      personId
      Nothing -- mbPaymentOrderValidity
      (Just DCommon.PAYOUT_REGISTRATION)
      DOrder.Normal
      False -- isTestTransaction
      createOrderReq
      createOrderCall
      Nothing -- mbCreateWalletCall
      False -- isMockPayment
      Nothing -- mbGroupId
  createOrderResp <- case mbCreateOrderResp of
    Just resp -> pure resp
    Nothing -> throwError $ InternalError "Failed to create registration payment order"

  pure $
    RegistrationResult
      { orderId = Id orderId,
        orderResp = createOrderResp
      }

-- ---------------------------------------------------------------------------
-- 2. Process Registration Payment (status check / webhook)
-- ---------------------------------------------------------------------------

-- | Process registration payment status. On CHARGED:
--   1. Captures VPA from payer
--   2. Stores VPA on PaymentOrder.domainEntityId (Option A)
--   3. Auto-refunds if flag is set
processRegistrationPayment ::
  ( EncFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    FinanceBeamFlow.BeamFlow m r
  ) =>
  Id DOrder.PaymentOrder ->
  Payment.TransactionStatus ->
  Maybe Text -> -- payerVpa from Juspay response
  Bool -> -- autoRefund
  (IPayout.CreatePayoutOrderReq -> m IPayout.CreatePayoutOrderResp) -> -- payout call
  Text -> -- remark for payout
  Text -> -- orderType for payout
  Text -> -- city
  m RegistrationStatusResult
processRegistrationPayment orderId transactionStatus mbPayerVpa autoRefund createPayoutOrderCall remark orderType city = do
  logDebug $ "Processing registration payment for order " <> orderId.getId <> " | status: " <> show transactionStatus

  case transactionStatus of
    Payment.CHARGED -> do
      -- Store captured VPA on order (Option A)
      whenJust mbPayerVpa $ \vpa -> do
        logInfo $ "Captured payer VPA " <> vpa <> " for registration order " <> orderId.getId
        QOrder.updateVpa orderId (Just vpa)

      -- Auto-refund if enabled
      refundDone <-
        if autoRefund
          then do
            logInfo $ "Auto-refunding registration amount for order " <> orderId.getId
            result <- try @_ @SomeException $ refundRegistrationAmount orderId createPayoutOrderCall remark orderType city
            case result of
              Right (Just (PayoutRequest.PayoutInitiated _ _)) -> pure True
              Right _ -> pure False
              Left err -> do
                logError $ "Auto-refund failed for order " <> orderId.getId <> ": " <> show err
                pure False
          else pure False

      pure $
        RegistrationStatusResult
          { paymentStatus = Payment.CHARGED,
            capturedVpa = mbPayerVpa,
            refundInitiated = refundDone
          }
    _ ->
      pure $
        RegistrationStatusResult
          { paymentStatus = transactionStatus,
            capturedVpa = Nothing,
            refundInitiated = False
          }

-- ---------------------------------------------------------------------------
-- 3. Refund Registration Amount
-- ---------------------------------------------------------------------------

-- | Refund the registration amount to the driver.
--   Only needs the orderId — all data comes from PaymentOrder.
--   Idempotent: skips if a REGISTRATION_REFUND PayoutRequest already exists.
refundRegistrationAmount ::
  ( EncFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    FinanceBeamFlow.BeamFlow m r
  ) =>
  Id DOrder.PaymentOrder ->
  (IPayout.CreatePayoutOrderReq -> m IPayout.CreatePayoutOrderResp) -> -- payout call
  Text -> -- remark
  Text -> -- orderType
  Text -> -- city
  m (Maybe PayoutRequest.PayoutResult)
refundRegistrationAmount orderId createPayoutOrderCall remark orderType city = do
  -- 1. Look up the PaymentOrder
  order <- QOrder.findById orderId >>= maybe (throwError $ InvalidRequest $ "Registration order not found: " <> orderId.getId) pure

  -- 2. Verify the order was charged
  unless (order.status == Payment.CHARGED) $ do
    logError $ "Registration order " <> orderId.getId <> " not CHARGED (status: " <> show order.status <> "), skipping refund"
    throwError $ InvalidRequest $ "Registration order not in CHARGED status: " <> show order.status

  -- 3. Idempotency check
  mbExisting <- PayoutRequest.getPayoutRequestByEntity (Just DCommon.REGISTRATION_REFUND) orderId.getId
  case mbExisting of
    Just existing -> do
      logInfo $ "Registration refund already exists for order " <> orderId.getId <> " (PayoutRequest: " <> existing.id.getId <> "), skipping"
      pure Nothing
    Nothing -> do
      -- 4. Get VPA from order.vpa (stored during processRegistrationPayment)
      vpa <- case order.vpa of
        Just v -> pure v
        Nothing -> do
          logError $ "No VPA found on registration order " <> orderId.getId <> ", cannot refund"
          throwError $ InvalidRequest "No VPA captured for this registration order"

      -- 5. Build submission and call payout
      let submission =
            PayoutRequest.PayoutSubmission
              { beneficiaryId = order.personId.getId,
                entityName = DCommon.REGISTRATION_REFUND,
                entityId = orderId.getId,
                entityRefId = Nothing,
                amount = order.amount,
                merchantId = order.merchantId.getId,
                merchantOpCityId = maybe "" (.getId) order.merchantOperatingCityId,
                city = city,
                vpa = vpa,
                customerName = Nothing,
                customerPhone = Nothing,
                customerEmail = Nothing,
                remark = remark,
                orderType = orderType,
                scheduledAt = Nothing
              }

      logInfo $ "Initiating registration refund for order " <> orderId.getId <> " | amount: " <> show order.amount <> " | vpa: " <> vpa
      result <- PayoutRequest.submitPayoutRequest submission createPayoutOrderCall
      pure $ Just result
