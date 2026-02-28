{-# LANGUAGE ApplicativeDo #-}

module SharedLogic.PaymentInvoice
  ( generateInvoiceNumber,
    buildInvoice,
    createPaymentInvoiceAfterOrder,
    createInvoiceIfNotExists,
    createOrUpdateRefundInvoice,
    refundStatusToInvoiceStatus,
    refundPurposeToPaymentPurpose,
    showPurpose,
  )
where

import qualified Domain.Types.Booking as DBooking
import Domain.Types.Extra.MerchantPaymentMethod (CardType (..), PaymentInstrument (..))
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMerchant
import Domain.Types.PaymentInvoice (InvoicePaymentStatus (..), InvoiceType (..), PaymentPurpose (..))
import qualified Domain.Types.PaymentInvoice as DPI
import qualified Domain.Types.RefundRequest as DRefundRequest
import qualified Domain.Types.Ride as DRide
import Data.Time (utctDay)
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, Currency, EsqDBFlow, HighPrecMoney, MonadFlow, generateGUID, getCurrentTime)
import qualified Lib.Finance.Invoice.InvoiceNumber as FInvNum
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Storage.Queries.PaymentInvoice as QPaymentInvoice
import qualified Storage.Queries.PaymentInvoiceExtra as QPaymentInvoiceExtra

-- | Generate invoice number for payment invoice using the shared finance-kernel logic.
-- Format: DDMMYY-MERCHANT_ABBR-PURPOSE-TYPE-XXXXXX
-- Examples:
--   - 120126-NY-RF-PMT-000001 (Namma Yatri ride payment)
--   - 120126-LC-T-PMT-000002 (Lynx Cabs tip payment)
--   - 120126-NY-TRF-PMT-000003 (ride with tip)
generateInvoiceNumber ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  ShortId DMerchant.Merchant ->
  DPI.PaymentPurpose ->
  DPI.InvoiceType ->
  UTCTime ->
  m Text
generateInvoiceNumber merchantShortId purpose invoiceType createdAt = do
  let currentDate = utctDay createdAt
      dbFallback = fmap (.invoiceNumber) <$> QPaymentInvoiceExtra.findLatestForDate currentDate
  FInvNum.generateInvoiceNumber
    (getShortId merchantShortId)
    (showPurpose purpose)
    (showInvoiceType invoiceType)
    createdAt
    dbFallback

-- | Show PaymentPurpose as abbreviated string for invoice number
showPurpose :: DPI.PaymentPurpose -> Text
showPurpose = \case
  RIDE -> FInvNum.purposeRideFare
  TIP -> FInvNum.purposeTip
  RIDE_TIP -> FInvNum.purposeRideTip
  CANCELLATION_FEE -> FInvNum.purposeCancellation

-- | Show InvoiceType as abbreviated string for invoice number
showInvoiceType :: DPI.InvoiceType -> Text
showInvoiceType = \case
  PAYMENT -> FInvNum.typePayment
  REFUNDS -> FInvNum.typeRefund

-- | Core function to build an invoice (payment or refund)
-- This is the unified invoice builder used by both payment and refund flows
buildInvoice ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  ShortId DMerchant.Merchant ->
  Id DRide.Ride ->
  Maybe (Id DOrder.PaymentOrder) ->
  DPI.InvoiceType -> -- PAYMENT or REFUNDS
  DPI.PaymentPurpose -> -- RIDE, TIP, RIDE_TIP, CANCELLATION_FEE
  DPI.InvoicePaymentStatus -> -- PENDING, CAPTURED, FAILED
  HighPrecMoney -> -- amount
  Currency ->
  PaymentInstrument ->
  Id DMerchant.Merchant ->
  Id DMerchant.MerchantOperatingCity ->
  UTCTime ->
  m DPI.PaymentInvoice
buildInvoice merchantShortId rideId mbPaymentOrderId invoiceType paymentPurpose paymentStatus amount currency paymentInstrument merchantId merchantOperatingCityId now = do
  -- Generate invoice number
  invoiceNumber <- generateInvoiceNumber merchantShortId paymentPurpose invoiceType now

  -- Generate invoice ID
  invoiceId <- Id <$> generateGUID

  -- Create PaymentInvoice
  return $
    DPI.PaymentInvoice
      { id = invoiceId,
        rideId = rideId,
        paymentOrderId = mbPaymentOrderId,
        paymentStatus = paymentStatus,
        paymentInstrument = paymentInstrument,
        paymentPurpose = paymentPurpose,
        invoiceType = invoiceType,
        invoiceNumber = invoiceNumber,
        amount = amount,
        currency = currency,
        createdAt = now,
        updatedAt = now,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId
      }

-- | Create PaymentInvoice after order creation
-- This is a unified function called after payment order is created (or for cash payments)
-- Handles:
--   - Online payments: Creates invoice with order details (caller passes order info)
--   - Cash payments: Creates invoice without PaymentOrder (payment_order_id = NULL)
-- Parameters:
--   - merchantShortId: For invoice number generation
--   - rideId: The ride this invoice is for
--   - booking: Booking details (for payment instrument, amounts, merchantId)
--   - mbPaymentOrderInfo: For online payments, Just (orderId, amount, currency); Nothing for cash
--   - mbTipAmount: Optional tip amount to determine purpose (RIDE vs RIDE_TIP)
--   - now: Current timestamp
createPaymentInvoiceAfterOrder ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  ShortId DMerchant.Merchant ->
  Id DRide.Ride ->
  DBooking.Booking ->
  Maybe (Id DOrder.PaymentOrder, HighPrecMoney, Currency) -> -- order info for online payments
  Maybe HighPrecMoney -> -- tip amount (if any)
  UTCTime ->
  m ()
createPaymentInvoiceAfterOrder merchantShortId rideId booking mbPaymentOrderInfo mbTipAmount now = do
  -- Determine payment purpose based on tip
  let paymentPurpose = case mbTipAmount of
        Just tipAmount | tipAmount > 0 -> DPI.RIDE_TIP
        _ -> DPI.RIDE
      paymentInstrument = fromMaybe Cash booking.paymentInstrument
      -- Determine payment status: Cash = CAPTURED, Card = PENDING
      paymentStatus = case paymentInstrument of
        Cash -> CAPTURED
        _ -> PENDING

  createInvoiceIfNotExists merchantShortId rideId mbOrderId DPI.PAYMENT paymentPurpose paymentStatus amount currency paymentInstrument booking.merchantId booking.merchantOperatingCityId now
  where
    (mbOrderId, amount, currency) = case mbPaymentOrderInfo of
      Just (orderId, orderAmount, orderCurrency) -> (Just orderId, orderAmount, orderCurrency)
      Nothing -> (Nothing, booking.estimatedFare.amount, booking.estimatedFare.currency)

-- | Create invoice if it doesn't exist
createInvoiceIfNotExists ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  ShortId DMerchant.Merchant ->
  Id DRide.Ride ->
  Maybe (Id DOrder.PaymentOrder) ->
  DPI.InvoiceType ->
  DPI.PaymentPurpose ->
  DPI.InvoicePaymentStatus ->
  HighPrecMoney ->
  Currency ->
  PaymentInstrument ->
  Id DMerchant.Merchant ->
  Id DMerchant.MerchantOperatingCity ->
  UTCTime ->
  m ()
createInvoiceIfNotExists merchantShortId rideId mbPaymentOrderId invoiceType paymentPurpose paymentStatus amount currency paymentInstrument merchantId merchantOperatingCityId now = do
  -- Check if invoice already exists
  mbExistingInvoice <- QPaymentInvoiceExtra.findByRideIdAndTypeAndPurpose rideId invoiceType paymentPurpose
  case mbExistingInvoice of
    Just _ -> pure () -- Invoice already exists, skip creation
    Nothing -> do
      invoice <- buildInvoice merchantShortId rideId mbPaymentOrderId invoiceType paymentPurpose paymentStatus amount currency paymentInstrument merchantId merchantOperatingCityId now
      QPaymentInvoice.create invoice

-- | Create or update refund invoice
-- This is a unified function for refund invoice handling
-- Handles:
--   - Looking up original payment invoice to get payment method
--   - Checking if refund invoice already exists (retry case)
--   - Creating new refund invoice if doesn't exist
--   - Updating existing refund invoice status if exists
createOrUpdateRefundInvoice ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  ShortId DMerchant.Merchant ->
  Id DRide.Ride ->
  Id DOrder.PaymentOrder ->
  DPI.PaymentPurpose ->
  DPI.InvoicePaymentStatus ->
  HighPrecMoney ->
  Currency ->
  Id DMerchant.Merchant ->
  Id DMerchant.MerchantOperatingCity ->
  m ()
createOrUpdateRefundInvoice merchantShortId rideId orderId paymentPurpose invoiceStatus refundAmount currency merchantId merchantOperatingCityId = do
  now <- getCurrentTime
  -- Lookup original payment invoice to get payment method
  mbOriginalInvoice <- QPaymentInvoiceExtra.findByRideIdAndTypeAndPurpose rideId DPI.PAYMENT paymentPurpose
  let paymentInstrument = maybe (Card DefaultCardType) (.paymentInstrument) mbOriginalInvoice
  -- Check if refund invoice for this specific purpose already exists
  mbExistingRefundInvoice <- QPaymentInvoiceExtra.findByRideIdAndTypeAndPurpose rideId DPI.REFUNDS paymentPurpose
  case mbExistingRefundInvoice of
    Just _ -> do
      -- Update existing invoice status (retry case)
      QPaymentInvoiceExtra.updatePaymentStatusByRideIdAndTypeAndPurpose rideId DPI.REFUNDS paymentPurpose invoiceStatus
    Nothing -> do
      -- Create new refund invoice
      refundInvoice <- buildInvoice merchantShortId rideId (Just orderId) DPI.REFUNDS paymentPurpose invoiceStatus refundAmount currency paymentInstrument merchantId merchantOperatingCityId now
      QPaymentInvoice.create refundInvoice

-- Shared helper used by webhook handlers and refund status refresh
refundStatusToInvoiceStatus :: Payment.RefundStatus -> DPI.InvoicePaymentStatus
refundStatusToInvoiceStatus = \case
  Payment.REFUND_PENDING -> DPI.PENDING
  Payment.REFUND_SUCCESS -> DPI.CAPTURED
  Payment.REFUND_FAILURE -> DPI.FAILED
  Payment.MANUAL_REVIEW -> DPI.PENDING
  Payment.REFUND_CANCELED -> DPI.CANCELLED
  Payment.REFUND_REQUIRES_ACTION -> DPI.PENDING

-- | Convert RefundPurpose to PaymentPurpose for correct invoice lookup
-- Used when we need to find the original payment invoice for a refund
refundPurposeToPaymentPurpose :: DRefundRequest.RefundPurpose -> DPI.PaymentPurpose
refundPurposeToPaymentPurpose = \case
  DRefundRequest.RIDE_FARE -> DPI.RIDE
  DRefundRequest.CANCELLATION_FEE -> DPI.CANCELLATION_FEE
