{-# LANGUAGE ApplicativeDo #-}

module SharedLogic.PaymentInvoice
  ( generateInvoiceNumber,
    buildInvoice,
    createPaymentInvoiceAfterOrder,
    createOrUpdateRefundInvoice,
    refundStatusToInvoiceStatus,
    refundPurposeToPaymentPurpose,
  )
where

import qualified Data.Text as T
import Data.Time (Day, UTCTime (UTCTime), addUTCTime, diffUTCTime, fromGregorian, toGregorian, utctDay)
import qualified Domain.Types.Booking as DBooking
import Domain.Types.Extra.MerchantPaymentMethod (CardType (..), PaymentInstrument (..))
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMerchant
import Domain.Types.PaymentInvoice (InvoicePaymentStatus (..), InvoiceType (..), PaymentPurpose (..))
import qualified Domain.Types.PaymentInvoice as DPI
import qualified Domain.Types.RefundRequest as DRefundRequest
import qualified Domain.Types.Ride as DRide
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude hiding (length, replicate)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, Currency, EsqDBFlow, HighPrecMoney, MonadFlow, generateGUID, getCurrentTime, nominalDiffTimeToSeconds)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Storage.Queries.PaymentInvoice as QPaymentInvoice
import qualified Storage.Queries.PaymentInvoiceExtra as QPaymentInvoiceExtra
import Prelude (length, map, replicate)

-- | Generate invoice number for payment invoice
-- Format: DDMMYY-MERCHANT_ABBR-PURPOSE-TYPE-XXXXXX
-- Examples:
--   - 120126-NY-RF-PMT-000001 (Namma Yatri ride payment)
--   - 120126-LC-T-PMT-000002 (Lynx Cabs tip payment)
--   - 120126-NY-TRF-PMT-000003 (ride with tip)
-- Uses Redis INCR for atomic sequence (global per date)
-- Falls back to database query if Redis key doesn't exist
generateInvoiceNumber ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  ShortId DMerchant.Merchant ->
  DPI.PaymentPurpose ->
  DPI.InvoiceType ->
  UTCTime ->
  m Text
generateInvoiceNumber merchantShortId purpose invoiceType createdAt = do
  let (year, month, day) = toGregorian $ utctDay createdAt
      -- Format date as DDMMYY (6 chars)
      yy = year `mod` 100
      dateStr = T.pack $ padLeft 2 '0' (show day) <> padLeft 2 '0' (show month) <> padLeft 2 '0' (show yy)
      merchantAbbr = abbreviateMerchant (getShortId merchantShortId)
      purposeStr = showPurpose purpose
      typeStr = showInvoiceType invoiceType

  -- Get next sequence number (Redis with database fallback, resets daily, global for all rides)
  seqNum <- getNextSequenceForDate dateStr createdAt

  -- Format: DDMMYY-MERCHANT_ABBR-PURPOSE-TYPE-XXXXXX
  return $ dateStr <> "-" <> merchantAbbr <> "-" <> purposeStr <> "-" <> typeStr <> "-" <> T.pack (padLeft 6 '0' (show seqNum))

-- | Abbreviate merchant shortId by taking first letter of each word
-- "NAMMA_YATRI" → "NY", "LYNX_CABS" → "LC", "BRIDGE_CABS" → "BC"
abbreviateMerchant :: Text -> Text
abbreviateMerchant merchantShortId =
  let parts = T.splitOn "_" merchantShortId
      firstLetters = T.concat $ Prelude.map (T.take 1 . T.toUpper) parts
   in if T.null firstLetters then merchantShortId else firstLetters

-- | Global per-date sequence generator (resets daily, shared across all rides)
-- Uses Redis lock to prevent race conditions in DB fallback case
getNextSequenceForDate ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  Text -> -- dateStr in DDMMYY format
  UTCTime ->
  m Int
getNextSequenceForDate dateStr createdAt = do
  let redisKey = "InvoiceSequence:" <> dateStr
      lockKey = "InvoiceSequenceLock:" <> dateStr

  -- First check if key exists
  mbCounter <- Hedis.safeGet redisKey
  case (mbCounter :: Maybe Integer) of
    Just _ -> pure () -- Key exists, no initialization needed
    Nothing -> do
      -- Key doesn't exist - use lock to initialize from DB
      Redis.withWaitOnLockRedisWithExpiry lockKey 5 10 $ do
        -- Double-check if key was created while waiting for lock
        mbCounter' <- Hedis.safeGet redisKey
        case (mbCounter' :: Maybe Integer) of
          Just _ -> pure () -- Another process initialized it
          Nothing -> do
            -- Initialize from database
            mbLatest <- QPaymentInvoiceExtra.findLatestGlobal
            let currentDate = utctDay createdAt
                startSeq =
                  case mbLatest >>= (parseInvoiceNumberSequence . (.invoiceNumber)) of
                    Just (lastSeq, lastDate) | lastDate == currentDate -> lastSeq
                    _ -> 0 -- Start at 0 so first INCR gives 1
            Hedis.set redisKey (fromIntegral startSeq :: Integer)
            -- Set expiry only when initializing key (not on every increment)
            setExpiry redisKey

  -- Now atomically increment and return
  seqNum <- Hedis.incr redisKey
  return $ fromIntegral seqNum
  where
    setExpiry key = do
      now <- getCurrentTime
      -- Expire at end of day (midnight next day)
      let midnightToday = UTCTime (utctDay now) 0
          midnightTomorrow = addUTCTime 86400 midnightToday -- 24 * 60 * 60 = 86400 seconds
          secondsUntilMidnight = nominalDiffTimeToSeconds $ diffUTCTime midnightTomorrow now
      when (secondsUntilMidnight > 0) $ Hedis.expire key $ secondsUntilMidnight.getSeconds

-- | Show PaymentPurpose as abbreviated string for invoice number
showPurpose :: DPI.PaymentPurpose -> Text
showPurpose = \case
  RIDE -> "RF" -- Ride Fare
  TIP -> "T" -- Tip
  RIDE_TIP -> "TRF" -- Tip + Ride Fare
  CANCELLATION_FEE -> "C" -- Cancellation

-- | Show InvoiceType as abbreviated string for invoice number
showInvoiceType :: DPI.InvoiceType -> Text
showInvoiceType = \case
  PAYMENT -> "PMT" -- Payment
  REFUNDS -> "REF" -- Refund

-- | Parse invoice number in format DDMMYY-MERCHANT_ABBR-PURPOSE-TYPE-XXXXXX
-- Returns (sequenceNumber, date) if parse succeeds
parseInvoiceNumberSequence :: Text -> Maybe (Int, Day)
parseInvoiceNumberSequence invoiceNumber = do
  let parts = T.splitOn "-" invoiceNumber
  guard (Prelude.length parts == 5) -- DDMMYY-MERCHANT-PURPOSE-TYPE-XXXXXX has 5 parts
  -- Use safe list access instead of unsafe !!
  datePart <- listToMaybe parts
  seqPart <- listToMaybe (drop 4 parts) -- Last part is sequence
  guard (T.length datePart == 6) -- DDMMYY = 6 characters
  guard (T.length seqPart == 6) -- XXXXXX = 6 characters
  seqNum <- readMaybe (T.unpack seqPart)
  -- Parse DDMMYY
  dd <- readMaybe (T.unpack $ T.take 2 datePart)
  mm <- readMaybe (T.unpack $ T.take 2 $ T.drop 2 datePart)
  yy <- readMaybe (T.unpack $ T.drop 4 datePart)
  let fullYear = 2000 + yy
  pure (seqNum, fromGregorian fullYear mm dd)

-- | Helper function to pad string with character on the left
padLeft :: Int -> Char -> String -> String
padLeft len char str =
  let padding = len - Prelude.length str
   in if padding > 0
        then Prelude.replicate padding char ++ str
        else str

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

  -- Check if invoice already exists
  mbExistingInvoice <- QPaymentInvoiceExtra.findByRideIdAndTypeAndPurpose rideId DPI.PAYMENT paymentPurpose
  case mbExistingInvoice of
    Just _ -> pure () -- Invoice already exists, skip creation
    Nothing -> do
      case mbPaymentOrderInfo of
        Just (paymentOrderId, orderAmount, orderCurrency) -> do
          -- Online payment: create invoice with order details
          paymentInvoice <- buildInvoice merchantShortId rideId (Just paymentOrderId) DPI.PAYMENT paymentPurpose paymentStatus orderAmount orderCurrency paymentInstrument booking.merchantId booking.merchantOperatingCityId now
          QPaymentInvoice.create paymentInvoice
        Nothing -> do
          -- Cash payment: create invoice without order
          paymentInvoice <- buildInvoice merchantShortId rideId Nothing DPI.PAYMENT paymentPurpose paymentStatus booking.estimatedFare.amount booking.estimatedFare.currency paymentInstrument booking.merchantId booking.merchantOperatingCityId now
          QPaymentInvoice.create paymentInvoice

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
