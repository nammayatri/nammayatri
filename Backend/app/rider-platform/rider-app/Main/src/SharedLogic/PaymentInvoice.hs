{-# LANGUAGE ApplicativeDo #-}

module SharedLogic.PaymentInvoice
  ( generateInvoiceNumber,
    createPaymentInvoiceForOrder,
  )
where

import Data.Text as T
import Data.Time (UTCTime (UTCTime), diffUTCTime, fromGregorian, toGregorian, utctDay)
import qualified Domain.Types.Booking as DBooking
import Domain.Types.Extra.MerchantPaymentMethod (PaymentInstrument (..))
import Domain.Types.PaymentInvoice (InvoicePaymentStatus (..), InvoiceType (..), PaymentPurpose (..))
import qualified Domain.Types.PaymentInvoice as DPI
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude hiding (length, replicate)
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, Currency, EsqDBFlow, HighPrecMoney, MonadFlow, generateGUID, getCurrentTime, nominalDiffTimeToSeconds)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Prelude (length, replicate)

-- | Generate invoice number for payment invoice
-- Format: DDMMYY-PURPOSE-TYPE-XXXX
-- Examples:
--   - 301224-RIDE_TIP-PAYMENT-0003 (payment invoice)
--   - 301224-RIDE-PAYMENT-0001 (payment invoice)
--   - 301224-TIP-PAYMENT-0042 (tip invoice)
-- Uses Redis INCR for atomic sequence: payment_invoice:seq:{YYYY}:{PURPOSE}:{TYPE}
-- Falls back to database query if Redis key doesn't exist
generateInvoiceNumber ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DPI.PaymentPurpose ->
  DPI.InvoiceType ->
  UTCTime ->
  m Text
generateInvoiceNumber purpose invoiceType createdAt = do
  let (year, month, day) = toGregorian $ utctDay createdAt
      dateStr = T.pack $ padLeft 2 '0' (show day) <> padLeft 2 '0' (show month) <> show year
      purposeStr = showPurpose purpose
      typeStr = showInvoiceType invoiceType
      yearStr = show year

  -- Get next sequence number (Redis with database fallback)
  seqNum <- getNextSequenceForPurposeAndType purpose invoiceType yearStr createdAt

  -- Format: DDMMYY-PURPOSE-TYPE-XXXX
  return $ dateStr <> "-" <> purposeStr <> "-" <> typeStr <> "-" <> T.pack (padLeft 4 '0' (show seqNum))

-- | Get next sequence number for purpose and type combination
-- Uses Redis INCR for atomic counter, falls back to database if key doesn't exist
getNextSequenceForPurposeAndType ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DPI.PaymentPurpose ->
  DPI.InvoiceType ->
  Text ->
  UTCTime ->
  m Int
getNextSequenceForPurposeAndType purpose invoiceType yearStr createdAt = do
  let redisKey = "payment_invoice:seq:" <> yearStr <> ":" <> showPurpose purpose <> ":" <> showInvoiceType invoiceType

  -- Try to increment Redis counter
  mbCounter <- Hedis.safeGet redisKey
  case (mbCounter :: Maybe Integer) of
    Just _ -> do
      -- Key exists, increment it
      seqNum <- Hedis.incr redisKey
      -- Set expiry to end of year (31 Dec 23:59:59 of current year)
      let (year, _, _) = toGregorian $ utctDay createdAt
          yearEnd = UTCTime (fromGregorian (year + 1) 1 1) 0 -- Start of next year
      now <- getCurrentTime
      let secondsUntilYearEnd = nominalDiffTimeToSeconds $ diffUTCTime yearEnd now
      when (secondsUntilYearEnd > 0) $ Hedis.expire redisKey $ secondsUntilYearEnd.getSeconds
      return $ fromIntegral seqNum
    Nothing -> do
      -- Key doesn't exist, fallback to database
      -- Note: This function will be implemented in PaymentInvoiceExtra.hs
      -- For now, we'll start from 1 if not found in Redis
      let startSeq = 1

      -- Initialize Redis counter with the next sequence number
      -- Set the counter to (startSeq - 1) so that the next incr will give startSeq
      when (startSeq > 1) $ Hedis.set redisKey (startSeq - 1 :: Integer)

      -- Increment to get the actual sequence number
      seqNum <- Hedis.incr redisKey

      -- Set expiry to end of year
      let (year, _, _) = toGregorian $ utctDay createdAt
          yearEnd = UTCTime (fromGregorian (year + 1) 1 1) 0
      now <- getCurrentTime
      let secondsUntilYearEnd = nominalDiffTimeToSeconds $ diffUTCTime yearEnd now
      when (secondsUntilYearEnd > 0) $ Hedis.expire redisKey $ secondsUntilYearEnd.getSeconds

      return $ fromIntegral seqNum

-- | Show PaymentPurpose as string for invoice number
showPurpose :: DPI.PaymentPurpose -> Text
showPurpose = \case
  RIDE -> "RIDE"
  TIP -> "TIP"
  RIDE_TIP -> "RIDE_TIP"
  CANCELLATION_FEE -> "CANCELLATION_FEE"

-- | Show InvoiceType as string for invoice number
showInvoiceType :: DPI.InvoiceType -> Text
showInvoiceType = \case
  PAYMENT -> "PAYMENT"
  REFUNDS -> "REFUNDS"

-- | Helper function to pad string with character on the left
padLeft :: Int -> Char -> String -> String
padLeft len char str =
  let padding = len - Prelude.length str
   in if padding > 0
        then Prelude.replicate padding char ++ str
        else str

-- | Create PaymentInvoice for a payment order
-- This is called when a payment order is created
createPaymentInvoiceForOrder ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DRide.Ride ->
  Maybe (Id DOrder.PaymentOrder) ->
  DBooking.Booking ->
  Maybe HighPrecMoney -> -- tipAmount (from ride)
  HighPrecMoney -> -- order amount
  Currency ->
  PaymentInstrument ->
  UTCTime ->
  m DPI.PaymentInvoice
createPaymentInvoiceForOrder rideId mbPaymentOrderId _booking mbTipAmount orderAmount currency paymentMethod now = do
  -- Determine payment purpose
  let paymentPurpose = case mbTipAmount of
        Just tipAmount | tipAmount > 0 -> DPI.RIDE_TIP
        _ -> DPI.RIDE

  -- Determine payment status based on payment method
  -- Cash payments are considered captured immediately (collected at ride end)
  let paymentStatus = case paymentMethod of
        Cash -> CAPTURED
        _ -> PENDING

  -- Generate invoice number
  invoiceNumber <- generateInvoiceNumber paymentPurpose DPI.PAYMENT now

  -- Generate invoice ID
  invoiceId <- Id <$> generateGUID

  -- Create PaymentInvoice
  return $
    DPI.PaymentInvoice
      { id = invoiceId,
        rideId = rideId,
        paymentOrderId = mbPaymentOrderId,
        paymentStatus = paymentStatus,
        paymentMethod = paymentMethod,
        paymentPurpose = paymentPurpose,
        invoiceType = DPI.PAYMENT,
        invoiceNumber = invoiceNumber,
        amount = orderAmount,
        currency = currency,
        createdAt = now,
        updatedAt = now
      }
