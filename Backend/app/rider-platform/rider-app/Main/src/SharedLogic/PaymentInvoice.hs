{-# LANGUAGE ApplicativeDo #-}

module SharedLogic.PaymentInvoice
  ( generateInvoiceNumber,
    buildPaymentInvoiceForOrder,
  )
where

import Data.Text as T
import Data.Time (Day, UTCTime (UTCTime), addUTCTime, diffUTCTime, fromGregorian, toGregorian, utctDay)
import qualified Domain.Types.Booking as DBooking
import Domain.Types.Extra.MerchantPaymentMethod (PaymentInstrument (..))
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.PaymentInvoice (InvoicePaymentStatus (..), InvoiceType (..), PaymentPurpose (..))
import qualified Domain.Types.PaymentInvoice as DPI
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude hiding (length, replicate)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, Currency, EsqDBFlow, HighPrecMoney, MonadFlow, generateGUID, getCurrentTime, nominalDiffTimeToSeconds)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
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

  -- Now atomically increment and return
  seqNum <- Hedis.incr redisKey
  setExpiry redisKey
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
  let datePart = parts !! 0
      seqPart = parts !! 4 -- Last part is sequence
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

-- | Build PaymentInvoice for a payment order (caller persists via QPaymentInvoice.create)
-- This is called when a payment order is created
buildPaymentInvoiceForOrder ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  ShortId DMerchant.Merchant ->
  Id DRide.Ride ->
  Maybe (Id DOrder.PaymentOrder) ->
  DBooking.Booking ->
  Maybe HighPrecMoney -> -- tipAmount (from ride)
  HighPrecMoney -> -- order amount
  Currency ->
  PaymentInstrument ->
  UTCTime ->
  m DPI.PaymentInvoice
buildPaymentInvoiceForOrder merchantShortId rideId mbPaymentOrderId booking mbTipAmount orderAmount currency paymentMethod now = do
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
  invoiceNumber <- generateInvoiceNumber merchantShortId paymentPurpose DPI.PAYMENT now

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
        updatedAt = now,
        merchantId = Just booking.merchantId,
        merchantOperatingCityId = Just booking.merchantOperatingCityId
      }
