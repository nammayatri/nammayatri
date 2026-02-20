{-
  Finance.Invoice.InvoiceNumber

  Structured invoice number generation with Redis-based daily sequences.
  Format: DDMMYY-MERCHANT_ABBR-PURPOSE-TYPE-XXXXXX

  Moved from rider-app SharedLogic.PaymentInvoice to be shared across modules.
-}
module Lib.Finance.Invoice.InvoiceNumber
  ( generateInvoiceNumber,
    abbreviateMerchant,
    parseInvoiceNumberSequence,

    -- * Purpose abbreviations
    purposeRideFare,
    purposeTip,
    purposeRideTip,
    purposeCancellation,
    purposeSubscription,

    -- * Type abbreviations
    typePayment,
    typeRefund,
  )
where

import Control.Lens ((^?), _head)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (UTCTime), addUTCTime, diffUTCTime, fromGregorian, toGregorian, utctDay)
import Kernel.Prelude hiding (length, replicate)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Utils.Common (CacheFlow, MonadFlow, getCurrentTime, nominalDiffTimeToSeconds)
import Prelude (length, map, replicate)

-- | Purpose abbreviations
purposeRideFare, purposeTip, purposeRideTip, purposeCancellation, purposeSubscription :: Text
purposeRideFare = "RF"
purposeTip = "T"
purposeRideTip = "TRF"
purposeCancellation = "C"
purposeSubscription = "S"

-- | Type abbreviations
typePayment, typeRefund :: Text
typePayment = "PMT"
typeRefund = "REF"

-- | Generate invoice number
-- Format: DDMMYY-MERCHANT_ABBR-PURPOSE-TYPE-XXXXXX
-- Examples:
--   - 120126-NY-RF-PMT-000001 (Namma Yatri ride fare payment)
--   - 120126-LC-S-PMT-000002 (Lynx Cabs subscription)
--   - 120126-NY-TRF-REF-000003 (ride+tip refund)
-- Uses Redis INCR for atomic sequence (global per date)
-- Falls back to database query if Redis key doesn't exist
generateInvoiceNumber ::
  (MonadFlow m, CacheFlow m r, Redis.HedisFlow m r) =>
  Text -> -- Merchant short ID (e.g., "NAMMA_YATRI")
  Text -> -- Purpose abbreviation (e.g., "S", "RF")
  Text -> -- Type abbreviation (e.g., "PMT", "REF")
  UTCTime ->
  m (Maybe Text) -> -- DB fallback: get latest invoice number for sequence recovery
  m Text
generateInvoiceNumber merchantShortId purposeAbbr typeAbbr createdAt dbFallback = do
  let (year, month, day) = toGregorian $ utctDay createdAt
      yy = year `mod` 100
      dateStr = T.pack $ padLeft 2 '0' (show day) <> padLeft 2 '0' (show month) <> padLeft 2 '0' (show yy)
      merchantAbbr = abbreviateMerchant merchantShortId

  -- Get next sequence number (Redis with database fallback, resets daily)
  seqNum <- getNextSequenceForDate dateStr createdAt dbFallback

  -- Format: DDMMYY-MERCHANT_ABBR-PURPOSE-TYPE-XXXXXX
  return $ dateStr <> "-" <> merchantAbbr <> "-" <> purposeAbbr <> "-" <> typeAbbr <> "-" <> T.pack (padLeft 6 '0' (show seqNum))

-- | Abbreviate merchant shortId by taking first letter of each word
-- "NAMMA_YATRI" → "NY", "LYNX_CABS" → "LC", "BRIDGE_CABS" → "BC"
abbreviateMerchant :: Text -> Text
abbreviateMerchant merchantShortId =
  let parts = T.splitOn "_" merchantShortId
      firstLetters = T.concat $ Prelude.map (T.take 1 . T.toUpper) parts
   in if T.null firstLetters then merchantShortId else firstLetters

-- | Global per-date sequence generator (resets daily, shared across all invoices)
-- Uses Redis lock to prevent race conditions in DB fallback case
getNextSequenceForDate ::
  (MonadFlow m, CacheFlow m r, Redis.HedisFlow m r) =>
  Text -> -- dateStr in DDMMYY format
  UTCTime ->
  m (Maybe Text) -> -- DB fallback: get latest invoice number
  m Int
getNextSequenceForDate dateStr createdAt dbFallback = do
  let redisKey = "FinanceInvoiceSequence:" <> dateStr
      lockKey = "FinanceInvoiceSequenceLock:" <> dateStr

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
            -- Initialize from database using caller-provided fallback
            mbLatestInvoiceNumber <- dbFallback
            let currentDate = utctDay createdAt
                startSeq =
                  case mbLatestInvoiceNumber >>= parseInvoiceNumberSequence of
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
      let midnightToday = UTCTime (utctDay now) 0
          midnightTomorrow = addUTCTime 86400 midnightToday
          secondsUntilMidnight = nominalDiffTimeToSeconds $ diffUTCTime midnightTomorrow now
      when (secondsUntilMidnight > 0) $ Hedis.expire key $ secondsUntilMidnight.getSeconds

-- | Parse invoice number in format DDMMYY-MERCHANT_ABBR-PURPOSE-TYPE-XXXXXX
-- Returns (sequenceNumber, date) if parse succeeds
parseInvoiceNumberSequence :: Text -> Maybe (Int, Day)
parseInvoiceNumberSequence invoiceNumber = do
  let parts = T.splitOn "-" invoiceNumber
  guard (Prelude.length parts == 5) -- DDMMYY-MERCHANT-PURPOSE-TYPE-XXXXXX has 5 parts
  datePart <- parts ^? _head
  seqPart <- drop 4 parts ^? _head -- Last part is sequence
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
