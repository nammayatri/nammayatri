{-
  Finance.Invoice.InvoiceNumber

  Structured invoice number generation with Redis-based daily sequences.
  Format: DDMMYY-PURPOSE-XXXXXX

  Moved from rider-app SharedLogic.PaymentInvoice to be shared across modules.
-}
module Lib.Finance.Invoice.InvoiceNumber
  ( generateInvoiceNumber,
    parseInvoiceNumberSequence,

    -- * Purpose abbreviations
    purposeRideFare,
    purposeTip,
    purposeRideTip,
    purposeCancellation,
    purposeSubscription,
    purposeDebtSettlement,
    purposeCommission,
    purposeAggregatedCommission,
    purposeRefund,
  )
where

import qualified Data.Text as T
import Data.Time (Day, UTCTime (UTCTime), addUTCTime, diffUTCTime, fromGregorian, toGregorian, utctDay)
import Kernel.Prelude hiding (length, replicate)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Utils.Common (CacheFlow, MonadFlow, getCurrentTime, nominalDiffTimeToSeconds)
import Prelude (length, replicate)

-- | Purpose abbreviations
purposeRideFare, purposeTip, purposeRideTip, purposeCancellation, purposeSubscription, purposeDebtSettlement, purposeCommission, purposeAggregatedCommission, purposeRefund :: Text
purposeRideFare = "RF"
purposeTip = "T"
purposeRideTip = "TRF"
purposeCancellation = "C"
purposeSubscription = "S"
purposeDebtSettlement = "DS"
purposeCommission = "CM"
purposeAggregatedCommission = "CMB"
purposeRefund = "R"

-- | Generate invoice number
-- Format: DDMMYY-PURPOSE-XXXXXX, padded to a fixed 16-character total (including both hyphens).
-- The sequence width flexes with the purpose length so the whole number is always 16 chars.
-- Examples (all 16 chars):
--   - 270426-S-0000008 (subscription, 7-digit seq)
--   - 120126-RF-000001 (ride fare, 6-digit seq)
--   - 120126-TRF-00003 (ride+tip, 5-digit seq)
-- Uses Redis INCR for atomic sequence (global per date, unless key suffix is used for isolation).
-- Falls back to database query if Redis key doesn't exist.
generateInvoiceNumber ::
  (MonadFlow m, CacheFlow m r, Redis.HedisFlow m r) =>
  Text -> -- Purpose abbreviation (e.g., "S", "RF")
  Maybe Text -> -- Optional isolation key (e.g., Just "CMB" to give AggregatedCommission its own counter)
  UTCTime ->
  m (Maybe Text) -> -- DB fallback: get latest invoice number for sequence recovery (must match isolation)
  m Text
generateInvoiceNumber purposeAbbr mbKeySuffix createdAt dbFallback = do
  let (year, month, day) = toGregorian $ utctDay createdAt
      yy = year `mod` 100
      dateStr = T.pack $ padLeft 2 '0' (show day) <> padLeft 2 '0' (show month) <> padLeft 2 '0' (show yy)

  -- Get next sequence number (Redis with database fallback, resets daily)
  seqNum <- getNextSequenceForDate dateStr mbKeySuffix createdAt dbFallback

  -- Format: DDMMYY-PURPOSE-XXXXXX, fixed at 16 chars total
  let seqWidth = max 1 (16 - T.length dateStr - 2 - T.length purposeAbbr)
  return $ dateStr <> "-" <> purposeAbbr <> "-" <> T.pack (padLeft seqWidth '0' (show seqNum))

-- | Per-date sequence generator (resets daily). When 'mbKeySuffix' is 'Just s',
-- the Redis key includes that suffix so the counter is isolated from the global
-- sequence. Uses a Redis lock to prevent race conditions in the DB fallback path.
getNextSequenceForDate ::
  (MonadFlow m, CacheFlow m r, Redis.HedisFlow m r) =>
  Text -> -- dateStr in DDMMYY format
  Maybe Text -> -- isolation key suffix (Nothing = global counter)
  UTCTime ->
  m (Maybe Text) -> -- DB fallback: get latest invoice number (scoped to the same isolation)
  m Int
getNextSequenceForDate dateStr mbKeySuffix createdAt dbFallback = do
  let suffix = maybe "" (\s -> ":" <> s) mbKeySuffix
      redisKey = "FinanceInvoiceSequence" <> suffix <> ":" <> dateStr
      lockKey = "FinanceInvoiceSequenceLock" <> suffix <> ":" <> dateStr

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

-- | Parse invoice number in format DDMMYY-PURPOSE-XXXXXX
-- Returns (sequenceNumber, date) if parse succeeds
parseInvoiceNumberSequence :: Text -> Maybe (Int, Day)
parseInvoiceNumberSequence invoiceNumber = do
  let parts = T.splitOn "-" invoiceNumber
  guard (Prelude.length parts == 3) -- DDMMYY-PURPOSE-XXXXXX has 3 parts
  datePart <- listToMaybe parts
  seqPart <- listToMaybe (drop 2 parts) -- Last part is sequence
  guard (T.length datePart == 6) -- DDMMYY = 6 characters
  guard (not (T.null seqPart))
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
