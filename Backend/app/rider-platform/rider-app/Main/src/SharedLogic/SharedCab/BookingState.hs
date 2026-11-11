-- | A booking's allocation state, in one Redis hash (`sharedcab:booking:{id}`) with one TTL: the alloc record, the lifetime attempts
-- count, when this FINDING stint began and the plates the rider will not be offered again. The alloc record is deleted on every
-- close while the rest outlives it, so each lives in its own field.
module SharedLogic.SharedCab.BookingState
  ( readAlloc,
    readAllocs,
    writeAlloc,
    clearAlloc,
    readAttempts,
    writeAttempts,
    readFindingSince,
    writeFindingSince,
    readSkipped,
    addSkipped,
    readRiderSkipped,
    addRiderSkipped,
    readOffersMade,
    writeOffersMade,
    readPreferred,
    writePreferred,
    readPreferenceDropped,
    writePreferenceDropped,
    writeDriverPassed,
    clearBookingState,
    bookingStateTtl,
    BookingStateView (..),
    bookingStateFrom,
    readBookingState,
  )
where

import qualified Data.Aeson as A
import Data.List (nub)
import qualified Database.Redis as RawRedis
import qualified Domain.Types.FRFSTicketBooking as DFTB
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.SharedCab.Allocation.Types (AllocationState)
import SharedLogic.SharedCab.Booking (shared)
import SharedLogic.SharedCab.Keys (bookingStateKey, fallbackPushedKey, riderSkippedKey)

type BookingId = Id DFTB.FRFSTicketBooking

type StateFlow m r = (Redis.HedisFlow m r, MonadFlow m)

readField :: (StateFlow m r, FromJSON a) => BookingId -> Text -> m (Maybe a)
readField bookingId = shared . Redis.hGet (bookingStateKey bookingId)

-- | Every write sets the hash's TTL, so it is the TTL of the latest writer.
writeField :: (StateFlow m r, ToJSON a) => Int -> BookingId -> Text -> a -> m ()
writeField ttl bookingId field value = shared $ Redis.hSetExp (bookingStateKey bookingId) field value ttl

readAlloc :: StateFlow m r => BookingId -> m (Maybe AllocationState)
readAlloc bookingId = readField bookingId "alloc"

-- | Every booking's alloc record in one pipelined round trip, in the bookings' order; a failed read is no record.
readAllocs :: StateFlow m r => [BookingId] -> m [Maybe AllocationState]
readAllocs bookingIds = do
  replies <- shared $ do
    keys <- traverse (Redis.buildKey . bookingStateKey) bookingIds
    Redis.runPipelined "sharedCabReadAllocs" (`RawRedis.hget` "alloc") keys
  pure $ either (const (map (const Nothing) bookingIds)) (map (either (const Nothing) (>>= A.decodeStrict))) replies

writeAlloc :: StateFlow m r => Int -> BookingId -> AllocationState -> m ()
writeAlloc ttl bookingId = writeField ttl bookingId "alloc"

clearAlloc :: StateFlow m r => BookingId -> m ()
clearAlloc bookingId = shared $ Redis.hDel (bookingStateKey bookingId) ["alloc"]

readAttempts :: StateFlow m r => BookingId -> m Int
readAttempts bookingId = fromMaybe 0 <$> readField bookingId "attempts"

writeAttempts :: StateFlow m r => Int -> BookingId -> Int -> m ()
writeAttempts ttl bookingId = writeField ttl bookingId "attempts"

readFindingSince :: StateFlow m r => BookingId -> UTCTime -> m UTCTime
readFindingSince bookingId createdAt = fromMaybe createdAt <$> readField bookingId "findingSince"

writeFindingSince :: StateFlow m r => Int -> BookingId -> UTCTime -> m ()
writeFindingSince ttl bookingId = writeField ttl bookingId "findingSince"

readSkipped :: StateFlow m r => BookingId -> m [Text]
readSkipped bookingId = fromMaybe [] <$> readField bookingId "skipped"

-- | Read-modify-write: call it under the booking lock, like every other writer of the booking's state.
addSkipped :: StateFlow m r => Int -> BookingId -> Text -> m ()
addSkipped ttl bookingId plate = do
  skipped <- readSkipped bookingId
  writeField ttl bookingId "skipped" (nub (plate : skipped))

-- | The plates the rider skipped themselves within the last `ttlSec`. Unlike the rest of `skipped` (driver declines, lapsed offers, missed
-- timers), these stay excluded in FALLBACK too, until `ttlSec` has passed since the skip; by then no other cab was found, so the old
-- one may come back. Each plate carries its own skip time, so a later skip does not extend an earlier one.
readRiderSkipped :: StateFlow m r => Int -> UTCTime -> BookingId -> m [Text]
readRiderSkipped ttlSec now bookingId = map fst . liveRiderSkips ttlSec now <$> readRiderSkips bookingId

-- | Read-modify-write, under the booking lock like `addSkipped`. The key lives `ttlSec` from the newest skip.
addRiderSkipped :: StateFlow m r => Int -> UTCTime -> BookingId -> Text -> m ()
addRiderSkipped ttlSec now bookingId plate = do
  skips <- liveRiderSkips ttlSec now <$> readRiderSkips bookingId
  shared $ Redis.setExp (riderSkippedKey bookingId) ((plate, now) : filter ((/= plate) . fst) skips) ttlSec

readRiderSkips :: StateFlow m r => BookingId -> m [(Text, UTCTime)]
readRiderSkips bookingId = fromMaybe [] <$> shared (Redis.get (riderSkippedKey bookingId))

liveRiderSkips :: Int -> UTCTime -> [(Text, UTCTime)] -> [(Text, UTCTime)]
liveRiderSkips ttlSec now = filter (\(_, skippedAt) -> diffUTCTime now skippedAt < fromIntegral ttlSec)

-- | The rider has used the one skip this booking allows.
-- | How many offers this booking has had (the next one's length depends on it).
readOffersMade :: StateFlow m r => BookingId -> m Int
readOffersMade bookingId = fromMaybe 0 <$> readField bookingId "offers"

writeOffersMade :: StateFlow m r => Int -> BookingId -> Int -> m ()
writeOffersMade ttl bookingId = writeField ttl bookingId "offers"

-- | The plate the rider picked at booking. It stays after the preference is dropped so the leg can say which cab was replaced.
readPreferred :: StateFlow m r => BookingId -> m (Maybe Text)
readPreferred bookingId = readField bookingId "preferred"

writePreferred :: StateFlow m r => Int -> BookingId -> Text -> m ()
writePreferred ttl bookingId = writeField ttl bookingId "preferred"

readPreferenceDropped :: StateFlow m r => BookingId -> m Bool
readPreferenceDropped bookingId = fromMaybe False <$> readField bookingId "preferredDropped"

writePreferenceDropped :: StateFlow m r => Int -> BookingId -> m ()
writePreferenceDropped ttl bookingId = writeField ttl bookingId "preferredDropped" True

-- | A cab was released without the rider choosing it (declined, lapsed, timed out at the stop), so another is being found.
writeDriverPassed :: StateFlow m r => Int -> BookingId -> m ()
writeDriverPassed ttl bookingId = writeField ttl bookingId "driverPassed" True

-- | The hash's fields a leg poll shows, from one HGETALL instead of a read per field.
data BookingStateView = BookingStateView
  { allocState :: Maybe AllocationState,
    attemptCount :: Int,
    findingStintSince :: Maybe UTCTime,
    preferredPlate :: Maybe Text,
    preferenceLost :: Bool,
    driverPassed :: Bool
  }

-- | A missing or undecodable field reads as its single-field reader's default.
bookingStateFrom :: [(Text, A.Value)] -> BookingStateView
bookingStateFrom fields =
  BookingStateView
    { allocState = field "alloc",
      attemptCount = fromMaybe 0 (field "attempts"),
      findingStintSince = field "findingSince",
      preferredPlate = field "preferred",
      preferenceLost = fromMaybe False (field "preferredDropped"),
      driverPassed = fromMaybe False (field "driverPassed")
    }
  where
    field :: FromJSON a => Text -> Maybe a
    field name =
      lookup name fields >>= \v -> case A.fromJSON v of
        A.Success a -> Just a
        A.Error _ -> Nothing

readBookingState :: StateFlow m r => BookingId -> m BookingStateView
readBookingState bookingId = bookingStateFrom <$> shared (Redis.hGetAll (bookingStateKey bookingId))

-- | The booking is done with allocation (cancelled or finished): the hash and the fallback-push flag go.
clearBookingState :: StateFlow m r => BookingId -> m ()
clearBookingState bookingId = shared $ do
  Redis.del (bookingStateKey bookingId)
  Redis.del (fallbackPushedKey bookingId)
  Redis.del (riderSkippedKey bookingId)

-- | Seconds the hash has left, for a deadline when no timer is armed.
bookingStateTtl :: StateFlow m r => BookingId -> m Integer
bookingStateTtl bookingId = shared $ Redis.ttl (bookingStateKey bookingId)
