-- | A booking's allocation state, in one Redis hash (`sharedcab:booking:{id}`) with one TTL: the alloc record, the lifetime attempts
-- count, when this FINDING stint began and the plates the rider will not be offered again. The alloc record is deleted on every
-- close while the rest outlives it, so each lives in its own field.
module SharedLogic.SharedCab.BookingState
  ( readAlloc,
    writeAlloc,
    clearAlloc,
    readAttempts,
    writeAttempts,
    readFindingSince,
    writeFindingSince,
    readSkipped,
    addSkipped,
    readRiderSkipUsed,
    writeRiderSkipUsed,
    readOffersMade,
    writeOffersMade,
    readPreferred,
    writePreferred,
    readPreferenceDropped,
    writePreferenceDropped,
    clearBookingState,
    bookingStateTtl,
  )
where

import Data.List (nub)
import qualified Domain.Types.FRFSTicketBooking as DFTB
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.SharedCab.Allocation.Types (AllocationState)
import SharedLogic.SharedCab.Booking (shared)
import SharedLogic.SharedCab.Keys (bookingStateKey, fallbackPushedKey)

type BookingId = Id DFTB.FRFSTicketBooking

type StateFlow m r = (Redis.HedisFlow m r, MonadFlow m)

readField :: (StateFlow m r, FromJSON a) => BookingId -> Text -> m (Maybe a)
readField bookingId = shared . Redis.hGet (bookingStateKey bookingId)

-- | Every write sets the hash's TTL, so it is the TTL of the latest writer.
writeField :: (StateFlow m r, ToJSON a) => Int -> BookingId -> Text -> a -> m ()
writeField ttl bookingId field value = shared $ Redis.hSetExp (bookingStateKey bookingId) field value ttl

readAlloc :: StateFlow m r => BookingId -> m (Maybe AllocationState)
readAlloc bookingId = readField bookingId "alloc"

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

-- | The rider has used the one skip this booking allows.
readRiderSkipUsed :: StateFlow m r => BookingId -> m Bool
readRiderSkipUsed bookingId = fromMaybe False <$> readField bookingId "riderSkipUsed"

writeRiderSkipUsed :: StateFlow m r => Int -> BookingId -> m ()
writeRiderSkipUsed ttl bookingId = writeField ttl bookingId "riderSkipUsed" True

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

-- | The booking is done with allocation (cancelled or finished): the hash and the fallback-push flag go.
clearBookingState :: StateFlow m r => BookingId -> m ()
clearBookingState bookingId = shared $ do
  Redis.del (bookingStateKey bookingId)
  Redis.del (fallbackPushedKey bookingId)

-- | Seconds the hash has left, for a deadline when no timer is armed.
bookingStateTtl :: StateFlow m r => BookingId -> m Integer
bookingStateTtl bookingId = shared $ Redis.ttl (bookingStateKey bookingId)
