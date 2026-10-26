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

allocField, attemptsField, findingSinceField, skippedField, preferredField, preferenceDroppedField, offersField, riderSkipUsedField :: Text
allocField = "alloc"
attemptsField = "attempts"
findingSinceField = "findingSince"
skippedField = "skipped"
preferredField = "preferred"
preferenceDroppedField = "preferredDropped"
offersField = "offers"
riderSkipUsedField = "riderSkipUsed"

readAlloc :: StateFlow m r => BookingId -> m (Maybe AllocationState)
readAlloc bookingId = shared $ Redis.hGet (bookingStateKey bookingId) allocField

-- | Every write sets the hash's TTL, so it is the TTL of the latest writer.
writeAlloc :: StateFlow m r => Int -> BookingId -> AllocationState -> m ()
writeAlloc ttl bookingId st = shared $ Redis.hSetExp (bookingStateKey bookingId) allocField st ttl

clearAlloc :: StateFlow m r => BookingId -> m ()
clearAlloc bookingId = shared $ Redis.hDel (bookingStateKey bookingId) [allocField]

readAttempts :: StateFlow m r => BookingId -> m Int
readAttempts bookingId = shared $ fromMaybe 0 <$> Redis.hGet (bookingStateKey bookingId) attemptsField

writeAttempts :: StateFlow m r => Int -> BookingId -> Int -> m ()
writeAttempts ttl bookingId attempts = shared $ Redis.hSetExp (bookingStateKey bookingId) attemptsField attempts ttl

readFindingSince :: StateFlow m r => BookingId -> UTCTime -> m UTCTime
readFindingSince bookingId createdAt = shared $ fromMaybe createdAt <$> Redis.hGet (bookingStateKey bookingId) findingSinceField

writeFindingSince :: StateFlow m r => Int -> BookingId -> UTCTime -> m ()
writeFindingSince ttl bookingId since = shared $ Redis.hSetExp (bookingStateKey bookingId) findingSinceField since ttl

readSkipped :: StateFlow m r => BookingId -> m [Text]
readSkipped bookingId = shared $ fromMaybe [] <$> Redis.hGet (bookingStateKey bookingId) skippedField

-- | Read-modify-write: call it under the booking lock, like every other writer of the booking's state.
addSkipped :: StateFlow m r => Int -> BookingId -> Text -> m ()
addSkipped ttl bookingId plate = do
  skipped <- readSkipped bookingId
  shared $ Redis.hSetExp (bookingStateKey bookingId) skippedField (nub (plate : skipped)) ttl

-- | The rider has used the one skip this booking allows.
readRiderSkipUsed :: StateFlow m r => BookingId -> m Bool
readRiderSkipUsed bookingId = shared $ fromMaybe False <$> Redis.hGet (bookingStateKey bookingId) riderSkipUsedField

writeRiderSkipUsed :: StateFlow m r => Int -> BookingId -> m ()
writeRiderSkipUsed ttl bookingId = shared $ Redis.hSetExp (bookingStateKey bookingId) riderSkipUsedField True ttl

-- | How many offers this booking has had (the next one's length depends on it).
readOffersMade :: StateFlow m r => BookingId -> m Int
readOffersMade bookingId = shared $ fromMaybe 0 <$> Redis.hGet (bookingStateKey bookingId) offersField

writeOffersMade :: StateFlow m r => Int -> BookingId -> Int -> m ()
writeOffersMade ttl bookingId n = shared $ Redis.hSetExp (bookingStateKey bookingId) offersField n ttl

-- | The plate the rider picked at booking. It stays after the preference is dropped so the leg can say which cab was replaced.
readPreferred :: StateFlow m r => BookingId -> m (Maybe Text)
readPreferred bookingId = shared $ Redis.hGet (bookingStateKey bookingId) preferredField

writePreferred :: StateFlow m r => Int -> BookingId -> Text -> m ()
writePreferred ttl bookingId plate = shared $ Redis.hSetExp (bookingStateKey bookingId) preferredField plate ttl

readPreferenceDropped :: StateFlow m r => BookingId -> m Bool
readPreferenceDropped bookingId = shared $ fromMaybe False <$> Redis.hGet (bookingStateKey bookingId) preferenceDroppedField

writePreferenceDropped :: StateFlow m r => Int -> BookingId -> m ()
writePreferenceDropped ttl bookingId = shared $ Redis.hSetExp (bookingStateKey bookingId) preferenceDroppedField True ttl

-- | The booking is done with allocation (cancelled or finished): the hash and the fallback-push flag go.
clearBookingState :: StateFlow m r => BookingId -> m ()
clearBookingState bookingId = shared $ do
  Redis.del (bookingStateKey bookingId)
  Redis.del (fallbackPushedKey bookingId)

-- | Seconds the hash has left, for a deadline when no timer is armed.
bookingStateTtl :: StateFlow m r => BookingId -> m Integer
bookingStateTtl bookingId = shared $ Redis.ttl (bookingStateKey bookingId)
