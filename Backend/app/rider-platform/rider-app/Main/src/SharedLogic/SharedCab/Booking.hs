module SharedLogic.SharedCab.Booking
  ( isSharedCabBooking,
    withBookingLock,
    tryWithBookingLock,
    ridersOnBoard,
    recordRiderFix,
    readRiderFix,
    recordCancelReason,
    readCancelReason,
    findingOnRoute,
    shared,
    nonTerminalStatuses,
    liveBookingsForVehicle,
    plateBookings,
    candidateIds,
    stillOn,
    noteOnPlate,
    bookingSeats,
    partySizes,
    partyMap,
    partyOf,
    cityLiveWindowSec,
    plateLiveWindowSec,
    liveSince,
    boardedTouchDue,
    readRiderFixes,
    alignByKey,
  )
where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.SharedCab.Allocation.Types (RiderFix)
import SharedLogic.SharedCab.Keys (bookingLockKey, cabBookingsKey, cancelReasonKey, riderFixKey)
import SharedLogic.SharedCab.LegState (CancelReason, seatsHeld)
import SharedLogic.SharedCab.Lock (shared, tryWithOwnedLock, withOwnedLock)
import SharedLogic.SharedCab.RefundDecision (isSharedCabBooking)
import SharedLogic.SharedCab.SessionState (sessionTtlSec)
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking

-- | A shared-cab booking stays CONFIRMED after its ride, so a scan of "live" bookings is bounded by when they were last written.
-- City-wide scans: no city cab ride lasts 6 h, and stop progress touches a boarded booking hourly ('boardedTouchDue').
cityLiveWindowSec :: Int
cityLiveWindowSec = 6 * 3600

-- | A plate's bookings: each is written (claim or boarding) during that plate's session, which lives at most sessionTtlSec, so ending
-- a session still finds every rider on board even if the tick was down.
plateLiveWindowSec :: Int
plateLiveWindowSec = sessionTtlSec + 3600

liveSince :: Int -> UTCTime -> UTCTime
liveSince windowSec = addUTCTime (negate (fromIntegral windowSec))

-- | A boarded booking last written over an hour ago is touched, so it stays inside 'cityLiveWindowSec' however long the ride.
boardedTouchDue :: UTCTime -> UTCTime -> Bool
boardedTouchDue now updatedAt = diffUTCTime now updatedAt > 3600

withBookingLock :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Id DFRFSTicketBooking.FRFSTicketBooking -> m a -> m a
withBookingLock bookingId = withOwnedLock "waitForBookingLock" (bookingLockKey bookingId) bookingLockTtlSec 10000

-- | A cancel holds the booking lock across the BPP cancel and the payment status call: the lock must outlive them.
bookingLockTtlSec :: Int
bookingLockTtlSec = 30

-- | The same lock without waiting: Nothing when held.
tryWithBookingLock :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Id DFRFSTicketBooking.FRFSTicketBooking -> m a -> m (Maybe a)
tryWithBookingLock bookingId = tryWithOwnedLock (bookingLockKey bookingId) bookingLockTtlSec

ridersOnBoard :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m [DFRFSTicketBooking.FRFSTicketBooking]
ridersOnBoard plate = do
  bookings <- plateBookings [plate] [DFRFSTicketBookingStatus.CONFIRMED]
  if null bookings
    then pure []
    else do
      tickets <- QFRFSTicket.findAllByTicketBookingIds (map (.id) bookings)
      let boarded = [ticket.frfsTicketBookingId | ticket <- tickets, ticket.status == DFRFSTicket.INPROGRESS]
      pure $ filter ((`elem` boarded) . (.id)) bookings

-- | A booking holds its whole party's seats while any of its ticket rows is still held (one group ticket or one per rider).
bookingSeats :: Int -> [DFRFSTicket.FRFSTicketStatus] -> Int
bookingSeats party statuses = if seatsHeld statuses > 0 then max 1 party else 0

partySizes :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => [DFRFSTicketBooking.FRFSTicketBooking] -> m (Map.Map Text Int)
partySizes bookings
  | null bookings = pure Map.empty
  | otherwise = do
    categories <- QFRFSQuoteCategory.findAllByQuoteIds (map (.quoteId) bookings)
    pure $ partyMap [(b.id.getId, b.quoteId.getId) | b <- bookings] [(c.quoteId.getId, c.selectedQuantity) | c <- categories]

partyMap :: [(Text, Text)] -> [(Text, Int)] -> Map.Map Text Int
partyMap bookingQuotes categoryRows = Map.fromList [(bookingId, Map.findWithDefault 1 quoteId byQuote) | (bookingId, quoteId) <- bookingQuotes]
  where
    byQuote = Map.fromListWith (+) categoryRows

partyOf :: Map.Map Text Int -> DFRFSTicketBooking.FRFSTicketBooking -> Int
partyOf parties b = max 1 (Map.findWithDefault 1 b.id.getId parties)

-- | The rider's latest position, for the tick's passed-stop judgement. The TTL only collects garbage; freshness is judged by `takenAt`.
recordRiderFix :: (Redis.HedisFlow m r, MonadFlow m) => Id DFRFSTicketBooking.FRFSTicketBooking -> RiderFix -> m ()
recordRiderFix bookingId riderFix = shared $ Redis.setExp (riderFixKey bookingId) riderFix 3600

readRiderFix :: (Redis.HedisFlow m r, MonadFlow m) => Id DFRFSTicketBooking.FRFSTicketBooking -> m (Maybe RiderFix)
readRiderFix = shared . Redis.safeGet . riderFixKey

-- | Every booking's rider fix in one cluster MGET (grouped by slot), in the bookings' order.
readRiderFixes :: (Redis.HedisFlow m r, MonadFlow m) => [Id DFRFSTicketBooking.FRFSTicketBooking] -> m [Maybe RiderFix]
readRiderFixes bookingIds = alignByKey keys <$> shared (Redis.mGetClusterWithKeys keys)
  where
    keys = map riderFixKey bookingIds

-- | An MGET's (key, value) pairs back in the order of the keys asked for; a key it didn't return is Nothing.
alignByKey :: [Text] -> [(Text, a)] -> [Maybe a]
alignByKey keys found = map (`Map.lookup` Map.fromList found) keys

cancelReasonTtlSec :: Int
cancelReasonTtlSec = 24 * 3600

-- | Who cancelled the booking, for the leg state. clearAllocationKeys deliberately leaves it, since the app's poll may still show the cancelled leg.
recordCancelReason :: (Redis.HedisFlow m r, MonadFlow m) => Id DFRFSTicketBooking.FRFSTicketBooking -> CancelReason -> m ()
recordCancelReason bookingId reason = shared $ Redis.setExp (cancelReasonKey bookingId) reason cancelReasonTtlSec

readCancelReason :: (Redis.HedisFlow m r, MonadFlow m) => Id DFRFSTicketBooking.FRFSTicketBooking -> m (Maybe CancelReason)
readCancelReason = shared . Redis.safeGet . cancelReasonKey

findingOnRoute :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> Text -> m [DFRFSTicketBooking.FRFSTicketBooking]
findingOnRoute = QFRFSTicketBooking.findSharedCabFindingOnRoute

-- | Statuses that still hold a seat or can come to; terminal: FAILED, CANCELLED, COUNTER_CANCELLED, CANCEL_INITIATED, RESCHEDULED.
nonTerminalStatuses :: [DFRFSTicketBookingStatus.FRFSTicketBookingStatus]
nonTerminalStatuses = [DFRFSTicketBookingStatus.NEW, DFRFSTicketBookingStatus.APPROVED, DFRFSTicketBookingStatus.PAYMENT_PENDING, DFRFSTicketBookingStatus.CONFIRMING, DFRFSTicketBookingStatus.CONFIRMED, DFRFSTicketBookingStatus.TECHNICAL_CANCEL_REJECTED]

liveBookingsForVehicle :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m [DFRFSTicketBooking.FRFSTicketBooking]
liveBookingsForVehicle plate = plateBookings [plate] nonTerminalStatuses

-- | The plates' shared-cab bookings in these statuses. A query by plate reads only the DB (vehicleNumber is no KV key), which an
-- assignment still draining from KV hasn't reached; so the plates' assignment index joins it, and every row is re-read by id from KV.
plateBookings :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => [Text] -> [DFRFSTicketBookingStatus.FRFSTicketBookingStatus] -> m [DFRFSTicketBooking.FRFSTicketBooking]
plateBookings [] _ = pure []
plateBookings plates statuses = do
  now <- getCurrentTime
  fromDb <- QFRFSTicketBooking.findSharedCabPlatesUpdatedSince plates statuses (liveSince plateLiveWindowSec now)
  indexed <- concat <$> mapM (shared . Redis.sMembers . cabBookingsKey) plates
  rows <- QFRFSTicketBooking.findAllByIds (map Id (candidateIds (map (.id.getId) fromDb) indexed))
  pure [b | b <- rows, stillOn plates statuses b.vehicleNumber b.status]

candidateIds :: [Text] -> [Text] -> [Text]
candidateIds fromDb indexed = nub (fromDb <> indexed)

-- | The KV row decides: an index member since released, cancelled or moved to another plate is no longer the plate's.
stillOn :: [Text] -> [DFRFSTicketBookingStatus.FRFSTicketBookingStatus] -> Maybe Text -> DFRFSTicketBookingStatus.FRFSTicketBookingStatus -> Bool
stillOn plates statuses mbPlate status = maybe False (`elem` plates) mbPlate && status `elem` statuses

-- | Under the plate lock, before the write that puts the booking on the cab; the TTL outlives the plate's session.
noteOnPlate :: (Redis.HedisFlow m r, MonadFlow m) => Text -> Id DFRFSTicketBooking.FRFSTicketBooking -> m ()
noteOnPlate plate bookingId = shared $ Redis.sAddExp (cabBookingsKey plate) [bookingId.getId] plateLiveWindowSec
