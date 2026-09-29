module SharedLogic.SharedCab.Booking
  ( isSharedCabBooking,
    withBookingLock,
    tryWithBookingLock,
    markDropped,
    ridersOnBoard,
    liveSeatsOnVehicle,
    boardedSeatsOnVehicle,
    recordRiderFix,
    readRiderFix,
    findingOnRoute,
    shared,
    nonTerminalStatuses,
    liveBookingsForVehicle,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import qualified Data.Aeson as A
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.SharedCab.Allocation.Types (RiderFix)
import qualified SharedLogic.SharedCab.Events as Events
import SharedLogic.SharedCab.LegState (seatsHeld)
import SharedLogic.SharedCab.RefundDecision (isSharedCabBooking)
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking

-- | The cross-app master cell, unprefixed: allocation (`sharedcab:alloc:`) and degraded-boarding (`sharedcab:degraded:`)
-- keys live here so every app and the scheduler see them. Session keys and the plate lock stay app-prefixed.
shared :: (Redis.HedisFlow m r, MonadFlow m) => m a -> m a
shared = Redis.runInMasterCloudRedisCellWithCrossAppRedis . Redis.withMasterRedis

-- | `05` §2: every write that moves a shared-cab booking between states runs under this lock.
withBookingLock :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Id DFRFSTicketBooking.FRFSTicketBooking -> m a -> m a
withBookingLock bookingId =
  -- cross-app: the allocation tick (scheduler) and the API take this lock on the same booking
  Redis.withWaitAndLockMasterCloudCrossAppRedis "sharedCab" "waitForBookingLock" (bookingLockKey bookingId) 10 10000

bookingLockKey :: Id DFRFSTicketBooking.FRFSTicketBooking -> Text
bookingLockKey bookingId = "sharedcab:lock:booking:" <> bookingId.getId

-- | The same lock without waiting, for read paths that have no MonadMask (the status poll): Nothing when it is
-- held. Not exception-safe: a throw inside leaves the lock to its 10 s expiry.
tryWithBookingLock :: (Redis.HedisFlow m r, MonadFlow m) => Id DFRFSTicketBooking.FRFSTicketBooking -> m a -> m (Maybe a)
tryWithBookingLock bookingId action = do
  acquired <- Redis.runInMasterCloudRedisCellWithCrossAppRedis $ Redis.tryLockRedis (bookingLockKey bookingId) 10
  if not acquired
    then pure Nothing
    else do
      result <- action
      Redis.runInMasterCloudRedisCellWithCrossAppRedis $ Redis.unlockRedis (bookingLockKey bookingId)
      pure (Just result)

-- | "I got down" (R8): boarded tickets go USED, which ends the leg and takes the seat out of the cab's live set. A ticket
-- that never boarded is not consumed here: the rider's drop of one is a cancel (R54).
-- TODO(7.4): clear sharedcab:alloc:{bookingId} once allocation keys exist.
markDropped :: (Events.EventFlow m r, MonadMask m) => Events.DropBy -> DFRFSTicketBooking.FRFSTicketBooking -> m ()
markDropped by booking = withBookingLock booking.id $ do
  droppable <- filter ((== DFRFSTicket.INPROGRESS) . (.status)) <$> QFRFSTicket.findAllByTicketBookingId booking.id
  forM_ droppable $ \ticket ->
    QFRFSTicket.updateStatusByTBookingIdAndTicketNumber DFRFSTicket.USED ticket.scannedByVehicleNumber booking.id ticket.ticketNumber
  unless (null droppable) $ Events.forBooking (Events.Dropped by) booking

-- | `04` §4: the cab's bookings with a seat on board (a ticket INPROGRESS). `plate` is canonical.
ridersOnBoard :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m [DFRFSTicketBooking.FRFSTicketBooking]
ridersOnBoard plate = do
  bookings <- QFRFSTicketBooking.findAllByVehicleNumberAndServiceTierTypeAndStatus (Just plate) (Just Spec.SHARED_CAB) [DFRFSTicketBookingStatus.CONFIRMED]
  if null bookings
    then pure []
    else do
      tickets <- QFRFSTicket.findAllByTicketBookingIds (map (.id) bookings)
      let boarded = [ticket.frfsTicketBookingId | ticket <- tickets, ticket.status == DFRFSTicket.INPROGRESS]
      pure $ filter ((`elem` boarded) . (.id)) bookings

-- | `04` §3: seats app bookings hold on the cab, one per ticket still held (ALLOCATED or BOARDED). A degraded
-- boarding holds none (`05` §5); its marker is cross-app, like the allocation engine's keys. `plate` is canonical.
liveSeatsOnVehicle :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m Int
liveSeatsOnVehicle = seatsOnVehicle (const True)

-- | The seats of the cab's bookings with someone on board: what `liveSeatsOnVehicle` leaves once every unboarded
-- allocation is released (R19 cab full).
boardedSeatsOnVehicle :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m Int
boardedSeatsOnVehicle = seatsOnVehicle (DFRFSTicket.INPROGRESS `elem`)

-- | Seats held by the plate's counted bookings whose ticket statuses pass `keep`.
seatsOnVehicle :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => ([DFRFSTicket.FRFSTicketStatus] -> Bool) -> Text -> m Int
seatsOnVehicle keep plate = do
  bookings <- QFRFSTicketBooking.findAllByVehicleNumberAndServiceTierTypeAndStatus (Just plate) (Just Spec.SHARED_CAB) [DFRFSTicketBookingStatus.CONFIRMED]
  counted <- filterM (fmap isNothing . shared . Redis.get @A.Value . ("sharedcab:degraded:" <>) . getId . (.id)) bookings
  if null counted
    then pure 0
    else do
      tickets <- QFRFSTicket.findAllByTicketBookingIds (map (.id) counted)
      pure $ sum [seatsHeld statuses | b <- counted, let statuses = [t.status | t <- tickets, t.frfsTicketBookingId == b.id], keep statuses]

riderFixKey :: Id DFRFSTicketBooking.FRFSTicketBooking -> Text
riderFixKey bookingId = "sharedcab:riderfix:" <> bookingId.getId

-- | R15: the rider's latest position, written from the API's journey poll so the scheduler's tick can judge a
-- passed stop. The TTL only collects garbage; freshness is judged by `takenAt`.
recordRiderFix :: (Redis.HedisFlow m r, MonadFlow m) => Id DFRFSTicketBooking.FRFSTicketBooking -> RiderFix -> m ()
recordRiderFix bookingId riderFix = shared $ Redis.setExp (riderFixKey bookingId) riderFix 3600

readRiderFix :: (Redis.HedisFlow m r, MonadFlow m) => Id DFRFSTicketBooking.FRFSTicketBooking -> m (Maybe RiderFix)
readRiderFix = shared . Redis.safeGet . riderFixKey

-- | `05` §3: FINDING = CONFIRMED with no cab yet.
findingOnRoute :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m [DFRFSTicketBooking.FRFSTicketBooking]
findingOnRoute routeCode =
  filter (isNothing . (.vehicleNumber))
    <$> QFRFSTicketBooking.findAllByRouteCodeAndServiceTierTypeAndStatus (Just routeCode) (Just Spec.SHARED_CAB) DFRFSTicketBookingStatus.CONFIRMED

-- | Statuses that still hold a seat or can come to: retryable payment flow (NEW/APPROVED/PAYMENT_PENDING) plus
-- CONFIRMING/CONFIRMED live and TECHNICAL_CANCEL_REJECTED bounced back. Terminal: FAILED, CANCELLED,
-- COUNTER_CANCELLED, CANCEL_INITIATED, RESCHEDULED.
nonTerminalStatuses :: [DFRFSTicketBookingStatus.FRFSTicketBookingStatus]
nonTerminalStatuses = [DFRFSTicketBookingStatus.NEW, DFRFSTicketBookingStatus.APPROVED, DFRFSTicketBookingStatus.PAYMENT_PENDING, DFRFSTicketBookingStatus.CONFIRMING, DFRFSTicketBookingStatus.CONFIRMED, DFRFSTicketBookingStatus.TECHNICAL_CANCEL_REJECTED]

-- | The plate's live app bookings (04 §3's flush-recovery join). No pagination: a
-- cab holds few books. `plate` must be canonicalised by the caller.
liveBookingsForVehicle :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m [DFRFSTicketBooking.FRFSTicketBooking]
liveBookingsForVehicle plate = QFRFSTicketBooking.findAllByVehicleNumberAndServiceTierTypeAndStatus (Just plate) (Just Spec.SHARED_CAB) nonTerminalStatuses
