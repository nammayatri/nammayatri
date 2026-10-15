module SharedLogic.SharedCab.Booking
  ( isSharedCabBooking,
    reseatOnWaybill,
    withBookingLock,
    tryWithBookingLock,
    markDropped,
    ridersOnBoard,
    liveSeatsOnVehicle,
    boardedSeatsOnVehicle,
    recordRiderFix,
    readRiderFix,
    recordCancelReason,
    readCancelReason,
    findingOnRoute,
    shared,
    nonTerminalStatuses,
    liveBookingsForVehicle,
    bookingSeats,
    partySizes,
    partyMap,
    partyOf,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import qualified Data.Aeson as A
import qualified Data.Map.Strict as Map
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.SharedCab.Allocation.Types (RiderFix)
import qualified SharedLogic.SharedCab.Events as Events
import SharedLogic.SharedCab.Keys (bookingLockKey, cancelReasonKey, degradedKey, riderFixKey)
import SharedLogic.SharedCab.LegState (CancelReason, seatsHeld)
import SharedLogic.SharedCab.RefundDecision (isSharedCabBooking)
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking

-- | The cross-app master cell, unprefixed, so every app and the scheduler see the allocation and degraded-boarding keys.
shared :: (Redis.HedisFlow m r, MonadFlow m) => m a -> m a
shared = Redis.runInMasterCloudRedisCellWithCrossAppRedis . Redis.withMasterRedis

withBookingLock :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Id DFRFSTicketBooking.FRFSTicketBooking -> m a -> m a
withBookingLock bookingId =
  Redis.withWaitAndLockMasterCloudCrossAppRedis "sharedCab" "waitForBookingLock" (bookingLockKey bookingId) 10 10000

-- | The same lock without waiting, for read paths with no MonadMask: Nothing when held. Not exception-safe (a throw leaves it to its 10 s expiry).
tryWithBookingLock :: (Redis.HedisFlow m r, MonadFlow m) => Id DFRFSTicketBooking.FRFSTicketBooking -> m a -> m (Maybe a)
tryWithBookingLock bookingId action = do
  acquired <- Redis.runInMasterCloudRedisCellWithCrossAppRedis $ Redis.tryLockRedis (bookingLockKey bookingId) 10
  if not acquired
    then pure Nothing
    else do
      result <- action
      Redis.runInMasterCloudRedisCellWithCrossAppRedis $ Redis.unlockRedis (bookingLockKey bookingId)
      pure (Just result)

-- | "I got down": boarded tickets go USED; a ticket that never boarded is a cancel, not consumed here.
markDropped :: (Events.EventFlow m r, MonadMask m) => Events.DropBy -> DFRFSTicketBooking.FRFSTicketBooking -> m ()
markDropped by booking = withBookingLock booking.id $ do
  droppable <- filter ((== DFRFSTicket.INPROGRESS) . (.status)) <$> QFRFSTicket.findAllByTicketBookingId booking.id
  forM_ droppable $ \ticket ->
    QFRFSTicket.updateStatusByTBookingIdAndTicketNumber DFRFSTicket.USED ticket.scannedByVehicleNumber booking.id ticket.ticketNumber
  unless (null droppable) $ Events.forBooking (Events.Dropped by) booking

ridersOnBoard :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m [DFRFSTicketBooking.FRFSTicketBooking]
ridersOnBoard plate = do
  bookings <- QFRFSTicketBooking.findAllByVehicleNumberAndServiceTierTypeAndStatus (Just plate) (Just Spec.SHARED_CAB) [DFRFSTicketBookingStatus.CONFIRMED]
  if null bookings
    then pure []
    else do
      tickets <- QFRFSTicket.findAllByTicketBookingIds (map (.id) bookings)
      let boarded = [ticket.frfsTicketBookingId | ticket <- tickets, ticket.status == DFRFSTicket.INPROGRESS]
      pure $ filter ((`elem` boarded) . (.id)) bookings

-- | Seats app bookings hold on the cab, one per ticket still held. A degraded boarding holds none. `plate` is canonical.
liveSeatsOnVehicle :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m Int
liveSeatsOnVehicle = seatsOnVehicle (const True)

boardedSeatsOnVehicle :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m Int
boardedSeatsOnVehicle = seatsOnVehicle (DFRFSTicket.INPROGRESS `elem`)

seatsOnVehicle :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => ([DFRFSTicket.FRFSTicketStatus] -> Bool) -> Text -> m Int
seatsOnVehicle keep plate = do
  bookings <- QFRFSTicketBooking.findAllByVehicleNumberAndServiceTierTypeAndStatus (Just plate) (Just Spec.SHARED_CAB) [DFRFSTicketBookingStatus.CONFIRMED]
  counted <- filterM (fmap isNothing . shared . Redis.get @A.Value . degradedKey . (.id)) bookings
  if null counted
    then pure 0
    else do
      tickets <- QFRFSTicket.findAllByTicketBookingIds (map (.id) counted)
      parties <- partySizes counted
      pure $ sum [bookingSeats (partyOf parties b) statuses | b <- counted, let statuses = [t.status | t <- tickets, t.frfsTicketBookingId == b.id], keep statuses]

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

-- | Who cancelled the booking, for the leg state. clearAllocationKeys deliberately leaves it, since the app's poll may still show the cancelled leg.
recordCancelReason :: (Redis.HedisFlow m r, MonadFlow m) => Id DFRFSTicketBooking.FRFSTicketBooking -> CancelReason -> m ()
recordCancelReason bookingId reason = shared $ Redis.setExp (cancelReasonKey bookingId) reason (24 * 3600)

readCancelReason :: (Redis.HedisFlow m r, MonadFlow m) => Id DFRFSTicketBooking.FRFSTicketBooking -> m (Maybe CancelReason)
readCancelReason = shared . Redis.safeGet . cancelReasonKey

findingOnRoute :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m [DFRFSTicketBooking.FRFSTicketBooking]
findingOnRoute routeCode =
  filter (isNothing . (.vehicleNumber))
    <$> QFRFSTicketBooking.findAllByRouteCodeAndServiceTierTypeAndStatus (Just routeCode) (Just Spec.SHARED_CAB) DFRFSTicketBookingStatus.CONFIRMED

-- | Statuses that still hold a seat or can come to; terminal: FAILED, CANCELLED, COUNTER_CANCELLED, CANCEL_INITIATED, RESCHEDULED.
nonTerminalStatuses :: [DFRFSTicketBookingStatus.FRFSTicketBookingStatus]
nonTerminalStatuses = [DFRFSTicketBookingStatus.NEW, DFRFSTicketBookingStatus.APPROVED, DFRFSTicketBookingStatus.PAYMENT_PENDING, DFRFSTicketBookingStatus.CONFIRMING, DFRFSTicketBookingStatus.CONFIRMED, DFRFSTicketBookingStatus.TECHNICAL_CANCEL_REJECTED]

liveBookingsForVehicle :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m [DFRFSTicketBooking.FRFSTicketBooking]
liveBookingsForVehicle plate = QFRFSTicketBooking.findAllByVehicleNumberAndServiceTierTypeAndStatus (Just plate) (Just Spec.SHARED_CAB) nonTerminalStatuses

-- | Seats move under the booking lock with expected-value CAS writes; a waybill refresh holds no lock, so it is guarded the same way.
reseatOnWaybill :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> Id DFRFSTicketBooking.FRFSTicketBooking -> m ()
reseatOnWaybill effectiveBus bookingId = withBookingLock bookingId $ do
  mbFresh <- QFRFSTicketBooking.findById bookingId
  whenJust (mbFresh >>= (.vehicleNumber)) $ \held ->
    QFRFSTicketBooking.updateAllocatedVehicle effectiveBus bookingId (Just held)
