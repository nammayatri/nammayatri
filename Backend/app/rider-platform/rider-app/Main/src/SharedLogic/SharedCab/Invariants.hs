-- | Cross-store drift the database can't see. Read-only; a violation is logged as `invariant_violation` and counted,
-- never thrown, so the rider path is never blocked.
module SharedLogic.SharedCab.Invariants
  ( Violation (..),
    BookingFacts (..),
    CabFacts (..),
    LiveSession (..),
    allocatedHasTimer,
    boardedHasTrip,
    noBusTripId,
    noPaymentOnPayOnBoard,
    seatedHasHold,
    holdHasSeat,
    tripMatchesSession,
    bookingViolations,
    cabViolations,
    checkBooking,
    checkCab,
  )
where

import qualified Data.Aeson as A
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketBookingStatus as DFTBS
import Domain.Types.FRFSTicketStatus (FRFSTicketStatus (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FRFSUtils (isPayOnBoard)
import SharedLogic.SharedCab.Booking (isSharedCabBooking, shared)
import SharedLogic.SharedCab.BookingState (readAlloc)
import qualified SharedLogic.SharedCab.Events as Events
import SharedLogic.SharedCab.Keys (degradedKey)
import SharedLogic.SharedCab.LegState (seatsHeld)
import qualified SharedLogic.SharedCab.Seats as Seats
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (SessionStatus (ENDED))
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.VehicleTrip as QVT

data Violation
  = AllocatedWithoutTimer
  | BoardedWithoutTrip
  | BusTripIdSet
  | PaymentOnPayOnBoard
  | -- | the booking is on a cab with a ticket still held, but the seat engine holds no seat for it: the cab can be oversold
    SeatedWithoutHold
  | -- | the seat engine holds seats for a booking that is not on a cab any more: the cab shows full when it isn't
    HoldWithoutSeat
  | LiveSessionWithoutTrip
  | SessionTripMismatch
  | TripWithoutLiveSession
  deriving (Show, Eq)

data BookingFacts = BookingFacts
  { confirmed :: Bool,
    vehicleNumber :: Maybe Text,
    tripId :: Maybe Text,
    vehicleTripId :: Maybe Text,
    ticketStatuses :: [FRFSTicketStatus],
    hasAllocTimer :: Bool,
    degraded :: Bool,
    payOnBoard :: Bool,
    paymentRows :: Int,
    seatHold :: Bool
  }
  deriving (Show, Eq)

newtype LiveSession = LiveSession
  { vehicleTripId :: Text
  }
  deriving (Show, Eq)

data CabFacts = CabFacts
  { liveSession :: Maybe LiveSession,
    activeTripId :: Maybe Text
  }
  deriving (Show, Eq)

flag :: Violation -> Bool -> Maybe Violation
flag v broken = if broken then Just v else Nothing

-- | ALLOCATED (CONFIRMED, plate set, every ticket ACTIVE) must have its `sharedcab:alloc` timer.
allocatedHasTimer :: BookingFacts -> Maybe Violation
allocatedHasTimer b =
  flag AllocatedWithoutTimer $
    b.confirmed && isJust b.vehicleNumber && not (null b.ticketStatuses) && all (== ACTIVE) b.ticketStatuses && not b.hasAllocTimer

boardedHasTrip :: BookingFacts -> Maybe Violation
boardedHasTrip b = flag BoardedWithoutTrip $ INPROGRESS `elem` b.ticketStatuses && isNothing b.vehicleTripId && not b.degraded

noBusTripId :: BookingFacts -> Maybe Violation
noBusTripId b = flag BusTripIdSet $ isJust b.tripId

noPaymentOnPayOnBoard :: BookingFacts -> Maybe Violation
noPaymentOnPayOnBoard b = flag PaymentOnPayOnBoard $ b.payOnBoard && b.paymentRows > 0

-- | Seats are held exactly while the booking is on a cab with a ticket held; a degraded boarding holds none. Drift either way is
-- Redis and the DB disagreeing, e.g. a Redis failover that lost holds.
seated :: BookingFacts -> Bool
seated b = b.confirmed && isJust b.vehicleNumber && seatsHeld b.ticketStatuses > 0 && not b.degraded

seatedHasHold :: BookingFacts -> Maybe Violation
seatedHasHold b = flag SeatedWithoutHold $ seated b && not b.seatHold

holdHasSeat :: BookingFacts -> Maybe Violation
holdHasSeat b = flag HoldWithoutSeat $ b.seatHold && not (seated b)

tripMatchesSession :: CabFacts -> Maybe Violation
tripMatchesSession c = case (c.liveSession, c.activeTripId) of
  (Just _, Nothing) -> Just LiveSessionWithoutTrip
  (Just s, Just trip) -> flag SessionTripMismatch (trip /= s.vehicleTripId)
  (Nothing, Just _) -> Just TripWithoutLiveSession
  (Nothing, Nothing) -> Nothing

bookingViolations :: BookingFacts -> [Violation]
bookingViolations b = mapMaybe ($ b) [allocatedHasTimer, boardedHasTrip, noBusTripId, noPaymentOnPayOnBoard, seatedHasHold, holdHasSeat]

cabViolations :: CabFacts -> [Violation]
cabViolations c = mapMaybe ($ c) [tripMatchesSession]

ruleName :: Violation -> Text
ruleName = \case
  AllocatedWithoutTimer -> "allocated_without_timer"
  BoardedWithoutTrip -> "boarded_without_trip"
  BusTripIdSet -> "bus_trip_id_set"
  PaymentOnPayOnBoard -> "payment_on_pay_on_board"
  SeatedWithoutHold -> "seated_without_hold"
  HoldWithoutSeat -> "hold_without_seat"
  LiveSessionWithoutTrip -> "live_session_without_trip"
  SessionTripMismatch -> "session_trip_mismatch"
  TripWithoutLiveSession -> "trip_without_live_session"

type InvariantFlow m r = (Events.EventFlow m r, Metrics.CoreMetrics m)

report :: InvariantFlow m r => Text -> (Events.EventKind -> m ()) -> [Violation] -> m ()
report subject emitAs = mapM_ $ \v -> do
  logError $ "invariant_violation " <> ruleName v <> " " <> subject <> ": " <> show v
  Metrics.incrementGenericMetrics $ "shared_cab_invariant_violation_" <> ruleName v
  emitAs $ Events.InvariantViolation (ruleName v) (show v)

-- | A failed read is logged and dropped: the checker must never fail the transition that called it.
guarded :: InvariantFlow m r => Text -> m () -> m ()
guarded subject check =
  withTryCatch "sharedCabInvariants" check >>= either (\e -> logWarning $ "invariant check failed " <> subject <> ": " <> show e) pure

redisKeyExists :: InvariantFlow m r => Text -> m Bool
redisKeyExists key = isJust <$> shared (Redis.get @A.Value key)

bookingFacts :: InvariantFlow m r => DFTB.FRFSTicketBooking -> m BookingFacts
bookingFacts booking = do
  tickets <- QFRFSTicket.findAllByTicketBookingId booking.id
  allocTimer <- isJust <$> readAlloc booking.id
  degradedMarker <- redisKeyExists (degradedKey booking.id)
  cashOnBoard <- isPayOnBoard booking
  payments <- QFRFSTicketBookingPayment.findAllTBPByBookingId booking.id
  hold <- Seats.bookingHold booking.id
  pure
    BookingFacts
      { confirmed = booking.status == DFTBS.CONFIRMED,
        vehicleNumber = booking.vehicleNumber,
        tripId = booking.tripId,
        vehicleTripId = (.getId) <$> booking.vehicleTripId,
        ticketStatuses = map (.status) tickets,
        hasAllocTimer = allocTimer,
        degraded = degradedMarker,
        payOnBoard = cashOnBoard,
        paymentRows = length payments,
        seatHold = isJust hold
      }

checkBooking :: InvariantFlow m r => Id DFTB.FRFSTicketBooking -> m ()
checkBooking bookingId = guarded subject $ do
  mbBooking <- QFRFSTicketBooking.findById bookingId
  whenJust (mfilter isSharedCabBooking mbBooking) $ \booking ->
    bookingFacts booking >>= report subject (`Events.forBooking` booking) . bookingViolations
  where
    subject = "booking " <> bookingId.getId

checkCab :: InvariantFlow m r => Text -> m ()
checkCab plate = guarded subject $ do
  -- read-only: a flushed session shows as trip_without_live_session instead of being rebuilt by the checker
  mbSession <- Session.readSession plate
  activeTrip <- QVT.findActiveByVehicleNumber plate
  let live = do
        s <- mfilter ((/= ENDED) . (.status)) mbSession
        pure LiveSession {vehicleTripId = s.vehicleTripId.getId}
  let emitAs k = case (mbSession, activeTrip) of
        (Just s, _) -> Events.forSession k s
        (Nothing, Just trip) -> getCurrentTime >>= Events.emit trip.merchantOperatingCityId . Events.sessionEvent k plate trip.routeCode trip.driverId
        (Nothing, Nothing) -> pure ()
  report subject emitAs $
    cabViolations
      CabFacts
        { liveSession = live,
          activeTripId = getId . (.id) <$> activeTrip
        }
  where
    subject = "cab " <> plate
