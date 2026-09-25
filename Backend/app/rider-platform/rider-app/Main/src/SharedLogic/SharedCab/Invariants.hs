-- | Validator layer 4 (board 08): cross-store drift the database can't see. Read-only; a violation
-- is logged as `invariant_violation` and counted, never thrown, so the rider path is never blocked.
module SharedLogic.SharedCab.Invariants
  ( Violation (..),
    BookingFacts (..),
    CabFacts (..),
    LiveSession (..),
    allocatedHasTimer,
    boardedHasTrip,
    noBusTripId,
    noPaymentOnPayOnBoard,
    seatsWithinCapacity,
    tripMatchesSession,
    bookingViolations,
    cabViolations,
    checkBooking,
    checkCab,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
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
import SharedLogic.SharedCab.Booking (isSharedCabBooking)
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
  | -- | held (walk-ups + booked), capacity
    SeatsOverCapacity Int Int
  | LiveSessionWithoutTrip
  | SessionTripMismatch
  | TripWithoutLiveSession
  deriving (Show, Eq)

-- | One shared-cab booking as the rules see it (`05` §2 encodes its state in these fields).
data BookingFacts = BookingFacts
  { confirmed :: Bool,
    vehicleNumber :: Maybe Text,
    tripId :: Maybe Text,
    vehicleTripId :: Maybe Text,
    ticketStatuses :: [FRFSTicketStatus],
    hasAllocTimer :: Bool,
    degraded :: Bool,
    payOnBoard :: Bool,
    paymentRows :: Int
  }
  deriving (Show, Eq)

data LiveSession = LiveSession
  { vehicleTripId :: Text,
    capacity :: Int,
    walkupCount :: Int
  }
  deriving (Show, Eq)

-- | One cab: its non-ENDED session, its ACTIVE vehicle_trip row, seats held by non-degraded bookings on the plate.
data CabFacts = CabFacts
  { liveSession :: Maybe LiveSession,
    activeTripId :: Maybe Text,
    seatsTaken :: Int
  }
  deriving (Show, Eq)

flag :: Violation -> Bool -> Maybe Violation
flag v broken = if broken then Just v else Nothing

-- | ALLOCATED (CONFIRMED, plate set, every ticket ACTIVE) must have its `sharedcab:alloc` timer.
allocatedHasTimer :: BookingFacts -> Maybe Violation
allocatedHasTimer b =
  flag AllocatedWithoutTimer $
    b.confirmed && isJust b.vehicleNumber && not (null b.ticketStatuses) && all (== ACTIVE) b.ticketStatuses && not b.hasAllocTimer

-- | BOARDED (a ticket INPROGRESS) must point at the trip it rode, unless it boarded degraded (`05` §5).
boardedHasTrip :: BookingFacts -> Maybe Violation
boardedHasTrip b = flag BoardedWithoutTrip $ INPROGRESS `elem` b.ticketStatuses && isNothing b.vehicleTripId && not b.degraded

-- | `trip_id` is the bus-schedule key; a shared cab has none (`05` §2 guard audit).
noBusTripId :: BookingFacts -> Maybe Violation
noBusTripId b = flag BusTripIdSet $ isJust b.tripId

noPaymentOnPayOnBoard :: BookingFacts -> Maybe Violation
noPaymentOnPayOnBoard b = flag PaymentOnPayOnBoard $ b.payOnBoard && b.paymentRows > 0

seatsWithinCapacity :: CabFacts -> Maybe Violation
seatsWithinCapacity c = do
  s <- c.liveSession
  let held = s.walkupCount + c.seatsTaken
  flag (SeatsOverCapacity held s.capacity) (held > s.capacity)

-- | A live session and its ACTIVE vehicle_trip row come and go together (`04` §3a).
tripMatchesSession :: CabFacts -> Maybe Violation
tripMatchesSession c = case (c.liveSession, c.activeTripId) of
  (Just _, Nothing) -> Just LiveSessionWithoutTrip
  (Just s, Just trip) -> flag SessionTripMismatch (trip /= s.vehicleTripId)
  (Nothing, Just _) -> Just TripWithoutLiveSession
  (Nothing, Nothing) -> Nothing

bookingViolations :: BookingFacts -> [Violation]
bookingViolations b = mapMaybe ($ b) [allocatedHasTimer, boardedHasTrip, noBusTripId, noPaymentOnPayOnBoard]

cabViolations :: CabFacts -> [Violation]
cabViolations c = mapMaybe ($ c) [seatsWithinCapacity, tripMatchesSession]

ruleName :: Violation -> Text
ruleName = \case
  AllocatedWithoutTimer -> "allocated_without_timer"
  BoardedWithoutTrip -> "boarded_without_trip"
  BusTripIdSet -> "bus_trip_id_set"
  PaymentOnPayOnBoard -> "payment_on_pay_on_board"
  SeatsOverCapacity {} -> "seats_over_capacity"
  LiveSessionWithoutTrip -> "live_session_without_trip"
  SessionTripMismatch -> "session_trip_mismatch"
  TripWithoutLiveSession -> "trip_without_live_session"

type InvariantFlow m r = (CacheFlow m r, EsqDBFlow m r, MonadFlow m, Metrics.CoreMetrics m)

report :: InvariantFlow m r => Text -> [Violation] -> m ()
report subject = mapM_ $ \v -> do
  logError $ "invariant_violation " <> ruleName v <> " " <> subject <> ": " <> show v
  Metrics.incrementGenericMetrics $ "shared_cab_invariant_violation_" <> ruleName v

-- | A failed read is logged and dropped: the checker must never fail the transition that called it.
guarded :: InvariantFlow m r => Text -> m () -> m ()
guarded subject check =
  withTryCatch "sharedCabInvariants" check >>= either (\e -> logWarning $ "invariant check failed " <> subject <> ": " <> show e) pure

-- Key formats owned by the allocation engine (`05` §2) and degraded boarding (`05` §5).
redisKeyExists :: InvariantFlow m r => Text -> m Bool
redisKeyExists key = isJust <$> Redis.withMasterRedis (Redis.get @A.Value key)

bookingFacts :: InvariantFlow m r => DFTB.FRFSTicketBooking -> m BookingFacts
bookingFacts booking = do
  tickets <- QFRFSTicket.findAllByTicketBookingId booking.id
  allocTimer <- redisKeyExists ("sharedcab:alloc:" <> booking.id.getId)
  degradedMarker <- redisKeyExists ("sharedcab:degraded:" <> booking.id.getId)
  cashOnBoard <- isPayOnBoard booking
  payments <- QFRFSTicketBookingPayment.findAllTBPByBookingId booking.id
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
        paymentRows = length payments
      }

-- | Re-reads the booking, so callers can pass the id of one they just changed.
checkBooking :: InvariantFlow m r => Id DFTB.FRFSTicketBooking -> m ()
checkBooking bookingId = guarded subject $ do
  mbBooking <- QFRFSTicketBooking.findById bookingId
  whenJust (mfilter isSharedCabBooking mbBooking) $ bookingFacts >=> report subject . bookingViolations
  where
    subject = "booking " <> bookingId.getId

-- | `plate` is canonical. Seats are the booked tickets (ACTIVE or INPROGRESS) of the plate's live bookings.
checkCab :: InvariantFlow m r => Text -> m ()
checkCab plate = guarded subject $ do
  mbSession <- Session.getSession plate
  activeTrip <- QVT.findActiveByVehicleNumber plate
  bookings <- QFRFSTicketBooking.findAllByVehicleNumberAndServiceTierTypeAndStatus (Just plate) (Just Spec.SHARED_CAB) [DFTBS.CONFIRMED]
  counted <- filterM (fmap not . redisKeyExists . ("sharedcab:degraded:" <>) . getId . (.id)) bookings
  tickets <- if null counted then pure [] else QFRFSTicket.findAllByTicketBookingIds (map (.id) counted)
  let live = do
        s <- mfilter ((/= ENDED) . (.status)) mbSession
        pure LiveSession {vehicleTripId = s.vehicleTripId.getId, capacity = s.capacity, walkupCount = s.walkupCount}
  report subject $
    cabViolations
      CabFacts
        { liveSession = live,
          activeTripId = getId . (.id) <$> activeTrip,
          seatsTaken = length $ filter ((`elem` [ACTIVE, INPROGRESS]) . (.status)) tickets
        }
  where
    subject = "cab " <> plate
