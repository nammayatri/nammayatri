-- | Shared-cab seats, counted from the bookings on a cab. A live booking on a cab takes its party's seats for [board stop, drop stop)
-- of the cab's route, so a seat freed by a drop is sold again to a rider boarding there or later; walk-ups take seats on every stop.
-- Nothing is held: a dropped, cancelled or released booking stops counting.
--
-- INVARIANT: every write that puts a booking on a cab, or raises its walk-ups, runs under that cab's plate lock with the count
-- checked in the same critical section ('takeSeats', 'setWalkups'). That lock, not any seat state, is what keeps two assignments
-- from selling one seat. Known exceptions:
--   * a spot booking is born in the cab with its plate (main's confirm, no plate lock): it is indexed at birth, counts from then,
--     and is checked when it boards, which refuses it with CabFull if the cab has no room; 'takeSeats' logs an overbooked cab.
--   * a waybill refresh never moves a shared cab's plate ('waybillMovesBooking').
module SharedLogic.SharedCab.Seats
  ( CabSeats (..),
    Rider (..),
    Seated (..),
    ridersOn,
    routeEnd,
    stopIndex,
    bookingRange,
    nextStopIdx,
    occupies,
    rideOn,
    freeOver,
    fits,
    overbooked,
    walkupsFitting,
    SeatFlow,
    cabSeats,
    cabsSeats,
    takeSeats,
    markDropped,
    freeForStops,
    freeAhead,
    riderLoad,
    setWalkups,
    carryOver,
  )
where

import Data.List (elemIndex, sortOn)
import qualified Domain.Types.FRFSTicketBooking as DFTB
import Domain.Types.FRFSTicketBookingStatus (FRFSTicketBookingStatus (CONFIRMED))
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.SharedCab.Booking (noteOnPlate, partyOf, partySizes, plateBookings, withBookingLock)
import qualified SharedLogic.SharedCab.Lts as Lts
import SharedLogic.SharedCab.SessionState (Session (..))
import qualified Storage.CachedQueries.IntegratedBPPConfig as CQIBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import Tools.Error

-- | A live booking on a cab: its party's seats over the stops it rides.
data Rider = Rider
  { bookingId :: Text,
    seats :: Int,
    ride :: (Int, Int)
  }
  deriving (Show, Eq)

data CabSeats = CabSeats
  { capacity :: Int,
    walkups :: Int,
    -- | the route's stops in sequence: a ride's indices are places here
    stopCodes :: [Text],
    riders :: [Rider]
  }
  deriving (Show, Eq)

routeEnd :: CabSeats -> Int
routeEnd c = max 0 (length c.stopCodes - 1)

stopIndex :: CabSeats -> Text -> Maybe Int
stopIndex c code = elemIndex code c.stopCodes

-- | A ride's [board, drop) on this cab's route; Nothing when the route doesn't serve it in that order.
bookingRange :: CabSeats -> Text -> Text -> Maybe (Int, Int)
bookingRange c board dropStop = do
  fromIdx <- stopIndex c board
  toIdx <- stopIndex c dropStop
  guard (fromIdx < toIdx)
  pure (fromIdx, toIdx)

-- | The stop the cab stands at or heads to (LTS keeps a stop Upcoming while the cab stands at it); the route's start without a fix.
nextStopIdx :: CabSeats -> Maybe LT.VehicleInfo -> Int
nextStopIdx c mbVehicle = fromMaybe 0 $ do
  vehicle <- mbVehicle
  next <- find ((== LT.Upcoming) . (.status)) =<< vehicle.upcomingStops
  stopIndex c next.stop.stopCode

-- | A booking takes seats on the plate it names while it is live there: allocated, or anyone of its party aboard. A drop turns the
-- boarded tickets USED, which ends it even if one of the party never boarded.
occupies :: Text -> FRFSTicketBookingStatus -> Maybe Text -> [DFRFSTicket.FRFSTicketStatus] -> Bool
occupies plate status mbPlate statuses =
  status == CONFIRMED
    && mbPlate == Just plate
    && any (`elem` [DFRFSTicket.ACTIVE, DFRFSTicket.INPROGRESS]) statuses
    && DFRFSTicket.USED `notElem` statuses

-- | The stops a rider takes on the cab's current trip. A rider who boarded on an earlier trip of the plate came over with a route
-- change, and is aboard until they get down wherever that is: the whole route. So is a ride the route doesn't serve.
rideOn :: [Text] -> Text -> Maybe Text -> Text -> Text -> (Int, Int)
rideOn stopCodes currentTrip boardedTrip board dropStop
  | maybe True (== currentTrip) boardedTrip = fromMaybe whole (bookingRange c board dropStop)
  | otherwise = whole
  where
    c = CabSeats {capacity = 0, walkups = 0, stopCodes, riders = []}
    whole = (0, routeEnd c)

-- | Seats free for the whole of [from, to): the capacity, less the walk-ups, less the most seats riders take at any one stop of it.
freeOver :: CabSeats -> (Int, Int) -> Int
freeOver c range = max 0 (c.capacity - c.walkups - peakLoad c range)

peakLoad :: CabSeats -> (Int, Int) -> Int
peakLoad c (fromIdx, toIdx) = maximum (0 : [sum [r.seats | r <- c.riders, fst r.ride <= i, i < snd r.ride] | i <- [fromIdx .. toIdx - 1]])

-- | More seats taken at some stop than the cab has: the plate-lock invariant was broken somewhere.
overbooked :: CabSeats -> Bool
overbooked c = c.walkups + peakLoad c (0, routeEnd c) > c.capacity

-- | Whether a party riding `board` to `drop` fits beside everyone else on the cab (its own booking, if already counted, aside).
fits :: CabSeats -> Text -> Text -> Text -> Int -> Bool
fits c bookingId board dropStop party = maybe False ((>= party) . freeOver others) (bookingRange c board dropStop)
  where
    others = c {riders = filter ((/= bookingId) . (.bookingId)) c.riders}

-- | Walk-ups ride from the cab's next stop to the route's end: lowering the count always lands, raising it stops where those seats
-- run out, so a seat an app rider has never becomes a walk-up.
walkupsFitting :: CabSeats -> Int -> Int -> Int
walkupsFitting c nextStop wanted = max 0 (min wanted (c.walkups + freeOver c (nextStop, routeEnd c)))

type SeatFlow m r = (MonadFlow m, Redis.HedisFlow m r, CacheFlow m r, EsqDBFlow m r, HasShortDurationRetryCfg r ())

routeStops :: SeatFlow m r => Session -> m [Text]
routeStops s = do
  integratedBppConfig <- CQIBC.findById s.integratedBppConfigId >>= fromMaybeM IntegratedBPPConfigNotFound
  map (.stopCode) . sortOn (.sequenceNum) <$> OTPRest.getRouteStopMappingByRouteCode s.routeCode integratedBppConfig

cabSeats :: SeatFlow m r => Session -> m CabSeats
cabSeats s = do
  stops <- routeStops s
  cabSeatsWith stops s <$> seatedOn [s.vehicleNumber]

-- | Every cab's seats from one read of their bookings.
cabsSeats :: SeatFlow m r => [Session] -> m [CabSeats]
cabsSeats sessions = do
  rows <- seatedOn (map (.vehicleNumber) sessions)
  forM sessions $ \s -> (\stops -> cabSeatsWith stops s rows) <$> routeStops s

cabSeatsWith :: [Text] -> Session -> [Seated] -> CabSeats
cabSeatsWith stopCodes s rows =
  CabSeats {capacity = s.capacity, walkups = s.walkupCount, stopCodes, riders = ridersOn stopCodes s.vehicleNumber s.vehicleTripId.getId rows}

-- | A booking as the count reads it: its KV row, its tickets and its party.
data Seated = Seated
  { bookingId :: Text,
    status :: FRFSTicketBookingStatus,
    plate :: Maybe Text,
    boardedTrip :: Maybe Text,
    boardStop :: Text,
    dropStop :: Text,
    tickets :: [DFRFSTicket.FRFSTicketStatus],
    party :: Int
  }
  deriving (Show, Eq)

-- | The riders on the plate's current trip, out of the rows read for it.
ridersOn :: [Text] -> Text -> Text -> [Seated] -> [Rider]
ridersOn stopCodes plate trip rows =
  [ Rider {bookingId = r.bookingId, seats = r.party, ride = rideOn stopCodes trip r.boardedTrip r.boardStop r.dropStop}
    | r <- rows,
      occupies plate r.status r.plate r.tickets
  ]

seatedOn :: SeatFlow m r => [Text] -> m [Seated]
seatedOn plates = do
  bookings <- plateBookings plates [CONFIRMED]
  tickets <- if null bookings then pure [] else QFRFSTicket.findAllByTicketBookingIds (map (.id) bookings)
  parties <- partySizes bookings
  pure
    [ Seated
        { bookingId = b.id.getId,
          status = b.status,
          plate = b.vehicleNumber,
          boardedTrip = getId <$> b.vehicleTripId,
          boardStop = b.fromStationCode,
          dropStop = b.toStationCode,
          tickets = [t.status | t <- tickets, t.frfsTicketBookingId == b.id],
          party = partyOf parties b
        }
      | b <- bookings
    ]

-- | The capacity check of every assignment: under the cab's plate lock, right before the write that puts the booking on the cab.
-- When the party fits, the booking joins the plate's index first, so the next check sees it before the DB does.
takeSeats :: SeatFlow m r => Session -> Id DFTB.FRFSTicketBooking -> Text -> Text -> Int -> m Bool
takeSeats s bookingId board dropStop party = do
  c <- cabSeats s
  alarmIfOverbooked s c
  if fits c bookingId.getId board dropStop party
    then True <$ noteOnPlate s.vehicleNumber bookingId
    else do
      when (isNothing (bookingRange c board dropStop)) $
        logWarning ("shared-cab seats: route " <> s.routeCode <> " doesn't serve " <> board <> " -> " <> dropStop)
      pure False

alarmIfOverbooked :: MonadFlow m => Session -> CabSeats -> m ()
alarmIfOverbooked s c =
  when (overbooked c) $
    logError ("shared-cab seats: " <> s.vehicleNumber <> " is overbooked (capacity " <> show c.capacity <> ", walk-ups " <> show c.walkups <> ", riders " <> show c.riders <> ")")

-- | "I got down": boarded tickets go USED, which ends the booking's seats; a ticket that never boarded is a cancel, not consumed here.
markDropped :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, MonadMask m) => DFTB.FRFSTicketBooking -> m ()
markDropped booking = withBookingLock booking.id $ do
  droppable <- filter ((== DFRFSTicket.INPROGRESS) . (.status)) <$> QFRFSTicket.findAllByTicketBookingId booking.id
  forM_ droppable $ \ticket ->
    QFRFSTicket.updateStatusByTBookingIdAndTicketNumber DFRFSTicket.USED ticket.scannedByVehicleNumber booking.id ticket.ticketNumber

freeForStops :: SeatFlow m r => Session -> Text -> Text -> m Int
freeForStops s board dropStop = do
  c <- cabSeats s
  pure $ maybe 0 (freeOver c) (bookingRange c board dropStop)

-- | Seats a walk-up could take now: free from the cab's next stop to the route's end.
freeAhead :: SeatFlow m r => Session -> Maybe LT.VehicleInfo -> m Int
freeAhead s mbVehicle = do
  c <- cabSeats s
  pure $ freeOver c (nextStopIdx c mbVehicle, routeEnd c)

-- | A rider's view of a cab: seats free for their [board, drop) when the route serves it, else from `mbBoard` (else from the cab's
-- next stop) to the route's end; and seats taken at the next stop.
riderLoad :: CabSeats -> Maybe Text -> Maybe Text -> Maybe LT.VehicleInfo -> (Int, Int)
riderLoad c mbBoard mbDrop mbVehicle =
  let next = nextStopIdx c mbVehicle
      toEnd = (fromMaybe next (mbBoard >>= stopIndex c), routeEnd c)
   in (freeOver c (fromMaybe toEnd (join (bookingRange c <$> mbBoard <*> mbDrop))), c.capacity - freeOver c (next, next + 1))

-- | Under the plate lock: the walk-up count the driver asks for, as far as seats are free. Returns the count to keep.
setWalkups :: (SeatFlow m r, Redis.HedisLTSFlowEnv r) => Session -> Int -> m Int
setWalkups s wanted = do
  c <- cabSeats s
  alarmIfOverbooked s c
  mbTrack <- Lts.readRoutePosition s.routeCode s.vehicleNumber
  pure $ walkupsFitting c (nextStopIdx c ((.vehicleInfo) <$> mbTrack)) wanted

-- | A route change starts a new trip: the riders aboard count on it to the new route's end ('rideOn'), and the walk-ups come along
-- as far as seats are left. Returns the walk-ups that fit.
carryOver :: SeatFlow m r => Session -> m Int
carryOver new = do
  c <- cabSeats new
  pure $ walkupsFitting c 0 new.walkupCount
