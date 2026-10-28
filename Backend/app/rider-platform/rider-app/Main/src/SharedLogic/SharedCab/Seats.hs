-- | Shared-cab seats on the FRFS seat engine ('SharedLogic.FRFSSeatBooking'). A cab's run is a trip of `capacity` virtual seats over
-- its route's stops; a booking's party holds seats for [board stop, drop stop), so a seat freed by a drop is sold again to a rider
-- boarding there or later. The engine's atomic Lua hold is the seat guard: no read-then-write, so no stale count can oversell a cab.
-- Seat state lives in Redis like bus seats: a flush or failover loses holds. A flush is restored when the session is rebuilt; a
-- failover that keeps the session is only reported, by the seated_without_hold invariant.
module SharedLogic.SharedCab.Seats
  ( SeatEngine (..),
    redisEngine,
    CabSeats (..),
    virtualSeats,
    routeEnd,
    stopIndex,
    bookingRange,
    nextStopIdx,
    holdRange,
    freeOver,
    releaseWith,
    adjustWalkups,
    SeatFlow,
    cabSeats,
    cabSeatsOn,
    holdForStops,
    releaseHeld,
    heldOn,
    bookingHold,
    recordHold,
    releaseBooking,
    releaseCancelled,
    markDropped,
    freeForStops,
    freeAhead,
    cabLoad,
    riderLoad,
    setWalkups,
    clearWalkups,
    carryOver,
  )
where

import Data.List (elemIndex, sortOn)
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.Seat as Seat
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FRFSSeatBooking (SeatHoldMeta (..))
import qualified SharedLogic.FRFSSeatBooking as SeatBooking
import SharedLogic.SharedCab.Booking (shared, withBookingLock)
import qualified SharedLogic.SharedCab.Events as Events
import SharedLogic.SharedCab.Keys (seatHoldKey, walkupSeatsKey)
import qualified SharedLogic.SharedCab.Lts as Lts
import SharedLogic.SharedCab.RefundDecision (readRefundDecision)
import SharedLogic.SharedCab.SessionState (Session)
import qualified Storage.CachedQueries.IntegratedBPPConfig as CQIBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import Tools.Error

-- | The engine operations shared cabs use, as values so the seat rules can be run against a model in tests.
data SeatEngine m = SeatEngine
  { freeSeatIds :: Text -> Int -> Int -> [Id Seat.Seat] -> m [Id Seat.Seat],
    -- | atomic: every seat is free for the range and is taken, or none is
    holdSeats :: Text -> [Id Seat.Seat] -> Int -> Int -> m Bool,
    releaseSeats :: Text -> [Id Seat.Seat] -> Int -> Int -> m ()
  }

-- | A hold is confirmed at once (the bus flow confirms at payment), so the reaper never sees it and the seats stay until released.
redisEngine :: (MonadFlow m, Redis.HedisFlow m r) => SeatEngine m
redisEngine =
  SeatEngine
    { freeSeatIds = \tripId fromIdx toIdx -> crossApp . SeatBooking.freeSeatIds tripId fromIdx toIdx,
      holdSeats = \tripId seatIds fromIdx toIdx -> crossApp $ do
        holdId <- generateGUID
        held <- SeatBooking.holdSeats tripId seatIds fromIdx toIdx holdId unconfirmedTtlSec seatTtlSec
        held <$ when held (SeatBooking.confirmBooking tripId holdId `onException` SeatBooking.releaseHold tripId holdId),
      releaseSeats = \tripId seatIds fromIdx toIdx -> crossApp $ SeatBooking.releaseConfirmedSeats tripId seatIds fromIdx toIdx
    }
  where
    unconfirmedTtlSec = 60

-- | The API and the scheduler prefix keys differently, so seat keys must be unprefixed for both to see one cab. The engine picks the
-- master cell itself, which would drop an outer withCrossAppRedis, so that cell is made unprefixed too.
crossApp :: Redis.HedisFlow m r => m a -> m a
crossApp =
  local (\env -> setField @"secondaryHedisClusterEnv" env (unprefixed <$> env.secondaryHedisClusterEnv))
    . Redis.withCrossAppRedis
    . Redis.withMasterRedis
  where
    unprefixed cell = cell {Redis.keyModifier = identity}

-- | Outlives a session (Session.sessionTtlSec): a trip's seats and records go with it.
seatTtlSec :: Int
seatTtlSec = 16 * 60 * 60

data CabSeats = CabSeats
  { tripId :: Text,
    capacity :: Int,
    -- | the route's stops in sequence: a stop's index here is its place in the seat bitmaps
    stopCodes :: [Text]
  }
  deriving (Show, Eq)

virtualSeats :: CabSeats -> [Id Seat.Seat]
virtualSeats c = [Id (show n) | n <- [1 .. c.capacity]]

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

-- | Hold `n` seats for [from, to). A seat taken since the read loses the Lua hold and the next free ones are tried: never sold twice.
holdRange :: Monad m => SeatEngine m -> CabSeats -> (Int, Int) -> Int -> m (Maybe SeatHoldMeta)
holdRange engine c (fromIdx, toIdx) n
  | n <= 0 = pure Nothing
  | otherwise = do
    free <- engine.freeSeatIds c.tripId fromIdx toIdx (virtualSeats c)
    held <- SeatBooking.selectAndHold (\_ chosen -> guard <$> engine.holdSeats c.tripId chosen fromIdx toIdx) free n holdAttempts
    pure $ either (const Nothing) (\(chosen, ()) -> Just SeatHoldMeta {tripId = c.tripId, seatIds = map (.getId) chosen, fromIdx, toIdx}) held
  where
    holdAttempts = 3

freeOver :: Functor m => SeatEngine m -> CabSeats -> (Int, Int) -> m Int
freeOver engine c (fromIdx, toIdx) = length <$> engine.freeSeatIds c.tripId fromIdx toIdx (virtualSeats c)

releaseWith :: SeatEngine m -> SeatHoldMeta -> m ()
releaseWith engine h = engine.releaseSeats h.tripId (map Id h.seatIds) h.fromIdx h.toIdx

-- | Walk-ups ride from the cab's next stop to the route's end, one hold each, so lowering the count gives back the latest. Raising
-- takes only seats free for that whole range, so a seat an app rider holds never becomes a walk-up. Returns the holds kept.
adjustWalkups :: Monad m => SeatEngine m -> CabSeats -> Int -> Int -> [SeatHoldMeta] -> m [SeatHoldMeta]
adjustWalkups engine c nextStop wanted holds
  | wanted <= length holds = do
    let (kept, given) = splitAt (max 0 wanted) holds
    mapM_ (releaseWith engine) given
    pure kept
  | otherwise = do
    let range = (nextStop, routeEnd c)
    free <- freeOver engine c range
    added <- holdRange engine c range (min free (wanted - length holds))
    pure (holds <> maybe [] (\h -> [h {seatIds = [seatId]} | seatId <- h.seatIds]) added)

type SeatFlow m r = (MonadFlow m, Redis.HedisFlow m r, CacheFlow m r, EsqDBFlow m r, HasShortDurationRetryCfg r ())

cabSeats :: SeatFlow m r => Session -> m CabSeats
cabSeats s = do
  integratedBppConfig <- CQIBC.findById s.integratedBppConfigId >>= fromMaybeM IntegratedBPPConfigNotFound
  stops <- sortOn (.sequenceNum) <$> OTPRest.getRouteStopMappingByRouteCode s.routeCode integratedBppConfig
  pure (cabSeatsOn (map (.stopCode) stops) s)

-- | A session's seats on its route's stops, given in sequence: a route's cabs share one stop read.
cabSeatsOn :: [Text] -> Session -> CabSeats
cabSeatsOn stopCodes s = CabSeats {tripId = s.vehicleTripId.getId, capacity = s.capacity, stopCodes}

-- | Seats for a party riding `board` to `drop` on the session's cab; the caller records them once its own writes land.
holdForStops :: SeatFlow m r => Session -> Text -> Text -> Int -> m (Maybe SeatHoldMeta)
holdForStops s board dropStop party = do
  c <- cabSeats s
  case bookingRange c board dropStop of
    Nothing -> Nothing <$ logWarning ("shared-cab seats: route " <> s.routeCode <> " doesn't serve " <> board <> " -> " <> dropStop)
    Just range -> holdRange redisEngine c range party

releaseHeld :: (MonadFlow m, Redis.HedisFlow m r) => SeatHoldMeta -> m ()
releaseHeld = releaseWith redisEngine

heldOn :: Session -> Maybe SeatHoldMeta -> Bool
heldOn s = maybe False ((== s.vehicleTripId.getId) . (.tripId))

bookingHold :: (MonadFlow m, Redis.HedisFlow m r) => Id DFTB.FRFSTicketBooking -> m (Maybe SeatHoldMeta)
bookingHold = shared . Redis.safeGet . seatHoldKey

recordHold :: (MonadFlow m, Redis.HedisFlow m r) => Id DFTB.FRFSTicketBooking -> SeatHoldMeta -> m ()
recordHold bookingId h = shared $ Redis.setExp (seatHoldKey bookingId) h seatTtlSec

-- | Gives back whatever the booking holds; none is a no-op, so every path that ends a ride can call it.
releaseBooking :: (MonadFlow m, Redis.HedisFlow m r) => Id DFTB.FRFSTicketBooking -> m ()
releaseBooking bookingId =
  bookingHold bookingId >>= traverse_ \h -> do
    releaseHeld h
    void . shared $ Redis.del (seatHoldKey bookingId)

-- | A cancel's seat release. The shared-cab cancel runs it under the booking lock it already holds (its refund decision marks
-- that); a seller's cancel bypasses it, so it takes the lock and cannot race a claim recording a hold.
releaseCancelled :: (MonadFlow m, Redis.HedisFlow m r, MonadMask m) => Id DFTB.FRFSTicketBooking -> m ()
releaseCancelled bookingId = do
  underCancelLock <- isJust <$> readRefundDecision bookingId
  (if underCancelLock then identity else withBookingLock bookingId) (releaseBooking bookingId)

-- | "I got down": boarded tickets go USED and the party's seats are free for the rest of the route; a ticket that never boarded is a
-- cancel, not consumed here.
markDropped :: (Events.EventFlow m r, MonadMask m) => Events.DropBy -> DFTB.FRFSTicketBooking -> m ()
markDropped by booking = withBookingLock booking.id $ do
  droppable <- filter ((== DFRFSTicket.INPROGRESS) . (.status)) <$> QFRFSTicket.findAllByTicketBookingId booking.id
  forM_ droppable $ \ticket ->
    QFRFSTicket.updateStatusByTBookingIdAndTicketNumber DFRFSTicket.USED ticket.scannedByVehicleNumber booking.id ticket.ticketNumber
  unless (null droppable) $ do
    releaseBooking booking.id
    Events.forBooking (Events.Dropped by) booking

freeForStops :: SeatFlow m r => Session -> Text -> Text -> m Int
freeForStops s board dropStop = do
  c <- cabSeats s
  maybe (pure 0) (freeOver redisEngine c) (bookingRange c board dropStop)

-- | Seats a walk-up could take now: free from the cab's next stop to the route's end.
freeAhead :: SeatFlow m r => Session -> Maybe LT.VehicleInfo -> m Int
freeAhead s mbVehicle = do
  c <- cabSeats s
  freeOver redisEngine c (nextStopIdx c mbVehicle, routeEnd c)

cabLoad :: SeatFlow m r => Session -> Maybe Text -> Maybe LT.VehicleInfo -> m (Int, Int)
cabLoad s mbBoard mbVehicle = cabSeats s >>= \c -> riderLoad redisEngine c mbBoard Nothing mbVehicle

-- | A rider's view of a cab: seats free for their [board, drop) when the route serves it, else from `mbBoard` (else from the cab's
-- next stop) to the route's end; and seats taken at the next stop.
riderLoad :: Monad m => SeatEngine m -> CabSeats -> Maybe Text -> Maybe Text -> Maybe LT.VehicleInfo -> m (Int, Int)
riderLoad engine c mbBoard mbDrop mbVehicle = do
  let next = nextStopIdx c mbVehicle
      toEnd = (fromMaybe next (mbBoard >>= stopIndex c), routeEnd c)
  free <- freeOver engine c (fromMaybe toEnd (join (bookingRange c <$> mbBoard <*> mbDrop)))
  freeHere <- freeOver engine c (next, next + 1)
  pure (free, c.capacity - freeHere)

walkupHolds :: (MonadFlow m, Redis.HedisFlow m r) => Text -> m [SeatHoldMeta]
walkupHolds tripId = fromMaybe [] <$> shared (Redis.safeGet (walkupSeatsKey tripId))

-- | Under the plate lock: walk-up seats follow the driver's count as far as seats are free. Returns the count now held.
setWalkups :: (SeatFlow m r, Redis.HedisLTSFlowEnv r) => Session -> Int -> m Int
setWalkups s wanted = do
  c <- cabSeats s
  holds <- walkupHolds c.tripId
  mbTrack <- either (const Nothing) identity <$> withTryCatch "sharedCab:seats:readRoutePosition" (Lts.readRoutePosition s.routeCode s.vehicleNumber)
  kept <- adjustWalkups redisEngine c (nextStopIdx c ((.vehicleInfo) <$> mbTrack)) wanted holds
  shared $ Redis.setExp (walkupSeatsKey c.tripId) kept seatTtlSec
  pure (length kept)

clearWalkups :: (MonadFlow m, Redis.HedisFlow m r) => Session -> m ()
clearWalkups s = do
  walkupHolds s.vehicleTripId.getId >>= mapM_ releaseHeld
  void . shared $ Redis.del (walkupSeatsKey s.vehicleTripId.getId)

-- | A route change starts a new trip: the riders aboard and the walk-ups keep their seats on it, to the new route's end (where they
-- get down may not be on it). Riders go first; on an empty trip everyone fits. Returns the walk-ups now held.
carryOver :: SeatFlow m r => Session -> Session -> [Id DFTB.FRFSTicketBooking] -> m Int
carryOver old new onBoard = do
  c <- cabSeats new
  let whole = (0, routeEnd c)
  forM_ onBoard $ \bookingId ->
    bookingHold bookingId >>= traverse_ \h -> do
      releaseHeld h
      holdRange redisEngine c whole (length h.seatIds) >>= \case
        Just moved -> recordHold bookingId moved
        Nothing -> do
          void . shared $ Redis.del (seatHoldKey bookingId)
          logError $ "shared-cab seats: no seat on the new trip for booking " <> bookingId.getId <> " aboard " <> new.vehicleNumber
  walkups <- walkupHolds old.vehicleTripId.getId
  clearWalkups old
  kept <- adjustWalkups redisEngine c 0 (length walkups) []
  shared $ Redis.setExp (walkupSeatsKey c.tripId) kept seatTtlSec
  pure (length kept)
