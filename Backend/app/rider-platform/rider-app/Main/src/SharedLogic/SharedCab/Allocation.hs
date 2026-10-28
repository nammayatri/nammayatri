{-# LANGUAGE TypeApplications #-}

-- | The allocation tick and its entry points: read the city's bookings and cab positions, expire timers, claim.
module SharedLogic.SharedCab.Allocation
  ( InternalEndpointFlow,
    runSharedCabAllocationTick,
    triggerSharedCabAllocation,
    releaseSharedCabAllocation,
    skipSharedCabAllocation,
    releaseUnboarded,
    closeUnboarded,
    afterUnboarded,
    allocationPass,
    cityLiveBookings,
    mergeLive,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Extra (whenJustM)
import Data.List (nub)
import qualified Domain.Types.FRFSTicketBooking as DFTB
import Domain.Types.FRFSTicketBookingStatus (FRFSTicketBookingStatus (..))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.SharedCab.Allocation.Claim (AllocFlow, InternalEndpointFlow, claimFallbackPush, claimFirst, liveBookingsOf)
import SharedLogic.SharedCab.Allocation.Reallocate (Closed (..), afterClose, closeAllocation, closeLocked)
import SharedLogic.SharedCab.Allocation.Select (FindingBooking (..), LiveBooking (..), PreferenceLoss (..), PreferenceVerdict (..), RankedCandidate, allocatedPlate, eligibleCandidates, findingOf, groupAllOn, isFinding, movingCab, onlyPreferred, preferenceVerdict, silentCab, skippedWhileFinding, withoutSkipped)
import SharedLogic.SharedCab.Allocation.Types
import SharedLogic.SharedCab.Booking (cityLiveWindowSec, liveSince, partyOf, partySizes, plateLiveWindowSec, withBookingLock)
import SharedLogic.SharedCab.BookingState
import qualified SharedLogic.SharedCab.Config as Config
import SharedLogic.SharedCab.Enabled (sharedCabEnabled)
import SharedLogic.SharedCab.LegState (fallbackTimeElapsed)
import SharedLogic.SharedCab.Lts (readRoutePositions)
import qualified SharedLogic.SharedCab.Notify as Notify
import SharedLogic.SharedCab.Plate (samePlate)
import SharedLogic.SharedCab.Schedule (withCityTickLease)
import qualified SharedLogic.SharedCab.Seats as Seats
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (Session, SessionStatus (ACTIVE))
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking

cityLiveBookings ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [LiveBooking]
cityLiveBookings cityId = do
  finding <- QFRFSTicketBooking.findSharedCabFindingInCity cityId
  now <- getCurrentTime
  plated <- QFRFSTicketBooking.findSharedCabPlatedCityUpdatedSince cityId (liveSince cityLiveWindowSec now)
  liveBookingsOf (mergeLive (.id) finding plated)

-- | The tick's two reads: riders still waiting (no window) and bookings with a cab (recent ones). A booking claimed between the two
-- reads comes back from both; its plated row is the newer.
mergeLive :: Eq k => (a -> k) -> [a] -> [a] -> [a]
mergeLive key finding plated = plated <> filter ((`notElem` platedKeys) . key) finding
  where
    platedKeys = map key plated

-- | Idempotent close: whoever loses the booking CAS emits nothing.
releaseSharedCabAllocation ::
  AllocFlow m r =>
  Config.SharedCabTunables ->
  Id DFTB.FRFSTicketBooking ->
  Text -> -- expected plate -- the cab we believe holds the booking
  AllocationOutcome ->
  m Bool
releaseSharedCabAllocation cfg bookingId expectedPlate outcome = do
  closed <- closeAllocation cfg bookingId expectedPlate outcome (pure ())
  whenJust closed (triggerSharedCabAllocation . (.closedCity))
  pure (isJust closed)

-- | Rider skip: the plate joins the skipped set, and the booking's one skip is marked used, in the same booking-lock hold, so no claim can re-offer it first.
skipSharedCabAllocation :: AllocFlow m r => Config.SharedCabTunables -> Id DFTB.FRFSTicketBooking -> Text -> SkipReason -> m Bool
skipSharedCabAllocation cfg bookingId plate reason = do
  let outcome = RiderSkipped reason
  closed <- closeAllocation cfg bookingId plate outcome (addSkipped (allocKeyTtl cfg) bookingId plate >> when (usesRiderSkip reason) (writeRiderSkipUsed (allocKeyTtl cfg) bookingId))
  whenJust closed (triggerSharedCabAllocation . (.closedCity))
  pure (isJust closed)

-- | Releases a leaving cab's unboarded allocations without penalty. Call after the session write, outside the plate lock.
releaseUnboarded :: AllocFlow m r => Text -> AllocationOutcome -> m ()
releaseUnboarded plate outcome = closeUnboarded plate outcome >>= afterUnboarded plate outcome

type ClosedUnboarded = (Config.SharedCabTunables, Id DFTB.FRFSTicketBooking, Closed)

-- | The closes of `releaseUnboarded`, each under its booking lock and nothing else, so a caller holding the plate lock may run them.
closeUnboarded :: AllocFlow m r => Text -> AllocationOutcome -> m [ClosedUnboarded]
closeUnboarded plate outcome = do
  now <- getCurrentTime
  live <- QFRFSTicketBooking.findSharedCabPlateUpdatedSince plate [CONFIRMED] (liveSince plateLiveWindowSec now) >>= liveBookingsOf
  fmap catMaybes . forM [lb.booking | lb <- live, isJust (allocatedPlate lb)] $ \b -> do
    cfg <- Config.getTunables b.merchantOperatingCityId
    fmap (cfg,b.id,) <$> withBookingLock b.id (closeLocked cfg b.id plate outcome)

-- | The effects of `closeUnboarded`'s closes, outside every lock.
afterUnboarded :: AllocFlow m r => Text -> AllocationOutcome -> [ClosedUnboarded] -> m ()
afterUnboarded plate outcome closed = do
  forM_ closed $ \(cfg, bookingId, c) -> afterClose cfg bookingId plate outcome (Just c)
  mapM_ triggerSharedCabAllocation (nub [c.closedCity | (_, _, c) <- closed])

triggerSharedCabAllocation ::
  AllocFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  m ()
triggerSharedCabAllocation cityId =
  void $ fork "shared-cab-allocation-trigger" $ runSharedCabAllocationTick cityId

runSharedCabAllocationTick ::
  AllocFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  m ()
runSharedCabAllocationTick cityId = withCityTickLease cityId . void $ allocationPass cityId

type RoutePositions = [(Text, [LT.VehicleTrackingOnRouteResp])]

-- | One pass of the tick: read the city, close what has run out, then (if the city is on) claim cabs for the bookings still finding one.
-- A suspended city drains (timers, and the job's timeouts and refunds) but allocates nothing new.
allocationPass ::
  AllocFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  m ([LiveBooking], RoutePositions)
allocationPass cityId = do
  cfg <- Config.getTunables cityId
  enabled <- sharedCabEnabled cityId
  now <- getCurrentTime
  (live, positionsByRoute) <- readCity cityId
  closeExpired cfg now live positionsByRoute
  when enabled $ claimFindings cfg now live positionsByRoute
  pure (live, positionsByRoute)

-- | The city's live bookings and the LTS positions of every route that has one waiting for, or holding, a cab.
readCity :: AllocFlow m r => Id DMOC.MerchantOperatingCity -> m ([LiveBooking], RoutePositions)
readCity cityId = do
  live <- cityLiveBookings cityId
  let routes = nub [route | lb <- live, isFinding lb || isJust (allocatedPlate lb), Just route <- [lb.booking.routeCode]]
  positionsByRoute <- fmap catMaybes . forM routes $ \routeCode ->
    try (readRoutePositions routeCode) >>= \case
      -- Known limitation: an LTS outage is not told apart from every cab being silent. A failed read skips the route, which is
      -- fail-safe: no allocation from stale data, no stand timer cleared.
      Left (e :: SomeException) -> Nothing <$ logError ("shared-cab tick: LTS read failed for route " <> routeCode <> ": " <> show e)
      Right positions -> pure (Just (routeCode, positions))
  pure (live, positionsByRoute)

closeExpired :: AllocFlow m r => Config.SharedCabTunables -> UTCTime -> [LiveBooking] -> RoutePositions -> m ()
closeExpired cfg now live positionsByRoute = expireTimers cfg now movingOn silentOn live
  where
    onRoute route = fromMaybe [] (lookup route positionsByRoute)
    movingOn plate route = movingCab now cfg.ltsMaxAgeSec plate (onRoute route)
    silentOn plate route = silentCab now cfg.ltsMaxAgeSec plate (onRoute route)

claimFindings :: AllocFlow m r => Config.SharedCabTunables -> UTCTime -> [LiveBooking] -> RoutePositions -> m ()
claimFindings cfg now live positionsByRoute = do
  parties <- partySizes (map (.booking) (filter isFinding live)) -- one query for the tick; a party needs all its seats
  findings <- forM (mapMaybe (\lb -> (lb.booking,) <$> findingOf (partyOf parties) lb) live) $ \(b, fb) -> do
    since <- readFindingSince fb.bookingId fb.findingSince
    pure (b, fb {findingSince = since})
  pushTimeFallbacks cfg now findings
  forM_ (groupAllOn (.routeCode) (map snd findings)) $ \(routeCode, bookings) ->
    whenJust (lookup routeCode positionsByRoute) $ \positions -> do
      sessions <- Session.activeSessionsOnRoute routeCode
      forM_ bookings $ \booking -> do
        attempts <- readAttempts booking.bookingId
        skipped <- skippedWhileFinding cfg now attempts booking.findingSince <$> readSkipped booking.bookingId
        candidates <- narrowToPreferred cfg now sessions booking (withoutSkipped skipped (eligibleCandidates now cfg positions sessions booking))
        void $ claimFirst cfg booking candidates

-- | A booking whose rider picked a cab is offered only that cab until the pick stops holding; then it is dropped for good (the rider is told
-- once, unless the cab lapsed on them and they already were) and the booking is an ordinary finding one.
narrowToPreferred :: AllocFlow m r => Config.SharedCabTunables -> UTCTime -> [Session] -> FindingBooking -> [RankedCandidate] -> m [RankedCandidate]
narrowToPreferred cfg now sessions booking candidates =
  readPreferred booking.bookingId >>= \case
    Nothing -> pure candidates
    Just plate ->
      readPreferenceDropped booking.bookingId >>= \case
        True -> pure candidates
        False -> do
          skipped <- readSkipped booking.bookingId
          freeSeats <- case find (\s -> s.status == ACTIVE && samePlate plate s.vehicleNumber) sessions of
            Just s -> Seats.freeForStops s booking.boardStopCode booking.dropStopCode
            Nothing -> pure 0
          case preferenceVerdict cfg now booking.findingSince skipped sessions freeSeats booking.seats plate of
            Honour p -> pure (onlyPreferred p candidates)
            Lost loss -> do
              writePreferenceDropped (allocKeyTtl cfg) booking.bookingId
              whenJust (reassignReasonFor' loss) $ \reason ->
                whenJustM (QFRFSTicketBooking.findById booking.bookingId) (Notify.notifyReassigned reason)
              pure candidates
  where
    reassignReasonFor' = \case
      PrefSkipped -> Nothing
      PrefFull -> Just Notify.SEAT_LOST
      PrefTimedOut -> Just Notify.CAB_PULLED
      PrefOffRoute -> Just Notify.CAB_PULLED

-- | A timed-out or key-less ALLOCATED booking goes back to FINDING, decided under the booking lock so a claim still writing its key is not mistaken for lost.
expireTimers ::
  AllocFlow m r =>
  Config.SharedCabTunables ->
  UTCTime ->
  (Text -> Text -> Bool) -> -- plate moving on route, per fresh LTS positions
  (Text -> Text -> Bool) -> -- plate silent on route
  [LiveBooking] ->
  m ()
expireTimers cfg now movingOn silentOn live =
  forM_ [(lb.booking, plate) | lb <- live, Just plate <- [allocatedPlate lb]] $ \(b, plate) -> do
    result <- withBookingLock b.id $ do
      mbState <- readAlloc b.id
      case timerExpiry now mbState <|> (CabSilent <$ guard (maybe False (silentOn plate) b.routeCode)) of
        Just outcome -> fmap (outcome,) <$> closeLocked cfg b.id plate outcome
        Nothing -> do
          whenJust mbState $ \st ->
            when (clearOnMove st.timer /= st.timer && maybe False (movingOn plate) b.routeCode) $ do
              writeAlloc (allocKeyTtl cfg) b.id st {timer = clearOnMove st.timer}
          pure Nothing
    whenJust result $ \(outcome, closedInfo) -> afterClose cfg b.id plate outcome (Just closedInfo)

pushTimeFallbacks :: AllocFlow m r => Config.SharedCabTunables -> UTCTime -> [(DFTB.FRFSTicketBooking, FindingBooking)] -> m ()
pushTimeFallbacks cfg now findings =
  forM_ [b | (b, fb) <- findings, fallbackTimeElapsed now fb.findingSince cfg.fallbackAfterSec] $ \b ->
    whenM (claimFallbackPush cfg b.id) $ Notify.notifyBoardAny b
