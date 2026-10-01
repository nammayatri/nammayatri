{-# LANGUAGE TypeApplications #-}

-- |
-- M7.2-7.4 skeleton: shared-cab allocation engine (rider-app).
-- Plan anchors: 05-allocation-plan.md §2 (booking CAS + lock contract), §3 (engine, phases),
-- §6 (tick stop-progress actions -- stubbed, 7.5+), §7 (events + tunables), §8.4/§8.5 (blame / CAS).
--
-- SEQUENCING CONTRACT (everything unsafe is flagged here and at the site):
--
--   * OFFERS-ONLY-LOCK-FREE: Phase 1 (read positions, screen, rank) holds NO lock. Its reads
--     (LTS hash, session hash, route set) may be stale by up to a tick; that is intentional --
--     every fact that can be raced (seat count, booking vehicleNumber, session liveness) is
--     re-checked in Phase 2 under the locks.
--
--   * PHASE 2 LOCK ORDER (05 §2): target cab lock FIRST (`sharedcab:lock:{plate}`, the same lock
--     walk-up writes take in SharedLogic.SharedCab.Session), then the booking lock
--     (`sharedcab:lock:booking:{bookingId}`). NEVER reverse. The §4 re-bind path must join this
--     same order when it lands (report Q2).
--
--   * BOOKING CAS: frfs_ticket_booking is KV-enabled outside local dev (05 §2), so a DB-level CAS
--     read can observe a stale row mid-drain. The Redis booking lock IS the row mutex; the
--     updateAllocatedVehicle expected-value predicate (spec/Storage/FrfsTicket.yaml:659,
--     `vehicleNumber IS NOT DISTINCT FROM :expected`) is the belt-and-braces inside it.
--
--   * NON-REENTRANT locks: withWaitAndLockRedis deadlocks on a key you already hold.
--     withPlateLock / readSession come from SharedLogic.SharedCab.Session and withBookingLock from
--     SharedLogic.SharedCab.Booking, so every caller shares one key per lock.
--     NEVER call releaseSharedCabAllocation or the §4 re-bind from inside cancelLeg while it
--     holds the booking lock (parent relay, 2026-09).
--
--   * EVENT-ORDERING HAZARD: allocation_created goes out AFTER the CAS + alloc-key write. A crash in
--     between leaves an allocated booking with no event and eventually an orphan allocation; the
--     alloc key TTL (findingTimeoutSec) bounds it and the close path treats a missing alloc key as
--     attempts=0. There is no transactional bridge KV x Redis x Kafka.
module SharedLogic.SharedCab.Allocation
  ( InternalEndpointFlow,
    -- entrypoints (M7.2 wire sites: the job module + booking create/release callers)
    runSharedCabAllocationTick,
    sharedCabAllocationEnabled,
    cityConfig,
    triggerSharedCabAllocation,
    releaseSharedCabAllocation,
    skipSharedCabAllocation,
    releaseUnboarded,
    clearAllocationKeys,
    allocatedPlate,
    closable,
    claimable,
    withCityTickLease,
    allocationPass,
    readRoutePositions,
    readRoutePosition,
    shared,
    -- pure phase-1 pieces, exported for unit tests (rider-app-test SharedCab suites exist)
    planRouteAllocation,
    eligibleCandidates,
    rankCandidates,
    withoutSkipped,
    isSkipped,
    skipsPlateOnClose,
    standTimerOnClaim,
    claimTimerSec,
    claimTimerKind,
    releaseCancelledBooking,
    ticketsCountedAtConfirm,
    ClaimPush (..),
    claimPush,
    clearsWhenMoving,
    silentCab,
    silentReleaseMult,
    skippedWhileFinding,
    isFreshPosition,
    RankedCandidate (..),
    FindingBooking (..),
    -- redis key contract (report Q1)
    allocKey,
    attemptsKey,
    bookingLockKey,
    skippedKey,
    findingSinceKey,
    fallbackPushedKey,
    readFindingSince,
    crossedMaxAttempts,
    isMissedCabOutcome,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import Control.Applicative ((<|>))
import Control.Monad.Extra (whenJustM)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.List (groupBy, nub, sortOn)
import qualified Data.Text as T
import qualified Domain.Types.FRFSTicketBooking as DFTB
import Domain.Types.FRFSTicketBookingStatus (FRFSTicketBookingStatus (..))
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonPTStats as DPUS
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.FRFSCancelJourney as FRFSCancelJourney
import qualified SharedLogic.FRFSPassOverride as FRFSPassOverride
import qualified SharedLogic.PersonPTStats as SPUS
import SharedLogic.SharedCab.Allocation.Types
import SharedLogic.SharedCab.Booking (liveSeatsOnVehicle, partyOf, partySizes, recordCancelReason, shared, withBookingLock)
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Degraded as Degraded
import SharedLogic.SharedCab.DegradedSweepSchedule (sharedCabAllocationEnabled)
import qualified SharedLogic.SharedCab.Events as Events
import qualified SharedLogic.SharedCab.Invariants as Invariants
import SharedLogic.SharedCab.LegState (CancelReason (NO_SHOW_CAP), fallbackReached, fallbackTimeElapsed)
import qualified SharedLogic.SharedCab.Misses as Misses
import qualified SharedLogic.SharedCab.Notify as Notify
import SharedLogic.SharedCab.Plate (canonicalisePlate)
import SharedLogic.SharedCab.RefundDecision (Refund (..), gateByPayment, refundAmounts)
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (Session (..), SessionStatus (..))
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as QPS

--------------------------------------------------------------------------------
-- Redis key contract
--------------------------------------------------------------------------------

-- | Everything the tick, claims and releases need (the release re-triggers the tick, hence LTS).
-- ServiceFlow: afterClose/claimFirst push the rider on a timer close or a stationary claim (R16/R17).
-- EncFlow: R68's person-stats reversal in cancelForNoShows decrypts the phone for staticPersonId.
-- | Calling the driver-app internal API (the allocation push) needs the endpoint map.
type InternalEndpointFlow m r = HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]

type AllocFlow m r =
  ( MonadFlow m,
    Redis.HedisFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadMask m,
    Log m,
    Redis.HedisLTSFlowEnv r,
    Metrics.CoreMetrics m,
    ServiceFlow m r,
    Events.EventFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  )

-- | Unprefixed keys in the master cloud cell: the tick runs in the scheduler, whose key prefix differs from
-- the API's, and both sides must see the same alloc, attempts and lease keys.
-- | 05 §2: `sharedcab:alloc:{bookingId}` -> AllocationState JSON.
allocKey :: Text -> Text
allocKey bookingId = "sharedcab:alloc:" <> bookingId

-- | Lifetime attempts counter (05 §3 maxAttempts / fallbackAfterMin). NOT in the plan's key
-- list: the alloc value is deleted on close (§3 "closing is idempotent"), so the counter needs
-- its own home with the booking's outer TTL. Report Q1 flags this as a plan gap, not a
-- deviation from it.
attemptsKey :: Text -> Text
attemptsKey bookingId = "sharedcab:attempts:" <> bookingId

-- | R19: plates the rider skipped for this booking, kept out of its claims for findingTimeoutSec.
skippedKey :: Text -> Text
skippedKey bookingId = "sharedcab:skipped:" <> bookingId

-- | When the booking's current FINDING stint began (createdAt until the first release): the fallbackAfterSec clock.
findingSinceKey :: Text -> Text
findingSinceKey bookingId = "sharedcab:findingsince:" <> bookingId

-- | Claimed once per FINDING stint by whoever pushes "board any cab" (the attempts crossing or the time crossing).
fallbackPushedKey :: Text -> Text
fallbackPushedKey bookingId = "sharedcab:fallbackpushed:" <> bookingId

-- | 05 §2: `sharedcab:lock:booking:{bookingId}`.
bookingLockKey :: Text -> Text
bookingLockKey bookingId = "sharedcab:lock:booking:" <> bookingId

--------------------------------------------------------------------------------
-- Engine input views
--------------------------------------------------------------------------------

-- | One FINDING shared-cab booking as the engine needs it (05 §2 state table: booking
-- CONFIRMED, ticket ACTIVE, vehicleNumber NULL), built by findingOf from the city scan.
data FindingBooking = FindingBooking
  { bookingId :: Id DFTB.FRFSTicketBooking,
    riderId :: Id DP.Person,
    routeCode :: Text,
    boardStopCode :: Text, -- == frfs_ticket_booking.fromStationCode
    dropStopCode :: Text, -- == frfs_ticket_booking.toStationCode
    seats :: Int, -- number of ticket rows (05 decision 1 "quantity = seats"); 1-4 (decision 10)
    findingSince :: UTCTime -- drives fallbackAfterSec / findingTimeoutSec (05 §7, §8.11)
  }
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Gate + the city scan (FINDING and ALLOCATED views)
--------------------------------------------------------------------------------

-- | batch9 H1: `sharedCabAllocationEnabled`'s definition now lives in SharedLogic.SharedCab.DegradedSweepSchedule
-- (the shared sweep+refund chain needs the flag to gate the refund pass's claim, and this module's import
-- closure reaches that module through Session); the name stays exported from here, unchanged for all callers.

-- | The city's live shared-cab bookings (partial index idx_frfs_ticket_booking_shared_cab_city), each
-- with its ticket statuses: the tick's FINDING and ALLOCATED views both come from this one read.
cityLiveBookings ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [(DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus])]
cityLiveBookings cityId = do
  bookings <- QFRFSTicketBooking.findAllByMerchantOperatingCityIdAndServiceTierTypeAndStatus cityId (Just Spec.SHARED_CAB) [CONFIRMED]
  tickets <- if null bookings then pure [] else QFRFSTicket.findAllByTicketBookingIds (map (.id) bookings)
  pure [(b, [t.status | t <- tickets, t.frfsTicketBookingId == b.id]) | b <- bookings]

-- | 05 §2 FINDING: no plate yet, tickets still ACTIVE; seats = ticket rows.
findingOf :: (DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus]) -> Maybe FindingBooking
findingOf (b, statuses) = do
  guard (isNothing b.vehicleNumber)
  route <- b.routeCode
  let seatCount = length (filter (== DFRFSTicket.ACTIVE) statuses)
  guard (seatCount > 0)
  pure
    FindingBooking
      { bookingId = b.id,
        riderId = b.riderId,
        routeCode = route,
        boardStopCode = b.fromStationCode,
        dropStopCode = b.toStationCode,
        seats = seatCount,
        findingSince = b.createdAt
      }

-- | 05 §2 ALLOCATED: plate set and nobody boarded (every ticket still ACTIVE); the plate it holds.
allocatedPlate :: (DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus]) -> Maybe Text
allocatedPlate (b, statuses) = plateIfUnboarded b.vehicleNumber statuses

plateIfUnboarded :: Maybe Text -> [DFRFSTicket.FRFSTicketStatus] -> Maybe Text
plateIfUnboarded mbPlate statuses = do
  plate <- mbPlate
  guard (not (null statuses) && all (== DFRFSTicket.ACTIVE) statuses)
  pure plate

-- | The under-lock gate of every close: the booking is still live, still holds the plate the closer
-- believes it does, and nobody has boarded since the closer's pre-lock read. A boarded rider keeps the plate.
closable :: Text -> FRFSTicketBookingStatus -> Maybe Text -> [DFRFSTicket.FRFSTicketStatus] -> Bool
closable expectedPlate status mbPlate statuses =
  status == CONFIRMED && plateIfUnboarded mbPlate statuses == Just expectedPlate

-- | The under-lock gate of a claim: the booking is still live, has no cab, nobody has boarded (a degraded boarding
-- flips its tickets and sets the marker under the booking lock, after the tick read it as FINDING), and no degrade marker.
claimable :: FRFSTicketBookingStatus -> Maybe Text -> [DFRFSTicket.FRFSTicketStatus] -> Bool -> Bool
claimable status mbPlate statuses markerAlive =
  status == CONFIRMED && isNothing mbPlate && not (null statuses) && all (== DFRFSTicket.ACTIVE) statuses && not markerAlive

--------------------------------------------------------------------------------
-- LTS positions: one HGETALL on route:{routeCode} (05 §3 pseudo-code; 04 §3 join key)
--------------------------------------------------------------------------------

-- | The scheduler env has no ltsCfg for the HTTP client but does have ltsHedisEnv, and 05 §3's
-- pseudo-code calls exactly this "1 HGETALL on route:{routeCode}" -- so the tick reads the LTS
-- hash directly. Decision 8's "raw client, not FRFSUtils.trackVehicles" is respected:
-- FRFSUtils.trackVehicles resolves every vehicle's next stop via OTPRest bus logic
-- (FRFSUtils.hs:604 trackVehicles, OTPRest next-stop logic within), wrong for cabs.
--
-- Reliability note: all events that mutate what the allocator decides are Redis-side; replica
-- staleness on the LTS cell is acceptable because Phase 2 re-verifies under locks.
--
-- Shape checked against location-tracking-service (redis/keys.rs driver_loc_based_on_route_key,
-- commands.rs set_route_location): key route:{routeCode}, field = plate, value = VehicleTrackingInfo
-- JSON. Fields are decoded one by one, so a bad one is logged and dropped, not the whole route.
readRoutePositions ::
  (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, Log m) =>
  Text ->
  m [LT.VehicleTrackingOnRouteResp]
readRoutePositions routeCode = do
  -- 04 §6: shared-cab route codes are prefixed (SC-*), so this key does not collide with the
  -- bus-feed cache key shape (mkRouteKey, Storage/CachedQueries/Merchant/MultiModalBus.hs:120).
  pairs <- Redis.runInMultiCloudLTSRedisForListFromReplica $ Redis.hGetAll @A.Value ("route:" <> routeCode)
  fmap catMaybes . forM pairs $ \(plate, raw) -> case A.fromJSON raw of
    A.Success info -> pure (Just (LT.VehicleTrackingOnRouteResp plate info))
    A.Error err -> Nothing <$ logWarning ("shared-cab tick: dropping LTS field " <> plate <> " on route " <> routeCode <> ": " <> T.pack err)

-- | One cab's field of the same hash (a single HGET): the rider's status poll knows the plate, so it never decodes the
-- route's other cabs. Nothing on a missing or undecodable field.
readRoutePosition ::
  (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Text ->
  Text ->
  m (Maybe LT.VehicleTrackingOnRouteResp)
readRoutePosition routeCode plate = do
  fields <- Redis.runInMultiCloudLTSRedisForListFromReplica $ maybeToList <$> Redis.hGet @A.Value ("route:" <> routeCode) plate
  pure $ case listToMaybe fields of
    Just raw | A.Success info <- A.fromJSON raw -> Just (LT.VehicleTrackingOnRouteResp plate info)
    _ -> Nothing

--------------------------------------------------------------------------------
-- Phase 1 -- lock-free, once per route (05 §3)
--------------------------------------------------------------------------------

data RankedCandidate = RankedCandidate
  { rcSession :: Session,
    rcEtaToBoardStopSec :: Int,
    rcUpcomingStop :: LT.UpcomingStop,
    rcMoving :: Bool, -- per its fresh LTS position: decides the claim's timer mode (05 §2)
    rcAtStop :: Bool -- within atStopRadiusM of the board stop: only then is a stationary cab waiting for the rider (R32)
  }
  deriving (Show, Generic)

-- | Freshness gate (05 §3; ltsMaxAgeSec from §7): LTS never drops a silent cab by age (04 §3),
-- so anything older than the gate is dropped. A missing or unparseable timestamp is NOT fresh
-- (parseLtsTimestamp is lenient about the format). //TODO(05 §8.9): the city-wide outage rule must
-- suspend this gate.
isFreshPosition :: UTCTime -> Int -> LT.VehicleInfo -> Bool
isFreshPosition now maxAgeSec vi = case vi.timestamp >>= parseLtsTimestamp of
  Nothing -> False -- silent-by-absence must not read as fresh
  -- a skewed-clock fix from the future must not be fresh either: its negative age would clear every gate forever
  Just ts -> let age = diffUTCTime now ts in 0 <= age && age <= fromIntegral maxAgeSec

-- | 05 §3 eligibility, per booking:
--   * cab not past the board stop (§6 item 1) -- LTS keeps the stop listed as Upcoming while
--     the cab is still standing at it (verified semantics of get_upcoming_stops_by_route_code;
--     report Q8 asks for an integration check)
--   * optimistic seat screen (capacity − walkupCount): the booking-sum join is Phase 2's job
--     under the lock; the plan literally lists `available >= seats` in Phase 1 too -- report Q3
--     (per-plate query per tick vs under-lock authority).
--   * ETA gate: upcoming-stop eta within allocationWindowSec.
-- Queued-route check ("queued route change still serves boardStop") is stubbed by
-- queuedRouteStillServes below.
eligibleCandidates ::
  UTCTime ->
  AllocationConfig ->
  [LT.VehicleTrackingOnRouteResp] ->
  [Session] ->
  FindingBooking ->
  [RankedCandidate]
eligibleCandidates now cfg tracking sessions booking =
  [ RankedCandidate {rcSession = s, rcEtaToBoardStopSec = etaSec, rcUpcomingStop = stop, rcMoving = isMovingSpeed veh.vehicleInfo.speed, rcAtStop = distanceBetweenInMeters (LatLong veh.vehicleInfo.latitude veh.vehicleInfo.longitude) stop.stop.coordinate <= fromIntegral cfg.atStopRadiusM}
    | s <- sessions,
      -- route sets hold ACTIVE plates only (SessionState.routeSetMoves), but this read is
      -- lock-free; status is the truth (04 §3), so re-check.
      s.status == ACTIVE,
      queuedRouteStillServes s booking.boardStopCode,
      s.capacity - s.walkupCount >= booking.seats,
      Just veh <- [find (\vt -> vt.vehicleNumber == s.vehicleNumber) tracking],
      isFreshPosition now cfg.ltsMaxAgeSec veh.vehicleInfo,
      Just stop <- [find (\u -> u.stop.stopCode == booking.boardStopCode && u.status == LT.Upcoming) =<< veh.vehicleInfo.upcomingStops],
      let etaSec = max 0 (floor (diffUTCTime stop.eta now)),
      etaSec <= cfg.allocationWindowSec
  ]

-- | R32: the stand timer ("start moving or lose the booking") belongs to a stationary cab waiting at the board stop.
-- A stationary cab still minutes away (traffic, a red light) gets no stand timer; stop-progress arms the moving timer at the stop.
standTimerOnClaim :: RankedCandidate -> Bool
standTimerOnClaim c = not c.rcMoving && c.rcAtStop

-- | The timer a claim arms, in seconds: a moving cab none (stop-progress arms the moving timer at the stop), a stationary
-- cab at the stop the stand timer, and a stationary cab away from the stop a bounded wait of allocationWindowSec, the ETA
-- it was admitted under, so a parked cab that keeps pinging cannot hold the rider until the alloc key's TTL. The stand timer
-- is cleared once the cab is seen moving, like any other.
claimTimerSec :: AllocationConfig -> RankedCandidate -> Maybe Int
claimTimerSec cfg c
  | c.rcMoving = Nothing
  | c.rcAtStop = Just cfg.standTimerSec
  | otherwise = Just cfg.allocationWindowSec

-- | R64: which timer a claim's deadline belongs to. Only a cab that sat at the stop gets the stand timer, whose expiry
-- blames the rider (a no-show, user decision 2026-09-30); the bounded wait of a cab still away from the stop is its own kind, nobody's miss and no skip.
claimTimerKind :: RankedCandidate -> TimerKind
claimTimerKind c
  | c.rcMoving || c.rcAtStop = StandTimer
  | otherwise = AwayTimer

-- | A stand or away timer is cleared once its cab is seen moving; the moving timer is stop-progress's.
clearsWhenMoving :: TimerKind -> Bool
clearsWhenMoving = (/= MovingTimer)

-- | R67: what a fresh claim tells the rider. A cab already at the stop, stationary, is ARRIVING with its countdown; any
-- other claim is ASSIGNED ("on its way"), and stop-progress sends ARRIVING once the cab gets to the stop. One push per
-- claim, and only the claim's CAS winner gets here, so it is deduped.
data ClaimPush = PushArriving | PushAssigned
  deriving (Show, Eq)

claimPush :: RankedCandidate -> ClaimPush
claimPush c = if standTimerOnClaim c then PushArriving else PushAssigned

-- | R31: a cab the rider failed to board in time is skipped for this booking too, or the same stationary cab is
-- re-claimed at once and its stand timer would charge the rider a second no-show for the same cab.
skipsPlateOnClose :: AllocationOutcome -> Bool
skipsPlateOnClose = isMissedCabOutcome

-- | R38: an allocated cab is released once its last LTS fix is older than this many ltsMaxAgeSec.
silentReleaseMult :: Int
silentReleaseMult = 5

-- | R38: the cab has sent nothing for silentReleaseMult x ltsMaxAgeSec (or LTS lost it) while the route's other cabs are
-- reporting. When nobody on the route is fresh it reads as an LTS outage (05 §8.9), which decides nothing.
-- Plates compare canonical (LTS today already stores the canonical busNumber; don't lean on that).
silentCab :: UTCTime -> Int -> Text -> [LT.VehicleTrackingOnRouteResp] -> Bool
silentCab now maxAgeSec plate tracking =
  any (isFreshPosition now maxAgeSec . (.vehicleInfo)) tracking
    && maybe True (not . isFreshPosition now (silentReleaseMult * maxAgeSec) . (.vehicleInfo)) (find ((== canonicalisePlate plate) . canonicalisePlate . (.vehicleNumber)) tracking)

-- | //TODO(§3 + §10 stale feed): a queued route must still serve the board stop; deciding that
-- needs the queued route's stop list from OTPRest. Tolerant here (one-tick window; Phase 2
-- re-reads the session's route) -- deliberately, and documented in the module haddock.
queuedRouteStillServes :: Session -> Text -> Bool
queuedRouteStillServes _ _ = True

-- | 05 §3: "rank by eta; tie: stand match → fewer live allocations → earlier route select".
-- //TODO(heuristic): the tie-breakers are genuinely heuristic -- stand membership is deferred
-- (04 §3), live-allocation counts need the bookings join; ETA order is the correct first
-- approximation and the only one the skeleton commits to.
rankCandidates :: [RankedCandidate] -> [RankedCandidate]
rankCandidates = sortOn rcEtaToBoardStopSec

-- | Phase 1 entry: ranked candidates for one booking on one route. Lock-free by design.
planRouteAllocation ::
  UTCTime ->
  AllocationConfig ->
  [LT.VehicleTrackingOnRouteResp] ->
  [Session] ->
  FindingBooking ->
  [RankedCandidate]
planRouteAllocation now cfg tracking sessions booking =
  rankCandidates (eligibleCandidates now cfg tracking sessions booking)

-- | R19: a cab the rider skipped is never offered to that booking again.
withoutSkipped :: [Text] -> [RankedCandidate] -> [RankedCandidate]
withoutSkipped skipped = filter (not . (`isSkipped` skipped) . (.rcSession.vehicleNumber))

isSkipped :: Text -> [Text] -> Bool
isSkipped = elem

-- | R44: once the booking is in FALLBACK ("board any cab") the rider's skips stop binding, so a skipped cab that
-- has since freed up can be offered again.
skippedWhileFinding :: AllocationConfig -> UTCTime -> Int -> UTCTime -> [Text] -> [Text]
skippedWhileFinding cfg now attempts findingSince skipped
  | fallbackReached now cfg.maxAttempts attempts findingSince cfg.fallbackAfterSec = []
  | otherwise = skipped

--------------------------------------------------------------------------------
-- Phase 2 -- target cab lock, then booking lock (05 §2/§3 order)
--------------------------------------------------------------------------------

-- | What a winning close reports to its after-lock work.
data Closed = Closed
  { closedCity :: Id DMOC.MerchantOperatingCity,
    fallbackJustTriggered :: Bool, -- this close crossed maxAttempts (R16)
    autoCancelled :: Bool, -- this close was the rider's last allowed no-show: the booking is cancelled, not FINDING (R54)
    heldTrip :: Maybe Text -- the trip the allocation was made to (R45), from its alloc key
  }

data ClaimMiss = ClaimSessionGone | ClaimSeatsGone | ClaimCasLost | ClaimSkipped
  deriving (Show, Eq)

-- | 05 §3 Phase 2, one candidate. Under `sharedcab:lock:{plate}` then
-- `sharedcab:lock:booking:{id}`: re-read the session, recompute seats from Postgres (04 §3),
-- CAS vehicleNumber (Nothing -> plate), write the alloc key. On miss the caller tries the next
-- candidate; a miss increments NOTHING (§3: attempts counts allocations, not candidates).
attemptClaim ::
  (MonadFlow m, Redis.HedisFlow m r, CacheFlow m r, EsqDBFlow m r, MonadMask m) =>
  AllocationConfig ->
  FindingBooking ->
  RankedCandidate ->
  m (Either ClaimMiss UTCTime)
attemptClaim cfg booking cand = do
  let plate = cand.rcSession.vehicleNumber
  Session.withPlateLock plate $ do
    -- Re-read the session inside the lock: the Phase 1 snapshot is stale by design.
    mbSession <- Session.readSession plate
    case mbSession of
      Just s
        | s.status == ACTIVE && s.routeCode == booking.routeCode -> do
          withBookingLock booking.bookingId $ do
            -- the tick read the skipped set before it waited on these locks; a skip that landed meanwhile is only visible now
            now <- getCurrentTime
            attempts <- readAttempts (attemptsKey booking.bookingId.getId)
            skipped <- skippedWhileFinding cfg now attempts booking.findingSince <$> shared (Redis.sMembers (skippedKey booking.bookingId.getId))
            liveSeats <- liveSeatsOnVehicle plate
            let available = s.capacity - s.walkupCount - liveSeats
            if plate `isSkipped` skipped
              then pure (Left ClaimSkipped)
              else
                if available < booking.seats
                  then pure (Left ClaimSeatsGone)
                  else do
                    QFRFSTicketBooking.findById booking.bookingId >>= \case
                      Just b -> do
                        statuses <- map (.status) <$> QFRFSTicket.findAllByTicketBookingId b.id
                        markerAlive <- Degraded.isMarkerAlive b.id
                        if claimable b.status b.vehicleNumber statuses markerAlive
                          then do
                            QFRFSTicketBooking.updateAllocatedVehicle (Just plate) b.id Nothing
                            -- 05 §2 timer mode: a stationary cab gets a timer (see claimTimerSec),
                            -- a moving one none; stop-progress (7.5) arms the moving timer at the board stop.
                            -- The key's TTL is only a garbage-collection backstop.
                            let deadline = (\sec -> addUTCTime (intToNominalDiffTime sec) now) <$> claimTimerSec cfg cand
                            shared $
                              Redis.setExp
                                (allocKey booking.bookingId.getId)
                                AllocationState {vehicleNumber = plate, driverId = Just s.driverId, vehicleTripId = Just s.vehicleTripId.getId, allocatedAt = now, expiresAt = deadline, attempts, timerKind = claimTimerKind cand}
                                cfg.findingTimeoutSec
                            pure (Right now)
                          else pure (Left ClaimCasLost)
                      Nothing -> pure (Left ClaimCasLost)
      _ -> pure (Left ClaimSessionGone)

readFindingSince :: (Redis.HedisFlow m r, MonadFlow m) => Id DFTB.FRFSTicketBooking -> UTCTime -> m UTCTime
readFindingSince bookingId createdAt = shared $ fromMaybe createdAt <$> Redis.safeGet (findingSinceKey bookingId.getId)

-- | True for the caller that wins the once-per-stint right to push "board any cab".
claimFallbackPush :: (Redis.HedisFlow m r, MonadFlow m) => AllocationConfig -> Id DFTB.FRFSTicketBooking -> m Bool
claimFallbackPush cfg bookingId = shared $ Redis.tryLockRedis (fallbackPushedKey bookingId.getId) cfg.findingTimeoutSec

readAttempts :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m Int
readAttempts key = shared $ fromMaybe 0 <$> Redis.safeGet key

--------------------------------------------------------------------------------
-- Release (timer / driver cancel / passed stop / seat lost) -- 05 §3
--------------------------------------------------------------------------------

-- | Idempotent close (05 §3): whoever loses the booking CAS emits nothing. On win:
-- alloc key cleared, attempts bumped per §8.4 blame rules, allocation_closed emitted, immediate
-- tick re-triggered (§3 engine rule: tick + trigger on create/release). maxAttempts overflow is
-- a warning + //TODO -- the R10 fallback surface (§3) belongs to the rider-notification work.
releaseSharedCabAllocation ::
  AllocFlow m r =>
  AllocationConfig ->
  Id DFTB.FRFSTicketBooking ->
  Text -> -- expected plate -- the cab we believe holds the booking
  AllocationOutcome ->
  m Bool
releaseSharedCabAllocation cfg bookingId expectedPlate outcome = do
  closed <- withBookingLock bookingId $ closeLocked cfg bookingId expectedPlate outcome
  afterClose cfg bookingId expectedPlate outcome closed
  whenJust closed (triggerSharedCabAllocation . (.closedCity))
  pure (isJust closed)

-- | R19 rider "skip this cab": the allocation closes as RIDER_SKIPPED and, in the same booking-lock hold, the
-- plate joins the booking's skipped set, so no claim can re-offer it before the set is written.
skipSharedCabAllocation :: AllocFlow m r => AllocationConfig -> Id DFTB.FRFSTicketBooking -> Text -> SkipReason -> m Bool
skipSharedCabAllocation cfg bookingId plate reason = do
  let outcome = RiderSkipped reason
  closed <- withBookingLock bookingId $ do
    closedInfo <- closeLocked cfg bookingId plate outcome
    when (isJust closedInfo) $ shared $ Redis.sAddExp (skippedKey bookingId.getId) [plate] cfg.findingTimeoutSec
    pure closedInfo
  afterClose cfg bookingId plate outcome closed
  whenJust closed (triggerSharedCabAllocation . (.closedCity))
  pure (isJust closed)

-- | The city's engine tunables from rider_config (05 §7), defaults where unset.
cityConfig :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m AllocationConfig
cityConfig cityId = do
  t <- Config.getTunables cityId
  pure
    AllocationConfig
      { allocationWindowSec = t.allocationWindowSec,
        atStopRadiusM = t.atStopRadiusM,
        walkBufferSec = t.walkBufferSec,
        standTimerSec = t.standTimerSec,
        movingTimerSec = t.movingTimerSec,
        maxAttempts = t.maxAttempts,
        maxNoShows = t.maxNoShows,
        fallbackAfterSec = t.fallbackAfterSec,
        noCabGraceSec = t.noCabGraceSec,
        findingTimeoutSec = t.findingTimeoutSec,
        tickSec = t.tickSec,
        ltsMaxAgeSec = t.ltsMaxAgeSec,
        autoEndAfterDropSec = t.autoEndAfterDropSec,
        degradedTimeoutSec = t.degradedTimeoutSec
      }

-- | 05 §8.7: a cab leaving ACTIVE (pause, end) releases its unboarded allocations without penalty.
-- Call it after the session write, outside the plate lock.
releaseUnboarded :: AllocFlow m r => Text -> AllocationOutcome -> m ()
releaseUnboarded plate outcome = do
  bookings <- QFRFSTicketBooking.findAllByVehicleNumberAndServiceTierTypeAndStatus (Just plate) (Just Spec.SHARED_CAB) [CONFIRMED]
  tickets <- if null bookings then pure [] else QFRFSTicket.findAllByTicketBookingIds (map (.id) bookings)
  let unboarded = [b | b <- bookings, isJust (allocatedPlate (b, [t.status | t <- tickets, t.frfsTicketBookingId == b.id]))]
  cities <- forM unboarded $ \b -> do
    cfg <- cityConfig b.merchantOperatingCityId
    closed <- withBookingLock b.id $ closeLocked cfg b.id plate outcome
    afterClose cfg b.id plate outcome closed
    pure closed
  mapM_ triggerSharedCabAllocation (nub (map (.closedCity) (catMaybes cities)))

-- | R54: a cancelled booking leaves no allocation state behind (its plate is already off the cab's live seats once the
-- status is CANCELLED). Run inside the booking lock, after the cancel went through.
clearAllocationKeys :: (MonadFlow m, Redis.HedisFlow m r) => Id DFTB.FRFSTicketBooking -> m ()
clearAllocationKeys bookingId =
  shared . forM_ [allocKey, attemptsKey, skippedKey, findingSinceKey, fallbackPushedKey] $ \key -> Redis.del (key bookingId.getId)

-- | Run inside the booking lock: KV read, CAS plate -> null (back to FINDING), clear the alloc key,
-- bump attempts. The city it closed in (and whether this close is the one that just crossed
-- maxAttempts, R16), or Nothing when another closer won.
closeLocked ::
  (MonadFlow m, Redis.HedisFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  AllocationConfig ->
  Id DFTB.FRFSTicketBooking ->
  Text ->
  AllocationOutcome ->
  m (Maybe Closed)
closeLocked cfg bookingId expectedPlate outcome =
  QFRFSTicketBooking.findById bookingId >>= \case
    Nothing -> pure Nothing
    Just b -> do
      -- boarding flips tickets under this same booking lock, so this read is the truth the closers' pre-lock ones weren't
      statuses <- map (.status) <$> QFRFSTicket.findAllByTicketBookingId b.id
      if closable expectedPlate b.status b.vehicleNumber statuses then Just <$> close b else pure Nothing
  where
    close b = do
      -- the rider no-show counter rides the same KV write that clears the plate, under this booking lock
      case blameFor outcome of
        BlameRider -> QFRFSTicketBooking.releaseAllocatedVehicle Nothing (Misses.noShowsAfter BlameRider b.sharedCabNoShows) b.id (Just expectedPlate)
        _ -> QFRFSTicketBooking.updateAllocatedVehicle Nothing b.id (Just expectedPlate)
      heldTrip <- shared $ (>>= (.vehicleTripId)) <$> Redis.safeGet @AllocationState (allocKey bookingId.getId)
      if Misses.actionAfterClose (blameFor outcome) cfg.maxNoShows b.sharedCabNoShows == Misses.AutoCancel
        then do
          cancelForNoShows b
          clearAllocationKeys bookingId
          pure Closed {closedCity = b.merchantOperatingCityId, fallbackJustTriggered = False, autoCancelled = True, heldTrip}
        else reallocate b heldTrip
    reallocate b heldTrip = do
      shared $ Redis.del (allocKey bookingId.getId)
      when (skipsPlateOnClose outcome) $ shared $ Redis.sAddExp (skippedKey bookingId.getId) [expectedPlate] cfg.findingTimeoutSec
      -- //TODO(report Q1): a Redis flush resets attempts by design (05 §11 flush row).
      attemptsBefore <- readAttempts (attemptsKey bookingId.getId)
      let attemptsNow = attemptsBefore + (if countsTowardAttempts outcome then 1 else 0)
      shared $ Redis.setExp (attemptsKey bookingId.getId) attemptsNow cfg.findingTimeoutSec
      -- R16/R10: push "board any cab" once, exactly on the close that crosses maxAttempts (not on
      -- every close after -- the engine keeps retrying a FALLBACK booking, this just stops the spam).
      let fallbackJustTriggered = crossedMaxAttempts cfg.maxAttempts attemptsBefore attemptsNow
      -- a new FINDING stint: its fallbackAfterSec clock restarts, and its time-fallback push is owed again unless the
      -- attempts already put the rider in FALLBACK (that push stays claimed)
      now <- getCurrentTime
      shared $ Redis.setExp (findingSinceKey bookingId.getId) now cfg.findingTimeoutSec
      when (attemptsNow < cfg.maxAttempts) $ shared $ Redis.del (fallbackPushedKey bookingId.getId)
      when fallbackJustTriggered $
        logWarning $ "shared-cab booking " <> bookingId.getId <> " exhausted allocation attempts (" <> show attemptsNow <> "); R10 fallback surface"
      pure Closed {closedCity = b.merchantOperatingCityId, fallbackJustTriggered, autoCancelled = False, heldTrip}

-- | R54: the rider's last allowed no-show. Cancelled by the system, fare kept: the payment is left charged (nothing marks
-- it refund-pending). Runs in the tick's own env, so it writes only what the leg state and the ticket need; the
-- payment, recon and journey side effects of a rider cancel are skipped on purpose. Inside the booking lock.
-- Note the refund-pending mark is exactly what would make the payment service refund: R54 means the charging (the
-- NoRefund tier paid out by decideCancel) survives the system cancel.
--
-- R68 -- the cancel side effects of a settled no-refund rider cancel (SharedLogic.FRFSCancel.handleCancelledStatus),
-- revisited under the tick's constraints (tick env: CacheFlow/EsqDBFlow/Hedis/EncFlow; anything else is documented,
-- not done):
--
--   * Pass-trip release -- IMPLEMENTED. refundPassOverrideTrip hands back the trips the booking's confirm debited;
--     it is exactly-once per searchId (TripReleased / TripRefundPending markers), so a retried tick can never
--     double-credit the pass. releaseBookedTrip drops the booked-window overlap entries; its srem is a no-op on a
--     replay. The trip comes back even though the money is kept: handleCancelledStatus's refundOwed gate looks at
--     the pre-write status, not at the refund amount, so a settled no-refund rider cancel does the same.
--   * Person-stats reversal -- IMPLEMENTED. reversePurchase undoes OnConfirm's recordPurchase on the same
--     dimensions (EncFlow only buys the staticPersonId phone decrypt); ticketsBookedInEvent ticks back down and the
--     PersonStats cache is cleared, mirroring handleCancelledStatus. Not crash-idempotent, and doesn't need to be:
--     the close only happens once per booking (a re-read sees CANCELLED and `closable` bails), and a crash between
--     the row flip and this point leaves stats un-reversed rather than reversed twice -- the same failure
--     preference handleCancelledStatus documents for its own non-idempotent counters.
--   * Cancel SMS -- BLOCKED. SharedLogic.FRFSCancel.sendTicketCancelSMS is typed to the rider-app API's concrete
--     Flow (its AppEnv record carries the SMS/template/urlShortner plumbing; the message builder needs partner-org
--     config through it). The tick runs in the scheduler's env record and this engine layer is
--     constraint-polymorphic, so the SMS can not be sent from here without re-typing the engine to the API Flow and
--     dragging the whole messaging stack into SharedLogic. Cover: the R54 booking-cancelled push
--     (Notify.notifyBookingCancelled) fires in afterClose.
--   * Google Wallet status flip -- BLOCKED, same shape: handleGoogleWalletStatusUpdate needs the API Flow for the
--     JWT service-account bindings (GWLink/GWSA). The wallet refreshes the ticket's CANCELLED state off the booking
--     row on its next fetch.
--
-- Contrariwise invariant: the booking row flips to CANCELLED first and only then any of this runs; between the row
-- flip and clearAllocationKeys/event a failure can only lose an observability step, never leave a live booking on
-- a cancelled cab (and every individual effect above is wrapped in withTryCatch for the same reason).
cancelForNoShows :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, EncFlow m r) => DFTB.FRFSTicketBooking -> m ()
cancelForNoShows b = do
  void $ QFRFSTicketBooking.updateStatusById CANCELLED b.id
  void $ QFRFSTicket.updateAllStatusByBookingId DFRFSTicket.CANCELLED b.id
  void $ QFRFSRecon.updateStatusByTicketBookingId (Just DFRFSTicket.CANCELLED) b.id
  decision <- gateByPayment b NoRefund
  let (charges, refundAmount) = refundAmounts (fromMaybe b.totalPrice.amount b.overriddenAmount) decision
  QFRFSTicketBooking.updateRefundCancellationChargesAndIsCancellableByBookingId (Just refundAmount) (Just charges) (Just True) b.id
  -- the journey-level part of a cancel (legs Finished, journey CANCELLED), as a rider cancel does
  void . withTryCatch "sharedCab:cancelForNoShows:cancelJourney" $
    QJourneyLeg.findByLegSearchId (Just b.searchId.getId) >>= mapM_ (FRFSCancelJourney.cancelJourneyById . (.journeyId))
  -- R55: the leg state shows 'cancelled after missed cabs'. clearAllocationKeys (next, in the caller) leaves this key.
  withTryCatch "sharedCab:cancelForNoShows:recordCancelReason" (recordCancelReason b.id NO_SHOW_CAP)
    >>= either (\e -> logError $ "shared-cab cancel-reason not recorded for booking " <> b.id.getId <> ": " <> show e) pure
  releaseCancelledBooking b

-- | The R68 side effects of a system cancel, shared by every cancel the tick does itself (no-show cap, finding timeout),
-- each effect wrapped so one failure never skips the rest: the pass's spent trip handed back (exactly-once by its marker)
-- and the persona counters OnConfirm wrote reversed. Not crash-idempotent for the counters, and doesn't need to be: the
-- booking flips to CANCELLED once, and a crash before this point leaves stats un-reversed rather than reversed twice.
-- Run after the row flip; needs no lock of its own.
releaseCancelledBooking :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => DFTB.FRFSTicketBooking -> m ()
releaseCancelledBooking b = do
  quantity <- FRFSPassOverride.ticketQuantityForBooking b
  mbPerson <- QPerson.findById b.riderId
  -- (1) give the pass's spent trip back; the marker inside makes a tick retry safe
  whenJust b.overrideAppliedEntityId $ \entityId -> do
    void . withTryCatch "sharedCab:releaseCancelledBooking:refundPassOverrideTrip" $
      FRFSPassOverride.refundPassOverrideTrip b.searchId (Id entityId) quantity
    void . withTryCatch "sharedCab:releaseCancelledBooking:releaseBookedTrip" $
      whenJust mbPerson $ \person ->
        FRFSPassOverride.releaseBookedTrip person (Id entityId) b.id.getId (fromMaybe b.createdAt b.startTime)
  -- (2) undo the persona counters OnConfirm's recordPurchase wrote, in handleCancelledStatus's order:
  -- PersonStats cache cleared, then the reverse, then the legacy tickets-booked counter.
  void . withTryCatch "sharedCab:releaseCancelledBooking:clearPSCache" $ CQP.clearPSCache b.riderId
  case mbPerson of
    Nothing -> logError $ "shared-cab releaseCancelledBooking: person " <> b.riderId.getId <> " not found, stats reversal skipped"
    Just person -> do
      reverseStatsResult <-
        withTryCatch "sharedCab:releaseCancelledBooking:reversePersonPTStats" $ do
          purchaseEvent <- SPUS.mkPurchaseEvent person (Just b.vehicleType) b.serviceTierType DPUS.TICKET Nothing (Just quantity) b.merchantId b.merchantOperatingCityId
          SPUS.reversePurchase purchaseEvent
      either (\e -> logError $ "Failed to reverse PersonPTStats for booking " <> b.id.getId <> ": " <> show e) pure reverseStatsResult
  -- OnConfirm counts a child (rescheduled) booking's tickets only through its parent
  when (ticketsCountedAtConfirm b.parentBookingId) . void . withTryCatch "sharedCab:releaseCancelledBooking:ticketsBooked" $
    QPS.incrementTicketsBookedInEvent b.riderId (- quantity)

-- | OnConfirm increments ticketsBookedInEvent `unless (isJust parentBookingId)`; the reversal mirrors it.
ticketsCountedAtConfirm :: Maybe parent -> Bool
ticketsCountedAtConfirm = isNothing

-- | R16: the close that takes the attempt count over maxAttempts (not one past it).
crossedMaxAttempts :: Int -> Int -> Int -> Bool
crossedMaxAttempts maxAttempts before now = before < maxAttempts && now >= maxAttempts

-- | R17: only the timer outcomes mean the rider didn't board in time.
isMissedCabOutcome :: AllocationOutcome -> Bool
isMissedCabOutcome = (`elem` [StandTimeout, MovingTimeout])

eventBlame :: Blame -> Events.Blame
eventBlame = \case
  BlameDriver -> Events.BlameDriver
  BlameRider -> Events.BlameRider
  BlameNone -> Events.BlameNone

-- | Outside every lock, after a close attempt.
afterClose :: AllocFlow m r => AllocationConfig -> Id DFTB.FRFSTicketBooking -> Text -> AllocationOutcome -> Maybe Closed -> m ()
afterClose cfg bookingId plate outcome closed =
  whenJust closed $ \Closed {closedCity = cityId, fallbackJustTriggered, autoCancelled, heldTrip} -> do
    now <- getCurrentTime
    trip <- fmap (getId . (.vehicleTripId)) <$> Session.readSession plate
    Events.emit cityId . Events.withTrip trip $
      Events.bookingEvent (Events.AllocationClosed (outcomeText outcome) (eventBlame (blameFor outcome))) bookingId.getId (Just plate) Nothing now
    Invariants.checkBooking bookingId
    Invariants.checkCab plate
    mbBooking <- QFRFSTicketBooking.findById bookingId
    -- R18: miss / no-show counters (05 §8.4); a bump failure never fails the release.
    withTryCatch "sharedCabMisses" (Misses.record (blameFor outcome) (mbBooking <&> (.riderId)) (Id <$> heldTrip))
      >>= either (\e -> logError $ "shared-cab miss count bump failed for booking " <> bookingId.getId <> ": " <> show e) pure
    -- R17/F7: the cab is gone; every close but the rider's own skip tells them. Once per release: only the CAS winner is here.
    -- R54: the last allowed no-show cancelled the booking; that push replaces the reassign one.
    if autoCancelled
      then do
        Events.emit cityId $ Events.bookingEvent (Events.BookingCancelled "system" "none" (Just "max_no_shows")) bookingId.getId (Just plate) Nothing now
        mapM_ (\b -> Notify.notifyBookingCancelled b.sharedCabNoShows b) mbBooking
      else forM_ (Notify.reassignReasonFor outcome) $ \reason ->
        mapM_ (Notify.notifyReassigned reason) mbBooking
    -- R16/R10: the rider's leg state just flipped to FALLBACK; push "board any cab" once.
    when fallbackJustTriggered $
      whenM (claimFallbackPush cfg bookingId) $
        mapM_ Notify.notifyBoardAny mbBooking

-- | 05 §2/§3 timers: an ALLOCATED booking whose timer ran out, or whose alloc key is gone, goes back
-- to FINDING; a stand timer is cleared once its cab is seen moving (timer mode follows the cab).
-- Decided under the booking lock, so a claim still writing its key is never mistaken for a lost one.
expireTimers ::
  AllocFlow m r =>
  AllocationConfig ->
  UTCTime ->
  (Text -> Text -> Bool) -> -- plate moving on route, per fresh LTS positions
  (Text -> Text -> Bool) -> -- plate silent on route (R38)
  [(DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus])] ->
  m ()
expireTimers cfg now movingOn silentOn live =
  forM_ [(b, plate) | entry@(b, _) <- live, Just plate <- [allocatedPlate entry]] $ \(b, plate) -> do
    result <- withBookingLock b.id $ do
      mbState <- shared $ Redis.safeGet (allocKey b.id.getId)
      case timerExpiry now mbState <|> (CabSilent <$ guard (maybe False (silentOn plate) b.routeCode)) of
        Just outcome -> fmap (outcome,) <$> closeLocked cfg b.id plate outcome
        Nothing -> do
          whenJust mbState $ \st ->
            when (clearsWhenMoving st.timerKind && isJust st.expiresAt && maybe False (movingOn plate) b.routeCode) $
              shared $ Redis.setExp (allocKey b.id.getId) st {expiresAt = Nothing} cfg.findingTimeoutSec
          pure Nothing
    whenJust result $ \(outcome, closedInfo) -> afterClose cfg b.id plate outcome (Just closedInfo)

--------------------------------------------------------------------------------
-- Driver notification todo
--------------------------------------------------------------------------------

-- | 05 §3 Phase-2 tail: "FCM the driver" via the driver-app internal endpoint. Log-only on failure:
-- a missed push must never fail (or roll back) a claim. Called after attemptClaim released its locks.
notifyDriverOfAllocation ::
  AllocFlow m r =>
  FindingBooking ->
  RankedCandidate ->
  m ()
notifyDriverOfAllocation booking cand =
  withTryCatch "sharedCabNotifyDriver" call >>= \case
    Left e -> logWarning $ "notifyDriverOfAllocation failed driver=" <> cand.rcSession.driverId <> " booking=" <> booking.bookingId.getId <> ": " <> show e
    Right _ -> pure ()
  where
    call = do
      let s = cand.rcSession
      merchant <- CQM.findById s.merchantId >>= fromMaybeM (MerchantNotFound s.merchantId.getId)
      void . CallBPPInternal.sharedCabAllocationFCM merchant.driverOfferApiKey merchant.driverOfferBaseUrl $
        CallBPPInternal.SharedCabAllocationReq
          { bookingId = booking.bookingId.getId,
            driverId = s.driverId,
            seats = Just booking.seats,
            boardingCode = Nothing,
            vehicleNumber = Just s.vehicleNumber,
            boardStopCode = Just booking.boardStopCode,
            etaSeconds = Just cand.rcEtaToBoardStopSec
          }

--------------------------------------------------------------------------------
-- Tick (05 §3): "one tick per city (Redis lease) + trigger on booking create / release"
--------------------------------------------------------------------------------

-- | Immediate, non-blocking re-run. Wire sites (TODO, other milestones): shared-cab confirm
-- branch in SharedLogic.FRFSConfirm (05 §3; the tier itself is not built on this base) and
-- releaseSharedCabAllocation. NEVER call from inside a held cab/booking lock.
triggerSharedCabAllocation ::
  ( MonadFlow m,
    Redis.HedisFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadMask m,
    Log m,
    Redis.HedisLTSFlowEnv r,
    Metrics.CoreMetrics m,
    Events.EventFlow m r,
    ServiceFlow m r,
    EncFlow m r,
    InternalEndpointFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  m ()
triggerSharedCabAllocation cityId =
  void $ fork "shared-cab-allocation-trigger" $ runSharedCabAllocationTick cityId

-- | One city, all routes with FINDING bookings. Reads lock-free; claims behind the two locks.
runSharedCabAllocationTick ::
  ( MonadFlow m,
    Redis.HedisFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadMask m,
    Log m,
    Redis.HedisLTSFlowEnv r,
    Metrics.CoreMetrics m,
    Events.EventFlow m r,
    ServiceFlow m r,
    InternalEndpointFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  m ()
runSharedCabAllocationTick cityId = withCityTickLease cityId . void $ allocationPass cityId

-- | One tick per city (05 §3, §8.5): a pod or trigger that finds the lease held skips. The lease is
-- released when the tick ends; its TTL only bounds a crashed holder, so it spans many ticks.
withCityTickLease :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m () -> m ()
withCityTickLease cityId action = do
  cfg <- cityConfig cityId
  withCityLease ("sharedcab:alloc:lease:" <> cityId.getId) (10 * cfg.tickSec) action

-- | The allocation work of one tick; run it under withCityTickLease. Returns the city's live bookings and the
-- LTS positions it read, for the stop-progress pass that follows in the same tick.
allocationPass ::
  AllocFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  m ([(DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus])], [(Text, [LT.VehicleTrackingOnRouteResp])])
allocationPass cityId = do
  cfg <- cityConfig cityId
  if not sharedCabAllocationEnabled
    then ([], []) <$ logDebug "shared-cab allocation gated off (sharedCabAllocationEnabled); tick is a no-op"
    else do
      live <- cityLiveBookings cityId
      now <- getCurrentTime
      let routes = nub [route | entry@(b, _) <- live, isJust (findingOf entry) || isJust (allocatedPlate entry), Just route <- [b.routeCode]]
      positionsByRoute <- fmap catMaybes . forM routes $ \routeCode ->
        try (readRoutePositions routeCode) >>= \case
          -- //TODO(05 §8.9): "LTS outage ≠ everyone silent" -- most sessions of a city stale in the
          -- same minute must suspend the freshness gate (and NO_LOCATION pause/end). Skipping the
          -- route is fail-safe: no allocation from stale data, and no stand timer cleared either.
          Left (e :: SomeException) -> Nothing <$ logError ("shared-cab tick: LTS read failed for route " <> routeCode <> ": " <> show e)
          Right positions -> pure (Just (routeCode, positions))
      let movingOn plate route =
            any (\vt -> canonicalisePlate vt.vehicleNumber == canonicalisePlate plate && isFreshPosition now cfg.ltsMaxAgeSec vt.vehicleInfo && isMovingSpeed vt.vehicleInfo.speed) $
              fromMaybe [] (lookup route positionsByRoute)
      -- expired timers first: the seats they free are claimable in this same tick
      let silentOn plate route = silentCab now cfg.ltsMaxAgeSec plate (fromMaybe [] (lookup route positionsByRoute))
      expireTimers cfg now movingOn silentOn live
      let findingEntries = mapMaybe (\entry -> (fst entry,) <$> findingOf entry) live
      parties <- partySizes (map fst findingEntries) -- one query for the tick; a party needs all its seats
      findings <- forM findingEntries $ \(b, fb) -> do
        since <- readFindingSince fb.bookingId fb.findingSince
        pure (b, fb {findingSince = since, seats = partyOf parties b})
      pushTimeFallbacks cfg now findings
      forM_ (groupAllOn (.routeCode) (map snd findings)) $ \(routeCode, bookings) ->
        whenJust (lookup routeCode positionsByRoute) $ \positions -> do
          sessions <- Session.activeSessionsOnRoute routeCode
          forM_ bookings $ \booking -> do
            attempts <- readAttempts (attemptsKey booking.bookingId.getId)
            skipped <- skippedWhileFinding cfg now attempts booking.findingSince <$> shared (Redis.sMembers (skippedKey booking.bookingId.getId))
            void $ claimFirst cfg booking (withoutSkipped skipped (planRouteAllocation now cfg positions sessions booking))
      pure (live, positionsByRoute)

-- | R16/R10 by the clock: a FINDING booking that has been FINDING for fallbackAfterSec is shown FALLBACK by the leg
-- state, so it gets its "board any cab" push here, once per stint. Allocation carries on afterwards. Outside every lock.
pushTimeFallbacks :: AllocFlow m r => AllocationConfig -> UTCTime -> [(DFTB.FRFSTicketBooking, FindingBooking)] -> m ()
pushTimeFallbacks cfg now findings =
  forM_ [b | (b, fb) <- findings, fallbackTimeElapsed now fb.findingSince cfg.fallbackAfterSec] $ \b ->
    whenM (claimFallbackPush cfg b.id) $ Notify.notifyBoardAny b

-- | whenWithLockRedis, on the cross-app key, so the scheduler's ticks and the API's triggers share one lease.
withCityLease :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Text -> Int -> m () -> m ()
withCityLease key ttl action =
  whenM (shared $ Redis.tryLockRedis key ttl) $ action `finally` shared (Redis.unlockRedis key)

groupAllOn :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupAllOn f = map (\grp -> (f (head grp), grp)) . groupBy ((==) `on` f) . sortOn f

-- | Try ranked candidates, best first (05 §3: "CAS fails or seats gone -> next candidate").
claimFirst ::
  AllocFlow m r =>
  AllocationConfig ->
  FindingBooking ->
  [RankedCandidate] ->
  m (Maybe RankedCandidate)
claimFirst cfg booking = go (0 :: Int)
  where
    go _ [] = pure Nothing
    go rank (c : cs) =
      attemptClaim cfg booking c >>= \case
        Right _ -> do
          -- attemptClaim has released both locks by now.
          now <- getCurrentTime
          Events.emit c.rcSession.merchantOperatingCityId . Events.withTrip (Just c.rcSession.vehicleTripId.getId) $
            Events.bookingEvent (Events.AllocationCreated (Just (c.rcEtaToBoardStopSec `div` 60)) rank) booking.bookingId.getId (Just c.rcSession.vehicleNumber) (Just booking.routeCode) now
          Invariants.checkBooking booking.bookingId
          Invariants.checkCab c.rcSession.vehicleNumber
          -- forked: a slow driver-app must not stall the city's tick (R50); it takes no lock and logs its own failures
          fork "sharedCabNotifyDriver" (notifyDriverOfAllocation booking c)
          -- R17: a cab claimed while stationary already has its stand timer running (attemptClaim
          -- armed it at claim, standDeadline = now + standTimerSec) -- push now rather than waiting
          -- on stop-progress, which only arms the moving timer for a cab that was moving at claim.
          whenJustM (QFRFSTicketBooking.findById booking.bookingId) $ case claimPush c of
            PushArriving -> Notify.notifyArriving c.rcSession.vehicleNumber cfg.standTimerSec
            PushAssigned -> Notify.notifyAssigned c.rcSession.vehicleNumber
          pure (Just c)
        Left miss -> do
          logDebug $ "shared-cab claim missed booking=" <> booking.bookingId.getId <> " cab=" <> c.rcSession.vehicleNumber <> " reason=" <> show miss
          go (rank + 1) cs
