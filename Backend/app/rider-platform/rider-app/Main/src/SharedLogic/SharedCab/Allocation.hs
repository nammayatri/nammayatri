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
import SharedLogic.SharedCab.Allocation.Types
import SharedLogic.SharedCab.Booking (liveSeatsOnVehicle, shared, withBookingLock)
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Degraded as Degraded
import qualified SharedLogic.SharedCab.Events as Events
import qualified SharedLogic.SharedCab.Invariants as Invariants
import SharedLogic.SharedCab.LegState (fallbackReached, fallbackTimeElapsed)
import qualified SharedLogic.SharedCab.Misses as Misses
import qualified SharedLogic.SharedCab.Notify as Notify
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (Session (..), SessionStatus (..))
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking

--------------------------------------------------------------------------------
-- Redis key contract
--------------------------------------------------------------------------------

-- | Everything the tick, claims and releases need (the release re-triggers the tick, hence LTS).
-- ServiceFlow: afterClose/claimFirst push the rider on a timer close or a stationary claim (R16/R17).
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

-- | HARD GATE. The queries are real now (FINDING below, seats via Booking.liveSeatsOnVehicle), but the
-- engine has never run end to end: flip only after the 7.x scenario run, as a human decision.
sharedCabAllocationEnabled :: Bool
sharedCabAllocationEnabled = False

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
  Just ts -> diffUTCTime now ts <= fromIntegral maxAgeSec

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

-- | R31: a cab the rider failed to board in time is skipped for this booking too, or the same stationary cab is
-- re-claimed at once and its stand timer blames the driver for the rider's miss.
skipsPlateOnClose :: AllocationOutcome -> Bool
skipsPlateOnClose = isMissedCabOutcome

-- | R38: an allocated cab is released once its last LTS fix is older than this many ltsMaxAgeSec.
silentReleaseMult :: Int
silentReleaseMult = 5

-- | R38: the cab has sent nothing for silentReleaseMult x ltsMaxAgeSec (or LTS lost it) while the route's other cabs are
-- reporting. When nobody on the route is fresh it reads as an LTS outage (05 §8.9), which decides nothing.
silentCab :: UTCTime -> Int -> Text -> [LT.VehicleTrackingOnRouteResp] -> Bool
silentCab now maxAgeSec plate tracking =
  any (isFreshPosition now maxAgeSec . (.vehicleInfo)) tracking
    && maybe True (not . isFreshPosition now (silentReleaseMult * maxAgeSec) . (.vehicleInfo)) (find ((== plate) . (.vehicleNumber)) tracking)

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
                                AllocationState {vehicleNumber = plate, driverId = Just s.driverId, vehicleTripId = Just s.vehicleTripId.getId, allocatedAt = now, expiresAt = deadline, attempts, timerKind = StandTimer}
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
  (MonadFlow m, Redis.HedisFlow m r, CacheFlow m r, EsqDBFlow m r) =>
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
      pure Closed {closedCity = b.merchantOperatingCityId, fallbackJustTriggered, heldTrip}

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
  whenJust closed $ \Closed {closedCity = cityId, fallbackJustTriggered, heldTrip} -> do
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
    forM_ (Notify.reassignReasonFor outcome) $ \reason ->
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
            when (st.timerKind == StandTimer && isJust st.expiresAt && maybe False (movingOn plate) b.routeCode) $
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
            any (\vt -> vt.vehicleNumber == plate && isFreshPosition now cfg.ltsMaxAgeSec vt.vehicleInfo && isMovingSpeed vt.vehicleInfo.speed) $
              fromMaybe [] (lookup route positionsByRoute)
      -- expired timers first: the seats they free are claimable in this same tick
      let silentOn plate route = silentCab now cfg.ltsMaxAgeSec plate (fromMaybe [] (lookup route positionsByRoute))
      expireTimers cfg now movingOn silentOn live
      findings <- forM (mapMaybe (\entry -> (fst entry,) <$> findingOf entry) live) $ \(b, fb) -> do
        since <- readFindingSince fb.bookingId fb.findingSince
        pure (b, fb {findingSince = since})
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
          when (standTimerOnClaim c) $
            whenJustM (QFRFSTicketBooking.findById booking.bookingId) (Notify.notifyArriving c.rcSession.vehicleNumber cfg.standTimerSec)
          pure (Just c)
        Left miss -> do
          logDebug $ "shared-cab claim missed booking=" <> booking.bookingId.getId <> " cab=" <> c.rcSession.vehicleNumber <> " reason=" <> show miss
          go (rank + 1) cs
