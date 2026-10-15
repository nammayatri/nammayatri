{-# LANGUAGE TypeApplications #-}

-- | Shared-cab allocation engine: claim, close and tick.
module SharedLogic.SharedCab.Allocation
  ( InternalEndpointFlow,
    runSharedCabAllocationTick,
    triggerSharedCabAllocation,
    releaseSharedCabAllocation,
    skipSharedCabAllocation,
    releaseUnboarded,
    clearAllocationKeys,
    allocatedPlate,
    closable,
    claimable,
    allocationPass,
    readRoutePositions,
    readRoutePosition,
    shared,
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
    readFindingSince,
    crossedMaxAttempts,
    isMissedCabOutcome,
    warnAllocKeyTtlOnce,
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
import SharedLogic.SharedCab.Enabled (sharedCabEnabled)
import qualified SharedLogic.SharedCab.Events as Events
import qualified SharedLogic.SharedCab.Invariants as Invariants
import SharedLogic.SharedCab.Keys (allocKey, allocKeyTtlWarnKey, attemptsKey, fallbackPushedKey, findingSinceKey, skippedKey)
import SharedLogic.SharedCab.LegState (CancelReason (NO_SHOW_CAP), fallbackReached, fallbackTimeElapsed)
import qualified SharedLogic.SharedCab.NoShow as NoShow
import qualified SharedLogic.SharedCab.Notify as Notify
import SharedLogic.SharedCab.Plate (canonicalisePlate)
import SharedLogic.SharedCab.RefundDecision (Refund (..), gateByPayment, refundAmounts, refundWord)
import SharedLogic.SharedCab.Schedule (withCityTickLease)
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (Session (..), SessionStatus (..))
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as QPS

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
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    Events.EventFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  )

data FindingBooking = FindingBooking
  { bookingId :: Id DFTB.FRFSTicketBooking,
    riderId :: Id DP.Person,
    routeCode :: Text,
    boardStopCode :: Text, -- == frfs_ticket_booking.fromStationCode
    dropStopCode :: Text, -- == frfs_ticket_booking.toStationCode
    seats :: Int, -- number of ticket rows (quantity = seats); 1-4
    findingSince :: UTCTime -- drives fallbackAfterSec / findingTimeoutSec
  }
  deriving (Show, Eq, Generic)

cityLiveBookings ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [(DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus])]
cityLiveBookings cityId = do
  bookings <- QFRFSTicketBooking.findAllByMerchantOperatingCityIdAndServiceTierTypeAndStatus cityId (Just Spec.SHARED_CAB) [CONFIRMED]
  tickets <- if null bookings then pure [] else QFRFSTicket.findAllByTicketBookingIds (map (.id) bookings)
  pure [(b, [t.status | t <- tickets, t.frfsTicketBookingId == b.id]) | b <- bookings]

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

allocatedPlate :: (DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus]) -> Maybe Text
allocatedPlate (b, statuses) = plateIfUnboarded b.vehicleNumber statuses

plateIfUnboarded :: Maybe Text -> [DFRFSTicket.FRFSTicketStatus] -> Maybe Text
plateIfUnboarded mbPlate statuses = do
  plate <- mbPlate
  guard (not (null statuses) && all (== DFRFSTicket.ACTIVE) statuses)
  pure plate

-- | The under-lock gate of every close: still live, still holding the plate the closer believes, nobody boarded since.
closable :: Text -> FRFSTicketBookingStatus -> Maybe Text -> [DFRFSTicket.FRFSTicketStatus] -> Bool
closable expectedPlate status mbPlate statuses =
  status == CONFIRMED && plateIfUnboarded mbPlate statuses == Just expectedPlate

-- | The under-lock gate of a claim: still live, no cab, nobody boarded, no degrade marker (a degraded boarding can land after the tick read FINDING).
claimable :: FRFSTicketBookingStatus -> Maybe Text -> [DFRFSTicket.FRFSTicketStatus] -> Bool -> Bool
claimable status mbPlate statuses markerAlive =
  status == CONFIRMED && isNothing mbPlate && not (null statuses) && all (== DFRFSTicket.ACTIVE) statuses && not markerAlive

-- | The scheduler env has no ltsCfg for the HTTP client, so the tick reads the LTS hash directly (FRFSUtils.trackVehicles uses
-- bus next-stop logic, wrong for cabs). Fields decode one by one: a bad one is dropped, not the route.
readRoutePositions ::
  (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, Log m) =>
  Text ->
  m [LT.VehicleTrackingOnRouteResp]
readRoutePositions routeCode = do
  pairs <- Redis.runInMultiCloudLTSRedisForListFromReplica $ Redis.hGetAll @A.Value ("route:" <> routeCode)
  fmap catMaybes . forM pairs $ \(plate, raw) -> case A.fromJSON raw of
    A.Success info -> pure (Just (LT.VehicleTrackingOnRouteResp plate info))
    A.Error err -> Nothing <$ logWarning ("shared-cab tick: dropping LTS field " <> plate <> " on route " <> routeCode <> ": " <> T.pack err)

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

data RankedCandidate = RankedCandidate
  { rcSession :: Session,
    rcEtaToBoardStopSec :: Int,
    rcUpcomingStop :: LT.UpcomingStop,
    rcMoving :: Bool, -- per its fresh LTS position: decides the claim's timer mode
    rcAtStop :: Bool -- within atStopRadiusM of the board stop: only then is a stationary cab waiting for the rider
  }
  deriving (Show, Generic)

-- | Freshness gate: LTS never drops a silent cab by age, so anything older than the gate is dropped. A missing or
-- unparseable timestamp is NOT fresh. TODO: the city-wide LTS outage rule must suspend this gate.
isFreshPosition :: UTCTime -> Int -> LT.VehicleInfo -> Bool
isFreshPosition now maxAgeSec vi = case vi.timestamp >>= parseLtsTimestamp of
  Nothing -> False -- silent-by-absence must not read as fresh
  Just ts -> let age = diffUTCTime now ts in 0 <= age && age <= fromIntegral maxAgeSec

-- | Cab not past the board stop (LTS keeps the stop Upcoming while the cab stands at it), an optimistic seat screen
-- (Phase 2 recounts under the lock), and ETA within allocationWindowSec.
eligibleCandidates ::
  UTCTime ->
  Config.SharedCabTunables ->
  [LT.VehicleTrackingOnRouteResp] ->
  [Session] ->
  FindingBooking ->
  [RankedCandidate]
eligibleCandidates now cfg tracking sessions booking =
  [ RankedCandidate {rcSession = s, rcEtaToBoardStopSec = etaSec, rcUpcomingStop = stop, rcMoving = isMovingSpeed veh.vehicleInfo.speed, rcAtStop = distanceBetweenInMeters (LatLong veh.vehicleInfo.latitude veh.vehicleInfo.longitude) stop.stop.coordinate <= fromIntegral cfg.atStopRadiusM}
    | s <- sessions,
      s.status == ACTIVE,
      queuedRouteStillServes s booking.boardStopCode,
      s.capacity - s.walkupCount >= booking.seats,
      Just veh <- [find (\vt -> vt.vehicleNumber == s.vehicleNumber) tracking],
      isFreshPosition now cfg.ltsMaxAgeSec veh.vehicleInfo,
      Just stop <- [find (\u -> u.stop.stopCode == booking.boardStopCode && u.status == LT.Upcoming) =<< veh.vehicleInfo.upcomingStops],
      let etaSec = max 0 (floor (diffUTCTime stop.eta now)),
      etaSec <= cfg.allocationWindowSec
  ]

standTimerOnClaim :: RankedCandidate -> Bool
standTimerOnClaim c = not c.rcMoving && c.rcAtStop

-- | The timer a claim arms: a moving cab none, a stationary cab at the stop the stand timer, a stationary cab away from the stop
-- a bounded wait (allocationWindowSec), so a parked cab that keeps pinging cannot hold the rider until the key's TTL.
claimTimerSec :: Config.SharedCabTunables -> RankedCandidate -> Maybe Int
claimTimerSec cfg c
  | c.rcMoving = Nothing
  | c.rcAtStop = Just cfg.standTimerSec
  | otherwise = Just cfg.allocationWindowSec

claimTimerKind :: RankedCandidate -> TimerKind
claimTimerKind c
  | c.rcMoving || c.rcAtStop = StandTimer
  | otherwise = AwayTimer

clearsWhenMoving :: TimerKind -> Bool
clearsWhenMoving = (/= MovingTimer)

data ClaimPush = PushArriving | PushAssigned
  deriving (Show, Eq)

claimPush :: RankedCandidate -> ClaimPush
claimPush c = if standTimerOnClaim c then PushArriving else PushAssigned

-- | A cab the rider failed to board is skipped for this booking too, or its stand timer would charge a second no-show.
skipsPlateOnClose :: AllocationOutcome -> Bool
skipsPlateOnClose = isMissedCabOutcome

silentReleaseMult :: Int
silentReleaseMult = 5

-- | Silent for silentReleaseMult x ltsMaxAgeSec while the route's other cabs report; if nobody is fresh it is an LTS outage and decides nothing.
silentCab :: UTCTime -> Int -> Text -> [LT.VehicleTrackingOnRouteResp] -> Bool
silentCab now maxAgeSec plate tracking =
  any (isFreshPosition now maxAgeSec . (.vehicleInfo)) tracking
    && maybe True (not . isFreshPosition now (silentReleaseMult * maxAgeSec) . (.vehicleInfo)) (find ((== canonicalisePlate plate) . canonicalisePlate . (.vehicleNumber)) tracking)

-- | TODO: a queued route must still serve the board stop; needs its stop list from OTPRest. Tolerant for one tick (Phase 2 re-reads).
queuedRouteStillServes :: Session -> Text -> Bool
queuedRouteStillServes _ _ = True

rankCandidates :: [RankedCandidate] -> [RankedCandidate]
rankCandidates = sortOn rcEtaToBoardStopSec

planRouteAllocation ::
  UTCTime ->
  Config.SharedCabTunables ->
  [LT.VehicleTrackingOnRouteResp] ->
  [Session] ->
  FindingBooking ->
  [RankedCandidate]
planRouteAllocation now cfg tracking sessions booking =
  rankCandidates (eligibleCandidates now cfg tracking sessions booking)

withoutSkipped :: [Text] -> [RankedCandidate] -> [RankedCandidate]
withoutSkipped skipped = filter (not . (`isSkipped` skipped) . (.rcSession.vehicleNumber))

isSkipped :: Text -> [Text] -> Bool
isSkipped = elem

skippedWhileFinding :: Config.SharedCabTunables -> UTCTime -> Int -> UTCTime -> [Text] -> [Text]
skippedWhileFinding cfg now attempts findingSince skipped
  | fallbackReached now cfg.maxAttempts attempts findingSince cfg.fallbackAfterSec = []
  | otherwise = skipped

-- Lock order, Phase 2: target cab lock FIRST (`sharedcab:lock:{plate}`, shared with walk-up writes), then the booking
-- lock (`sharedcab:lock:booking:{id}`); NEVER reverse. The locks are non-reentrant (withWaitAndLockRedis deadlocks on a
-- key you hold), so never call releaseSharedCabAllocation or a re-bind from inside cancelLeg's booking lock.
-- frfs_ticket_booking is KV-enabled outside local dev, so a DB read can be stale mid-drain: the booking lock is the row
-- mutex and the updateAllocatedVehicle expected-value predicate is the belt-and-braces inside it.
-- allocation_created goes out AFTER the CAS + alloc-key write; a crash in between leaves an allocated booking with no
-- event, bounded by the alloc key TTL (no transactional bridge KV x Redis x Kafka).

data Closed = Closed
  { closedCity :: Id DMOC.MerchantOperatingCity,
    fallbackJustTriggered :: Bool, -- this close crossed maxAttempts
    autoCancelled :: Bool, -- this close was the rider's last allowed no-show: the booking is cancelled, not FINDING
    heldTrip :: Maybe Text -- the trip the allocation was made to, from its alloc key
  }

data ClaimMiss = ClaimSessionGone | ClaimSeatsGone | ClaimCasLost | ClaimSkipped
  deriving (Show, Eq)

-- | One candidate under both locks: re-read the session, recompute seats, CAS vehicleNumber, write the alloc key. A miss counts no attempt.
attemptClaim ::
  (MonadFlow m, Redis.HedisFlow m r, CacheFlow m r, EsqDBFlow m r, MonadMask m) =>
  Config.SharedCabTunables ->
  FindingBooking ->
  RankedCandidate ->
  m (Either ClaimMiss UTCTime)
attemptClaim cfg booking cand = do
  let plate = cand.rcSession.vehicleNumber
  Session.withPlateLock plate $ do
    mbSession <- Session.readSession plate
    case mbSession of
      Just s
        | s.status == ACTIVE && s.routeCode == booking.routeCode -> do
          withBookingLock booking.bookingId $ do
            now <- getCurrentTime
            attempts <- readAttempts (attemptsKey booking.bookingId)
            skipped <- skippedWhileFinding cfg now attempts booking.findingSince <$> shared (Redis.sMembers (skippedKey booking.bookingId))
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
                            -- the key's TTL is only a garbage-collection backstop and must outlive the stacked timers
                            warnAllocKeyTtlOnce cfg "claim"
                            let deadline = (\sec -> addUTCTime (intToNominalDiffTime sec) now) <$> claimTimerSec cfg cand
                            shared $
                              Redis.setExp
                                (allocKey booking.bookingId)
                                AllocationState {vehicleNumber = plate, driverId = Just s.driverId, vehicleTripId = Just s.vehicleTripId.getId, allocatedAt = now, expiresAt = deadline, attempts, timerKind = claimTimerKind cand}
                                (allocKeyTtl cfg)
                            pure (Right now)
                          else pure (Left ClaimCasLost)
                      Nothing -> pure (Left ClaimCasLost)
      _ -> pure (Left ClaimSessionGone)

readFindingSince :: (Redis.HedisFlow m r, MonadFlow m) => Id DFTB.FRFSTicketBooking -> UTCTime -> m UTCTime
readFindingSince bookingId createdAt = shared $ fromMaybe createdAt <$> Redis.safeGet (findingSinceKey bookingId)

-- | Warn ops once per site per hour that the alloc key TTL was raised past findingTimeoutSec; such a short value shortens every other TTL it feeds.
warnAllocKeyTtlOnce :: (MonadFlow m, Redis.HedisFlow m r, Log m) => Config.SharedCabTunables -> Text -> m ()
warnAllocKeyTtlOnce cfg site =
  when (allocKeyTtlShort cfg) . whenM (shared $ Redis.tryLockRedis (allocKeyTtlWarnKey site) 3600) $
    logWarning $
      "shared-cab config: alloc key TTL raised past findingTimeoutSec to cover the stacked timers (site "
        <> site
        <> "): findingTimeoutSec="
        <> show cfg.findingTimeoutSec
        <> ", standTimerSec="
        <> show cfg.standTimerSec
        <> ", movingTimerSec="
        <> show cfg.movingTimerSec
        <> ", allocKeyTtlMarginSec="
        <> show allocKeyTtlMarginSec
        <> " -- a key this short dies before its timers, the close reads TimerLost (no blame, no attempt) and the no-show path is unreachable; raise findingTimeoutSec"

claimFallbackPush :: (Redis.HedisFlow m r, MonadFlow m) => Config.SharedCabTunables -> Id DFTB.FRFSTicketBooking -> m Bool
claimFallbackPush cfg bookingId = shared $ Redis.tryLockRedis (fallbackPushedKey bookingId) cfg.findingTimeoutSec

readAttempts :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m Int
readAttempts key = shared $ fromMaybe 0 <$> Redis.safeGet key

-- | Idempotent close: whoever loses the booking CAS emits nothing.
releaseSharedCabAllocation ::
  AllocFlow m r =>
  Config.SharedCabTunables ->
  Id DFTB.FRFSTicketBooking ->
  Text -> -- expected plate -- the cab we believe holds the booking
  AllocationOutcome ->
  m Bool
releaseSharedCabAllocation cfg bookingId expectedPlate outcome = do
  closed <- withBookingLock bookingId $ closeLocked cfg bookingId expectedPlate outcome
  afterClose cfg bookingId expectedPlate outcome closed
  whenJust closed (triggerSharedCabAllocation . (.closedCity))
  pure (isJust closed)

-- | Rider skip: the plate joins the skipped set in the same booking-lock hold, so no claim can re-offer it first.
skipSharedCabAllocation :: AllocFlow m r => Config.SharedCabTunables -> Id DFTB.FRFSTicketBooking -> Text -> SkipReason -> m Bool
skipSharedCabAllocation cfg bookingId plate reason = do
  let outcome = RiderSkipped reason
  closed <- withBookingLock bookingId $ do
    closedInfo <- closeLocked cfg bookingId plate outcome
    when (isJust closedInfo) $ shared $ Redis.sAddExp (skippedKey bookingId) [plate] cfg.findingTimeoutSec
    pure closedInfo
  afterClose cfg bookingId plate outcome closed
  whenJust closed (triggerSharedCabAllocation . (.closedCity))
  pure (isJust closed)

-- | Releases a leaving cab's unboarded allocations without penalty. Call after the session write, outside the plate lock.
releaseUnboarded :: AllocFlow m r => Text -> AllocationOutcome -> m ()
releaseUnboarded plate outcome = do
  bookings <- QFRFSTicketBooking.findAllByVehicleNumberAndServiceTierTypeAndStatus (Just plate) (Just Spec.SHARED_CAB) [CONFIRMED]
  tickets <- if null bookings then pure [] else QFRFSTicket.findAllByTicketBookingIds (map (.id) bookings)
  let unboarded = [b | b <- bookings, isJust (allocatedPlate (b, [t.status | t <- tickets, t.frfsTicketBookingId == b.id]))]
  cities <- forM unboarded $ \b -> do
    cfg <- Config.getTunables b.merchantOperatingCityId
    closed <- withBookingLock b.id $ closeLocked cfg b.id plate outcome
    afterClose cfg b.id plate outcome closed
    pure closed
  mapM_ triggerSharedCabAllocation (nub (map (.closedCity) (catMaybes cities)))

clearAllocationKeys :: (MonadFlow m, Redis.HedisFlow m r) => Id DFTB.FRFSTicketBooking -> m ()
clearAllocationKeys bookingId =
  shared . forM_ [allocKey, attemptsKey, skippedKey, findingSinceKey, fallbackPushedKey] $ \key -> Redis.del (key bookingId)

-- | Inside the booking lock: CAS plate -> null (back to FINDING), clear the alloc key, bump attempts. Nothing when another closer won.
closeLocked ::
  (MonadFlow m, Redis.HedisFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  Config.SharedCabTunables ->
  Id DFTB.FRFSTicketBooking ->
  Text ->
  AllocationOutcome ->
  m (Maybe Closed)
closeLocked cfg bookingId expectedPlate outcome =
  QFRFSTicketBooking.findById bookingId >>= \case
    Nothing -> pure Nothing
    Just b -> do
      statuses <- map (.status) <$> QFRFSTicket.findAllByTicketBookingId b.id
      if closable expectedPlate b.status b.vehicleNumber statuses then Just <$> close b else pure Nothing
  where
    close b = do
      case blameFor outcome of
        BlameRider -> QFRFSTicketBooking.releaseAllocatedVehicle Nothing (NoShow.noShowsAfter BlameRider b.sharedCabNoShows) b.id (Just expectedPlate)
        _ -> QFRFSTicketBooking.updateAllocatedVehicle Nothing b.id (Just expectedPlate)
      heldTrip <- shared $ (>>= (.vehicleTripId)) <$> Redis.safeGet @AllocationState (allocKey bookingId)
      if NoShow.actionAfterClose (blameFor outcome) cfg.maxNoShows b.sharedCabNoShows == NoShow.AutoCancel
        then do
          cancelForNoShows b
          clearAllocationKeys bookingId
          pure Closed {closedCity = b.merchantOperatingCityId, fallbackJustTriggered = False, autoCancelled = True, heldTrip}
        else reallocate b heldTrip
    reallocate b heldTrip = do
      shared $ Redis.del (allocKey bookingId)
      when (skipsPlateOnClose outcome) $ shared $ Redis.sAddExp (skippedKey bookingId) [expectedPlate] cfg.findingTimeoutSec
      -- TODO: a Redis flush resets attempts.
      attemptsBefore <- readAttempts (attemptsKey bookingId)
      let attemptsNow = attemptsBefore + (if countsTowardAttempts outcome then 1 else 0)
      shared $ Redis.setExp (attemptsKey bookingId) attemptsNow cfg.findingTimeoutSec
      let fallbackJustTriggered = crossedMaxAttempts cfg.maxAttempts attemptsBefore attemptsNow
      now <- getCurrentTime
      shared $ Redis.setExp (findingSinceKey bookingId) now cfg.findingTimeoutSec
      when (attemptsNow < cfg.maxAttempts) $ shared $ Redis.del (fallbackPushedKey bookingId)
      when fallbackJustTriggered $
        logWarning $ "shared-cab booking " <> bookingId.getId <> " exhausted allocation attempts (" <> show attemptsNow <> "); fallback surface"
      pure Closed {closedCity = b.merchantOperatingCityId, fallbackJustTriggered, autoCancelled = False, heldTrip}

-- | The rider's last allowed no-show: cancelled by the system, fare kept (marking it refund-pending would make the payment
-- service refund). SMS and Google Wallet flips are skipped: they need the API Flow the scheduler env lacks. The row flips
-- to CANCELLED first, so a later failure can only lose an observability step (each effect is wrapped in withTryCatch).
cancelForNoShows :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, EncFlow m r) => DFTB.FRFSTicketBooking -> m ()
cancelForNoShows b = do
  void $ QFRFSTicketBooking.updateStatusById CANCELLED b.id
  void $ QFRFSTicket.updateAllStatusByBookingId DFRFSTicket.CANCELLED b.id
  void $ QFRFSRecon.updateStatusByTicketBookingId (Just DFRFSTicket.CANCELLED) b.id
  decision <- gateByPayment b NoRefund
  let (charges, refundAmount) = refundAmounts (fromMaybe b.totalPrice.amount b.overriddenAmount) decision
  QFRFSTicketBooking.updateRefundCancellationChargesAndIsCancellableByBookingId (Just refundAmount) (Just charges) (Just True) b.id
  void . withTryCatch "sharedCab:cancelForNoShows:cancelJourney" $
    FRFSCancelJourney.cancelJourneyIfOnlyTransitLeg b.searchId.getId
  withTryCatch "sharedCab:cancelForNoShows:recordCancelReason" (recordCancelReason b.id NO_SHOW_CAP)
    >>= either (\e -> logError $ "shared-cab cancel-reason not recorded for booking " <> b.id.getId <> ": " <> show e) pure
  releaseCancelledBooking b

-- | Side effects shared by every system cancel the tick does. Not crash-idempotent for the counters: a crash before them leaves
-- stats un-reversed rather than reversed twice.
releaseCancelledBooking :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => DFTB.FRFSTicketBooking -> m ()
releaseCancelledBooking b = do
  quantity <- FRFSPassOverride.ticketQuantityForBooking b
  mbPerson <- QPerson.findById b.riderId
  whenJust b.overrideAppliedEntityId $ \entityId -> do
    void . withTryCatch "sharedCab:releaseCancelledBooking:refundPassOverrideTrip" $
      FRFSPassOverride.refundPassOverrideTrip b (Id entityId) quantity
    void . withTryCatch "sharedCab:releaseCancelledBooking:releaseBookedTrip" $
      whenJust mbPerson $ \person ->
        FRFSPassOverride.releaseBookedTrip person (Id entityId) b.id.getId (fromMaybe b.createdAt b.startTime)
  void . withTryCatch "sharedCab:releaseCancelledBooking:clearPSCache" $ CQP.clearPSCache b.riderId
  case mbPerson of
    Nothing -> logError $ "shared-cab releaseCancelledBooking: person " <> b.riderId.getId <> " not found, stats reversal skipped"
    Just person -> do
      reverseStatsResult <-
        withTryCatch "sharedCab:releaseCancelledBooking:reversePersonPTStats" $ do
          purchaseEvent <- SPUS.mkPurchaseEvent person (Just b.vehicleType) b.serviceTierType DPUS.TICKET Nothing (Just quantity) b.merchantId b.merchantOperatingCityId
          SPUS.reversePurchase purchaseEvent
      either (\e -> logError $ "Failed to reverse PersonPTStats for booking " <> b.id.getId <> ": " <> show e) pure reverseStatsResult
  when (ticketsCountedAtConfirm b.parentBookingId) . void . withTryCatch "sharedCab:releaseCancelledBooking:ticketsBooked" $
    QPS.incrementTicketsBookedInEvent b.riderId (- quantity)

ticketsCountedAtConfirm :: Maybe parent -> Bool
ticketsCountedAtConfirm = isNothing

crossedMaxAttempts :: Int -> Int -> Int -> Bool
crossedMaxAttempts maxAttempts before now = before < maxAttempts && now >= maxAttempts

isMissedCabOutcome :: AllocationOutcome -> Bool
isMissedCabOutcome = (`elem` [StandTimeout, MovingTimeout])

eventBlame :: Blame -> Events.Blame
eventBlame = \case
  BlameDriver -> Events.BlameDriver
  BlameRider -> Events.BlameRider
  BlameNone -> Events.BlameNone

afterClose :: AllocFlow m r => Config.SharedCabTunables -> Id DFTB.FRFSTicketBooking -> Text -> AllocationOutcome -> Maybe Closed -> m ()
afterClose cfg bookingId plate outcome closed =
  whenJust closed $ \Closed {closedCity = cityId, fallbackJustTriggered, autoCancelled, heldTrip} -> do
    now <- getCurrentTime
    trip <- fmap (getId . (.vehicleTripId)) <$> Session.readSession plate
    Events.emit cityId . Events.withTrip trip $
      Events.bookingEvent (Events.AllocationClosed (outcomeText outcome) (eventBlame (blameFor outcome))) bookingId.getId (Just plate) Nothing now
    Invariants.checkBooking bookingId
    Invariants.checkCab plate
    mbBooking <- QFRFSTicketBooking.findById bookingId
    withTryCatch "sharedCabMisses" (NoShow.record (blameFor outcome) (mbBooking <&> (.riderId)) (Id <$> heldTrip))
      >>= either (\e -> logError $ "shared-cab miss count bump failed for booking " <> bookingId.getId <> ": " <> show e) pure
    if autoCancelled
      then do
        decision <- maybe (pure NoRefund) (`gateByPayment` NoRefund) mbBooking
        Events.emit cityId $ Events.bookingEvent (Events.BookingCancelled "system" (refundWord decision) (Just "max_no_shows")) bookingId.getId (Just plate) Nothing now
        mapM_ (\b -> Notify.notifyBookingCancelled b.sharedCabNoShows b) mbBooking
      else forM_ (Notify.reassignReasonFor outcome) $ \reason ->
        mapM_ (Notify.notifyReassigned reason) mbBooking
    when fallbackJustTriggered $
      whenM (claimFallbackPush cfg bookingId) $
        mapM_ Notify.notifyBoardAny mbBooking

-- | A timed-out or key-less ALLOCATED booking goes back to FINDING, decided under the booking lock so a claim still writing its key is not mistaken for lost.
expireTimers ::
  AllocFlow m r =>
  Config.SharedCabTunables ->
  UTCTime ->
  (Text -> Text -> Bool) -> -- plate moving on route, per fresh LTS positions
  (Text -> Text -> Bool) -> -- plate silent on route
  [(DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus])] ->
  m ()
expireTimers cfg now movingOn silentOn live =
  forM_ [(b, plate) | entry@(b, _) <- live, Just plate <- [allocatedPlate entry]] $ \(b, plate) -> do
    result <- withBookingLock b.id $ do
      mbState <- shared $ Redis.safeGet (allocKey b.id)
      case timerExpiry now mbState <|> (CabSilent <$ guard (maybe False (silentOn plate) b.routeCode)) of
        Just outcome -> fmap (outcome,) <$> closeLocked cfg b.id plate outcome
        Nothing -> do
          whenJust mbState $ \st ->
            when (clearsWhenMoving st.timerKind && isJust st.expiresAt && maybe False (movingOn plate) b.routeCode) $ do
              warnAllocKeyTtlOnce cfg "timer-clear"
              shared $ Redis.setExp (allocKey b.id) st {expiresAt = Nothing} (allocKeyTtl cfg)
          pure Nothing
    whenJust result $ \(outcome, closedInfo) -> afterClose cfg b.id plate outcome (Just closedInfo)

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

allocationPass ::
  AllocFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  m ([(DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus])], [(Text, [LT.VehicleTrackingOnRouteResp])])
allocationPass cityId = do
  cfg <- Config.getTunables cityId
  enabled <- sharedCabEnabled cityId
  if not enabled
    then ([], []) <$ logDebug "shared-cab is off for this city; allocation tick is a no-op"
    else do
      live <- cityLiveBookings cityId
      now <- getCurrentTime
      let routes = nub [route | entry@(b, _) <- live, isJust (findingOf entry) || isJust (allocatedPlate entry), Just route <- [b.routeCode]]
      positionsByRoute <- fmap catMaybes . forM routes $ \routeCode ->
        try (readRoutePositions routeCode) >>= \case
          -- TODO: "LTS outage != everyone silent": most sessions of a city stale in the same minute must suspend the freshness
          -- gate. Skipping the route is fail-safe: no allocation from stale data, no stand timer cleared.
          Left (e :: SomeException) -> Nothing <$ logError ("shared-cab tick: LTS read failed for route " <> routeCode <> ": " <> show e)
          Right positions -> pure (Just (routeCode, positions))
      let movingOn plate route =
            any (\vt -> canonicalisePlate vt.vehicleNumber == canonicalisePlate plate && isFreshPosition now cfg.ltsMaxAgeSec vt.vehicleInfo && isMovingSpeed vt.vehicleInfo.speed) $
              fromMaybe [] (lookup route positionsByRoute)
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
            attempts <- readAttempts (attemptsKey booking.bookingId)
            skipped <- skippedWhileFinding cfg now attempts booking.findingSince <$> shared (Redis.sMembers (skippedKey booking.bookingId))
            void $ claimFirst cfg booking (withoutSkipped skipped (planRouteAllocation now cfg positions sessions booking))
      pure (live, positionsByRoute)

pushTimeFallbacks :: AllocFlow m r => Config.SharedCabTunables -> UTCTime -> [(DFTB.FRFSTicketBooking, FindingBooking)] -> m ()
pushTimeFallbacks cfg now findings =
  forM_ [b | (b, fb) <- findings, fallbackTimeElapsed now fb.findingSince cfg.fallbackAfterSec] $ \b ->
    whenM (claimFallbackPush cfg b.id) $ Notify.notifyBoardAny b

groupAllOn :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupAllOn f = map (\grp -> (f (head grp), grp)) . groupBy ((==) `on` f) . sortOn f

claimFirst ::
  AllocFlow m r =>
  Config.SharedCabTunables ->
  FindingBooking ->
  [RankedCandidate] ->
  m (Maybe RankedCandidate)
claimFirst cfg booking = go (0 :: Int)
  where
    go _ [] = pure Nothing
    go rank (c : cs) =
      attemptClaim cfg booking c >>= \case
        Right _ -> do
          now <- getCurrentTime
          Events.emit c.rcSession.merchantOperatingCityId . Events.withTrip (Just c.rcSession.vehicleTripId.getId) $
            Events.bookingEvent (Events.AllocationCreated (Just (c.rcEtaToBoardStopSec `div` 60)) rank) booking.bookingId.getId (Just c.rcSession.vehicleNumber) (Just booking.routeCode) now
          Invariants.checkBooking booking.bookingId
          Invariants.checkCab c.rcSession.vehicleNumber
          fork "sharedCabNotifyDriver" (notifyDriverOfAllocation booking c)
          whenJustM (QFRFSTicketBooking.findById booking.bookingId) $ case claimPush c of
            PushArriving -> Notify.notifyArriving c.rcSession.vehicleNumber cfg.standTimerSec
            PushAssigned -> Notify.notifyAssigned c.rcSession.vehicleNumber
          pure (Just c)
        Left miss -> do
          logDebug $ "shared-cab claim missed booking=" <> booking.bookingId.getId <> " cab=" <> c.rcSession.vehicleNumber <> " reason=" <> show miss
          go (rank + 1) cs
