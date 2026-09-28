-- | `05` §6 stop-progress actions for every live cab of a city, run by the allocation tick under its city lease:
-- board stop passed, drop auto-end, off-route pause, and the trip's `reachedEndAt`. Decisions live in `StopProgress.Rules`.
module SharedLogic.SharedCab.StopProgress
  ( runStopProgress,
  )
where

import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Kernel.External.Maps.Google.PolyLinePoints as KEPP
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.SharedCab.Allocation (allocKey, cityConfig, isFreshPosition, readRoutePositions, releaseSharedCabAllocation, releaseUnboarded, shared, sharedCabAllocationEnabled)
import SharedLogic.SharedCab.Allocation.Types (AllocationConfig (..), AllocationOutcome (..), AllocationState (..), Blame (BlameNone), TimerKind (..), passedStopBlame)
import SharedLogic.SharedCab.Booking (markDropped, readRiderFix, withBookingLock)
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Events as Events
import qualified SharedLogic.SharedCab.Invariants as Invariants
import SharedLogic.SharedCab.LtsAttach (LtsFlow)
import qualified SharedLogic.SharedCab.Notify as Notify
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (PauseReason (OFF_ROUTE), Session (..), SessionStatus (..))
import SharedLogic.SharedCab.StopProgress.Rules
import qualified Storage.CachedQueries.RoutePolylines as QRoutePolylines
import qualified Storage.Queries.VehicleTrip as QVT

-- LtsFlow: applyQueuedRoute re-attaches the cab to LTS when a queued route applies.
-- ServiceFlow: R17's "your cab is here" push, fired once the moving timer arms.
type StopProgressFlow m r c =
  ( LtsFlow m r c,
    Redis.HedisFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadMask m,
    Log m,
    Redis.HedisLTSFlowEnv r,
    Metrics.CoreMetrics m,
    Events.EventFlow m r,
    ServiceFlow m r
  )

dropClockKey :: Id DFTB.FRFSTicketBooking -> Text
dropClockKey bookingId = "sharedcab:dropclock:" <> bookingId.getId

offRouteKey :: Text -> Text
offRouteKey plate = "sharedcab:offroute:" <> plate

-- | The city's tunables (rider_config via Config.getTunables, defaults where unset).
stopProgressConfig :: Config.SharedCabTunables -> StopProgressConfig
stopProgressConfig t =
  StopProgressConfig
    { atStopRadiusM = fromIntegral t.atStopRadiusM,
      autoEndAfterDropSec = t.autoEndAfterDropSec,
      offRouteMeters = fromIntegral t.offRouteMeters,
      offRouteSec = t.offRouteSec,
      movingTimerSec = t.movingTimerSec
    }

-- | Live = not ENDED: a PAUSED cab still carries and drops its riders, it just isn't checked for off-route.
-- Runs under the tick's city lease, after allocationPass, reusing its live bookings and LTS reads.
runStopProgress ::
  StopProgressFlow m r c =>
  Id DMOC.MerchantOperatingCity ->
  [(DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus])] ->
  [(Text, [LT.VehicleTrackingOnRouteResp])] ->
  m ()
runStopProgress cityId live positionsByRoute = when sharedCabAllocationEnabled $ do
  cfg <- cityConfig cityId
  spc <- stopProgressConfig <$> Config.getTunables cityId
  trips <- QVT.findAllLiveByMerchantOperatingCityId cityId
  -- no flush recovery here: the expiry job owns that (it needs the scheduler's job-creation env)
  sessions <- filter ((/= ENDED) . (.status)) . catMaybes <$> mapM (Session.readSession . (.vehicleNumber)) trips
  now <- getCurrentTime
  forM_ (M.toList $ M.fromListWith (<>) [(s.routeCode, [s]) | s <- sessions]) $ \(routeCode, cabs) -> do
    positions <- case lookup routeCode positionsByRoute of
      Just read' -> pure read'
      Nothing ->
        withTryCatch "sharedCabStopProgressLts" (readRoutePositions routeCode)
          >>= either (\e -> [] <$ logError ("shared-cab stop progress: LTS read failed for route " <> routeCode <> ": " <> show e)) pure
    route <- if any ((== ACTIVE) . (.status)) cabs then routePolyline cityId routeCode else pure []
    forM_ cabs $ \s ->
      withTryCatch "sharedCabStopProgressCab" (stepCab cfg spc now live positions route s)
        >>= either (\e -> logError $ "shared-cab stop progress failed for cab " <> s.vehicleNumber <> ": " <> show e) pure

routePolyline :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> Text -> m [LatLong]
routePolyline cityId routeCode = maybe [] KEPP.decode . (>>= (.polyline)) <$> QRoutePolylines.getByRouteIdAndCity routeCode cityId

stepCab ::
  StopProgressFlow m r c =>
  AllocationConfig ->
  StopProgressConfig ->
  UTCTime ->
  [(DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus])] ->
  [LT.VehicleTrackingOnRouteResp] ->
  [LatLong] ->
  Session ->
  m ()
stepCab cfg spc now live positions route s = do
  let plate = s.vehicleNumber
      mbInfo = (.vehicleInfo) <$> find ((== plate) . (.vehicleNumber)) positions
      mbCab = cabFix <$> mbInfo
      freshCab = mfilter (const $ any (isFreshPosition now cfg.ltsMaxAgeSec) mbInfo) mbCab
      bookings = [entry | entry@(b, _) <- live, b.vehicleNumber == Just plate]
      awaiting = [b | (b, statuses) <- bookings, DFRFSTicket.ACTIVE `elem` statuses, DFRFSTicket.INPROGRESS `notElem` statuses]
      onBoard = [b | (b, statuses) <- bookings, DFRFSTicket.INPROGRESS `elem` statuses]
  forM_ awaiting $ \b ->
    if any (boardStopPassed b.fromStationCode) mbCab
      then do
        -- R15: the no-show is allocation_closed's blame (`05` §7); a stop missing from the cab's list can't be judged
        riderFix <- readRiderFix b.id
        let boardStop = (.coordinate) <$> (find ((== b.fromStationCode) . (.stopCode)) . (.stops) =<< mbCab)
            blame = maybe BlameNone (\stop -> passedStopBlame cfg.atStopRadiusM cfg.ltsMaxAgeSec now stop riderFix) boardStop
        void $ releaseSharedCabAllocation cfg b.id plate (PassedStop blame)
      else movingTimerStep cfg.findingTimeoutSec spc now freshCab plate b
  forM_ onBoard $ dropStep spc now mbCab plate
  when (s.status == ACTIVE) $ offRouteStep spc now route ((.position) <$> freshCab) plate
  whenJust mbCab $ \cab -> when (reachedRouteEnd spc.atStopRadiusM cab) $ markReachedEnd now s

cabFix :: LT.VehicleInfo -> CabFix
cabFix vi =
  CabFix
    { position = LatLong vi.latitude vi.longitude,
      stops = maybe [] (map mark) vi.upcomingStops
    }
  where
    mark u = StopMark {stopCode = u.stop.stopCode, stopIdx = u.stop.stopIdx, reached = u.status == LT.Reached, coordinate = u.stop.coordinate}

-- | Under the booking lock the key is re-read, so a close or re-bind racing the tick is never overwritten.
movingTimerStep :: StopProgressFlow m r c => Int -> StopProgressConfig -> UTCTime -> Maybe CabFix -> Text -> DFTB.FRFSTicketBooking -> m ()
movingTimerStep keyTtlSec spc now freshCab plate b = do
  mbState <- shared $ Redis.safeGet @AllocationState (allocKey b.id.getId)
  whenJust mbState $ \st -> whenJust (armMovingTimer spc now st.expiresAt freshCab b.fromStationCode) $ \_ -> do
    armed <- withBookingLock b.id $ do
      current <- shared $ Redis.safeGet @AllocationState (allocKey b.id.getId)
      case current of
        Just cur
          | cur.vehicleNumber == plate,
            Just deadline <- armMovingTimer spc now cur.expiresAt freshCab b.fromStationCode -> do
            shared $ Redis.setExp (allocKey b.id.getId) cur {expiresAt = Just deadline, timerKind = MovingTimer} keyTtlSec
            pure True
        _ -> pure False
    when armed $ do
      Invariants.checkBooking b.id
      -- R17: the cab just reached the board stop -- "board within Xs" (X = movingTimerSec, the deadline just armed).
      Notify.notifyArriving plate spc.movingTimerSec b

-- | A degraded boarding has no geofence end (`05` §5): its marker's expiry ends it instead.
dropStep :: StopProgressFlow m r c => StopProgressConfig -> UTCTime -> Maybe CabFix -> Text -> DFTB.FRFSTicketBooking -> m ()
dropStep spc now mbCab plate b = do
  degraded <- isJust <$> shared (Redis.get @A.Value ("sharedcab:degraded:" <> b.id.getId))
  unless degraded $ do
    clock <- Redis.withMasterRedis $ Redis.safeGet (dropClockKey b.id)
    case dropAction spc now clock mbCab b.toStationCode of
      StartDropClock -> Redis.withMasterRedis $ Redis.setExp (dropClockKey b.id) now (2 * spc.autoEndAfterDropSec)
      AutoEnd -> do
        markDropped Events.DroppedByTick b
        switched <- Session.applyQueuedRoute plate
        when switched $ releaseUnboarded plate RouteChanged
        Redis.withMasterRedis $ Redis.del (dropClockKey b.id)
        Invariants.checkBooking b.id
        Invariants.checkCab plate
      KeepWaiting -> pure ()

offRouteStep :: StopProgressFlow m r c => StopProgressConfig -> UTCTime -> [LatLong] -> Maybe LatLong -> Text -> m ()
offRouteStep spc now route freshPosition plate = do
  offSince <- Redis.withMasterRedis $ Redis.safeGet (offRouteKey plate)
  case offRouteAction spc now offSince route freshPosition of
    StartOffRouteClock -> Redis.withMasterRedis $ Redis.setExp (offRouteKey plate) now (2 * spc.offRouteSec)
    ClearOffRouteClock -> Redis.withMasterRedis $ Redis.del (offRouteKey plate)
    PauseOffRoute -> do
      Redis.withMasterRedis $ Redis.del (offRouteKey plate)
      withTryCatch "sharedCabOffRoutePause" (Session.pause plate OFF_ROUTE) >>= \case
        Left e -> logWarning $ "shared-cab off-route pause skipped for " <> plate <> ": " <> show e
        Right _ -> do
          -- TODO(7.6): Events paused {reason: OFF_ROUTE}
          -- `05` §8.7: boarded riders stay; releaseUnboarded checks the invariants of each booking it frees.
          releaseUnboarded plate SessionClosed
          Invariants.checkCab plate
    OffRouteNoChange -> pure ()

markReachedEnd :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, Metrics.CoreMetrics m, Events.EventFlow m r) => UTCTime -> Session -> m ()
markReachedEnd now s =
  QVT.findById s.vehicleTripId >>= traverse_ \trip ->
    when (isNothing trip.reachedEndAt) $ do
      QVT.updateReachedEndAt (Just now) trip.id
      Invariants.checkCab s.vehicleNumber
