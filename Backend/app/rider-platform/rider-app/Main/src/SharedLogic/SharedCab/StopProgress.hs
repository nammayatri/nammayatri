-- | Stop-progress actions for every live cab of a city, run by the allocation tick under its city lease: board stop passed,
-- drop auto-end and off-route pause. Decisions live in `StopProgress.Rules`.
module SharedLogic.SharedCab.StopProgress
  ( runStopProgress,
    bookingsByPlate,
  )
where

import Control.Monad.Extra (whenJustM)
import qualified Data.Aeson as A
import Data.List (findIndex)
import qualified Data.Map.Strict as M
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RouteStopMapping as DRSM
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error (MerchantError (MerchantNotFound))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import SharedLogic.SharedCab.Allocation (InternalEndpointFlow, eachIsolated, releaseSharedCabAllocation, releaseUnboarded)
import SharedLogic.SharedCab.Allocation.Select (LiveBooking (..), groupByKey, isFreshPosition)
import SharedLogic.SharedCab.Allocation.Types (AllocationOutcome (..), AllocationState (..), Blame (BlameNone), Timer (..), allocKeyTtl, passedStopBlame, runningTimerDeadline, seatIsRiders)
import SharedLogic.SharedCab.Booking (boardedTouchDue, readRiderFix, shared, withBookingLock)
import SharedLogic.SharedCab.BookingState (readAlloc, writeAlloc)
import qualified SharedLogic.SharedCab.CheckIn as CheckIn
import qualified SharedLogic.SharedCab.Config as Config
import SharedLogic.SharedCab.Keys (degradedKey, dropClockKey, livePlatesKey, offRouteKey, sessionKey)
import SharedLogic.SharedCab.Lts (CabTrack (..), RouteRead (..), nextStopCode, readRoutePositions)
import qualified SharedLogic.SharedCab.Notify as Notify
import SharedLogic.SharedCab.Seats (markDropped, routeStopRows)
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (PauseReason (OFF_ROUTE), Session (..), SessionFlow, SessionStatus (..))
import SharedLogic.SharedCab.StopProgress.Rules
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Tools.Fields as Fields

type StopProgressFlow m r c =
  ( SessionFlow m r c,
    Redis.HedisFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadMask m,
    Log m,
    Redis.HedisLTSFlowEnv r,
    Metrics.CoreMetrics m,
    ServiceFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    EncFlow m r,
    InternalEndpointFlow m r
  )

-- | Live = not ENDED: a PAUSED cab still carries and drops its riders, it just isn't checked for off-route.
runStopProgress ::
  StopProgressFlow m r c =>
  Id DMOC.MerchantOperatingCity ->
  [LiveBooking] ->
  [(Text, RouteRead)] ->
  m ()
runStopProgress cityId live positionsByRoute = do
  cfg <- Config.getTunables cityId
  plates <- shared $ Redis.sMembers (livePlatesKey cityId)
  -- one pipelined MGET (grouped by slot) for every live cab's session; stale members are the expiry pass's to repair
  sessions <- filter (\s -> s.status /= ENDED && s.merchantOperatingCityId == cityId) <$> shared (Redis.mGetCluster (map sessionKey plates))
  now <- getCurrentTime
  let byPlate = bookingsByPlate live
  forM_ (M.toList $ M.fromListWith (<>) [(s.routeCode, [s]) | s <- sessions]) $ \(routeCode, cabs@(cab : _)) -> do
    routeRead <- case lookup routeCode positionsByRoute of
      Just read' -> pure read'
      Nothing ->
        withTryCatch "sharedCabStopProgressRead" (RouteRead <$> readRoutePositions routeCode <*> routeStopRows cab.integratedBppConfigId routeCode)
          >>= either (\e -> RouteRead [] [] <$ logError ("shared-cab stop progress: position read failed for route " <> routeCode <> ": " <> show e)) pure
    eachIsolated
      (withTryCatch "sharedCabStopProgressCab")
      (\s e -> logError $ "shared-cab stop progress failed for cab " <> s.vehicleNumber <> ": " <> show e)
      (\s -> stepCab cfg now (M.findWithDefault [] s.vehicleNumber byPlate) routeRead s)
      cabs

-- | The tick's live bookings by their cab, built once per tick instead of a scan of all of them per cab.
bookingsByPlate :: [LiveBooking] -> M.Map Text [LiveBooking]
bookingsByPlate = groupByKey (.booking.vehicleNumber)

stepCab ::
  StopProgressFlow m r c =>
  Config.SharedCabTunables ->
  UTCTime ->
  [LiveBooking] ->
  RouteRead ->
  Session ->
  m ()
stepCab cfg now bookings routeRead s = do
  let plate = s.vehicleNumber
      mbTrack = find ((== plate) . (.vehicleNumber)) routeRead.positions
      mbCab = cabFix routeRead.stops <$> mbTrack
      freshTrack = mfilter (isFreshPosition now cfg.ltsMaxAgeSec) mbTrack
      freshCab = mfilter (const $ isJust freshTrack) mbCab
      awaiting = [lb.booking | lb <- bookings, DFRFSTicket.ACTIVE `elem` lb.ticketStatuses, DFRFSTicket.INPROGRESS `notElem` lb.ticketStatuses]
      onBoard = [lb.booking | lb <- bookings, DFRFSTicket.INPROGRESS `elem` lb.ticketStatuses]
  -- an offered booking is not the rider's yet: the cab passing its stop is nobody's miss
  unoffered <- filterM (fmap seatIsRiders . readAlloc . (.id)) awaiting
  forM_ unoffered $ \b ->
    if any (boardStopPassed b.fromStationCode) mbCab
      then do
        -- the no-show is allocation_closed's blame; a stop missing from the cab's list can't be judged
        riderFix <- readRiderFix b.id
        let boardStop = (.coordinate) <$> (find ((== b.fromStationCode) . (.stopCode)) . (.stops) =<< mbCab)
            blame = maybe BlameNone (\stop -> passedStopBlame cfg.atStopRadiusM cfg.ltsMaxAgeSec now stop riderFix) boardStop
        void $ releaseSharedCabAllocation cfg b.id plate (PassedStop blame)
      else do
        -- the key the moving timer is armed under must outlive it (see allocKeyTtl in Allocation.Types)
        movingTimerStep (allocKeyTtl cfg) cfg now freshCab plate b
  forM_ onBoard $ dropStep cfg now mbCab plate
  when (s.status == ACTIVE) $ offRouteStep cfg now ((.offRoute) <$> freshTrack) s

-- | gps-processor lists only the stops still ahead: every route stop before the cab's next one is reached.
-- ponytail: past the last stop gps-processor lists none, so nothing reads reached and drops there go by the at-stop radius.
cabFix :: [DRSM.RouteStopMapping] -> CabTrack -> CabFix
cabFix stops cab =
  CabFix
    { position = cab.position,
      stops = zipWith mark [0 ..] stops,
      -- `is_on_stop` is about gps-processor's next stop: with that stop off this route's mapping, the at-stop radius decides instead.
      onStop = cab.onStop <* nextIdx
    }
  where
    nextIdx = nextStopCode cab >>= \code -> findIndex ((== code) . (.stopCode)) stops
    mark idx stop = StopMark {stopCode = stop.stopCode, stopIdx = idx, reached = maybe False (idx <) nextIdx, coordinate = stop.stopPoint}

-- | Under the booking lock the key is re-read, so a close or re-bind racing the tick is never overwritten.
movingTimerStep :: StopProgressFlow m r c => Int -> Config.SharedCabTunables -> UTCTime -> Maybe CabFix -> Text -> DFTB.FRFSTicketBooking -> m ()
movingTimerStep keyTtlSec cfg now freshCab plate b = do
  mbState <- readAlloc b.id
  whenJust mbState $ \st -> whenJust (armMovingTimer cfg now (runningTimerDeadline st) freshCab b.fromStationCode) $ \_ -> do
    armed <- withBookingLock b.id $ do
      current <- readAlloc b.id
      case current of
        Just cur
          | cur.vehicleNumber == plate,
            Just deadline <- armMovingTimer cfg now (runningTimerDeadline cur) freshCab b.fromStationCode -> do
            writeAlloc keyTtlSec b.id cur {timer = MovingTimer deadline}
            pure True
        _ -> pure False
    when armed $ Notify.notifyArriving plate cfg.movingTimerSec b

dropStep :: StopProgressFlow m r c => Config.SharedCabTunables -> UTCTime -> Maybe CabFix -> Text -> DFTB.FRFSTicketBooking -> m ()
dropStep cfg now mbCab plate b = do
  degraded <- isJust <$> shared (Redis.get @A.Value (degradedKey b.id))
  unless degraded $ do
    -- keeps a long ride inside the city scan's window (Booking.cityLiveWindowSec)
    when (boardedTouchDue now b.updatedAt) $ QFRFSTicketBooking.updateUpdatedAtById b.id
    clock <- Redis.withMasterRedis $ Redis.safeGet (dropClockKey b.id)
    case dropAction cfg now clock mbCab b.toStationCode of
      StartDropClock -> Redis.withMasterRedis $ Redis.setExp (dropClockKey b.id) now (2 * cfg.autoEndAfterDropSec)
      AutoEnd -> do
        markDropped b
        switched <- Session.applyQueuedRoute plate
        when switched $ releaseUnboarded plate RouteChanged
        Redis.withMasterRedis $ Redis.del (dropClockKey b.id)
      KeepWaiting -> pure ()

-- | A driver who confirmed the route keeps the riders for one more `offRouteSec`, and is asked again within it.
offRouteStep :: StopProgressFlow m r c => Config.SharedCabTunables -> UTCTime -> Maybe Bool -> Session -> m ()
offRouteStep cfg now freshOffRoute s = do
  offSince <- Redis.withMasterRedis $ Redis.safeGet (offRouteKey plate)
  let offFor = maybe "" (\since -> "after " <> show (round (diffUTCTime now since) :: Int) <> " s off route") offSince
  case offRouteAction cfg now offSince freshOffRoute of
    StartOffRouteClock -> startClock
    ClearOffRouteClock -> do
      Redis.withMasterRedis $ Redis.del (offRouteKey plate)
      void $ CheckIn.clear plate
    AskCheckIn deadline -> whenJust offSince $ \since -> whenJustM (CheckIn.openIfNone plate since deadline) $ \c -> do
      logInfo $ "shared-cab check-in: opened " <> c.checkInId <> " for " <> plate <> " on route " <> s.routeCode <> " " <> offFor
      fork "sharedCabCheckInPush" (notifyCheckIn s)
    PauseOffRoute ->
      CheckIn.clear plate >>= \case
        Just c | c.answered -> do
          logInfo $ "shared-cab check-in: " <> plate <> " confirmed " <> c.checkInId <> " " <> offFor <> ", off-route clock restarted"
          startClock
        mbOpen -> do
          whenJust mbOpen $ \c -> logInfo $ "shared-cab check-in: " <> plate <> " did not answer " <> c.checkInId <> " " <> offFor <> ", pausing"
          Redis.withMasterRedis $ Redis.del (offRouteKey plate)
          withTryCatch "sharedCabOffRoutePause" (Session.pause plate OFF_ROUTE) >>= \case
            Left e -> logWarning $ "shared-cab off-route pause skipped for " <> plate <> ": " <> show e
            Right _ -> do
              releaseUnboarded plate SessionClosed
    OffRouteNoChange -> pure ()
  where
    plate = s.vehicleNumber
    startClock = Redis.withMasterRedis $ Redis.setExp (offRouteKey plate) now (2 * cfg.offRouteSec)

-- | Best-effort: the session poll shows the check-in anyway, so a failed push is only logged.
notifyCheckIn :: StopProgressFlow m r c => Session -> m ()
notifyCheckIn s =
  withTryCatch "sharedCabCheckInPush" push >>= \case
    Left e -> logWarning $ "shared-cab check-in push failed for " <> s.vehicleNumber <> ": " <> show e
    Right _ -> pure ()
  where
    push = do
      merchant <- CQM.findById s.merchantId >>= fromMaybeM (MerchantNotFound s.merchantId.getId)
      CallBPPInternal.sharedCabCheckInFCM merchant.driverOfferApiKey merchant.driverOfferBaseUrl $
        CallBPPInternal.SharedCabCheckInReq {driverId = s.driverId, routeCode = s.routeCode}
