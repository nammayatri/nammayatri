-- | Silent shared-cab sessions: no fresh LTS ping for `pauseAfter` → PAUSED(NO_LOCATION); for `endAfter` → ENDED
-- (driver-app's trip closed SESSION_TIMEOUT, rideEnd). A route whose LTS read fails is skipped, so an LTS outage pauses nobody.
module SharedLogic.Scheduler.Jobs.SharedCabSessionExpiry
  ( sharedCabSessionExpiry,
  )
where

import Data.List (nub, (\\))
import qualified Data.Map.Strict as M
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import qualified SharedLogic.SharedCab.Allocation as Allocation
import SharedLogic.SharedCab.Allocation.Types (AllocationOutcome (SessionClosed), parseLtsTimestamp)
import SharedLogic.SharedCab.Enabled (sharedCabEnabled)
import qualified SharedLogic.SharedCab.Lts as Lts
import SharedLogic.SharedCab.LtsAttach (LtsFlow)
import qualified SharedLogic.SharedCab.RefundRetry as RefundRetry
import SharedLogic.SharedCab.Schedule (claimExpiryRun, claimedTick, ensureDegradedSweep, ownExpiryChain, reseedAllocationTick, scheduleNextExpiry, watchPlan)
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState

-- rider_config.noLocationPauseMin / noLocationEndMin defaults; read from config once those fields exist.
pauseAfter :: NominalDiffTime
pauseAfter = 15 * 60

endAfter :: NominalDiffTime
endAfter = 60 * 60

type ExpiryFlow m r c = (LtsFlow m r c, MonadMask m, JobCreator r m, Redis.HedisLTSFlowEnv r, ServiceFlow m r, HasFlowEnv m r '["maxNotificationShards" ::: Int], EncFlow m r, Allocation.InternalEndpointFlow m r)

sharedCabSessionExpiry :: ExpiryFlow m r c => Job 'SharedCabSessionExpiry -> m ExecutionResult
sharedCabSessionExpiry Job {jobInfo} = do
  let jobData = jobInfo.jobData
  ownExpiryChain jobData.merchantId jobData.merchantOperatingCityId jobData.chainId $ \chainId -> do
    claimedTick
      (claimExpiryRun jobData.merchantOperatingCityId)
      (expireSilentSessions jobData.merchantId jobData.merchantOperatingCityId >>= watchChains jobData.merchantId jobData.merchantOperatingCityId)
      (scheduleNextExpiry jobData.merchantId jobData.merchantOperatingCityId chainId)
  pure Complete

-- | Re-seeds the allocation and sweep chains if their jobs were lost (see 'watchPlan'); a failure here never costs the expiry pass.
watchChains :: ExpiryFlow m r c => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> (Int, Int) -> m ()
watchChains merchantId mocId (liveBookings, liveSessions) =
  withTryCatch "sharedCab:watchChains" watch >>= \case
    Left err -> logError $ "sharedCab expiry: chain watchdog failed: " <> show err
    Right () -> pure ()
  where
    watch = do
      pendingRefunds <- length <$> RefundRetry.pendingRefundRetries mocId
      enabled <- sharedCabEnabled mocId
      let plan = watchPlan enabled liveBookings liveSessions pendingRefunds
      when plan.seedAllocation $ reseedAllocationTick merchantId mocId
      when plan.seedSweep $ ensureDegradedSweep merchantId mocId

-- | Answers the city's live booking and live session counts, for the watchdog.
expireSilentSessions :: ExpiryFlow m r c => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m (Int, Int)
expireSilentSessions merchantId mocId = do
  liveBookings <- Allocation.cityLiveBookings mocId
  sessions <- cityLiveSessions merchantId mocId [plate | b <- liveBookings, Just plate <- [b.booking.vehicleNumber]]
  now <- getCurrentTime
  pings <- M.fromList . catMaybes <$> mapM (lastPings now) (nub $ map (.routeCode) sessions)
  forM_ sessions $ \s ->
    withTryCatch "sharedCab:expiry" (checkSession pings now s) >>= \case
      Left err -> logError $ "sharedCab expiry failed for " <> s.vehicleNumber <> ": " <> show err
      Right () -> pure ()
  pure (length liveBookings, length sessions)

-- | The live-plate set, plus the plates of the city's live bookings that it doesn't hold: a Redis flush takes the set too, and a cab
-- with riders must still come back from driver-app's trip, and expire if it is silent.
cityLiveSessions :: ExpiryFlow m r c => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> [Text] -> m [Session]
cityLiveSessions merchantId mocId bookingPlates = do
  indexed <- Session.liveSessionsInCity mocId
  rebuilt <- forM (nub bookingPlates \\ map (.vehicleNumber) indexed) $ \plate ->
    withTryCatch "sharedCab:expiryRebuild" (Session.getSession merchantId plate)
      >>= either (\e -> Nothing <$ logError ("sharedCab expiry: couldn't read the session of " <> plate <> ": " <> show e)) pure
  pure $ indexed <> filter (\s -> s.status /= ENDED && s.merchantOperatingCityId == mocId) (catMaybes rebuilt)

-- | plate → latest ping on the route, or Nothing if LTS couldn't be read.
lastPings :: (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) => UTCTime -> Text -> m (Maybe (Text, M.Map Text Ping))
lastPings now route =
  withTryCatch "sharedCab:trackVehicles" (Lts.readRoutePositions route) >>= \case
    Left err -> do
      logError $ "sharedCab expiry: LTS read failed for route " <> route <> ", skipping it: " <> show err
      pure Nothing
    Right vehicles -> pure $ Just (route, M.fromList [(v.vehicleNumber, readPing now (v.vehicleInfo.timestamp >>= parseLtsTimestamp)) | v <- vehicles])

checkSession :: ExpiryFlow m r c => M.Map Text (M.Map Text Ping) -> UTCTime -> Session -> m ()
checkSession pings now s = whenJust (M.lookup s.routeCode pings) $ \routePings -> do
  let ping = M.lookup s.vehicleNumber routePings
  when (ping == Just Unreadable) $ logWarning $ "sharedCab expiry: unreadable or future LTS timestamp for " <> s.vehicleNumber
  whenJust (lastSeenFor endAfter now s.startedAt ping) $ \lastSeen ->
    case expiryAction pauseAfter endAfter now lastSeen s.status of
      -- leaving ACTIVE releases unboarded allocations without penalty, after the plate lock is released
      Just PauseSilent -> Session.pause s.vehicleNumber NO_LOCATION >> Allocation.releaseUnboarded s.vehicleNumber SessionClosed
      Just EndSilent -> Session.expire s.vehicleNumber >> Allocation.releaseUnboarded s.vehicleNumber SessionClosed
      Nothing -> pure ()
