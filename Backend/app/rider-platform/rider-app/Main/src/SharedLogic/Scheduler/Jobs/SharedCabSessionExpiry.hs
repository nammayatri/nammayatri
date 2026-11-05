-- | Silent shared-cab sessions: no fresh LTS ping for `pauseAfter` → PAUSED(NO_LOCATION); for `endAfter` → ENDED. Driver-app owns
-- the run, so the pause or end is asked of it and its answer applied here. A route whose LTS read fails is skipped, so an LTS outage
-- pauses nobody.
module SharedLogic.Scheduler.Jobs.SharedCabSessionExpiry
  ( sharedCabSessionExpiry,
  )
where

import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.SharedCabSession (CabEvent (..))
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import qualified SharedLogic.SharedCab.Allocation as Allocation
import SharedLogic.SharedCab.Allocation.Types (parseLtsTimestamp)
import SharedLogic.SharedCab.CabSync (CabSyncFlow, cabEvent)
import SharedLogic.SharedCab.Enabled (sharedCabEnabled)
import qualified SharedLogic.SharedCab.Lts as Lts
import qualified SharedLogic.SharedCab.RefundRetry as RefundRetry
import SharedLogic.SharedCab.Schedule (claimExpiryRun, claimedTick, ensureDegradedSweep, ownExpiryChain, reseedAllocationTick, scheduleNextExpiry, watchPlan)
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState

-- rider_config.noLocationPauseMin / noLocationEndMin defaults; read from config once those fields exist.
pauseAfter :: NominalDiffTime
pauseAfter = 15 * 60

endAfter :: NominalDiffTime
endAfter = 60 * 60

type ExpiryFlow m r c = (CabSyncFlow m r, MonadMask m, JobCreator r m, Redis.HedisLTSFlowEnv r, ServiceFlow m r, HasFlowEnv m r '["maxNotificationShards" ::: Int], EncFlow m r, Allocation.InternalEndpointFlow m r)

sharedCabSessionExpiry :: ExpiryFlow m r c => Job 'SharedCabSessionExpiry -> m ExecutionResult
sharedCabSessionExpiry Job {jobInfo} = do
  let jobData = jobInfo.jobData
  ownExpiryChain jobData.merchantId jobData.merchantOperatingCityId jobData.chainId $ \chainId -> do
    claimedTick
      (claimExpiryRun jobData.merchantOperatingCityId)
      (expireSilentSessions jobData.merchantOperatingCityId >> watchChains jobData.merchantId jobData.merchantOperatingCityId)
      (scheduleNextExpiry jobData.merchantId jobData.merchantOperatingCityId chainId)
  pure Complete

-- | Re-seeds the allocation and sweep chains if their jobs were lost (see 'watchPlan'); a failure here never costs the expiry pass.
watchChains :: ExpiryFlow m r c => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
watchChains merchantId mocId =
  withTryCatch "sharedCab:watchChains" watch >>= \case
    Left err -> logError $ "sharedCab expiry: chain watchdog failed: " <> show err
    Right () -> pure ()
  where
    watch = do
      liveBookings <- length <$> Allocation.cityLiveBookings mocId
      liveTrips <- length <$> Session.cityLiveSessions mocId
      pendingRefunds <- length <$> RefundRetry.pendingRefundRetries mocId
      enabled <- sharedCabEnabled mocId
      let plan = watchPlan enabled liveBookings liveTrips pendingRefunds
      when plan.seedAllocation $ reseedAllocationTick merchantId mocId
      when plan.seedSweep $ ensureDegradedSweep merchantId mocId

expireSilentSessions :: ExpiryFlow m r c => Id DMOC.MerchantOperatingCity -> m ()
expireSilentSessions mocId = do
  cabs <- Session.cityLiveSessions mocId
  now <- getCurrentTime
  pings <- M.fromList . catMaybes <$> mapM (lastPings now) (nub $ map (.routeCode) cabs)
  forM_ cabs $ \s ->
    withTryCatch "sharedCab:expiry" (checkCab pings now s) >>= \case
      Left err -> logError $ "sharedCab expiry failed for " <> s.vehicleNumber <> ": " <> show err
      Right () -> pure ()

-- | plate → latest ping on the route, or Nothing if LTS couldn't be read.
lastPings :: (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) => UTCTime -> Text -> m (Maybe (Text, M.Map Text Ping))
lastPings now route =
  withTryCatch "sharedCab:trackVehicles" (Lts.readRoutePositions route) >>= \case
    Left err -> do
      logError $ "sharedCab expiry: LTS read failed for route " <> route <> ", skipping it: " <> show err
      pure Nothing
    Right vehicles -> pure $ Just (route, M.fromList [(v.vehicleNumber, readPing now (v.vehicleInfo.timestamp >>= parseLtsTimestamp)) | v <- vehicles])

-- | Leaving ACTIVE releases the unboarded allocations without penalty: the applied update does it, after the plate lock.
checkCab :: ExpiryFlow m r c => M.Map Text (M.Map Text Ping) -> UTCTime -> Session -> m ()
checkCab pings now s = whenJust (M.lookup s.routeCode pings) $ \routePings -> do
  let ping = M.lookup s.vehicleNumber routePings
  when (ping == Just Unreadable) $ logWarning $ "sharedCab expiry: unreadable or future LTS timestamp for " <> s.vehicleNumber
  whenJust (lastSeenFor endAfter now s.startedAt ping) $ \lastSeen ->
    case expiryAction pauseAfter endAfter now lastSeen s.status of
      Just PauseSilent -> void $ cabEvent s CAB_SILENT_PAUSE
      Just EndSilent -> void $ cabEvent s CAB_SILENT_END
      Nothing -> pure ()
