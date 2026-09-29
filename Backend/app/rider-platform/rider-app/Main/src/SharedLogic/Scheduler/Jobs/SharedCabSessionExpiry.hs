-- | Silent shared-cab sessions: no fresh LTS ping for `pauseAfter` → PAUSED(NO_LOCATION); for `endAfter` → ENDED
-- (trip closed SESSION_TIMEOUT, rideEnd). A route whose LTS read fails is skipped, so an LTS outage pauses nobody.
module SharedLogic.Scheduler.Jobs.SharedCabSessionExpiry
  ( sharedCabSessionExpiry,
  )
where

import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.VehicleTrip as DVT
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified SharedLogic.External.LocationTrackingService.Flow as LTS
import SharedLogic.JobScheduler
import qualified SharedLogic.SharedCab.Allocation as Allocation
import SharedLogic.SharedCab.Allocation.Types (AllocationOutcome (SessionClosed), parseLtsTimestamp)
import qualified SharedLogic.SharedCab.Events as Events
import SharedLogic.SharedCab.ExpirySchedule (claimTick, scheduleNextExpiry)
import SharedLogic.SharedCab.LtsAttach (LtsFlow)
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState
import qualified Storage.Queries.VehicleTrip as QVT

-- rider_config.noLocationPauseMin / noLocationEndMin defaults; read from config once those fields exist.
pauseAfter :: NominalDiffTime
pauseAfter = 15 * 60

endAfter :: NominalDiffTime
endAfter = endSilentAfter

sharedCabSessionExpiry :: (LtsFlow m r c, Events.EventFlow m r, MonadMask m, JobCreator r m, Redis.HedisLTSFlowEnv r, ServiceFlow m r, Allocation.InternalEndpointFlow m r) => Job 'SharedCabSessionExpiry -> m ExecutionResult
sharedCabSessionExpiry Job {jobInfo} = do
  let jobData = jobInfo.jobData
  claimed <- claimTick jobData.merchantOperatingCityId
  if claimed
    then expireSilentSessions jobData.merchantOperatingCityId `finally` scheduleNextExpiry jobData.merchantId jobData.merchantOperatingCityId
    else logInfo "sharedCab expiry: another chain ran this tick; dropping this one"
  pure Complete

expireSilentSessions :: (LtsFlow m r c, Events.EventFlow m r, MonadMask m, JobCreator r m, Redis.HedisLTSFlowEnv r, ServiceFlow m r, Allocation.InternalEndpointFlow m r) => Id DMOC.MerchantOperatingCity -> m ()
expireSilentSessions mocId = do
  trips <- QVT.findAllLiveByMerchantOperatingCityId mocId
  now <- getCurrentTime
  pings <- M.fromList . catMaybes <$> mapM (lastPings now) (nub $ map (.routeCode) trips)
  forM_ trips $ \trip ->
    withTryCatch "sharedCab:expiry" (checkTrip pings now trip) >>= \case
      Left err -> logError $ "sharedCab expiry failed for " <> trip.vehicleNumber <> ": " <> show err
      Right () -> pure ()

-- | plate → latest ping on the route, or Nothing if LTS couldn't be read.
lastPings :: LtsFlow m r c => UTCTime -> Text -> m (Maybe (Text, M.Map Text Ping))
lastPings now route =
  withTryCatch "sharedCab:trackVehicles" (LTS.vehicleTrackingOnRoute (LTS.ByRoute route)) >>= \case
    Left err -> do
      logError $ "sharedCab expiry: LTS read failed for route " <> route <> ", skipping it: " <> show err
      pure Nothing
    Right vehicles -> pure $ Just (route, M.fromList [(v.vehicleNumber, readPing now (v.vehicleInfo.timestamp >>= parseLtsTimestamp)) | v <- vehicles])

checkTrip :: (LtsFlow m r c, Events.EventFlow m r, MonadMask m, JobCreator r m, Redis.HedisLTSFlowEnv r, ServiceFlow m r, Allocation.InternalEndpointFlow m r) => M.Map Text (M.Map Text Ping) -> UTCTime -> DVT.VehicleTrip -> m ()
checkTrip pings now trip = Session.getSession trip.vehicleNumber >>= traverse_ check
  where
    check s = whenJust (M.lookup s.routeCode pings) $ \routePings -> do
      let ping = M.lookup s.vehicleNumber routePings
      when (ping == Just Unreadable) $ logWarning $ "sharedCab expiry: unreadable or future LTS timestamp for " <> s.vehicleNumber
      whenJust (lastSeenFor endAfter now trip.startedAt ping) $ \lastSeen ->
        case expiryAction pauseAfter endAfter now lastSeen s.status of
          -- 05 §8.7: leaving ACTIVE releases unboarded allocations without penalty, after the plate lock is released
          Just PauseSilent -> Session.pause s.vehicleNumber NO_LOCATION >> Allocation.releaseUnboarded s.vehicleNumber SessionClosed
          Just EndSilent -> Session.expire s.vehicleNumber >> Allocation.releaseUnboarded s.vehicleNumber SessionClosed
          Nothing -> pure ()
