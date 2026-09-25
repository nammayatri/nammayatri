-- | Silent shared-cab sessions: no fresh LTS ping for `pauseAfter` → PAUSED(NO_LOCATION); for `endAfter` → ENDED
-- (trip closed SESSION_TIMEOUT, rideEnd). A route whose LTS read fails is skipped, so an LTS outage pauses nobody.
module SharedLogic.Scheduler.Jobs.SharedCabSessionExpiry
  ( sharedCabSessionExpiry,
  )
where

import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.VehicleTrip as DVT
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified SharedLogic.External.LocationTrackingService.Flow as LTS
import SharedLogic.External.LocationTrackingService.Types (VehicleInfo)
import SharedLogic.JobScheduler
import SharedLogic.SharedCab.ExpirySchedule (claimTick, scheduleNextExpiry)
import SharedLogic.SharedCab.LtsAttach (LtsFlow)
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState
import qualified Storage.Queries.VehicleTrip as QVT

-- rider_config.noLocationPauseMin / noLocationEndMin defaults; read from config once those fields exist.
pauseAfter :: NominalDiffTime
pauseAfter = 15 * 60

endAfter :: NominalDiffTime
endAfter = 60 * 60

sharedCabSessionExpiry :: (LtsFlow m r c, MonadMask m, JobCreator r m) => Job 'SharedCabSessionExpiry -> m ExecutionResult
sharedCabSessionExpiry Job {jobInfo} = do
  let jobData = jobInfo.jobData
  claimed <- claimTick jobData.merchantOperatingCityId
  if claimed
    then do
      expireSilentSessions jobData.merchantOperatingCityId
      scheduleNextExpiry jobData.merchantId jobData.merchantOperatingCityId
    else logInfo "sharedCab expiry: another chain ran this tick; dropping this one"
  pure Complete

expireSilentSessions :: (LtsFlow m r c, MonadMask m) => Id DMOC.MerchantOperatingCity -> m ()
expireSilentSessions mocId = do
  trips <- QVT.findAllLiveByMerchantOperatingCityId mocId
  pings <- M.fromList . catMaybes <$> mapM lastPings (nub $ map (.routeCode) trips)
  now <- getCurrentTime
  forM_ trips $ \trip ->
    withTryCatch "sharedCab:expiry" (checkTrip pings now trip) >>= \case
      Left err -> logError $ "sharedCab expiry failed for " <> trip.vehicleNumber <> ": " <> show err
      Right () -> pure ()

-- | plate → latest ping on the route, or Nothing if LTS couldn't be read.
lastPings :: LtsFlow m r c => Text -> m (Maybe (Text, M.Map Text UTCTime))
lastPings route =
  withTryCatch "sharedCab:trackVehicles" (LTS.vehicleTrackingOnRoute (LTS.ByRoute route)) >>= \case
    Left err -> do
      logError $ "sharedCab expiry: LTS read failed for route " <> route <> ", skipping it: " <> show err
      pure Nothing
    Right vehicles -> pure $ Just (route, M.fromList [(v.vehicleNumber, ts) | v <- vehicles, Just ts <- [pingTime v.vehicleInfo]])

-- LTS serialises chrono DateTime<Utc> as RFC 3339.
pingTime :: VehicleInfo -> Maybe UTCTime
pingTime info = info.timestamp >>= iso8601ParseM . T.unpack

checkTrip :: (LtsFlow m r c, MonadMask m) => M.Map Text (M.Map Text UTCTime) -> UTCTime -> DVT.VehicleTrip -> m ()
checkTrip pings now trip = Session.getSession trip.vehicleNumber >>= traverse_ check
  where
    check s = whenJust (M.lookup s.routeCode pings) $ \routePings -> do
      let lastSeen = maybe trip.startedAt (max trip.startedAt) (M.lookup s.vehicleNumber routePings)
      case expiryAction pauseAfter endAfter now lastSeen s.status of
        Just PauseSilent -> void $ Session.pause s.vehicleNumber NO_LOCATION
        Just EndSilent -> void $ Session.expire s.vehicleNumber
        Nothing -> pure ()
