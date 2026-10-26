-- | How far a cab is, in seconds, from a stop it has not reached: the rider's assigned cab for the skip rule, the other cabs for the "upcoming cabs" list.
module SharedLogic.SharedCab.Approach
  ( etaToStopSec,
    plausibleEtaToStopSec,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.SharedCab.Allocation.Select as Select
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Lts as Lts
import qualified SharedLogic.SharedCab.Nearby as Nearby

type ApproachFlow m r = (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r)

readTrack :: ApproachFlow m r => Text -> Text -> m (Maybe LT.VehicleTrackingOnRouteResp)
readTrack routeCode plate =
  either (const Nothing) identity <$> withTryCatch "sharedCab:approach:readRoutePosition" (Lts.readRoutePosition routeCode plate)

secondsTo :: UTCTime -> LT.UpcomingStop -> Int
secondsTo now u = max 0 (floor (diffUTCTime u.eta now))

-- | LTS's own ETA for a cab with a fresh fix whose stop is still ahead of it; none otherwise. No plausibility check: the rider's assigned cab.
etaToStopSec :: ApproachFlow m r => UTCTime -> Config.SharedCabTunables -> Text -> Text -> Text -> m (Maybe Int)
etaToStopSec now cfg routeCode plate stopCode = do
  mbTrack <- readTrack routeCode plate
  pure $ do
    track <- mbTrack
    guard (Select.isFreshPosition now cfg.ltsMaxAgeSec track.vehicleInfo)
    u <- find (\s -> s.stop.stopCode == stopCode && s.status == LT.Upcoming) =<< track.vehicleInfo.upcomingStops
    pure (secondsTo now u)

-- | The same, only when the cab counts like an allocation candidate: its distance to the stop can back the ETA.
plausibleEtaToStopSec :: ApproachFlow m r => UTCTime -> Config.SharedCabTunables -> Text -> Text -> Text -> m (Maybe Int)
plausibleEtaToStopSec now cfg routeCode plate stopCode = do
  mbTrack <- readTrack routeCode plate
  pure $ do
    track <- mbTrack
    entry <- find (\s -> s.stop.stopCode == stopCode) =<< track.vehicleInfo.upcomingStops
    secondsTo now <$> Nearby.cabStopEta now cfg stopCode entry.stop.coordinate track
