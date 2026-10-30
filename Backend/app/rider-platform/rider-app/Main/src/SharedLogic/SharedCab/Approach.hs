-- | How far a cab is, in seconds, from a stop it has not reached (the rider's assigned cab for the skip rule, the other cabs for the "upcoming cabs" list), and when a cab counts as approaching a stop.
module SharedLogic.SharedCab.Approach
  ( etaToStopSec,
    plausibleEtaToStopSec,
    plausibleEtaFrom,
    cabStopEta,
  )
where

import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.SharedCab.Allocation.Select as Select
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Lts as Lts

type ApproachFlow m r = (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r)

secondsTo :: UTCTime -> LT.UpcomingStop -> Int
secondsTo now u = max 0 (floor (diffUTCTime u.eta now))

-- | LTS's own ETA for a cab with a fresh fix whose stop is still ahead of it; none otherwise. No plausibility check: the rider's assigned cab.
etaToStopSec :: ApproachFlow m r => UTCTime -> Config.SharedCabTunables -> Text -> Text -> Text -> m (Maybe Int)
etaToStopSec now cfg routeCode plate stopCode = do
  mbTrack <- Lts.readRoutePosition routeCode plate
  pure $ do
    track <- mbTrack
    guard (Select.isFreshPosition now cfg.ltsMaxAgeSec track.vehicleInfo)
    u <- find (\s -> s.stop.stopCode == stopCode && s.status == LT.Upcoming) =<< track.vehicleInfo.upcomingStops
    pure (secondsTo now u)

-- | The same, only when the cab counts like an allocation candidate: its distance to the stop can back the ETA.
plausibleEtaToStopSec :: ApproachFlow m r => UTCTime -> Config.SharedCabTunables -> Text -> Text -> Text -> m (Maybe Int)
plausibleEtaToStopSec now cfg routeCode plate stopCode = (>>= plausibleEtaFrom now cfg stopCode) <$> Lts.readRoutePosition routeCode plate

plausibleEtaFrom :: UTCTime -> Config.SharedCabTunables -> Text -> LT.VehicleTrackingOnRouteResp -> Maybe Int
plausibleEtaFrom now cfg stopCode track = do
  entry <- find (\s -> s.stop.stopCode == stopCode) =<< track.vehicleInfo.upcomingStops
  secondsTo now <$> cabStopEta now cfg stopCode entry.stop.coordinate track

-- | The cab's entry for the stop when it counts as approaching it: a fresh fix, the stop still Upcoming, an ETA its distance can back.
cabStopEta :: UTCTime -> Config.SharedCabTunables -> Text -> LatLong -> LT.VehicleTrackingOnRouteResp -> Maybe LT.UpcomingStop
cabStopEta now cfg stopCode stopPoint cab = do
  let vi = cab.vehicleInfo
  guard (Select.isFreshPosition now cfg.ltsMaxAgeSec vi)
  u <- find (\s -> s.stop.stopCode == stopCode && s.status == LT.Upcoming) =<< vi.upcomingStops
  u <$ guard (Select.etaPlausible cfg (max 0 (floor (diffUTCTime u.eta now))) (LatLong vi.latitude vi.longitude) stopPoint)
