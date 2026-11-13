-- | How far a cab is, in seconds, from a stop it has not reached (the rider's assigned cab for the skip rule, the other cabs for the "upcoming cabs" list), and when a cab counts as approaching a stop.
module SharedLogic.SharedCab.Approach
  ( etaToStopSec,
    plausibleEtaFrom,
    cabStopEta,
  )
where

import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.SharedCab.Allocation.Select as Select
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Lts as Lts
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB

type ApproachFlow m r = (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, CacheFlow m r, EsqDBFlow m r)

secondsTo :: UTCTime -> UTCTime -> Int
secondsTo now eta = max 0 (floor (diffUTCTime eta now))

-- | gps-processor's ETA for a cab with a fresh fix whose stop is still ahead of it; none otherwise. No plausibility check: the rider's assigned cab.
etaToStopSec :: ApproachFlow m r => UTCTime -> Config.SharedCabTunables -> Id DIBC.IntegratedBPPConfig -> Text -> Text -> Text -> m (Maybe Int)
etaToStopSec now cfg integratedBppConfigId routeCode plate stopCode = do
  mbTrack <- Lts.readRoutePosition integratedBppConfigId routeCode plate
  pure $ do
    cab <- mbTrack
    guard (Select.isFreshPosition now cfg.ltsMaxAgeSec cab)
    secondsTo now <$> Lts.etaAt stopCode cab

-- | The same, only when the cab counts like an allocation candidate: its distance to the stop can back the ETA.
plausibleEtaFrom :: UTCTime -> Config.SharedCabTunables -> Text -> LatLong -> Lts.CabTrack -> Maybe Int
plausibleEtaFrom now cfg stopCode stopPoint cab = secondsTo now . (.arrivalTime) <$> cabStopEta now cfg stopCode stopPoint cab

-- | The cab's entry for the stop (arrival in UTC) when it counts as approaching it: a fresh fix, the stop still ahead, an ETA its
-- distance can back.
cabStopEta :: UTCTime -> Config.SharedCabTunables -> Text -> LatLong -> Lts.CabTrack -> Maybe CQMMB.BusStopETA
cabStopEta now cfg stopCode stopPoint cab = do
  guard (Select.isFreshPosition now cfg.ltsMaxAgeSec cab)
  e <- Lts.stopEta now <$> find ((== stopCode) . (.stopCode)) cab.etas
  e <$ guard (Select.etaPlausible cfg (secondsTo now e.arrivalTime) cab.position stopPoint)
