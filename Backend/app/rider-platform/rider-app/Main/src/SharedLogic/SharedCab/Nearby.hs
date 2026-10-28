-- | When cabs reach a stop: the Home "Share Taxis" card (routes through the rider's stop) and the pick-a-cab list.
module SharedLogic.SharedCab.Nearby
  ( cabStopEta,
  )
where

import Data.Time (diffUTCTime)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.SharedCab.Allocation.Select as Select
import qualified SharedLogic.SharedCab.Config as Config

-- | The cab's entry for the stop when it counts as approaching it: a fresh fix, the stop still Upcoming, an ETA its distance can back.
cabStopEta :: UTCTime -> Config.SharedCabTunables -> Text -> LatLong -> LT.VehicleTrackingOnRouteResp -> Maybe LT.UpcomingStop
cabStopEta now cfg stopCode stopPoint cab = do
  let vi = cab.vehicleInfo
  guard (Select.isFreshPosition now cfg.ltsMaxAgeSec vi)
  u <- find (\s -> s.stop.stopCode == stopCode && s.status == LT.Upcoming) =<< vi.upcomingStops
  u <$ guard (Select.etaPlausible cfg (max 0 (floor (diffUTCTime u.eta now))) (LatLong vi.latitude vi.longitude) stopPoint)
