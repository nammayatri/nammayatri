-- | When cabs reach a stop: the Home "Share Taxis" card (routes through the rider's stop) and the pick-a-cab list.
module SharedLogic.SharedCab.Nearby
  ( cabStopEta,
    nextCabEtaMin,
    sortByEta,
  )
where

import Data.List (sort, sortOn)
import Data.Time (diffUTCTime)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.SharedCab.Allocation.Select as Select
import qualified SharedLogic.SharedCab.Config as Config

-- | Whole minutes (rounded up) until the soonest cab on the route reaches the stop. A cab counts like a candidate in the allocator: a fresh
-- position, the stop still Upcoming for it, and an ETA its distance can back (LTS gives a static ETA from the snapped point).
nextCabEtaMin :: UTCTime -> Config.SharedCabTunables -> Text -> LatLong -> [LT.VehicleTrackingOnRouteResp] -> Maybe Int
nextCabEtaMin now cfg stopCode stopPoint cabs = case sort [max 0 (floor (diffUTCTime u.eta now)) | Just u <- map (cabStopEta now cfg stopCode stopPoint) cabs] of
  [] -> Nothing
  soonest : _ -> Just (ceiling (fromIntegral (soonest :: Int) / 60 :: Double))

-- | The cab's entry for the stop when it counts as approaching it: a fresh fix, the stop still Upcoming, an ETA its distance can back.
cabStopEta :: UTCTime -> Config.SharedCabTunables -> Text -> LatLong -> LT.VehicleTrackingOnRouteResp -> Maybe LT.UpcomingStop
cabStopEta now cfg stopCode stopPoint cab = do
  let vi = cab.vehicleInfo
  guard (Select.isFreshPosition now cfg.ltsMaxAgeSec vi)
  u <- find (\s -> s.stop.stopCode == stopCode && s.status == LT.Upcoming) =<< vi.upcomingStops
  u <$ guard (Select.etaPlausible cfg (max 0 (floor (diffUTCTime u.eta now))) (LatLong vi.latitude vi.longitude) stopPoint)

-- | Soonest first, routes with no ETA last, otherwise the given order.
sortByEta :: [(Maybe Int, a)] -> [a]
sortByEta = map snd . sortOn (maybe (1 :: Int, 0) (\m -> (0, m)) . fst)
