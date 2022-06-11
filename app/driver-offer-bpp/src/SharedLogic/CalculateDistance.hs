module SharedLogic.CalculateDistance where

import Beckn.Prelude (atan2)
import Beckn.Types.MapSearch
import EulerHS.Prelude hiding (id, state)

distanceBetweenInMeters :: LatLong -> LatLong -> Double
distanceBetweenInMeters (LatLong lat1 lon1) (LatLong lat2 lon2) =
  -- Calculating using haversine formula
  let r = 6371000 -- Radius of earth in meters
      dlat = deg2Rad $ lat2 - lat1
      dlon = deg2Rad $ lon2 - lon1
      rlat1 = deg2Rad lat1
      rlat2 = deg2Rad lat2
      sq x = x * x
      -- Calculated distance is real (not imaginary) when 0 <= h <= 1
      -- Ideally in our use case h wouldn't go out of bounds
      h = sq (sin (dlat / 2)) + cos rlat1 * cos rlat2 * sq (sin (dlon / 2))
   in 2 * r * atan2 (sqrt h) (sqrt (1 - h))

deg2Rad :: Double -> Double
deg2Rad degree = degree * pi / 180

getRouteLinearLength :: [LatLong] -> Double
getRouteLinearLength pts@(_ : t) = sum $ zipWith distanceBetweenInMeters pts t
getRouteLinearLength _ = 0
