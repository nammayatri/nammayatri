module MapUtils where

import Prelude

import Data.Array (head, filter)
import Data.Foldable (minimum)
import Data.Maybe (Maybe(..), maybe)
import Data.Number (pi, sin, cos, sqrt, asin, abs, atan2)
import Screens.Types as ST
import Services.API as API

-- Haversine Distance Calculation
haversineDistance :: API.LatLong -> API.LatLong -> Number
haversineDistance (API.LatLong point1) (API.LatLong point2) = 
  let 
    lat1 = toRadians point1.lat
    lon1 = toRadians point1.lon
    lat2 = toRadians point2.lat
    lon2 = toRadians point2.lon
    dLat = toRadians (lat2 - lat1)
    dLon = toRadians (lon2 - lon1)
    absDistance = sin (dLat / 2.0) * sin (dLat / 2.0) + (cos lat1) * (cos lat2) * sin (dLon / 2.0) * sin (dLon / 2.0)
    finalDistance = 2.0 * atan2 (sqrt absDistance) (sqrt (1.0 - absDistance))
  in
    earthRadiusKm * finalDistance
  where 
    toRadians :: Number -> Number
    toRadians degrees = degrees * pi / 180.0

    earthRadiusKm :: Number
    earthRadiusKm = 6371.0

-- Snapping the Lat Long on the nearest waypoints
calculateNearestWaypoint :: API.LatLong -> Array API.LatLong -> Number -> Maybe API.LatLong
calculateNearestWaypoint currentWaypoint waypoints maxAllowedDeviationInMeters = 
  maybe Nothing
    (\minimumDistance -> 
      let nearestWaypoint = head (filter (\waypoint -> haversineDistance currentWaypoint waypoint == minimumDistance) waypoints)
      in if (minimumDistance > maxAllowedDeviationInMeters) 
            then Nothing
            else nearestWaypoint)
    (minimum distances)
  where
    distances :: Array Number
    distances = map (\waypoint -> haversineDistance currentWaypoint waypoint) waypoints