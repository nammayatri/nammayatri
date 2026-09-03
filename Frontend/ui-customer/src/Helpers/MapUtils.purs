module MapUtils where

import Prelude

import Common.Types.App as CTA
import Data.Array (index, length, range, (!!), zip)
import Data.Foldable (minimumBy)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (atan2, cos, log, max, min, pi, pow, round, sin, sqrt, floor)
import Data.Ord (comparing)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import JBridge as JB
import Screens.Types (NearestWaypointConfig)
import Services.API as API

dummyWaypointConfig :: NearestWaypointConfig
dummyWaypointConfig = 
  { index: 0
  , deviationDistance: 0.0
  , vehicleLocationOnRoute: API.LatLong { lat: 0.0, lon: 0.0 }
  }

-- Vector Dot Product and Distance Ratio Projection Calculation
computeClosestPointOnEdgeOrPath :: API.LatLong -> API.LatLong -> API.LatLong -> Tuple Number API.LatLong
computeClosestPointOnEdgeOrPath (API.LatLong currentLatLon) (API.LatLong edge1) (API.LatLong edge2) =
  let
    -- Scaling longitude difference by cos(latitude) to get more accurate distance
    dLon = (edge2.lon - edge1.lon) * cos (toRadians ((edge1.lat + edge2.lat) / 2.0))
    dLat = edge2.lat - edge1.lat
    dLonC = (currentLatLon.lon - edge1.lon) * cos (toRadians ((edge1.lat + currentLatLon.lat) / 2.0))
    dLatC = currentLatLon.lat - edge1.lat
    
    dotProduct = dLonC * dLon + dLatC * dLat
    abSquared = dLon * dLon + dLat * dLat
    t = if abSquared == 0.0 then 0.0 else max 0.0 (min 1.0 (dotProduct / abSquared))

    -- Interpolating the closest point on the segment
    closestLat = edge1.lat + t * dLat
    closestLon = edge1.lon + t * (edge2.lon - edge1.lon)

    -- Computing distance using haversine formula
    closestPoint = API.LatLong { lat: closestLat, lon: closestLon }
    distance = haversineDistance (API.LatLong currentLatLon) closestPoint
  in Tuple distance closestPoint


calculateNearestWaypoint :: API.LatLong -> JB.Coordinates -> Number -> NearestWaypointConfig
calculateNearestWaypoint currentLatLon waypoints maxSnappingOnRouteDistance =
  go 0 1.0e9 currentLatLon (-1)
  where
    go :: Int -> Number -> API.LatLong -> Int -> NearestWaypointConfig
    go i minDist closest closestIndex =
      if i >= (length waypoints) - 1 then
        dummyWaypointConfig { index = closestIndex, deviationDistance = minDist, vehicleLocationOnRoute = if minDist < maxSnappingOnRouteDistance then closest else currentLatLon } 
      else
        case (index waypoints i), (index waypoints (i + 1)) of
          Just edge1, Just edge2 ->
            let Tuple distance closestPoint = computeClosestPointOnEdgeOrPath currentLatLon (convertPathToLatLong edge1) (convertPathToLatLong edge2)
            in if distance < minDist
                 then go (i + 1) distance closestPoint i
                 else go (i + 1) minDist closest closestIndex
          _, _ -> dummyWaypointConfig { index = closestIndex, deviationDistance = minDist, vehicleLocationOnRoute = closest } 

-- Haversine Distance Calculation
haversineDistance :: API.LatLong -> API.LatLong -> Number
haversineDistance (API.LatLong point1) (API.LatLong point2) = 
  let 
    lat1 = toRadians point1.lat
    lon1 = toRadians point1.lon
    lat2 = toRadians point2.lat
    lon2 = toRadians point2.lon
    dLat = toRadians (point2.lat - point1.lat)
    dLon = toRadians (point2.lon - point1.lon)
    a = sin (dLat / 2.0) * sin (dLat / 2.0) + cos lat1 * cos lat2 * sin (dLon / 2.0) * sin (dLon / 2.0)
    c = 2.0 * atan2 (sqrt a) (sqrt (1.0 - a))
  in
    earthRadiusKm * c
  where 
    toRadians :: Number -> Number
    toRadians degrees = degrees * pi / 180.0

    earthRadiusKm :: Number
    earthRadiusKm = 6371000.0  -- Convert to meters instead of km

convertPathToLatLong :: CTA.Paths -> API.LatLong
convertPathToLatLong path = API.LatLong { lat: path.lat, lon: path.lng }

toRadians :: Number -> Number
toRadians degrees = degrees * pi / 180.0

toDegrees :: Number -> Number
toDegrees rad = rad * 180.0 / pi

modulo :: Number -> Number -> Number
modulo x y = x - y * floor (x / y)

rotationBetweenLatLons :: API.LatLong -> API.LatLong -> Number
rotationBetweenLatLons (API.LatLong latLng1) (API.LatLong latLng2) =
  let lat1 = latLng1.lat * pi / 180.0
      long1 = latLng1.lon * pi / 180.0
      lat2 = latLng2.lat * pi / 180.0
      long2 = latLng2.lon * pi / 180.0
      dLon = long2 - long1
      y = sin dLon * cos lat2
      x = cos lat1 * sin lat2 - sin lat1 * cos lat2 * cos dLon
      brng = toDegrees (atan2 y x)
  in modulo (brng + 360.0) 360.0

getZoomLevel :: Number -> Number
getZoomLevel radius =
  let
    baseZoom = 16.0
  in
    if radius > 0.0 then
      let
        scale = radius / 500.0
        zoom = 16.0 - (log scale / log 2.0)
      in
        roundTo 2.0 zoom
    else
      baseZoom

roundTo :: Number -> Number -> Number
roundTo n num =
  let factor = pow 10.0 n
      rounded = round (num * factor)
  in rounded / factor

calculateNearestIndex :: CTA.Paths -> JB.Coordinates -> Int
calculateNearestIndex point route =
  let
    withIdx = zip route (range 0 (length route - 1))
    compareIdx (Tuple p1 _) (Tuple p2 _) =
      comparing (\p -> haversineDistance (convertPathToLatLong point) (API.LatLong { lat: p.lat, lon: p.lng })) p1 p2
  in
    fromMaybe 0 (snd <$> minimumBy compareIdx withIdx)

calculateVehicleBearingViaRoute :: API.LatLong -> JB.Coordinates -> Maybe Number
calculateVehicleBearingViaRoute (API.LatLong vehicle) route =
  let
    current = { lat: vehicle.lat, lng: vehicle.lon }
    idx = calculateNearestIndex current route
    nextIdx = if idx < length route - 1 then idx + 1 else idx - 1
    mbStartPoint = convertPathToLatLong <$> route !! idx
    mbEndPoint = convertPathToLatLong <$> route !! nextIdx
  in
    case mbStartPoint, mbEndPoint of
      Just startPoint, Just endPoint -> Just $ rotationBetweenLatLons startPoint endPoint
      _, _ -> Nothing

-- Deprecated this logic of calculating nearest waypoint using only haversine distance, can be used in some other cases
-- calculateNearestWaypoint :: API.LatLong -> JB.Coordinates -> Number -> Maybe API.LatLong
-- calculateNearestWaypoint currentWaypoint waypoints maxAllowedDeviationInMeters = 
--   maybe Nothing
--     (\minimumDistance -> do
--       let mbNearestWaypoint = head (filter (\waypoint -> haversineDistance currentWaypoint waypoint == minimumDistance) waypoints)
--       case mbNearestWaypoint, (minimumDistance < maxAllowedDeviationInMeters) of
--         Just nearestWaypoint, true -> Just $ API.LatLong {lat : nearestWaypoint.lat, lon : nearestWaypoint.lng}
--         _, _ -> Nothing)
--     (minimum distances)
--   where
--     distances :: Array Number
--     distances = map (\waypoint -> haversineDistance currentWaypoint waypoint) waypoints