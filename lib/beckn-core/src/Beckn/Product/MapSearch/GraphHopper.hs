module Beckn.Product.MapSearch.GraphHopper
  ( getRoutes,
    getRouteMb,
    getDistanceMb,
  )
where

import Beckn.External.Graphhopper.Flow as Grphr
import Beckn.External.Graphhopper.Types as Grphr
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common hiding (id)
import Beckn.Types.Error
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Common hiding (id)
import Data.Either.Combinators (mapBoth)
import Data.Geospatial hiding (bbox)
import EulerHS.Prelude

getRoutes' ::
  ( CoreMetrics m,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl]
  ) =>
  MapSearch.Request ->
  m (Either GraphHopperError [MapSearch.Route])
getRoutes' MapSearch.Request {..} = do
  -- Currently integrated only with graphhopper
  let mode' = fromMaybe MapSearch.CAR mode
      vehicle = mapToVehicle mode'
  graphhopperUrl <- asks (.graphhopperUrl)
  let toError = GraphHopperError (Just "UNABLE_TO_GET_ROUTE") graphhopperUrl
  Grphr.search graphhopperUrl (grphrReq waypoints vehicle)
    <&> toError `mapBoth` (map mapToRoute . Grphr.paths)
  where
    grphrReq points vehicle =
      Grphr.Request
        { points = latLongToGeoPoint <$> points,
          vehicle = vehicle,
          weighting = Nothing,
          elevation = Nothing,
          calc_points = Just calcPoints,
          points_encoded = False
        }

getRoutes ::
  ( CoreMetrics m,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl]
  ) =>
  MapSearch.Request ->
  m [MapSearch.Route]
getRoutes = getRoutes' >=> fromEitherM id

getRouteMb' ::
  ( CoreMetrics m,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl]
  ) =>
  MapSearch.Request ->
  m (Maybe MapSearch.Route)
getRouteMb' req =
  getRoutes' req
    >>= either (\e -> logError (makeLogSomeException $ toException e) $> Nothing) (pure . Just)
    <&> (>>= listToMaybe)

getRouteMb ::
  ( CoreMetrics m,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl]
  ) =>
  Maybe MapSearch.TravelMode ->
  [MapSearch.LatLong] ->
  m (Maybe MapSearch.Route)
getRouteMb mode waypoints = getRouteMb' MapSearch.Request {calcPoints = True, ..}

getDistanceMb ::
  ( CoreMetrics m,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl]
  ) =>
  Maybe MapSearch.TravelMode ->
  [MapSearch.LatLong] ->
  m (Maybe Double)
getDistanceMb mode waypoints =
  fmap (.distanceInM)
    <$> getRouteMb' MapSearch.Request {calcPoints = False, ..}

latLongToGeoPoint :: MapSearch.LatLong -> GeoPositionWithoutCRS
latLongToGeoPoint (MapSearch.LatLong lat lon) = GeoPointXY (PointXY lon lat)

mapToVehicle :: MapSearch.TravelMode -> Grphr.Vehicle
mapToVehicle MapSearch.CAR = Grphr.CAR
mapToVehicle MapSearch.MOTORCYCLE = Grphr.SCOOTER
mapToVehicle MapSearch.BICYCLE = Grphr.BIKE
mapToVehicle MapSearch.FOOT = Grphr.FOOT

mapToRoute :: Grphr.Path -> MapSearch.Route
mapToRoute Grphr.Path {..} =
  MapSearch.Route
    { distanceInM = distance,
      durationInS = div time 1000,
      boundingBox = bbox,
      snappedWaypoints = snapped_waypoints,
      points = points
    }
