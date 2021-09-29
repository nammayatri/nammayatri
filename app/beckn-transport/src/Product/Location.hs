module Product.Location where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Data.List.NonEmpty as NE
import Data.Time (diffUTCTime)
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.DriverLocation as DriverLocation
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import Types.API.Location as Location
import Types.Metrics
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Ride as SRide
import Utils.Common hiding (id)
import Prelude (atan2)

updateLocation :: Id Person.Person -> UpdateLocationReq -> FlowHandler APISuccess
updateLocation personId req = withFlowHandlerAPI $ do
  driver <-
    Person.findPersonById personId
      >>= fromMaybeM PersonNotFound
  unless (driver.role == Person.DRIVER) $ throwError AccessDenied
  let driverId = driver.id
  mbLoc <- DrLoc.findById driverId
  now <- getCurrentTime
  refreshPeriod <- asks (.updateLocationRefreshPeriod) <&> fromIntegral
  distanceMb <- case mbLoc of
    Just loc ->
      if now `diffUTCTime` loc.updatedAt > refreshPeriod
        then
          let lastWaypoint = locationToLatLong loc
              traversedWaypoints = lastWaypoint : waypointList
           in pure . Just $ getRouteLinearLength traversedWaypoints
        else logWarning "UpdateLocation called before refresh period passed, ignoring" $> Nothing
    Nothing -> pure . Just $ getRouteLinearLength waypointList
  let lastUpdate = fromMaybe now (req.lastUpdate)
  DB.runSqlDBTransaction $ do
    whenJust distanceMb $ QRide.updateDistance driver.id
    DrLoc.upsertGpsCoord driverId currPoint lastUpdate
  logTagInfo "driverLocationUpdate" (getId personId <> " " <> show req.waypoints)
  return Success
  where
    currPoint = NE.last (req.waypoints)
    waypointList = NE.toList (req.waypoints)

getLocation :: Id SRide.Ride -> FlowHandler GetLocationRes
getLocation rideId = withFlowHandlerAPI $ do
  ride <-
    QRide.findById rideId
      >>= fromMaybeM RideDoesNotExist
  status <-
    case ride.status of
      SRide.NEW -> pure PreRide
      SRide.INPROGRESS -> pure ActualRide
      _ -> throwError $ RideInvalidStatus "Cannot track this ride"
  driver <-
    ride.driverId
      & Person.findPersonById
      >>= fromMaybeM PersonNotFound
  currLocation <-
    DriverLocation.findById driver.id
      >>= fromMaybeM LocationNotFound
  let lastUpdate = currLocation.updatedAt
  let totalDistance = ride.finalDistance
      currPoint = locationToLatLong currLocation
  return $ GetLocationRes {..}

locationToLatLong :: (HasField "lat" a Double, HasField "long" a Double) => a -> MapSearch.LatLong
locationToLatLong loc =
  MapSearch.LatLong loc.lat loc.long

getRoute' ::
  ( CoreMetrics m,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl]
  ) =>
  [MapSearch.LatLong] ->
  m (Maybe MapSearch.Route)
getRoute' = MapSearch.getRouteMb (Just MapSearch.CAR)

getRoutes :: Id Person.Person -> Location.Request -> FlowHandler Location.Response
getRoutes _ = withFlowHandlerAPI . MapSearch.getRoutes

calculateDistance ::
  ( CoreMetrics m,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl]
  ) =>
  LatLong ->
  LatLong ->
  m (Maybe Double)
calculateDistance sourceLoc destinationLoc = do
  MapSearch.getDistanceMb (Just MapSearch.CAR) [sourceLoc, destinationLoc]

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
