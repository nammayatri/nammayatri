module Product.Location where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong)
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Data.List.NonEmpty as NE
import Data.Time (diffUTCTime)
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.DriverLocation as DriverLocation
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as QPI
import Types.API.Location as Location
import Types.Metrics
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as PI
import Utils.Common hiding (id)

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
        then do
          let lastWaypoint = locationToLatLong loc
          let traversedWaypoints = lastWaypoint : waypointList
          getDistanceMb traversedWaypoints
        else do
          logWarning "UpdateLocation called before refresh period passed, ignoring"
          return Nothing
    Nothing -> getDistanceMb waypointList
  let lastUpdate = fromMaybe now (req.lastUpdate)
  DB.runSqlDBTransaction $ do
    whenJust distanceMb $ QPI.updateDistance driver.id
    DrLoc.upsertGpsCoord driverId currPoint lastUpdate
  logTagInfo "driverLocationUpdate" (getId personId <> " " <> show req.waypoints)
  return Success
  where
    currPoint = NE.last (req.waypoints)
    waypointList = NE.toList (req.waypoints)
    getDistanceMb waypoints = do
      res <- MapSearch.getDistanceMb (Just MapSearch.CAR) waypoints
      whenNothing_ res $ logWarning "Can't calculate distance when updating location"
      return res

getLocation :: Id PI.ProductInstance -> FlowHandler GetLocationRes
getLocation piId = withFlowHandlerAPI $ do
  ride <-
    QPI.findByParentIdType piId Case.RIDEORDER
      >>= fromMaybeM PIDoesNotExist
  status <-
    case ride.status of
      PI.TRIP_ASSIGNED -> pure PreRide
      PI.INPROGRESS -> pure ActualRide
      _ -> throwError $ PIInvalidStatus "Cannot track this ride"
  driver <-
    (ride.personId & fromMaybeM (PIFieldNotPresent "person_id"))
      >>= Person.findPersonById
      >>= fromMaybeM PersonNotFound
  currLocation <-
    DriverLocation.findById driver.id
      >>= fromMaybeM LocationNotFound
  let lastUpdate = currLocation.updatedAt
  let totalDistance = ride.distance
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
