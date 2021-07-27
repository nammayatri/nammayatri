module Product.Location where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Data.List.NonEmpty as NE
import Data.Time (diffUTCTime)
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import qualified Storage.Queries.DriverLocation as DriverLocation
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as QPI
import Types.API.Location as Location
import Types.Metrics
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as PI
import Utils.Common hiding (id)
import qualified Storage.Queries.DriverLocation as DrLoc

updateLocation :: Id Person.Person -> UpdateLocationReq -> FlowHandler APISuccess
updateLocation personId req = withFlowHandlerAPI $ do
  driver <-
    Person.findPersonById personId
      >>= fromMaybeM PersonNotFound
  unless (driver.role == Person.DRIVER) $ throwError AccessDenied
  locationId <- driver.locationId & fromMaybeM (PersonFieldNotPresent "location_id")
  loc <-
    DrLoc.findById locationId
      >>= fromMaybeM LocationNotFound
  now <- getCurrentTime
  refreshPeriod <- asks (.updateLocationRefreshPeriod) <&> fromIntegral
  if now `diffUTCTime` loc.updatedAt > refreshPeriod
    then do
      let lastWaypoint = locationToLatLong loc
      let traversedWaypoints = maybe waypointList (: waypointList) lastWaypoint
      distanceMb <- MapSearch.getDistanceMb (Just MapSearch.CAR) traversedWaypoints
      whenNothing_ distanceMb $ logWarning "Can't calculate distance when updating location"
      let lastUpdate = fromMaybe now (req.lastUpdate)
      DB.runSqlDBTransaction $ do
        whenJust distanceMb $ QPI.updateDistance driver.id
        DrLoc.updateGpsCoord locationId lastUpdate currPoint
      logTagInfo "driverLocationUpdate" (getId personId <> " " <> show req.waypoints)
    else logWarning "UpdateLocation called before refresh period passed, ignoring"
  return Success
  where
    currPoint = NE.last (req.waypoints)
    waypointList = NE.toList (req.waypoints)

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
    driver.locationId
      & fromMaybeM (PersonFieldNotPresent "location_id")
      >>= DriverLocation.findById
      >>= fromMaybeM LocationNotFound
  let lastUpdate = currLocation.updatedAt
  let totalDistance = ride.distance
  currPoint <- locationToLatLong currLocation & fromMaybeM (LocationFieldNotPresent "lat or long")
  return $ GetLocationRes {..}

type HasLongLat l =
  ( HasField "lat" l (Maybe Double),
    HasField "long" l (Maybe Double)
  )

locationToLatLong :: HasLongLat l => l -> Maybe MapSearch.LatLong
locationToLatLong loc =
  MapSearch.LatLong
    <$> loc.lat
    <*> loc.long

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
  ( HasLongLat l,
    HasLongLat l1,
    CoreMetrics m,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl]
  ) =>
  l ->
  l1 ->
  m (Maybe Double)
calculateDistance sourceLoc destinationLoc = do
  source <- locationToLatLong sourceLoc & fromMaybeM (LocationFieldNotPresent "lat or long source")
  dest <- locationToLatLong destinationLoc & fromMaybeM (LocationFieldNotPresent "lat or long destination")
  MapSearch.getDistanceMb (Just MapSearch.CAR) [source, dest]
