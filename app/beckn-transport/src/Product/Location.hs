module Product.Location where

import App.Types
import Beckn.Prelude
import qualified Beckn.Product.MapSearch.GraphHopper as MapSearch
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.MapSearch
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Logging
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import GHC.Records.Extra
import SharedLogic.LocationUpdates
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import Tools.Metrics
import Types.API.Location as Location
import Types.Storage.DriverLocation (DriverLocation)
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Ride as SRide
import Utils.Common hiding (id)

data Handler m = Handler
  { refreshPeriod :: NominalDiffTime,
    allowedDelay :: NominalDiffTime,
    findPersonById :: Id Person.Person -> m (Maybe Person.Person),
    findDriverLocationById :: Id Person.Person -> m (Maybe DriverLocation),
    upsertDriverLocation :: Id Person.Person -> LatLong -> UTCTime -> m (),
    getInProgressByDriverId :: Id Person.Person -> m (Maybe SRide.Ride),
    missingUpdatesForThisRide :: Id SRide.Ride -> m Bool,
    ignoreUpdatesForThisRide :: Id SRide.Ride -> m (),
    interpolationHandler :: RideInterpolationHandler m
  }

updateLocation :: Id Person.Person -> UpdateLocationReq -> FlowHandler APISuccess
updateLocation personId waypoints = withFlowHandlerAPI $ do
  handler <- constructHandler
  updateLocationHandler handler personId waypoints
  where
    constructHandler = do
      refreshPeriod <- fromIntegral <$> askConfig (.updateLocationRefreshPeriod)
      allowedDelay <- fromIntegral <$> askConfig (.updateLocationAllowedDelay)
      pure $
        Handler
          { refreshPeriod,
            allowedDelay,
            findPersonById = Person.findPersonById,
            findDriverLocationById = DrLoc.findById,
            upsertDriverLocation = \driverId point timestamp ->
              DB.runSqlDBTransaction $ DrLoc.upsertGpsCoord driverId point timestamp,
            getInProgressByDriverId = QRide.getInProgressByDriverId,
            missingUpdatesForThisRide = \rideId -> isJust <$> Redis.getKeyRedis @() (missingLocationUpdatesKey rideId),
            ignoreUpdatesForThisRide = \rideId -> Redis.setExRedis (missingLocationUpdatesKey rideId) () (60 * 60 * 24),
            interpolationHandler = defaultRideInterpolationHandler
          }

updateLocationHandler :: Handler Flow -> Id Person.Person -> UpdateLocationReq -> Flow APISuccess
updateLocationHandler Handler {..} driverId waypoints = withLogTag "driverLocationUpdate" $ do
  logInfo $ "got location updates: " <> getId driverId <> " " <> encodeToText waypoints
  driver <-
    findPersonById driverId
      >>= fromMaybeM PersonNotFound
  unless (driver.role == Person.DRIVER) $ throwError AccessDenied
  mbOldLoc <- findDriverLocationById driver.id
  let currPoint = NE.last waypoints
  upsertDriverLocation driver.id currPoint.pt currPoint.ts
  now <- getCurrentTime
  let calledBeforeRefreshPeriod =
        mbOldLoc <&> (\loc -> now `diffUTCTime` loc.updatedAt < refreshPeriod)
      mbLastWaypoint =
        mbOldLoc
          >>= ( \loc ->
                  if now `diffUTCTime` loc.updatedAt > refreshPeriod
                    then Just $ mkLastWaypoint loc
                    else Nothing
              )
      waypointsList = toList waypoints
      pointsForCheck = maybe waypoints (:| waypointsList) mbLastWaypoint

  if calledBeforeRefreshPeriod == Just True
    then logWarning "Called before refresh period passed, ignoring"
    else afterMissingUpdatesCheck pointsForCheck $ processWaypoints interpolationHandler driver.id $ NE.map (.pt) waypoints

  pure Success
  where
    mkLastWaypoint loc =
      Waypoint
        { pt = locationToLatLong loc,
          ts = loc.updatedAt,
          acc = Nothing
        }

    afterMissingUpdatesCheck waypoints' action = do
      getInProgressByDriverId driverId
        >>= maybe
          (logInfo "No ride is assigned to driver, ignoring")
          ( \ride -> do
              missingLocationUpdates <- missingUpdatesForThisRide ride.id
              let missingUpdates =
                    missingLocationUpdates
                      || checkWaypointsForMissingUpdates allowedDelay waypoints'
              if missingUpdates
                then do
                  logInfo $ "missing updates for driver " <> driverId.getId
                  ignoreUpdatesForThisRide ride.id
                else action
          )

missingLocationUpdatesKey :: Id SRide.Ride -> Text
missingLocationUpdatesKey (Id rideId) = "BPP:missingLocationUpdates:" <> rideId

checkWaypointsForMissingUpdates :: NominalDiffTime -> NE.NonEmpty Waypoint -> Bool
checkWaypointsForMissingUpdates allowedDelay wps =
  or $ zipWith (\a b -> b.ts `diffUTCTime` a.ts > allowedDelay) wpsList (tail wpsList)
  where
    wpsList = toList wps

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
    DrLoc.findById driver.id
      >>= fromMaybeM LocationNotFound
  let lastUpdate = currLocation.updatedAt
  let totalDistance = ride.traveledDistance
      currPoint = locationToLatLong currLocation
  return $ GetLocationRes {..}

locationToLatLong :: (HasField "lat" a Double, HasField "lon" a Double) => a -> MapSearch.LatLong
locationToLatLong loc =
  MapSearch.LatLong loc.lat loc.lon

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
