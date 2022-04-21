module Product.Location where

import App.Types
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Prelude
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.MapSearch
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Logging
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Domain.Types.DriverLocation (DriverLocation)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import GHC.Records.Extra
import SharedLogic.LocationUpdates
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import Tools.Metrics
import Types.API.Location as Location
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
      refreshPeriod <- fromIntegral <$> asks (.updateLocationRefreshPeriod)
      allowedDelay <- fromIntegral <$> asks (.updateLocationAllowedDelay)
      pure $
        Handler
          { refreshPeriod,
            allowedDelay,
            findPersonById = Person.findById,
            findDriverLocationById = DrLoc.findById,
            upsertDriverLocation = \driverId point timestamp ->
              Esq.runTransaction $ DrLoc.upsertGpsCoord driverId point timestamp,
            getInProgressByDriverId = QRide.getInProgressByDriverId,
            missingUpdatesForThisRide = \rideId -> isJust <$> Redis.getKeyRedis @() (missingLocationUpdatesKey rideId),
            ignoreUpdatesForThisRide = \rideId -> Redis.setExRedis (missingLocationUpdatesKey rideId) () (60 * 60 * 24),
            interpolationHandler = defaultRideInterpolationHandler
          }

updateLocationHandler :: Handler Flow -> Id Person.Person -> UpdateLocationReq -> Flow APISuccess
updateLocationHandler Handler {..} driverId waypoints = withLogTag "driverLocationUpdate" $ do
  logInfo $ "got location updates: " <> getId driverId <> " " <> encodeToText waypoints
  driver <-
    Person.findById driverId
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
      & Person.findById
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

getRoute :: Id Person.Person -> Location.Request -> FlowHandler GoogleMaps.DirectionsResp
getRoute personId = withFlowHandlerAPI . withPersonIdLogTag personId . GoogleMaps.getRoutes

getDistanceDuration ::
  ( MonadFlow m,
    CoreMetrics m,
    GoogleMaps.HasGoogleMaps m r c
  ) =>
  [MapSearch.LatLong] ->
  m (Maybe Double, Maybe Integer)
getDistanceDuration fromTo = do
  let rec =
        MapSearch.Request
          { waypoints = fromTo,
            mode = Just CAR,
            calcPoints = True
          }
  routes <- GoogleMaps.getRoutes rec
  case routes.routes of
    [] -> return (Nothing, Nothing)
    (x : _) ->
      case x.legs of
        [] -> return (Nothing, Nothing)
        (y : _) -> do
          let dist :: Double = fromIntegral y.distance.value
              dur :: Integer = fromIntegral y.duration.value
          return (Just dist, Just dur)

calculateDistance ::
  ( MonadFlow m,
    CoreMetrics m,
    GoogleMaps.HasGoogleMaps m r c
  ) =>
  LatLong ->
  LatLong ->
  m (Maybe Double)
calculateDistance sourceLoc destinationLoc =
  getDistanceDuration [sourceLoc, destinationLoc] <&> fst

-- calculateDistance ::
--   ( CoreMetrics m,
--     HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl]
--   ) =>
--   LatLong ->
--   LatLong ->
--   m (Maybe Double)
-- calculateDistance sourceLoc destinationLoc = do
--   MapSearch.getDistanceMb (Just MapSearch.CAR) [sourceLoc, destinationLoc]
