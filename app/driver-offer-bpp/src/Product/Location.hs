module Product.Location where

import Beckn.Prelude hiding (Handler)
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.MapSearch
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Logging
import qualified Data.List.NonEmpty as NE
import Domain.Types.DriverLocation (DriverLocation)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import Environment
import GHC.Records.Extra
import qualified Lib.LocationUpdates as LocUpd
import SharedLogic.LocationUpdates
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import Types.API.Location as Location
import Utils.Common hiding (id)

type MonadHandler m = (MonadThrow m, Log m, MonadGuid m, MonadTime m)

data Handler m = Handler
  { refreshPeriod :: NominalDiffTime,
    allowedDelay :: NominalDiffTime,
    findPersonById :: Id Person.Person -> m (Maybe Person.Person),
    findDriverLocationById :: Id Person.Person -> m (Maybe DriverLocation),
    upsertDriverLocation :: Id Person.Person -> LatLong -> UTCTime -> m (),
    getInProgressByDriverId :: Id Person.Person -> m (Maybe SRide.Ride),
    interpolationHandler :: LocUpd.RideInterpolationHandler Person.Person m
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
            interpolationHandler = defaultRideInterpolationHandler
          }

updateLocationHandler :: MonadHandler m => Handler m -> Id Person.Person -> UpdateLocationReq -> m UpdateLocationRes
updateLocationHandler Handler {..} driverId waypoints = withLogTag "driverLocationUpdate" $ do
  logInfo $ "got location updates: " <> getId driverId <> " " <> encodeToText waypoints
  driver <-
    findPersonById driverId
      >>= fromMaybeM (PersonNotFound driverId.getId)
  unless (driver.role == Person.DRIVER) $ throwError AccessDenied
  mbOldLoc <- findDriverLocationById driver.id
  let currPoint = NE.last waypoints
  upsertDriverLocation driver.id currPoint.pt currPoint.ts
  now <- getCurrentTime
  let calledBeforeRefreshPeriod =
        mbOldLoc <&> (\loc -> now `diffUTCTime` loc.updatedAt < refreshPeriod)
  if calledBeforeRefreshPeriod == Just True
    then logWarning "Called before refresh period passed, ignoring"
    else
      getInProgressByDriverId driver.id
        >>= maybe
          (logInfo "No ride is assigned to driver, ignoring")
          (const $ LocUpd.updateIntermediateRideLocation interpolationHandler driver.id $ NE.map (.pt) waypoints)
  pure Success

getLocation :: Id SRide.Ride -> FlowHandler GetLocationRes
getLocation rideId = withFlowHandlerAPI $ do
  ride <-
    QRide.findById rideId
      >>= fromMaybeM (RideDoesNotExist rideId.getId)
  status <-
    case ride.status of
      SRide.NEW -> pure PreRide
      SRide.INPROGRESS -> pure ActualRide
      _ -> throwError $ RideInvalidStatus "Cannot track this ride"
  driver <-
    ride.driverId
      & Person.findById
      >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  currLocation <-
    DrLoc.findById driver.id
      >>= fromMaybeM LocationNotFound
  let lastUpdate = currLocation.updatedAt
  let totalDistance = ride.traveledDistance.getHighPrecMeters
      currPoint = locationToLatLong currLocation
  return $ GetLocationRes {..}

locationToLatLong :: (HasField "lat" a Double, HasField "lon" a Double) => a -> MapSearch.LatLong
locationToLatLong loc =
  MapSearch.LatLong loc.lat loc.lon

getRoute :: Id Person.Person -> Location.Request -> FlowHandler Location.Response
getRoute personId = withFlowHandlerAPI . withPersonIdLogTag personId . GoogleMaps.getRoutes
