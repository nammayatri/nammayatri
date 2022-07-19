module Domain.Action.UI.Location.UpdateLocation
  ( Handler (..),
    UpdateLocationReq,
    Waypoint (..),
    UpdateLocationRes,
    updateLocationHandler,
  )
where

import App.Types
import Beckn.Prelude
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Beckn.Utils.GenericPretty (PrettyShow)
import Beckn.Utils.Logging
import qualified Data.List.NonEmpty as NE
import Domain.Types.DriverLocation (DriverLocation)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import GHC.Records.Extra
import SharedLogic.LocationUpdates
import Utils.Common hiding (id)

type MonadHandler m = (MonadThrow m, Log m, MonadGuid m, MonadTime m)

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

type UpdateLocationReq = NonEmpty Waypoint

-- Short field names for lesser json array size:
data Waypoint = Waypoint
  { pt :: LatLong, -- point
    ts :: UTCTime, -- timestamp
    acc :: Maybe Double -- accuracy, optional for now
  }
  deriving (Generic, ToJSON, Show, FromJSON, ToSchema, PrettyShow)

type UpdateLocationRes = APISuccess

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
        { pt = GoogleMaps.getCoordinates loc,
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

checkWaypointsForMissingUpdates :: NominalDiffTime -> NE.NonEmpty Waypoint -> Bool
checkWaypointsForMissingUpdates allowedDelay wps =
  or $ zipWith (\a b -> b.ts `diffUTCTime` a.ts > allowedDelay) wpsList (tail wpsList)
  where
    wpsList = toList wps
