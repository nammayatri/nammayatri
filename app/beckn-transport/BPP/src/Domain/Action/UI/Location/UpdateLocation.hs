module Domain.Action.UI.Location.UpdateLocation
  ( Handler (..),
    UpdateLocationReq,
    Waypoint (..),
    UpdateLocationRes,
    updateLocationHandler,
    processWaypoints,
  )
where

import App.Types
import Beckn.Prelude hiding (Handler)
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
  if calledBeforeRefreshPeriod == Just True
    then logWarning "Called before refresh period passed, ignoring"
    else
      getInProgressByDriverId driver.id
        >>= maybe
          (logInfo "No ride is assigned to driver, ignoring")
          (const $ processWaypoints interpolationHandler driver.id $ NE.map (.pt) waypoints)
  pure Success

processWaypoints ::
  (Monad m, Log m) =>
  RideInterpolationHandler m ->
  Id Person.Person ->
  NonEmpty LatLong ->
  m ()
processWaypoints ih@RideInterpolationHandler {..} driverId waypoints = do
  addPoints driverId waypoints
  let ending = False
  recalcDistanceBatches ih ending driverId
