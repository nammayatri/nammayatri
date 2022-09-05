module Domain.Action.UI.Location.UpdateLocation
  ( UpdateLocationReq,
    Waypoint (..),
    UpdateLocationRes,
    UpdateLocationHandler (..),
    updateLocation,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Beckn.Utils.Common hiding (id)
import Beckn.Utils.GenericPretty (PrettyShow)
import Beckn.Utils.Logging
import qualified Data.List.NonEmpty as NE
import Domain.Types.DriverLocation (DriverLocation)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import GHC.Records.Extra
import SharedLogic.LocationUpdates

type UpdateLocationReq = NonEmpty Waypoint

-- Short field names for lesser json array size:
data Waypoint = Waypoint
  { pt :: LatLong, -- point
    ts :: UTCTime, -- timestamp
    acc :: Maybe Double -- accuracy, optional for now
  }
  deriving (Generic, ToJSON, Show, FromJSON, ToSchema, PrettyShow)

type UpdateLocationRes = APISuccess

data UpdateLocationHandler m = UpdateLocationHandler
  { refreshPeriod :: NominalDiffTime,
    allowedDelay :: NominalDiffTime,
    findPersonById :: Id Person.Person -> m (Maybe Person.Person),
    findDriverLocationById :: Id Person.Person -> m (Maybe DriverLocation),
    upsertDriverLocation :: Id Person.Person -> LatLong -> UTCTime -> m (),
    getInProgressByDriverId :: Id Person.Person -> m (Maybe SRide.Ride),
    interpolationHandler :: RideInterpolationHandler m
  }

updateLocation :: (Log m, MonadFlow m, MonadThrow m, MonadTime m) => UpdateLocationHandler m -> Id Person.Person -> UpdateLocationReq -> m APISuccess
updateLocation UpdateLocationHandler {..} driverId waypoints = withLogTag "driverLocationUpdate" $ do
  logInfo $ "got location updates: " <> getId driverId <> " " <> encodeToText waypoints
  driver <-
    findPersonById driverId
      >>= fromMaybeM (PersonNotFound driverId.getId)
  unless (driver.role == Person.DRIVER) $ throwError AccessDenied
  whenM (Redis.tryLockRedis lockKey 60) $ do
    mbOldLoc <- findDriverLocationById driver.id
    now <- getCurrentTime
    case (isCalledBeforeRefreshPeriod mbOldLoc now, areIncomingPointsOutdated mbOldLoc) of
      (True, _) -> logWarning "Called before refresh period passed, ignoring"
      (_, True) -> logWarning "Incoming points are older than current one, ignoring"
      _ -> do
        upsertDriverLocation driver.id currPoint.pt currPoint.ts
        getInProgressByDriverId driver.id
          >>= maybe
            (logInfo "No ride is assigned to driver, ignoring")
            (const $ processWaypoints interpolationHandler driver.id $ NE.map (.pt) waypoints)
    Redis.unlockRedis lockKey
  pure Success
  where
    isCalledBeforeRefreshPeriod mbLoc now =
      maybe False (\loc -> now `diffUTCTime` loc.updatedAt < refreshPeriod) mbLoc
    currPoint = NE.last waypoints
    areIncomingPointsOutdated = maybe False (\loc -> currPoint.ts <= loc.coordinatesCalculatedAt)
    lockKey = makeLockKey driverId

makeLockKey :: Id Person.Person -> Text
makeLockKey (Id driverId) = "ARDU:driverLocationUpdate:" <> driverId

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
