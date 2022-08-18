module Domain.Action.UI.Location.UpdateLocation
  ( Handler (..),
    UpdateLocationReq,
    Waypoint (..),
    UpdateLocationRes,
    updateLocationHandler,
    processWaypoints,
  )
where

import Beckn.Prelude hiding (Handler)
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Beckn.Utils.Common hiding (id)
import Beckn.Utils.GenericPretty (PrettyShow)
import qualified Data.List.NonEmpty as NE
import Domain.Types.DriverLocation (DriverLocation)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import GHC.Records.Extra
import SharedLogic.LocationUpdates

type MonadHandler m = (MonadFlow m, MonadThrow m, Log m, MonadGuid m, MonadTime m)

data Handler m = Handler
  { refreshPeriod :: NominalDiffTime,
    findPersonById :: Id Person.Person -> m (Maybe Person.Person),
    findDriverLocationById :: Id Person.Person -> m (Maybe DriverLocation),
    upsertDriverLocation :: Id Person.Person -> LatLong -> UTCTime -> m (),
    getInProgressByDriverId :: Id Person.Person -> m (Maybe SRide.Ride),
    thereWasFailedDistanceRecalculation :: Id Person.Person -> m Bool,
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
            ( \_ -> do
              failedDistanceForRide <- thereWasFailedDistanceRecalculation driverId
              if failedDistanceForRide
                then logInfo "Failed to calculate actual distance for this ride, ignoring"
                else processWaypoints interpolationHandler driver.id False $ NE.map (.pt) waypoints
            )
    Redis.unlockRedis lockKey
  pure Success
  where
    isCalledBeforeRefreshPeriod mbLoc now =
      maybe False (\loc -> now `diffUTCTime` loc.updatedAt < refreshPeriod) mbLoc
    currPoint = NE.last waypoints
    areIncomingPointsOutdated = maybe False (\loc -> currPoint.ts <= loc.coordinatesCalculatedAt)
    lockKey = makeLockKey driverId

makeLockKey :: Id Person.Person -> Text
makeLockKey (Id driverId) = "beckn-transport:driverLocationUpdate:" <> driverId
