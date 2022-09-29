module Domain.Action.UI.Location.UpdateLocation
  ( Handler (..),
    UpdateLocationReq,
    Waypoint (..),
    UpdateLocationRes,
    updateLocationHandler,
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

type MonadHandler m = (MonadFlow m, MonadThrow m, Log m, MonadGuid m, MonadTime m)

data Handler m = Handler
  { refreshPeriod :: NominalDiffTime,
    findPersonById :: Id Person.Person -> m (Maybe Person.Person),
    findDriverLocationById :: Id Person.Person -> m (Maybe DriverLocation),
    upsertDriverLocation :: Id Person.Person -> LatLong -> UTCTime -> m (),
    getInProgressByDriverId :: Id Person.Person -> m (Maybe SRide.Ride),
    addIntermediateRoutePoints :: Id Person.Person -> NonEmpty LatLong -> m ()
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
    case (isCalledBeforeRefreshPeriod mbOldLoc now, filterNewWaypoints mbOldLoc) of
      (True, _) -> logWarning "Called before refresh period passed, ignoring"
      (_, []) -> logWarning "Incoming points are older than current one, ignoring"
      (_, a : ax) -> do
        let newWaypoints = a :| ax
            currPoint = NE.last newWaypoints
        upsertDriverLocation driver.id currPoint.pt currPoint.ts
        getInProgressByDriverId driver.id
          >>= maybe
            (logInfo "No ride is assigned to driver, ignoring")
            (\_ -> addIntermediateRoutePoints driver.id $ NE.map (.pt) newWaypoints)

    Redis.unlockRedis lockKey
  pure Success
  where
    filterNewWaypoints mbOldLoc = do
      let sortedWaypoint = toList $ NE.sortWith (.ts) waypoints
      maybe sortedWaypoint (\oldLoc -> filter ((oldLoc.coordinatesCalculatedAt <) . (.ts)) sortedWaypoint) mbOldLoc

    isCalledBeforeRefreshPeriod mbLoc now =
      maybe False (\loc -> now `diffUTCTime` loc.updatedAt < refreshPeriod) mbLoc
    lockKey = makeLockKey driverId

makeLockKey :: Id Person.Person -> Text
makeLockKey (Id driverId) = "beckn-transport:driverLocationUpdate:" <> driverId
