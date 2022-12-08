module Domain.Action.UI.Location.UpdateLocation
  ( Handler (..),
    UpdateLocationReq,
    Waypoint (..),
    UpdateLocationRes,
    buildUpdateLocationHandle,
    updateLocationHandler,
  )
where

import Beckn.External.Maps.Types
import Beckn.Prelude hiding (Handler)
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Beckn.Utils.Common hiding (id)
import Beckn.Utils.GenericPretty (PrettyShow)
import Beckn.Utils.SlidingWindowLimiter (slidingWindowLimiter)
import qualified Data.List.NonEmpty as NE
import Domain.Types.DriverLocation (DriverLocation)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import Environment (Flow)
import GHC.Records.Extra
import qualified Lib.LocationUpdates as LocUpd
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import Tools.Metrics (CoreMetrics)

data Handler m = Handler
  { driver :: Person.Person,
    findDriverLocation :: m (Maybe DriverLocation),
    upsertDriverLocation :: LatLong -> UTCTime -> m (),
    getInProgress :: m (Maybe SRide.Ride),
    addIntermediateRoutePoints :: Id SRide.Ride -> NonEmpty LatLong -> m ()
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

buildUpdateLocationHandle :: Id Person.Person -> Flow (Handler Flow)
buildUpdateLocationHandle driverId = do
  driver <-
    QP.findById driverId
      >>= fromMaybeM (PersonNotFound driverId.getId)
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler driver.merchantId False
  pure $
    Handler
      { driver,
        findDriverLocation = DrLoc.findById driverId,
        upsertDriverLocation = \point timestamp ->
          Esq.runTransaction $ DrLoc.upsertGpsCoord driverId point timestamp,
        getInProgress = QRide.getInProgressByDriverId driverId,
        addIntermediateRoutePoints = \rideId ->
          LocUpd.addIntermediateRoutePoints defaultRideInterpolationHandler rideId driverId
      }

updateLocationHandler ::
  ( Redis.HedisFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasFlowEnv m r '["driverLocationUpdateRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["driverLocationUpdateNotificationTemplate" ::: Text],
    MonadTime m
  ) =>
  Handler m ->
  UpdateLocationReq ->
  m UpdateLocationRes
updateLocationHandler Handler {..} waypoints = withLogTag "driverLocationUpdate" $ do
  checkLocationUpdatesRateLimit driver.id
  logInfo $ "got location updates: " <> getId driver.id <> " " <> encodeToText waypoints
  unless (driver.role == Person.DRIVER) $ throwError AccessDenied

  LocUpd.whenWithLocationUpdatesLock driver.id $ do
    mbOldLoc <- findDriverLocation
    case filterNewWaypoints mbOldLoc of
      [] -> logWarning "Incoming points are older than current one, ignoring"
      (a : ax) -> do
        let newWaypoints = a :| ax
            currPoint = NE.last newWaypoints
        upsertDriverLocation currPoint.pt currPoint.ts
        getInProgress
          >>= maybe
            (logInfo "No ride is assigned to driver, ignoring")
            (\ride -> addIntermediateRoutePoints ride.id $ NE.map (.pt) newWaypoints)
  pure Success
  where
    filterNewWaypoints mbOldLoc = do
      let sortedWaypoint = toList $ NE.sortWith (.ts) waypoints
      maybe sortedWaypoint (\oldLoc -> filter ((oldLoc.coordinatesCalculatedAt <) . (.ts)) sortedWaypoint) mbOldLoc

checkLocationUpdatesRateLimit ::
  ( Redis.HedisFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasFlowEnv m r '["driverLocationUpdateRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["driverLocationUpdateNotificationTemplate" ::: Text],
    MonadTime m
  ) =>
  Id Person.Person ->
  m ()
checkLocationUpdatesRateLimit personId = do
  let key = locationUpdatesHitsCountKey personId
  hitsLimit <- asks (.driverLocationUpdateRateLimitOptions.limit)
  limitResetTimeInSec <- asks (.driverLocationUpdateRateLimitOptions.limitResetTimeInSec)
  unlessM (slidingWindowLimiter key hitsLimit limitResetTimeInSec) $ do
    logError "Location updates hitting limit, ignoring"
    throwError $ HitsLimitError limitResetTimeInSec

locationUpdatesHitsCountKey :: Id Person.Person -> Text
locationUpdatesHitsCountKey personId = "BPP:DriverLocationUpdates:" <> getId personId <> ":hitsCount"
