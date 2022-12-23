module Domain.Action.UI.Location.UpdateLocation
  ( UpdateLocationReq,
    Waypoint (..),
    UpdateLocationHandle (..),
    buildUpdateLocationHandle,
    updateLocationHandler,
  )
where

import Beckn.External.Maps.Types
import qualified Beckn.External.Slack.Flow as SF
import Beckn.External.Slack.Types (SlackConfig)
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Transactionable (runInReplica)
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
import qualified Data.Text as T
import Domain.Types.DriverLocation (DriverLocation)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import Environment (Flow)
import GHC.Records.Extra
import qualified Lib.LocationUpdates as LocUpd
import qualified SharedLogic.DriverLocation as DrLoc
import qualified SharedLogic.Ride as SRide
import qualified Storage.Queries.Person as QP
import Tools.Metrics (CoreMetrics)

type UpdateLocationReq = NonEmpty Waypoint

-- Short field names for lesser json array size:
data Waypoint = Waypoint
  { pt :: LatLong, -- point
    ts :: UTCTime, -- timestamp
    acc :: Maybe Double -- accuracy, optional for now
  }
  deriving (Generic, ToJSON, Show, FromJSON, ToSchema, PrettyShow)

data UpdateLocationHandle m = UpdateLocationHandle
  { driver :: Person.Person,
    findDriverLocation :: m (Maybe DriverLocation),
    upsertDriverLocation :: LatLong -> UTCTime -> m (),
    getInProgress :: m (Maybe (Id DRide.Ride)),
    addIntermediateRoutePoints :: Id DRide.Ride -> NonEmpty LatLong -> m ()
  }

buildUpdateLocationHandle ::
  Id Person.Person ->
  Flow (UpdateLocationHandle Flow)
buildUpdateLocationHandle driverId = do
  driver <-
    runInReplica $
      QP.findById driverId
        >>= fromMaybeM (PersonNotFound driverId.getId)
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler driver.merchantId False
  pure $
    UpdateLocationHandle
      { driver,
        findDriverLocation = DrLoc.findById driverId,
        upsertDriverLocation = DrLoc.upsertGpsCoord driverId,
        getInProgress = SRide.getInProgressRideIdByDriverId driverId,
        addIntermediateRoutePoints = \rideId ->
          LocUpd.addIntermediateRoutePoints defaultRideInterpolationHandler rideId driverId
      }

updateLocationHandler ::
  ( Redis.HedisFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasFlowEnv m r '["driverLocationUpdateRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["slackCfg" ::: SlackConfig],
    HasFlowEnv m r '["driverLocationUpdateNotificationTemplate" ::: Text],
    MonadTime m
  ) =>
  UpdateLocationHandle m ->
  UpdateLocationReq ->
  m APISuccess
updateLocationHandler UpdateLocationHandle {..} waypoints = withLogTag "driverLocationUpdate" $ do
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
            (\rideId -> addIntermediateRoutePoints rideId $ NE.map (.pt) newWaypoints)
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
    HasFlowEnv m r '["slackCfg" ::: SlackConfig],
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
    msgTemplate <- asks (.driverLocationUpdateNotificationTemplate)
    let message = T.replace "{#driver-id#}" (getId personId) msgTemplate
    _ <- SF.postMessage message
    logError "Location updates hitting limit, ignoring"
    throwError $ HitsLimitError limitResetTimeInSec

locationUpdatesHitsCountKey :: Id Person.Person -> Text
locationUpdatesHitsCountKey personId = "BPP:DriverLocationUpdates:" <> getId personId <> ":hitsCount"
