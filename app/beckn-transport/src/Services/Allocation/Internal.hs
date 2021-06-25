module Services.Allocation.Internal where

import App.BackgroundTaskManager.Types (DriverAllocationConfig)
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Storage.Organization
import qualified Beckn.Types.Storage.ProductInstance as PI
import Data.Time (NominalDiffTime, UTCTime)
import EulerHS.Prelude hiding (id)
import qualified Product.BecknProvider.BP as BP
import qualified Product.Person as Person
import qualified Product.ProductInstance as PI
import Services.Allocation.Allocation as Alloc
import Storage.Queries.AllocationEvent (logAllocationEvent)
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.NotificationStatus as QNS
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.ProductInstance as QPI
import qualified Storage.Queries.RideRequest as QRR
import Types.API.Ride (DriverResponse (..))
import Types.App
import Types.Error
import Types.Metrics (CoreMetrics)
import Types.Storage.AllocationEvent (AllocationEventType)
import qualified Types.Storage.DriverInformation as SDriverInfo
import qualified Types.Storage.NotificationStatus as SNS
import qualified Types.Storage.RideRequest as SRR
import Utils.Common
import Utils.Notifications

getDriverSortMode :: HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig] => m SortMode
getDriverSortMode = asks (.driverAllocationConfig.defaultSortMode)

getConfiguredNotificationTime :: HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig] => m NominalDiffTime
getConfiguredNotificationTime = asks (.driverAllocationConfig.driverNotificationExpiry)

getConfiguredAllocationTime :: HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig] => m NominalDiffTime
getConfiguredAllocationTime = asks (.driverAllocationConfig.rideAllocationExpiry)

getDriverPool :: (DBFlow m r, HasFlowEnv m r '["defaultRadiusOfSearch" ::: Integer]) => Id Ride -> m [Id Driver]
getDriverPool rideId = Person.getDriverPool (cast rideId)

getRequests :: DBFlow m r => ShortId Organization -> Integer -> m [RideRequest]
getRequests shortOrgId numRequests =
  map rideRequestToRideRequest
    <$> QRR.fetchOldest shortOrgId numRequests

assignDriver ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id Ride ->
  Id Driver ->
  m ()
assignDriver rideId = PI.assignDriver (cast rideId)

rideRequestToRideRequest :: SRR.RideRequest -> Alloc.RideRequest
rideRequestToRideRequest SRR.RideRequest {..} =
  Alloc.RideRequest
    { requestId = id,
      rideId = rideId,
      requestData = case _type of
        SRR.ALLOCATION -> Alloc.Allocation
        SRR.CANCELLATION -> Alloc.Cancellation
    }

getCurrentNotification ::
  DBFlow m r =>
  Id Ride ->
  m (Maybe CurrentNotification)
getCurrentNotification rideId = do
  notificationStatus <- QNS.findActiveNotificationByRideId rideId
  pure $ buildCurrentNotification <$> notificationStatus
  where
    buildCurrentNotification notificationStatus =
      Alloc.CurrentNotification
        (notificationStatus.driverId)
        (notificationStatus.expiresAt)

cleanupOldNotifications :: DBFlow m r => m Int
cleanupOldNotifications = QNS.cleanupOldNotifications

sendNewRideNotification ::
  ( DBFlow m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id Ride ->
  Id Driver ->
  m ()
sendNewRideNotification rideId driverId = do
  prodInst <- QPI.findById (cast rideId) >>= fromMaybeM PINotFound
  person <-
    QP.findPersonById (cast driverId)
      >>= fromMaybeM PersonNotFound
  notifyDriverNewAllocation prodInst person

sendRideNotAssignedNotification ::
  ( DBFlow m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id Ride ->
  Id Driver ->
  m ()
sendRideNotAssignedNotification (Id rideId) (Id driverId) = do
  prodInst <- QPI.findById (Id rideId) >>= fromMaybeM PINotFound
  person <-
    QP.findPersonById (Id driverId)
      >>= fromMaybeM PersonNotFound
  notifyDriverUnassigned prodInst person

updateNotificationStatus :: DBFlow m r => Id Ride -> Id Driver -> NotificationStatus -> m ()
updateNotificationStatus rideId driverId =
  QNS.updateStatus rideId driverId . allocNotifStatusToStorageStatus

resetLastRejectionTime :: DBFlow m r => Id Driver -> m ()
resetLastRejectionTime = QDS.updateIdleTimeFlow

getAttemptedDrivers :: DBFlow m r => Id Ride -> m [Id Driver]
getAttemptedDrivers rideId =
  QNS.fetchRefusedNotificationsByRideId rideId <&> map (.driverId)

getDriversWithNotification :: DBFlow m r => m [Id Driver]
getDriversWithNotification = QNS.fetchActiveNotifications <&> fmap (.driverId)

getFirstDriverInTheQueue :: DBFlow m r => NonEmpty (Id Driver) -> m (Id Driver)
getFirstDriverInTheQueue = QDS.getFirstDriverInTheQueue . toList

checkAvailability :: DBFlow m r => NonEmpty (Id Driver) -> m [Id Driver]
checkAvailability driverIds = do
  driversInfo <- QDriverInfo.fetchAllAvailableByIds $ toList driverIds
  pure $ map (cast . SDriverInfo.driverId) driversInfo

getDriverResponse :: DBFlow m r => Id Ride -> Id Driver -> m (Maybe DriverResponse)
getDriverResponse rideId driverId =
  Redis.getKeyRedis $ "beckn:" <> getId rideId <> ":" <> getId driverId <> ":response"

cancelRide ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id Ride ->
  m ()
cancelRide rideId = BP.cancelRide rideId False

cleanupNotifications :: DBFlow m r => Id Ride -> m ()
cleanupNotifications = QNS.cleanupNotifications

removeRequest :: DBFlow m r => Id SRR.RideRequest -> m ()
removeRequest = QRR.removeRequest

allocNotifStatusToStorageStatus ::
  Alloc.NotificationStatus ->
  SNS.AnswerStatus
allocNotifStatusToStorageStatus = \case
  Alloc.Notified -> SNS.NOTIFIED
  Alloc.Rejected -> SNS.REJECTED
  Alloc.Ignored -> SNS.IGNORED

addAllocationRequest :: DBFlow m r => ShortId Organization -> Id Ride -> m ()
addAllocationRequest shortOrgId rideId = do
  guid <- generateGUID
  currTime <- getCurrentTime
  let rideRequest =
        SRR.RideRequest
          { id = Id guid,
            rideId = rideId,
            shortOrgId = shortOrgId,
            createdAt = currTime,
            _type = SRR.ALLOCATION
          }
  QRR.createFlow rideRequest

addNotificationStatus ::
  DBFlow m r =>
  Id Ride ->
  Id Driver ->
  UTCTime ->
  m ()
addNotificationStatus rideId driverId expiryTime = do
  uuid <- generateGUID
  QNS.create
    SNS.NotificationStatus
      { id = Id uuid,
        rideId = rideId,
        driverId = driverId,
        status = SNS.NOTIFIED,
        expiresAt = expiryTime
      }

addAvailableDriver :: SDriverInfo.DriverInformation -> [Id Driver] -> [Id Driver]
addAvailableDriver driverInfo availableDriversIds =
  if driverInfo.active && not (driverInfo.onRide)
    then cast (driverInfo.driverId) : availableDriversIds
    else availableDriversIds

logEvent :: DBFlow m r => AllocationEventType -> Id Ride -> Maybe (Id Driver) -> m ()
logEvent = logAllocationEvent

getRideInfo :: DBFlow m r => Id Ride -> m RideInfo
getRideInfo rideId = do
  productInstance <- QPI.findById (cast rideId) >>= fromMaybeM PINotFound
  rideStatus <- castToRideStatus $ productInstance.status
  pure
    RideInfo
      { rideId = rideId,
        rideStatus = rideStatus,
        orderTime = OrderTime $ productInstance.createdAt
      }
  where
    castToRideStatus = \case
      PI.CONFIRMED -> pure Confirmed
      PI.TRIP_ASSIGNED -> pure Assigned
      PI.INPROGRESS -> pure InProgress
      PI.COMPLETED -> pure Completed
      PI.CANCELLED -> pure Cancelled
      _ -> throwError $ InternalError "Unknown status to cast."
