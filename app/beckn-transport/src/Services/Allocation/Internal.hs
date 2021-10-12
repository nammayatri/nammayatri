module Services.Allocation.Internal where

import App.BackgroundTaskManager.Types (DriverAllocationConfig)
import Beckn.Types.Common
import Beckn.Types.Id
import Data.Time (UTCTime)
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
import Types.App
import Types.Error
import Types.Metrics (CoreMetrics)
import Types.Storage.AllocationEvent (AllocationEventType)
import qualified Types.Storage.DriverInformation as SDriverInfo
import qualified Types.Storage.NotificationStatus as SNS
import Types.Storage.Organization
import qualified Types.Storage.ProductInstance as PI
import qualified Types.Storage.RideCancellationReason as SRCR
import qualified Types.Storage.RideRequest as SRR
import Utils.Common
import Utils.Notifications

getDriverSortMode :: HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig] => m SortMode
getDriverSortMode = asks (.driverAllocationConfig.defaultSortMode)

getConfiguredNotificationTime :: HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig] => m Seconds
getConfiguredNotificationTime = asks (.driverAllocationConfig.driverNotificationExpiry)

getConfiguredAllocationTime :: HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig] => m Seconds
getConfiguredAllocationTime = asks (.driverAllocationConfig.rideAllocationExpiry)

getDriverPool :: (DBFlow m r, HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]) => Id Ride -> m [Id Driver]
getDriverPool rideId = Person.getDriverPool (cast rideId)

getDriverBatchSize :: HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig] => m Int
getDriverBatchSize = asks (.driverAllocationConfig.driverBatchSize)

getRequests :: DBFlow m r => ShortId Organization -> Integer -> m [RideRequest]
getRequests shortOrgId numRequests = do
  allRequests <- QRR.fetchOldest shortOrgId numRequests
  let (errors, requests) =
        partitionEithers $ map toRideRequest allRequests
  traverse_ logError errors
  pure requests

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

toRideRequest :: SRR.RideRequest -> Either Text Alloc.RideRequest
toRideRequest req =
  case eRequestData of
    Right requestData ->
      Right
        Alloc.RideRequest
          { requestId = req.id,
            rideId = req.rideId,
            requestData = requestData
          }
    Left err -> Left err
  where
    eRequestData = case req._type of
      SRR.ALLOCATION -> Right Alloc.Allocation
      SRR.CANCELLATION -> Right Alloc.Cancellation
      SRR.DRIVER_RESPONSE ->
        case req.info >>= decodeFromText of
          Just driverResponse -> Right $ DriverResponse driverResponse
          Nothing -> Left $ "Error decoding driver response: " <> show req.info

getCurrentNotifications ::
  DBFlow m r =>
  Id Ride ->
  m [CurrentNotification]
getCurrentNotifications rideId = do
  notificationStatuses <- QNS.findActiveNotificationByRideId rideId
  pure $ map buildCurrentNotification notificationStatuses
  where
    buildCurrentNotification notificationStatus =
      Alloc.CurrentNotification
        (notificationStatus.driverId)
        (notificationStatus.expiresAt)

cleanupOldNotifications :: DBFlow m r => m Int
cleanupOldNotifications = QNS.cleanupOldNotifications

sendNewRideNotifications ::
  ( DBFlow m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id Ride ->
  NonEmpty (Id Driver) ->
  m ()
sendNewRideNotifications rideId = traverse_ sendNewRideNotification
  where
    sendNewRideNotification driverId = do
      prodInst <- QPI.findById (cast rideId) >>= fromMaybeM PINotFound
      person <-
        QP.findPersonById (cast driverId)
          >>= fromMaybeM PersonNotFound
      notifyDriverNewAllocation prodInst person.id person.deviceToken

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
  notifyRideNotAssigned prodInst person.id person.deviceToken

updateNotificationStatuses :: DBFlow m r => Id Ride -> NotificationStatus -> NonEmpty (Id Driver) -> m ()
updateNotificationStatuses rideId status driverIds = do
  let storageStatus = allocNotifStatusToStorageStatus status
  QNS.updateStatus rideId storageStatus $ toList driverIds

resetLastRejectionTimes :: DBFlow m r => NonEmpty (Id Driver) -> m ()
resetLastRejectionTimes driverIds = QDS.updateIdleTimesFlow $ toList driverIds

getAttemptedDrivers :: DBFlow m r => Id Ride -> m [Id Driver]
getAttemptedDrivers rideId =
  QNS.fetchRefusedNotificationsByRideId rideId <&> map (.driverId)

getDriversWithNotification :: DBFlow m r => m [Id Driver]
getDriversWithNotification = QNS.fetchActiveNotifications <&> fmap (.driverId)

getTopDriversByIdleTime :: DBFlow m r => Int -> [Id Driver] -> m [Id Driver]
getTopDriversByIdleTime = QDS.getTopDriversByIdleTime

checkAvailability :: DBFlow m r => NonEmpty (Id Driver) -> m [Id Driver]
checkAvailability driverIds = do
  driversInfo <- QDriverInfo.fetchAllAvailableByIds $ toList driverIds
  pure $ map (cast . SDriverInfo.driverId) driversInfo

cancelRide ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id Ride ->
  SRCR.RideCancellationReason ->
  m ()
cancelRide = BP.cancelRide

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
            _type = SRR.ALLOCATION,
            info = Nothing
          }
  QRR.createFlow rideRequest

addNotificationStatuses ::
  DBFlow m r =>
  Id Ride ->
  NonEmpty (Id Driver) ->
  UTCTime ->
  m ()
addNotificationStatuses rideId driverIds expiryTime = do
  notificationStatuses <- traverse createNotificationStatus driverIds
  QNS.createMany $ toList notificationStatuses
  where
    createNotificationStatus driverId = do
      uuid <- generateGUID
      pure $
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

logEvent :: DBFlow m r => AllocationEventType -> Id Ride -> m ()
logEvent eventType rideId = logAllocationEvent eventType rideId Nothing

logDriverEvents :: DBFlow m r => AllocationEventType -> Id Ride -> NonEmpty (Id Driver) -> m ()
logDriverEvents eventType rideId = traverse_ logDriverEvent
  where
    logDriverEvent driver = logAllocationEvent eventType rideId $ Just driver

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
