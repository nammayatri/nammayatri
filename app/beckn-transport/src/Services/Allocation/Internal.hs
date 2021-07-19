module Services.Allocation.Internal where

import App.BackgroundTaskManager.Types (DriverAllocationConfig)
import Beckn.External.Encryption (decrypt)
import qualified Beckn.External.FCM.Types as FCM
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)
import qualified Product.BecknProvider.BP as BP
import qualified Product.BecknProvider.Confirm as Confirm
import Services.Allocation.Allocation as Alloc
import qualified Storage.Queries.Allocation as QA
import Storage.Queries.AllocationEvent (logAllocationEvent)
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.NotificationStatus as QNS
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideRequest as QRR
import qualified Storage.Queries.Vehicle as QVeh
import Types.App
import Types.Error
import Types.Metrics (CoreMetrics)
import Types.Storage.AllocationEvent (AllocationEventType)
import qualified Types.Storage.DriverInformation as SDriverInfo
import qualified Types.Storage.NotificationStatus as SNS
import Types.Storage.Organization
import qualified Types.Storage.RideCancellationReason as SRCR
import Types.Storage.Ride (Ride)
import qualified Types.Storage.Ride as Ride
import qualified Types.Storage.RideRequest as SRR
import Utils.Common
import Utils.Notifications
import qualified Utils.Notifications as Notify
import qualified Storage.Queries.ProductInstance as QPI

getDriverSortMode :: HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig] => m SortMode
getDriverSortMode = asks (.driverAllocationConfig.defaultSortMode)

getConfiguredNotificationTime :: HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig] => m Seconds
getConfiguredNotificationTime = asks (.driverAllocationConfig.driverNotificationExpiry)

getConfiguredAllocationTime :: HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig] => m Seconds
getConfiguredAllocationTime = asks (.driverAllocationConfig.rideAllocationExpiry)

getDriverPool :: (DBFlow m r, HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]) => Id Ride -> m [Id Driver]
getDriverPool = Confirm.getDriverPool

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
assignDriver rideId driverId = do
  ride <- QRide.findById rideId >>= fromMaybeM RideDoesNotExist
  let searchPIId = ride.productInstanceId
  searchPi <- QPI.findById searchPIId >>= fromMaybeM RideNotFound
  driver <-
    QP.findPersonById (cast driverId)
      >>= fromMaybeM PersonDoesNotExist
  vehicleId <-
    driver.udf1
      & fromMaybeM (PersonFieldNotPresent "udf1 - vehicle")
      <&> Id
  vehicle <-
    QVeh.findVehicleById vehicleId
      >>= fromMaybeM VehicleNotFound
  decDriver <- decrypt driver
  DB.runSqlDBTransaction (QA.assignDriver ride.id vehicle decDriver)

  fork "assignDriver - Notify BAP" $ do
    BP.notifyUpdateToBAP searchPi ride Ride.TRIP_ASSIGNED
    Notify.notifyDriver notificationType notificationTitle (message ride) driver.id driver.deviceToken
  where
    notificationType = FCM.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message ride =
      unwords
        [ "You have been assigned a ride for",
          showTimeIst ride.startTime <> ".",
          "Check the app for more details."
        ]

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
  prodInst <- QRide.findById rideId >>= fromMaybeM RideNotFound
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
sendRideNotAssignedNotification rideId driverId = do
  ride <- QRide.findById rideId >>= fromMaybeM RideNotFound
  person <-
    QP.findPersonById (cast driverId)
      >>= fromMaybeM PersonNotFound
  notifyDriverUnassigned ride person.id person.deviceToken

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
  ride <- QRide.findById rideId >>= fromMaybeM RideNotFound
  rideStatus <- castToRideStatus $ ride.status
  pure
    RideInfo
      { rideId = rideId,
        rideStatus = rideStatus,
        orderTime = OrderTime $ ride.createdAt
      }
  where
    castToRideStatus = \case
      Ride.CONFIRMED -> pure Confirmed
      Ride.TRIP_ASSIGNED -> pure Assigned
      Ride.INPROGRESS -> pure InProgress
      Ride.COMPLETED -> pure Completed
      Ride.CANCELLED -> pure Cancelled
      _ -> throwError $ InternalError "Unknown status to cast."
