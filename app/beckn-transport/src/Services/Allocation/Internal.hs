{-# LANGUAGE OverloadedLabels #-}

module Services.Allocation.Internal where

import App.Types
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import Beckn.Types.ID
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Utils.Common (getCurrTime, throwErrorJSON500)
import qualified Beckn.Utils.Common as Common
import qualified Beckn.Utils.Logging as Log
import Data.Time
import EulerHS.Prelude
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
import Types.Storage.AllocationEvent (AllocationEventType)
import qualified Types.Storage.DriverInformation as SDriverInfo
import qualified Types.Storage.NotificationStatus as SNS
import qualified Types.Storage.RideRequest as SRR
import Utils.Notifications

getCurrentTime :: Flow UTCTime
getCurrentTime = getCurrTime

getDriverSortMode :: Flow SortMode
getDriverSortMode = asks (defaultSortMode . driverAllocationConfig)

getConfiguredNotificationTime :: Flow NominalDiffTime
getConfiguredNotificationTime = asks (driverNotificationExpiry . driverAllocationConfig)

getConfiguredAllocationTime :: Flow NominalDiffTime
getConfiguredAllocationTime = asks (rideAllocationExpiry . driverAllocationConfig)

getDriverPool :: ID Ride -> Flow [ID Driver]
getDriverPool (ID rideId) = Person.getDriverPool (ID rideId)

getRequests :: Integer -> Flow [RideRequest]
getRequests = fmap (map rideRequestToRideRequest) . QRR.fetchOldest

assignDriver :: ID Ride -> ID Driver -> Flow ()
assignDriver = PI.assignDriver

rideRequestToRideRequest :: SRR.RideRequest -> Alloc.RideRequest
rideRequestToRideRequest SRR.RideRequest {..} =
  Alloc.RideRequest
    { requestHeader =
        RequestHeader
          { requestId = _id,
            rideId = _rideId,
            requestTime = _createdAt
          },
      requestData = case _type of
        SRR.ALLOCATION -> Alloc.Allocation
        SRR.CANCELLATION -> Alloc.Cancellation
    }

getCurrentNotification ::
  ID Ride ->
  Flow (Maybe CurrentNotification)
getCurrentNotification rideId = do
  notificationStatus <- QNS.findActiveNotificationByRideId rideId
  pure $ buildCurrentNotification <$> notificationStatus
  where
    buildCurrentNotification notificationStatus =
      Alloc.CurrentNotification
        (notificationStatus ^. #_driverId)
        (notificationStatus ^. #_expiresAt)

sendNewRideNotification :: ID Ride -> ID Driver -> Flow ()
sendNewRideNotification (ID rideId) (ID driverId) = do
  prodInst <- QPI.findById (ID rideId)
  person <- QP.findPersonById (ID driverId)
  notifyDriverNewAllocation prodInst person

sendRideNotAssignedNotification :: ID Ride -> ID Driver -> Flow ()
sendRideNotAssignedNotification (ID rideId) (ID driverId) = do
  prodInst <- QPI.findById (ID rideId)
  person <- QP.findPersonById (ID driverId)
  notifyDriverUnassigned prodInst person

updateNotificationStatus :: ID Ride -> ID Driver -> NotificationStatus -> Flow ()
updateNotificationStatus rideId driverId =
  QNS.updateStatus rideId driverId . allocNotifStatusToStorageStatus

resetLastRejectionTime :: ID Driver -> Flow ()
resetLastRejectionTime = QDS.updateIdleTime

getAttemptedDrivers :: ID Ride -> Flow [ID Driver]
getAttemptedDrivers rideId =
  QNS.fetchRefusedNotificationsByRideId rideId <&> map (^. #_driverId)

getDriversWithNotification :: Flow [ID Driver]
getDriversWithNotification = QNS.fetchActiveNotifications <&> fmap (^. #_driverId)

getFirstDriverInTheQueue :: NonEmpty (ID Driver) -> Flow (ID Driver)
getFirstDriverInTheQueue = QDS.getFirstDriverInTheQueue . toList

checkAvailability :: NonEmpty (ID Driver) -> Flow [ID Driver]
checkAvailability driverIds = do
  driversInfo <- QDriverInfo.fetchAllAvailableByIds $ toList driverIds
  pure $ map SDriverInfo._driverId driversInfo

getDriverResponse :: ID Ride -> ID Driver -> Flow (Maybe DriverResponse)
getDriverResponse rideId driverId =
  Redis.getKeyRedis $ "beckn:" <> getId rideId <> ":" <> getId driverId <> ":response"

cancelRide :: ID Ride -> Flow ()
cancelRide = BP.cancelRide

cleanupNotifications :: ID Ride -> Flow ()
cleanupNotifications = QNS.cleanupNotifications

removeRequest :: ID SRR.RideRequest -> Flow ()
removeRequest = QRR.removeRequest

logOutput :: Log.LogLevel -> [Text] -> Text -> Flow ()
logOutput = Log.logOutput

runSafely :: (FromJSON a, ToJSON a) => Flow a -> Flow (Either Text a)
runSafely = Common.runSafeFlow

addLogTag :: Log.HasLogContext env => Text -> FlowR env a -> FlowR env a
addLogTag = Common.addLogTag

allocNotifStatusToStorageStatus ::
  Alloc.NotificationStatus ->
  SNS.AnswerStatus
allocNotifStatusToStorageStatus = \case
  Alloc.Notified -> SNS.NOTIFIED
  Alloc.Rejected -> SNS.REJECTED
  Alloc.Ignored -> SNS.IGNORED

addAllocationRequest :: ID Ride -> Flow ()
addAllocationRequest rideId = do
  guid <- generateGUID
  currTime <- getCurrTime
  let rideRequest =
        SRR.RideRequest
          { _id = ID guid,
            _rideId = rideId,
            _createdAt = currTime,
            _type = SRR.ALLOCATION
          }
  QRR.create rideRequest

addNotificationStatus ::
  ID Ride ->
  ID Driver ->
  UTCTime ->
  Flow ()
addNotificationStatus rideId driverId expiryTime = do
  uuid <- generateGUID
  QNS.create
    SNS.NotificationStatus
      { _id = ID uuid,
        _rideId = rideId,
        _driverId = driverId,
        _status = SNS.NOTIFIED,
        _expiresAt = expiryTime
      }

addAvailableDriver :: SDriverInfo.DriverInformation -> [ID Driver] -> [ID Driver]
addAvailableDriver driverInfo availableDriversIds =
  if driverInfo ^. #_active && not (driverInfo ^. #_onRide)
    then driverInfo ^. #_driverId : availableDriversIds
    else availableDriversIds

logEvent :: AllocationEventType -> ID Ride -> Flow ()
logEvent = logAllocationEvent

getRideInfo :: ID Ride -> Flow RideInfo
getRideInfo rideId = do
  productInstance <- QPI.findById rideId
  rideStatus <- castToRideStatus $ productInstance ^. #_status
  pure
    RideInfo
      { rideId = rideId,
        rideStatus = rideStatus,
        orderTime = OrderTime $ productInstance ^. #_createdAt
      }
  where
    castToRideStatus = \case
      PI.CONFIRMED -> pure Confirmed
      PI.TRIP_ASSIGNED -> pure Assigned
      PI.INPROGRESS -> pure InProgress
      PI.COMPLETED -> pure Completed
      PI.CANCELLED -> pure Cancelled
      _ -> throwErrorJSON500 "UNKNOWN_STATUS"
