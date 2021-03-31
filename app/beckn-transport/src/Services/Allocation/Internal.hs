{-# LANGUAGE OverloadedLabels #-}

module Services.Allocation.Internal where

import App.Types
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common (FlowR)
import qualified Beckn.Types.Common as Common
import Beckn.Types.Id
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Utils.Common (throwErrorWithInfo)
import qualified Beckn.Utils.Common as Common
import Data.Time (NominalDiffTime, UTCTime)
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
import Types.Error
import Types.Storage.AllocationEvent (AllocationEventType)
import qualified Types.Storage.DriverInformation as SDriverInfo
import qualified Types.Storage.NotificationStatus as SNS
import qualified Types.Storage.RideRequest as SRR
import Utils.Notifications

getCurrentTime :: Flow UTCTime
getCurrentTime = Common.getCurrentTime

getDriverSortMode :: Flow SortMode
getDriverSortMode = asks (defaultSortMode . driverAllocationConfig)

getConfiguredNotificationTime :: Flow NominalDiffTime
getConfiguredNotificationTime = asks (driverNotificationExpiry . driverAllocationConfig)

getConfiguredAllocationTime :: Flow NominalDiffTime
getConfiguredAllocationTime = asks (rideAllocationExpiry . driverAllocationConfig)

getDriverPool :: Id Ride -> Flow [Id Driver]
getDriverPool rideId = Person.getDriverPool (cast rideId)

getRequests :: Integer -> Flow [RideRequest]
getRequests = fmap (map rideRequestToRideRequest) . QRR.fetchOldest

assignDriver :: Id Ride -> Id Driver -> Flow ()
assignDriver = PI.assignDriver . cast

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
  Id Ride ->
  Flow (Maybe CurrentNotification)
getCurrentNotification rideId = do
  notificationStatus <- QNS.findActiveNotificationByRideId rideId
  pure $ buildCurrentNotification <$> notificationStatus
  where
    buildCurrentNotification notificationStatus =
      Alloc.CurrentNotification
        (notificationStatus ^. #_driverId)
        (notificationStatus ^. #_expiresAt)

sendNewRideNotification :: Id Ride -> Id Driver -> Flow ()
sendNewRideNotification (Id rideId) (Id driverId) = do
  prodInst <- QPI.findById (Id rideId)
  person <- QP.findPersonById (Id driverId)
  notifyDriverNewAllocation prodInst person

sendRideNotAssignedNotification :: Id Ride -> Id Driver -> Flow ()
sendRideNotAssignedNotification (Id rideId) (Id driverId) = do
  prodInst <- QPI.findById (Id rideId)
  person <- QP.findPersonById (Id driverId)
  notifyDriverUnassigned prodInst person

updateNotificationStatus :: Id Ride -> Id Driver -> NotificationStatus -> Flow ()
updateNotificationStatus rideId driverId =
  QNS.updateStatus rideId driverId . allocNotifStatusToStorageStatus

resetLastRejectionTime :: Id Driver -> Flow ()
resetLastRejectionTime = QDS.updateIdleTimeFlow

getAttemptedDrivers :: Id Ride -> Flow [Id Driver]
getAttemptedDrivers rideId =
  QNS.fetchRefusedNotificationsByRideId rideId <&> map (^. #_driverId)

getDriversWithNotification :: Flow [Id Driver]
getDriversWithNotification = QNS.fetchActiveNotifications <&> fmap (^. #_driverId)

getFirstDriverInTheQueue :: NonEmpty (Id Driver) -> Flow (Id Driver)
getFirstDriverInTheQueue = QDS.getFirstDriverInTheQueue . toList

checkAvailability :: NonEmpty (Id Driver) -> Flow [Id Driver]
checkAvailability driverIds = do
  driversInfo <- QDriverInfo.fetchAllAvailableByIds $ toList driverIds
  pure $ map SDriverInfo._driverId driversInfo

getDriverResponse :: Id Ride -> Id Driver -> Flow (Maybe DriverResponse)
getDriverResponse rideId driverId =
  Redis.getKeyRedis $ "beckn:" <> getId rideId <> ":" <> getId driverId <> ":response"

cancelRide :: Id Ride -> Flow ()
cancelRide = flip BP.cancelRide False

cleanupNotifications :: Id Ride -> Flow ()
cleanupNotifications = QNS.cleanupNotifications

removeRequest :: Id SRR.RideRequest -> Flow ()
removeRequest = QRR.removeRequest

logOutput :: Common.LogLevel -> [Text] -> Text -> Flow ()
logOutput = Common.logOutput

runSafely :: (FromJSON a, ToJSON a) => Flow a -> Flow (Either Text a)
runSafely = Common.runSafeFlow

addLogTag :: Common.HasLogContext env => Text -> FlowR env a -> FlowR env a
addLogTag = Common.addLogTag

allocNotifStatusToStorageStatus ::
  Alloc.NotificationStatus ->
  SNS.AnswerStatus
allocNotifStatusToStorageStatus = \case
  Alloc.Notified -> SNS.NOTIFIED
  Alloc.Rejected -> SNS.REJECTED
  Alloc.Ignored -> SNS.IGNORED

addAllocationRequest :: Id Ride -> Flow ()
addAllocationRequest rideId = do
  guid <- Common.generateGUID
  currTime <- Common.getCurrentTime
  let rideRequest =
        SRR.RideRequest
          { _id = Id guid,
            _rideId = rideId,
            _createdAt = currTime,
            _type = SRR.ALLOCATION
          }
  QRR.create rideRequest

addNotificationStatus ::
  Id Ride ->
  Id Driver ->
  UTCTime ->
  Flow ()
addNotificationStatus rideId driverId expiryTime = do
  uuid <- Common.generateGUID
  QNS.create
    SNS.NotificationStatus
      { _id = Id uuid,
        _rideId = rideId,
        _driverId = driverId,
        _status = SNS.NOTIFIED,
        _expiresAt = expiryTime
      }

addAvailableDriver :: SDriverInfo.DriverInformation -> [Id Driver] -> [Id Driver]
addAvailableDriver driverInfo availableDriversIds =
  if driverInfo ^. #_active && not (driverInfo ^. #_onRide)
    then driverInfo ^. #_driverId : availableDriversIds
    else availableDriversIds

logEvent :: AllocationEventType -> Id Ride -> Flow ()
logEvent = logAllocationEvent

getRideInfo :: Id Ride -> Flow RideInfo
getRideInfo rideId = do
  productInstance <- QPI.findById $ cast rideId
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
      _ -> throwErrorWithInfo CommonInternalError "Unknown status to cast."
