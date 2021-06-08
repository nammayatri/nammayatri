{-# LANGUAGE OverloadedLabels #-}

module Services.Allocation.Internal where

import App.BackgroundTaskManager.Types
import qualified App.Types as AppFlow
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Storage.Organization
import qualified Beckn.Types.Storage.ProductInstance as PI
import Control.Monad.Reader (withReaderT)
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
import Types.Storage.AllocationEvent (AllocationEventType)
import qualified Types.Storage.DriverInformation as SDriverInfo
import qualified Types.Storage.NotificationStatus as SNS
import qualified Types.Storage.RideRequest as SRR
import Utils.Common (throwError)
import Utils.Notifications

getDriverSortMode :: Flow SortMode
getDriverSortMode = asks defaultSortMode

getConfiguredNotificationTime :: Flow NominalDiffTime
getConfiguredNotificationTime = asks driverNotificationExpiry

getConfiguredAllocationTime :: Flow NominalDiffTime
getConfiguredAllocationTime = asks rideAllocationExpiry

getDriverPool :: Id Ride -> Flow [Id Driver]
getDriverPool rideId = withAppEnv $ Person.getDriverPool (cast rideId)

getRequests :: ShortId Organization -> Integer -> Flow [RideRequest]
getRequests shortOrgId numRequests =
  withAppEnv $
    map rideRequestToRideRequest
      <$> QRR.fetchOldest shortOrgId numRequests

assignDriver :: Id Ride -> Id Driver -> Flow ()
assignDriver rideId driverId = withAppEnv $ PI.assignDriver (cast rideId) driverId

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
  Id Ride ->
  Flow (Maybe CurrentNotification)
getCurrentNotification rideId = do
  notificationStatus <- withAppEnv $ QNS.findActiveNotificationByRideId rideId
  pure $ buildCurrentNotification <$> notificationStatus
  where
    buildCurrentNotification notificationStatus =
      Alloc.CurrentNotification
        (notificationStatus ^. #driverId)
        (notificationStatus ^. #expiresAt)

cleanupOldNotifications :: Flow Int
cleanupOldNotifications = withAppEnv QNS.cleanupOldNotifications

sendNewRideNotification :: Id Ride -> Id Driver -> Flow ()
sendNewRideNotification (Id rideId) (Id driverId) = withAppEnv $ do
  prodInst <- QPI.findById (Id rideId)
  person <- QP.findPersonById (Id driverId)
  notifyDriverNewAllocation prodInst person

sendRideNotAssignedNotification :: Id Ride -> Id Driver -> Flow ()
sendRideNotAssignedNotification (Id rideId) (Id driverId) = withAppEnv $ do
  prodInst <- QPI.findById (Id rideId)
  person <- QP.findPersonById (Id driverId)
  notifyDriverUnassigned prodInst person

updateNotificationStatus :: Id Ride -> Id Driver -> NotificationStatus -> Flow ()
updateNotificationStatus rideId driverId =
  withAppEnv . QNS.updateStatus rideId driverId . allocNotifStatusToStorageStatus

resetLastRejectionTime :: Id Driver -> Flow ()
resetLastRejectionTime = withAppEnv . QDS.updateIdleTimeFlow

getAttemptedDrivers :: Id Ride -> Flow [Id Driver]
getAttemptedDrivers rideId =
  withAppEnv $ QNS.fetchRefusedNotificationsByRideId rideId <&> map (^. #driverId)

getDriversWithNotification :: Flow [Id Driver]
getDriversWithNotification = withAppEnv $ QNS.fetchActiveNotifications <&> fmap (^. #driverId)

getFirstDriverInTheQueue :: NonEmpty (Id Driver) -> Flow (Id Driver)
getFirstDriverInTheQueue = withAppEnv . QDS.getFirstDriverInTheQueue . toList

checkAvailability :: NonEmpty (Id Driver) -> Flow [Id Driver]
checkAvailability driverIds = do
  driversInfo <- withAppEnv $ QDriverInfo.fetchAllAvailableByIds $ toList driverIds
  pure $ map (cast . SDriverInfo.driverId) driversInfo

getDriverResponse :: Id Ride -> Id Driver -> Flow (Maybe DriverResponse)
getDriverResponse rideId driverId =
  Redis.getKeyRedis $ "beckn:" <> getId rideId <> ":" <> getId driverId <> ":response"

cancelRide :: Id Ride -> Flow ()
cancelRide rideId = withAppEnv $ BP.cancelRide rideId False

cleanupNotifications :: Id Ride -> Flow ()
cleanupNotifications = withAppEnv . QNS.cleanupNotifications

removeRequest :: Id SRR.RideRequest -> Flow ()
removeRequest = withAppEnv . QRR.removeRequest

allocNotifStatusToStorageStatus ::
  Alloc.NotificationStatus ->
  SNS.AnswerStatus
allocNotifStatusToStorageStatus = \case
  Alloc.Notified -> SNS.NOTIFIED
  Alloc.Rejected -> SNS.REJECTED
  Alloc.Ignored -> SNS.IGNORED

addAllocationRequest :: ShortId Organization -> Id Ride -> Flow ()
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
  withAppEnv $ QRR.createFlow rideRequest

addNotificationStatus ::
  Id Ride ->
  Id Driver ->
  UTCTime ->
  Flow ()
addNotificationStatus rideId driverId expiryTime = do
  uuid <- generateGUID
  withAppEnv $
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
  if driverInfo ^. #active && not (driverInfo ^. #onRide)
    then cast (driverInfo ^. #driverId) : availableDriversIds
    else availableDriversIds

logEvent :: AllocationEventType -> Id Ride -> Maybe (Id Driver) -> Flow ()
logEvent evType rideId = withAppEnv . logAllocationEvent evType rideId

getRideInfo :: Id Ride -> Flow RideInfo
getRideInfo rideId = do
  productInstance <- withAppEnv . QPI.findById $ cast rideId
  rideStatus <- castToRideStatus $ productInstance ^. #status
  pure
    RideInfo
      { rideId = rideId,
        rideStatus = rideStatus,
        orderTime = OrderTime $ productInstance ^. #createdAt
      }
  where
    castToRideStatus = \case
      PI.CONFIRMED -> pure Confirmed
      PI.TRIP_ASSIGNED -> pure Assigned
      PI.INPROGRESS -> pure InProgress
      PI.COMPLETED -> pure Completed
      PI.CANCELLED -> pure Cancelled
      _ -> throwError $ InternalError "Unknown status to cast."

withAppEnv :: AppFlow.Flow a -> Flow a
withAppEnv = withReaderT appEnv
