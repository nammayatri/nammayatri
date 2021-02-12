{-# LANGUAGE OverloadedLabels #-}

module Services.Allocation.Flow (handle) where

import App.Types
-- import qualified Storage.Queries.RideRequest as QRR

-- import qualified Product.Person as Person

import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.App
import Beckn.Types.Common (generateGUID)
import Beckn.Types.ID (ID (..))
import Beckn.Utils.Common (fromMaybeM500, getCurrTime)
import EulerHS.Prelude
import qualified Product.BecknProvider.BP as BP (cancelRide)
import qualified Product.Person as Person
import Services.Allocation as Alloc
import qualified Services.Allocation.Internals as Internals
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.NotificationStatus as QNS
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.ProductInstance as QPI
import qualified Storage.Queries.RideRequest as QRR
import Types.App
import qualified Types.Storage.DriverInformation as SDriverInfo
import qualified Types.Storage.NotificationStatus as SNS
import qualified Types.Storage.RideRequest as SRR
import Utils.Notifications (notifyDriverNewAllocation)

handle :: ServiceHandle Flow
handle =
  ServiceHandle
    { getCurrentTime = getCurrTime,
      getDriverSortMode = asks (defaultSortMode . driverAllocationConfig),
      getConfiguredNotificationTime =
        asks (driverNotificationExpiry . driverAllocationConfig),
      getConfiguredAllocationTime =
        asks (rideAllocationExpiry . driverAllocationConfig),
      getRequests = fmap (map rideRequestToRideRequest) . QRR.fetchOldest,
      getDriverPool = \(RideId rideId) -> do
        personIds <- Person.getDriverPool (ProductInstanceId rideId)
        let driverIds = fmap toDriverId personIds
        pure driverIds,
      getCurrentNotification = getCurrentNotification',
      sendNotification = \(RideId rideId) (DriverId driverId) -> do
        prodInst <- QPI.findById (ProductInstanceId rideId)
        person <- QP.findPersonById (PersonId driverId)
        notifyDriverNewAllocation prodInst person,
      addNotificationStatus = addNotificationStatus',
      updateNotificationStatus = \rideId driverId ->
        QNS.updateStatus rideId driverId . allocNotifStatusToStorageStatus,
      resetLastRejectionTime = QDS.updateIdleTime,
      getAttemptedDrivers = \rideId ->
        QNS.fetchRefusedNotificationsByRideId rideId <&> map (^. #_driverId),
      getDriversWithNotification = QNS.fetchActiveNotifications <&> fmap (^. #_driverId),
      getFirstDriverInTheQueue = QDS.getFirstDriverInTheQueue . toList,
      checkAvailability = \driversIds -> do
        driversInfo <- QDriverInfo.findAllByIds $ toList driversIds
        pure $ foldr addAvailableDriver [] driversInfo,
      getDriverResponse = \rideId driverId ->
        Redis.getKeyRedis $ "beckn:" <> _getRideId rideId <> ":" <> _getDriverId driverId <> ":response",
      assignDriver = Internals.assignDriver,
      cancelRide = BP.cancelRide,
      completeRequest = QRR.markComplete,
      resetRequestTime = QRR.updateLastProcessTime
    }

-- TODO: move these somewhere else:

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
        SRR.ALLOCATION -> Alloc.Allocation (OrderTime _createdAt)
        SRR.CANCELLATION -> Alloc.Cancellation
    }

getCurrentNotification' ::
  RideId ->
  Flow (Maybe CurrentNotification)
getCurrentNotification' rideId =
  QNS.findActiveNotificationByRideId rideId
    >>= maybe (return Nothing) (fmap Just . buildCurrentNotification)
  where
    buildCurrentNotification notificationStatus = do
      notifiedAt <-
        notificationStatus ^. #_notifiedAt
          & fromMaybeM500 "NOTIFICATION_STATUS_NOT_NOTIFIED"
      let driverId = notificationStatus ^. #_driverId
      return (Alloc.CurrentNotification driverId notifiedAt)

allocNotifStatusToStorageStatus ::
  Alloc.NotificationStatus ->
  SNS.AnswerStatus
allocNotifStatusToStorageStatus = \case
  Alloc.Notified _ -> SNS.NOTIFIED
  Alloc.Rejected -> SNS.REJECTED
  Alloc.Ignored -> SNS.IGNORED
  Alloc.Accepted -> SNS.ACCEPTED

addNotificationStatus' ::
  RideId ->
  DriverId ->
  NotificationStatus ->
  Flow ()
addNotificationStatus' rideId driverId status = do
  uuid <- generateGUID
  QNS.create
    SNS.NotificationStatus
      { _id = ID uuid,
        _rideId = rideId,
        _driverId = driverId,
        _status = allocNotifStatusToStorageStatus status,
        _notifiedAt = notifiedAt
      }
  where
    notifiedAt =
      case status of
        Alloc.Notified time -> Just time
        _ -> Nothing

addAvailableDriver :: SDriverInfo.DriverInformation -> [DriverId] -> [DriverId]
addAvailableDriver driverInfo availableDriversIds =
  if driverInfo ^. #_active && not (driverInfo ^. #_onRide)
    then driverInfo ^. #_driverId : availableDriversIds
    else availableDriversIds

toDriverId :: PersonId -> DriverId
toDriverId = DriverId . _getPersonId
