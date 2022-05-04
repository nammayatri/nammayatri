module Services.Allocation.Internal where

import App.Allocator.Environment (Flow)
import qualified Beckn.External.FCM.Types as FCM
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.AllocationEvent (AllocationEventType)
import qualified Domain.Types.DriverInformation as SDriverInfo
import qualified Domain.Types.NotificationStatus as SNS
import Domain.Types.Organization
import qualified Domain.Types.Ride as SRide
import Domain.Types.RideBooking (RideBooking)
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.RideBookingCancellationReason as SBCR
import qualified Domain.Types.RideRequest as SRR
import EulerHS.Prelude hiding (id)
import qualified Product.BecknProvider.BP as BP
import Product.BecknProvider.Cancel
import Services.Allocation.Allocation as Alloc
import qualified SharedLogic.DriverPool as DrPool
import Storage.Queries.AllocationEvent (logAllocationEvent)
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.NotificationStatus as QNS
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideBooking as QRideBooking
import qualified Storage.Queries.RideBookingCancellationReason as QBCR
import qualified Storage.Queries.RideRequest as QRR
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Metrics (CoreMetrics)
import Types.App
import Types.Error
import Utils.Common
import Utils.Notifications
import qualified Utils.Notifications as Notify

getDriverSortMode :: Flow SortMode
getDriverSortMode = asks (.defaultSortMode)

getConfiguredNotificationTime :: Flow Seconds
getConfiguredNotificationTime = asks (.driverNotificationExpiry)

getConfiguredAllocationTime :: Flow Seconds
getConfiguredAllocationTime = asks (.rideAllocationExpiry)

getConfiguredReallocationsLimit :: Flow Int
getConfiguredReallocationsLimit = asks (.reallocationsLimit)

getDriverPool :: Id RideBooking -> Flow [Id Driver]
getDriverPool = DrPool.getDriverPool

getDriverBatchSize :: Flow Int
getDriverBatchSize = asks (.driverBatchSize)

getRequests :: ShortId Organization -> Integer -> Flow [RideRequest]
getRequests shortOrgId numRequests = do
  allRequests <- QRR.fetchOldest shortOrgId numRequests
  let (errors, requests) =
        partitionEithers $ map toRideRequest allRequests
  traverse_ logError errors
  pure requests

assignDriver ::
  Id RideBooking ->
  Id Driver ->
  Flow ()
assignDriver rideBookingId driverId = do
  rideBooking <- QRB.findById rideBookingId >>= fromMaybeM (RideBookingDoesNotExist rideBookingId.getId)
  driver <-
    QP.findById (cast driverId)
      >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  ride <- buildRide rideBooking driver
  Esq.runTransaction $ do
    QDI.updateOnRide (cast driver.id) True
    QRB.updateStatus rideBookingId SRB.TRIP_ASSIGNED
    QRide.create ride

  fork "assignDriver - Notify BAP" $ do
    uRideBooking <- QRB.findById rideBookingId >>= fromMaybeM (RideBookingNotFound rideBookingId.getId)
    BP.sendRideAssignedUpdateToBAP uRideBooking ride
    Notify.notifyDriver notificationType notificationTitle (message uRideBooking) driver.id driver.deviceToken
  where
    notificationType = FCM.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message ride =
      unwords
        [ "You have been assigned a ride for",
          showTimeIst ride.startTime <> ".",
          "Check the app for more details."
        ]
    buildRide rideBooking driver = do
      vehicleId <-
        driver.udf1
          & fromMaybeM (PersonFieldNotPresent "udf1 - vehicle")
          <&> Id
      vehicle <-
        QVeh.findById vehicleId
          >>= fromMaybeM (VehicleNotFound vehicleId.getId)
      guid <- generateGUID
      shortId <- generateShortId
      otp <- generateOTPCode
      now <- getCurrentTime
      return
        SRide.Ride
          { id = guid,
            bookingId = rideBooking.id,
            shortId = shortId,
            status = SRide.NEW,
            driverId = driver.id,
            vehicleId = vehicle.id,
            otp = otp,
            trackingUrl = "UNKNOWN", -- TODO: Fill this field
            fare = Nothing,
            totalFare = Nothing,
            traveledDistance = 0,
            chargeableDistance = Nothing,
            createdAt = now,
            updatedAt = now
          }

toRideRequest :: SRR.RideRequest -> Either Text Alloc.RideRequest
toRideRequest req =
  case eRequestData of
    Right requestData ->
      Right
        Alloc.RideRequest
          { requestId = req.id,
            rideBookingId = req.rideBookingId,
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
  Id RideBooking ->
  Flow [CurrentNotification]
getCurrentNotifications rideBookingId = do
  notificationStatuses <- QNS.findActiveNotificationByRBId rideBookingId
  pure $ map buildCurrentNotification notificationStatuses
  where
    buildCurrentNotification notificationStatus =
      Alloc.CurrentNotification
        (notificationStatus.driverId)
        (notificationStatus.expiresAt)

cleanupOldNotifications :: Flow Int
cleanupOldNotifications = Esq.runTransaction QNS.cleanupOldNotifications

sendNewRideNotifications ::
  Id RideBooking ->
  NonEmpty (Id Driver) ->
  Flow ()
sendNewRideNotifications rideBookingId = traverse_ sendNewRideNotification
  where
    sendNewRideNotification driverId = do
      rideBooking <- QRB.findById rideBookingId >>= fromMaybeM (RideBookingNotFound rideBookingId.getId)
      person <-
        QP.findById (cast driverId)
          >>= fromMaybeM (PersonNotFound driverId.getId)
      notifyDriverNewAllocation rideBooking.id person.id person.deviceToken

sendRideNotAssignedNotification ::
  Id RideBooking ->
  Id Driver ->
  Flow ()
sendRideNotAssignedNotification rideBookingId driverId = do
  rideBooking <- QRB.findById rideBookingId >>= fromMaybeM (RideBookingNotFound rideBookingId.getId)
  person <-
    QP.findById (cast driverId)
      >>= fromMaybeM (PersonNotFound driverId.getId)
  notifyRideNotAssigned rideBooking.id person.id person.deviceToken

updateNotificationStatuses :: EsqDBFlow m r => Id RideBooking -> NotificationStatus -> NonEmpty (Id Driver) -> m ()
updateNotificationStatuses rideBookingId status driverIds = do
  let storageStatus = allocNotifStatusToStorageStatus status
  Esq.runTransaction $ QNS.updateStatus rideBookingId storageStatus $ toList driverIds

resetLastRejectionTimes :: EsqDBFlow m r => NonEmpty (Id Driver) -> m ()
resetLastRejectionTimes driverIds = Esq.runTransaction . QDS.updateIdleTimes $ toList driverIds

getAttemptedDrivers :: EsqDBFlow m r => Id RideBooking -> m [Id Driver]
getAttemptedDrivers rideBookingId =
  QNS.fetchAttemptedNotificationsByRBId rideBookingId <&> map (.driverId)

getDriversWithNotification :: EsqDBFlow m r => m [Id Driver]
getDriversWithNotification = QNS.fetchActiveNotifications <&> fmap (.driverId)

getTopDriversByIdleTime :: EsqDBFlow m r => Int -> [Id Driver] -> m [Id Driver]
getTopDriversByIdleTime = QDS.getTopDriversByIdleTime

checkAvailability :: EsqDBFlow m r => NonEmpty (Id Driver) -> m [Id Driver]
checkAvailability driverIds = do
  driversInfo <- QDriverInfo.fetchAllAvailableByIds $ toList driverIds
  pure $ map (cast . SDriverInfo.driverId) driversInfo

cancelRideBooking ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id SRB.RideBooking ->
  SBCR.RideBookingCancellationReason ->
  m ()
cancelRideBooking rideBookingId reason = do
  rideBooking <- QRB.findById rideBookingId >>= fromMaybeM (RideBookingNotFound rideBookingId.getId)
  mbRide <- QRide.findActiveByRBId rideBookingId
  let transporterId = rideBooking.providerId
  transporter <-
    QOrg.findById transporterId
      >>= fromMaybeM (OrgNotFound transporterId.getId)
  Esq.runTransaction $ do
    QRB.updateStatus rideBooking.id SRB.CANCELLED
    QBCR.create reason
    whenJust mbRide $ \ride -> do
      QRide.updateStatus ride.id SRide.CANCELLED
      QDriverInfo.updateOnRide (cast ride.driverId) False
  logTagInfo ("rideBookingId-" <> getId rideBookingId) ("Cancellation reason " <> show reason.source)
  fork "cancelRideBooking - Notify BAP" $ do
    BP.sendRideBookingCanceledUpdateToBAP rideBooking transporter reason.source
  whenJust mbRide $ \ride ->
    notifyDriverOnCancel rideBooking ride reason

cleanupNotifications :: EsqDBFlow m r => Id RideBooking -> m ()
cleanupNotifications = Esq.runTransaction . QNS.cleanupNotifications

removeRequest :: EsqDBFlow m r => Id SRR.RideRequest -> m ()
removeRequest = Esq.runTransaction . QRR.removeRequest

allocNotifStatusToStorageStatus ::
  Alloc.NotificationStatus ->
  SNS.AnswerStatus
allocNotifStatusToStorageStatus = \case
  Alloc.Notified -> SNS.NOTIFIED
  Alloc.Rejected -> SNS.REJECTED
  Alloc.Ignored -> SNS.IGNORED

addAllocationRequest :: EsqDBFlow m r => ShortId Organization -> Id RideBooking -> m ()
addAllocationRequest shortOrgId rideBookingId = do
  guid <- generateGUID
  currTime <- getCurrentTime
  let rideRequest =
        SRR.RideRequest
          { id = Id guid,
            rideBookingId = rideBookingId,
            shortOrgId = shortOrgId,
            createdAt = currTime,
            _type = SRR.ALLOCATION,
            info = Nothing
          }
  Esq.runTransaction $ QRR.create rideRequest

addNotificationStatuses ::
  EsqDBFlow m r =>
  Id RideBooking ->
  NonEmpty (Id Driver) ->
  UTCTime ->
  m ()
addNotificationStatuses rideBookingId driverIds expiryTime = do
  notificationStatuses <- traverse createNotificationStatus driverIds
  Esq.runTransaction $ QNS.createMany $ toList notificationStatuses
  where
    createNotificationStatus driverId = do
      uuid <- generateGUID
      pure $
        SNS.NotificationStatus
          { id = Id uuid,
            rideBookingId = rideBookingId,
            driverId = driverId,
            status = SNS.NOTIFIED,
            expiresAt = expiryTime
          }

addAvailableDriver :: SDriverInfo.DriverInformation -> [Id Driver] -> [Id Driver]
addAvailableDriver driverInfo availableDriversIds =
  if driverInfo.active && not (driverInfo.onRide)
    then cast (driverInfo.driverId) : availableDriversIds
    else availableDriversIds

logEvent :: EsqDBFlow m r => AllocationEventType -> Id RideBooking -> m ()
logEvent eventType rideBookingId = Esq.runTransaction $ logAllocationEvent eventType rideBookingId Nothing

logDriverEvents :: EsqDBFlow m r => AllocationEventType -> Id RideBooking -> NonEmpty (Id Driver) -> m ()
logDriverEvents eventType rideBookingId driverList = Esq.runTransaction $ traverse_ logDriverEvent driverList
  where
    logDriverEvent driver = logAllocationEvent eventType rideBookingId $ Just driver

-- TODO: We don't need RideInfo anymore, we can just use RideBooking directly. Remove this.
getRideInfo :: EsqDBFlow m r => Id RideBooking -> m RideInfo
getRideInfo rideBookingId = do
  rideBooking <- QRB.findById rideBookingId >>= fromMaybeM (RideBookingNotFound rideBookingId.getId)
  rideStatus <- castToRideStatus $ rideBooking.status
  pure
    RideInfo
      { rideBookingId = rideBookingId,
        rideStatus = rideStatus,
        orderTime = OrderTime $ rideBooking.createdAt,
        reallocationsCount = rideBooking.reallocationsCount
      }
  where
    castToRideStatus = \case
      SRB.CONFIRMED -> Confirmed
      SRB.AWAITING_REASSIGNMENT -> AwaitingReassignment
      SRB.TRIP_ASSIGNED -> Assigned
      SRB.COMPLETED -> Completed
      SRB.CANCELLED -> Cancelled
      SRB.SCHEDULED -> Scheduled

findRideBookingById :: (EsqDBFlow m r) => Id SRB.RideBooking -> m SRB.RideBooking
findRideBookingById rbId = QRideBooking.findById rbId >>= fromMaybeM RideBookingNotFound

updateRideBookingStatus :: (EsqDBFlow m r) => SRB.RideBookingStatus -> Id SRB.RideBooking -> m ()
updateRideBookingStatus status rbId = Esq.runTransaction $ QRideBooking.updateStatus rbId status
