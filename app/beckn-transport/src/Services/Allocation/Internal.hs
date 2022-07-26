module Services.Allocation.Internal where

import App.Allocator.Environment (Flow)
import qualified Beckn.External.FCM.Types as FCM
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Data.Text as T
import Domain.Types.AllocationEvent (AllocationEventType)
import qualified Domain.Types.DriverInformation as SDriverInfo
import Domain.Types.DriverPool
import qualified Domain.Types.NotificationStatus as SNS
import Domain.Types.Organization
import qualified Domain.Types.Ride as SRide
import Domain.Types.RideBooking (RideBooking)
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.RideBookingCancellationReason as SBCR
import qualified Domain.Types.RideRequest as SRR
import EulerHS.Prelude hiding (id)
import qualified Product.BecknProvider.BP as BP
import Servant.Client (BaseUrl (..))
import Services.Allocation.Allocation as Alloc
import qualified SharedLogic.DriverPool as DrPool
import Storage.Queries.AllocationEvent (logAllocationEvent)
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.NotificationStatus as QNS
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideBookingCancellationReason as QBCR
import qualified Storage.Queries.RideRequest as QRR
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

getDriverPool :: Id RideBooking -> Flow SortedDriverPool
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
  ride <- buildRide rideBooking
  Esq.runTransaction $ do
    QDI.updateOnRide (cast driver.id) True
    QRB.updateStatus rideBookingId SRB.TRIP_ASSIGNED
    QRide.create ride
    QBE.logDriverAssignetEvent driverId rideBookingId ride.id
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
    buildRide rideBooking = do
      guid <- generateGUID
      trackingUrl <- buildTrackingUrl guid
      shortId <- generateShortId
      otp <- generateOTPCode
      now <- getCurrentTime
      return
        SRide.Ride
          { id = guid,
            bookingId = rideBooking.id,
            shortId = shortId,
            status = SRide.NEW,
            driverId = cast driverId,
            otp = otp,
            trackingUrl,
            fare = Nothing,
            totalFare = Nothing,
            traveledDistance = 0,
            chargeableDistance = Nothing,
            tripStartTime = Nothing,
            tripEndTime = Nothing,
            createdAt = now,
            updatedAt = now
          }
    buildTrackingUrl rideId = do
      bppUIUrl <- asks (.selfUIUrl)
      let rideid = T.unpack (getId rideId)
      return $
        bppUIUrl
          { --TODO: find a way to build it using existing types from Routes
            baseUrlPath = baseUrlPath bppUIUrl <> "/driver/location/" <> rideid
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
        case req.info of
          Just driverResponse -> Right . DriverResponse $ castDriverResponse driverResponse
          Nothing -> Left "Driver info is not present "
    castDriverResponse SRR.DriverResponse {..} = do
      let response = case status of
            SRR.ACCEPT -> Alloc.Accept
            SRR.REJECT -> Alloc.Reject
      Alloc.DriverResponseType {..}

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

checkAvailability :: EsqDBFlow m r => SortedDriverPool -> m SortedDriverPool
checkAvailability driverPool = do
  let driverIds = getDriverIds driverPool
  driversInfo <- QDriverInfo.fetchAllAvailableByIds driverIds
  let availableDriverIds = map (cast . SDriverInfo.driverId) driversInfo
  -- availableDriverIds is part of driverPool, but isn't sorted by distance
  -- So we can use order that we have in sorted pool driverPool
  pure $ filterDriverPool (`elem` availableDriverIds) driverPool

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
    BP.sendRideBookingCancelledUpdateToBAP rideBooking transporter reason.source
  whenJust mbRide $ \ride ->
    fork "cancelRide - Notify driver" $ do
      driver <- QP.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      Notify.notifyOnCancel rideBooking driver.id driver.deviceToken reason.source

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
  let rideStatus = castToRideStatus $ rideBooking.status
  pure
    RideInfo
      { rideBookingId = rideBookingId,
        rideStatus = rideStatus,
        orderTime = OrderTime $ rideBooking.createdAt,
        reallocationsCount = rideBooking.reallocationsCount
      }
  where
    castToRideStatus = \case
      SRB.NEW -> New
      SRB.CONFIRMED -> Confirmed
      SRB.AWAITING_REASSIGNMENT -> AwaitingReassignment
      SRB.TRIP_ASSIGNED -> Assigned
      SRB.COMPLETED -> Completed
      SRB.CANCELLED -> Cancelled
