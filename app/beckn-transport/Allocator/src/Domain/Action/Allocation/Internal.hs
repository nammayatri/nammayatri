module Domain.Action.Allocation.Internal where

import qualified Beckn.External.FCM.Types as FCM
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Data.Text as T
import Domain.Action.Allocation as Alloc
import Domain.Types.AllocationEvent (AllocationEventType)
import Domain.Types.Booking (Booking)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.DriverInformation as SDriverInfo
import Domain.Types.DriverPool
import qualified Domain.Types.NotificationStatus as SNS
import Domain.Types.Organization
import Domain.Types.Person (Driver)
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideRequest as SRR
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Servant.Client (BaseUrl (..))
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.DriverPool as DrPool
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Organization as QOrg
import Storage.Queries.AllocationEvent (logAllocationEvent)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.NotificationStatus as QNS
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideRequest as QRR
import Tools.Error
import Tools.Metrics (CoreMetrics)
import Tools.Notifications
import qualified Tools.Notifications as Notify

getDriverSortMode :: Flow SortMode
getDriverSortMode = asks (.defaultSortMode)

getConfiguredNotificationTime :: Flow Seconds
getConfiguredNotificationTime = asks (.driverNotificationExpiry)

getConfiguredAllocationTime :: Flow Seconds
getConfiguredAllocationTime = asks (.rideAllocationExpiry)

getConfiguredReallocationsLimit :: Flow Int
getConfiguredReallocationsLimit = asks (.reallocationsLimit)

getDriverPool :: Id Booking -> Flow SortedDriverPool
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
  Id Booking ->
  Id Driver ->
  Flow ()
assignDriver bookingId driverId = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  driver <-
    QP.findById (cast driverId)
      >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  ride <- buildRide booking
  Esq.runTransaction $ do
    QDI.updateOnRide (cast driver.id) True
    QRB.updateStatus bookingId SRB.TRIP_ASSIGNED
    QRide.create ride
    QBE.logDriverAssignetEvent driverId bookingId ride.id
  fork "assignDriver - Notify BAP" $ do
    uBooking <- QRB.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
    BP.sendRideAssignedUpdateToBAP uBooking ride
    Notify.notifyDriver notificationType notificationTitle (message uBooking) driver.id driver.deviceToken
  where
    notificationType = FCM.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message ride =
      unwords
        [ "You have been assigned a ride for",
          showTimeIst ride.startTime <> ".",
          "Check the app for more details."
        ]
    buildRide booking = do
      guid <- generateGUID
      trackingUrl <- buildTrackingUrl guid
      shortId <- generateShortId
      otp <- generateOTPCode
      now <- getCurrentTime
      return
        SRide.Ride
          { id = guid,
            bookingId = booking.id,
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
            tripStartPos = Nothing,
            tripEndPos = Nothing,
            rideRating = Nothing,
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
            bookingId = req.bookingId,
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
  Id Booking ->
  Flow [CurrentNotification]
getCurrentNotifications bookingId = do
  notificationStatuses <- QNS.findActiveNotificationByRBId bookingId
  pure $ map buildCurrentNotification notificationStatuses
  where
    buildCurrentNotification notificationStatus =
      Alloc.CurrentNotification
        (notificationStatus.driverId)
        (notificationStatus.expiresAt)

cleanupOldNotifications :: Flow Int
cleanupOldNotifications = Esq.runTransaction QNS.cleanupOldNotifications

sendNewRideNotifications ::
  Id Booking ->
  NonEmpty (Id Driver) ->
  Flow ()
sendNewRideNotifications bookingId = traverse_ sendNewRideNotification
  where
    sendNewRideNotification driverId = do
      booking <- QRB.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
      person <-
        QP.findById (cast driverId)
          >>= fromMaybeM (PersonNotFound driverId.getId)
      notifyDriverNewAllocation booking.id person.id person.deviceToken

sendRideNotAssignedNotification ::
  Id Booking ->
  Id Driver ->
  Flow ()
sendRideNotAssignedNotification bookingId driverId = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
  person <-
    QP.findById (cast driverId)
      >>= fromMaybeM (PersonNotFound driverId.getId)
  notifyRideNotAssigned booking.id person.id person.deviceToken

updateNotificationStatuses :: EsqDBFlow m r => Id Booking -> NotificationStatus -> NonEmpty (Id Driver) -> m ()
updateNotificationStatuses bookingId status driverIds = do
  let storageStatus = allocNotifStatusToStorageStatus status
  Esq.runTransaction $ QNS.updateStatus bookingId storageStatus $ toList driverIds

resetLastRejectionTimes :: EsqDBFlow m r => NonEmpty (Id Driver) -> m ()
resetLastRejectionTimes driverIds = Esq.runTransaction . QDS.updateIdleTimes $ toList driverIds

getAttemptedDrivers :: EsqDBFlow m r => Id Booking -> m [Id Driver]
getAttemptedDrivers bookingId =
  QNS.fetchAttemptedNotificationsByRBId bookingId <&> map (.driverId)

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

cancelBooking ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id SRB.Booking ->
  SBCR.BookingCancellationReason ->
  m ()
cancelBooking bookingId reason = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
  mbRide <- QRide.findActiveByRBId bookingId
  let transporterId = booking.providerId
  transporter <-
    QOrg.findById transporterId
      >>= fromMaybeM (OrgNotFound transporterId.getId)
  Esq.runTransaction $ do
    QRB.updateStatus booking.id SRB.CANCELLED
    QBCR.create reason
    whenJust mbRide $ \ride -> do
      QRide.updateStatus ride.id SRide.CANCELLED
      QDriverInfo.updateOnRide (cast ride.driverId) False
  logTagInfo ("bookingId-" <> getId bookingId) ("Cancellation reason " <> show reason.source)
  fork "cancelBooking - Notify BAP" $ do
    BP.sendBookingCancelledUpdateToBAP booking transporter reason.source
  whenJust mbRide $ \ride ->
    fork "cancelRide - Notify driver" $ do
      driver <- QP.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      Notify.notifyOnCancel booking driver.id driver.deviceToken reason.source

cleanupNotifications :: EsqDBFlow m r => Id Booking -> m ()
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

addAllocationRequest :: EsqDBFlow m r => ShortId Organization -> Id Booking -> m ()
addAllocationRequest shortOrgId bookingId = do
  guid <- generateGUID
  currTime <- getCurrentTime
  let rideRequest =
        SRR.RideRequest
          { id = Id guid,
            bookingId = bookingId,
            shortOrgId = shortOrgId,
            createdAt = currTime,
            _type = SRR.ALLOCATION,
            info = Nothing
          }
  Esq.runTransaction $ QRR.create rideRequest

addNotificationStatuses ::
  EsqDBFlow m r =>
  Id Booking ->
  NonEmpty (Id Driver) ->
  UTCTime ->
  m ()
addNotificationStatuses bookingId driverIds expiryTime = do
  notificationStatuses <- traverse createNotificationStatus driverIds
  Esq.runTransaction $ QNS.createMany $ toList notificationStatuses
  where
    createNotificationStatus driverId = do
      uuid <- generateGUID
      pure $
        SNS.NotificationStatus
          { id = Id uuid,
            bookingId = bookingId,
            driverId = driverId,
            status = SNS.NOTIFIED,
            expiresAt = expiryTime
          }

addAvailableDriver :: SDriverInfo.DriverInformation -> [Id Driver] -> [Id Driver]
addAvailableDriver driverInfo availableDriversIds =
  if driverInfo.active && not (driverInfo.onRide)
    then cast (driverInfo.driverId) : availableDriversIds
    else availableDriversIds

logEvent :: EsqDBFlow m r => AllocationEventType -> Id Booking -> m ()
logEvent eventType bookingId = Esq.runTransaction $ logAllocationEvent eventType bookingId Nothing

logDriverEvents :: EsqDBFlow m r => AllocationEventType -> Id Booking -> NonEmpty (Id Driver) -> m ()
logDriverEvents eventType bookingId driverList = Esq.runTransaction $ traverse_ logDriverEvent driverList
  where
    logDriverEvent driver = logAllocationEvent eventType bookingId $ Just driver

-- TODO: We don't need RideInfo anymore, we can just use Booking directly. Remove this.
getRideInfo :: EsqDBFlow m r => Id Booking -> m RideInfo
getRideInfo bookingId = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
  let rideStatus = castToRideStatus $ booking.status
  pure
    RideInfo
      { bookingId = bookingId,
        rideStatus = rideStatus,
        orderTime = OrderTime $ booking.createdAt,
        reallocationsCount = booking.reallocationsCount
      }
  where
    castToRideStatus = \case
      SRB.NEW -> New
      SRB.CONFIRMED -> Confirmed
      SRB.AWAITING_REASSIGNMENT -> AwaitingReassignment
      SRB.TRIP_ASSIGNED -> Assigned
      SRB.COMPLETED -> Completed
      SRB.CANCELLED -> Cancelled
