{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Domain.Action.Allocation.Internal
  ( getConfiguredNotificationTime,
    getConfiguredAllocationTime,
    getConfiguredReallocationsLimit,
    getRequests,
    assignDriver,
    toRideRequest,
    getCurrentNotifications,
    cleanupOldNotifications,
    sendNewRideNotifications,
    sendRideNotAssignedNotification,
    updateNotificationStatuses,
    resetLastRejectionTimes,
    getDriversWithNotification,
    getTopDriversByIdleTime,
    checkAvailability,
    cancelBooking,
    cleanupNotifications,
    removeRequest,
    allocNotifStatusToStorageStatus,
    addAllocationRequest,
    addNotificationStatuses,
    addAvailableDriver,
    logEvent,
    logDriverEvents,
    getBooking,
    incrementTaskCounter,
    incrementFailedTaskCounter,
    putTaskDuration,
    findAllocatorFCMConfigByMerchantId,
    module Reexport,
  )
where

import qualified Data.Text as T
import qualified Domain.Action.Allocation as Alloc
import Domain.Action.Allocation.Internal.DriverPool as Reexport
import Domain.Types.AllocationEvent (AllocationEventType)
import Domain.Types.Booking (Booking)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.DriverInformation as SDriverInfo
import Domain.Types.Merchant
import qualified Domain.Types.NotificationStatus as SNS
import Domain.Types.Person (Driver)
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideDetails as SRD
import qualified Domain.Types.RideRequest as SRR
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import qualified Kernel.External.FCM.Types as FCM
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant.Client (BaseUrl (..))
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.DriverLocation as SDrLoc
import SharedLogic.DriverPool.Types
import qualified SharedLogic.Ride as SRide
import SharedLogic.TransporterConfig
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as CQM
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
import qualified Storage.Queries.RideDetails as QRD
import qualified Storage.Queries.RideRequest as QRR
import qualified Storage.Queries.Vehicle as QV
import Tools.Error
import Tools.Metrics (CoreMetrics)
import qualified Tools.Metrics.AllocatorMetrics as TMetrics
import Tools.Notifications
import qualified Tools.Notifications as Notify

getConfiguredNotificationTime :: Flow Seconds
getConfiguredNotificationTime = asks (.driverNotificationExpiry)

getConfiguredAllocationTime :: Flow Seconds
getConfiguredAllocationTime = asks (.rideAllocationExpiry)

getConfiguredReallocationsLimit :: Flow Int
getConfiguredReallocationsLimit = asks (.reallocationsLimit)

getRequests :: ShortId Subscriber -> Integer -> Flow [Alloc.RideRequest]
getRequests subscriberId numRequests = do
  allRequests <- QRR.fetchOldest subscriberId numRequests (Proxy @Flow)
  let (errors, requests) =
        partitionEithers $ map toRideRequest allRequests
  traverse_ logError errors
  pure requests

assignDriver ::
  Id Booking ->
  Id Driver ->
  Flow ()
assignDriver bookingId driverId = do
  booking <- QRB.findById bookingId (Proxy @Flow) >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  driver <-
    QP.findById (cast driverId) (Proxy @Flow)
      >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  ride <- buildRide booking
  rideDetails <- buildRideDetails ride driver
  Esq.runTransaction $ do
    QDI.updateOnRide @Flow (cast driver.id) True
    QRB.updateStatus bookingId SRB.TRIP_ASSIGNED
    QRide.create ride
    QRD.create rideDetails
    QBE.logDriverAssignetEvent driverId bookingId ride.id
  fork "assignDriver - Notify BAP" $ do
    uBooking <- QRB.findById bookingId (Proxy @Flow) >>= fromMaybeM (BookingNotFound bookingId.getId)
    BP.sendRideAssignedUpdateToBAP uBooking ride
    fcmConfig <- findAllocatorFCMConfigByMerchantId booking.providerId
    Notify.notifyDriver fcmConfig notificationType notificationTitle (message uBooking) driver.id driver.deviceToken
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
            driverArrivalTime = Nothing,
            tripStartTime = Nothing,
            tripEndTime = Nothing,
            tripStartPos = Nothing,
            tripEndPos = Nothing,
            rideRating = Nothing,
            createdAt = now,
            updatedAt = now
          }

    buildRideDetails ride driver = do
      vehicle <-
        QV.findById ride.driverId (Proxy @Flow)
          >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
      return
        SRD.RideDetails
          { id = ride.id,
            driverName = driver.firstName,
            driverNumber = driver.mobileNumber,
            driverCountryCode = driver.mobileCountryCode,
            vehicleNumber = vehicle.registrationNo,
            vehicleColor = Just vehicle.color,
            vehicleVariant = Just vehicle.variant,
            vehicleModel = Just vehicle.model,
            vehicleClass = Nothing
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
          Just driverResponse -> Right . Alloc.DriverResponse $ castDriverResponse driverResponse
          Nothing -> Left "Driver info is not present "
    castDriverResponse SRR.DriverResponse {..} = do
      let response = case status of
            SRR.ACCEPT -> Alloc.Accept
            SRR.REJECT -> Alloc.Reject
      Alloc.DriverResponseType {..}

getCurrentNotifications ::
  Id Booking ->
  Flow [Alloc.CurrentNotification]
getCurrentNotifications bookingId = do
  notificationStatuses <- QNS.findActiveNotificationByRBId bookingId (Proxy @Flow)
  pure $ map buildCurrentNotification notificationStatuses
  where
    buildCurrentNotification notificationStatus =
      Alloc.CurrentNotification
        (notificationStatus.driverId)
        (notificationStatus.expiresAt)

cleanupOldNotifications :: Flow Int
cleanupOldNotifications = Esq.runTransaction (QNS.cleanupOldNotifications @Flow)

sendNewRideNotifications ::
  Id Booking ->
  NonEmpty (Id Driver) ->
  Flow ()
sendNewRideNotifications bookingId = traverse_ sendNewRideNotification
  where
    sendNewRideNotification driverId = do
      booking <- QRB.findById bookingId (Proxy @Flow) >>= fromMaybeM (BookingNotFound bookingId.getId)
      person <-
        QP.findById (cast driverId) (Proxy @Flow)
          >>= fromMaybeM (PersonNotFound driverId.getId)
      fcmConfig <- findAllocatorFCMConfigByMerchantId person.merchantId
      notifyDriverNewAllocation fcmConfig booking.id person.id person.deviceToken

sendRideNotAssignedNotification ::
  Id Booking ->
  Id Driver ->
  Flow ()
sendRideNotAssignedNotification bookingId driverId = do
  booking <- QRB.findById bookingId (Proxy @Flow) >>= fromMaybeM (BookingNotFound bookingId.getId)
  person <-
    QP.findById (cast driverId) (Proxy @Flow)
      >>= fromMaybeM (PersonNotFound driverId.getId)
  fcmConfig <- findAllocatorFCMConfigByMerchantId person.merchantId
  notifyRideNotAssigned fcmConfig booking.id person.id person.deviceToken

updateNotificationStatuses :: forall m r. EsqDBFlow m r => Id Booking -> Alloc.NotificationStatus -> NonEmpty (Id Driver) -> m ()
updateNotificationStatuses bookingId status driverIds = do
  let storageStatus = allocNotifStatusToStorageStatus status
  Esq.runTransaction $ QNS.updateStatus @m bookingId storageStatus $ toList driverIds

resetLastRejectionTimes :: forall m r. EsqDBFlow m r => NonEmpty (Id Driver) -> m ()
resetLastRejectionTimes driverIds = Esq.runTransaction . QDS.updateIdleTimes @m $ toList driverIds

getDriversWithNotification :: forall m r. EsqDBFlow m r => m [Id Driver]
getDriversWithNotification = QNS.fetchActiveNotifications (Proxy @m) <&> fmap (.driverId)

getTopDriversByIdleTime :: forall m r. EsqDBFlow m r => Int -> [Id Driver] -> m [Id Driver]
getTopDriversByIdleTime a b = QDS.getTopDriversByIdleTime a b (Proxy @m)

checkAvailability :: forall m r. EsqDBFlow m r => [DriverPoolResult] -> m [DriverPoolResult]
checkAvailability driverPool = do
  let driverIds = (.driverId) <$> driverPool
  driversInfo <- QDriverInfo.fetchAllAvailableByIds driverIds (Proxy @m)
  let availableDriverIds = map (cast . SDriverInfo.driverId) driversInfo
  -- availableDriverIds is part of driverPool, but isn't sorted by distance
  -- So we can use order that we have in sorted pool driverPool
  pure $ filter (\dpRes -> dpRes.driverId `elem` availableDriverIds) driverPool

cancelBooking ::
  forall m r c.
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasLongDurationRetryCfg r c,
    CoreMetrics m
  ) =>
  Id SRB.Booking ->
  SBCR.BookingCancellationReason ->
  m ()
cancelBooking bookingId reason = do
  booking <- QRB.findById bookingId (Proxy @m) >>= fromMaybeM (BookingNotFound bookingId.getId)
  mbRide <- QRide.findActiveByRBId bookingId (Proxy @m)
  let transporterId = booking.providerId
  transporter <-
    CQM.findById transporterId
      >>= fromMaybeM (MerchantNotFound transporterId.getId)
  Esq.runTransaction $ do
    QRB.updateStatus @m booking.id SRB.CANCELLED
    QBCR.upsert reason
    whenJust mbRide $ \ride -> do
      QRide.updateStatus ride.id SRide.CANCELLED
      QDriverInfo.updateOnRide (cast ride.driverId) False
  whenJust mbRide $ \ride -> do
    SRide.clearCache $ cast ride.driverId
    SDrLoc.clearDriverInfoCache $ cast ride.driverId
  logTagInfo ("bookingId-" <> getId bookingId) ("Cancellation reason " <> show reason.source)
  fork "cancelBooking - Notify BAP" $ do
    BP.sendBookingCancelledUpdateToBAP booking transporter reason.source
  whenJust mbRide $ \ride ->
    fork "cancelRide - Notify driver" $ do
      driver <- QP.findById ride.driverId (Proxy @m) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      fcmConfig <- findAllocatorFCMConfigByMerchantId driver.merchantId
      Notify.notifyOnCancel fcmConfig booking driver.id driver.deviceToken reason.source

cleanupNotifications :: forall m r. EsqDBFlow m r => Id Booking -> m ()
cleanupNotifications = Esq.runTransaction . QNS.cleanupNotifications @m

removeRequest :: forall m r. EsqDBFlow m r => Id SRR.RideRequest -> m ()
removeRequest = Esq.runTransaction . QRR.removeRequest @m

allocNotifStatusToStorageStatus ::
  Alloc.NotificationStatus ->
  SNS.AnswerStatus
allocNotifStatusToStorageStatus = \case
  Alloc.Notified -> SNS.NOTIFIED
  Alloc.Rejected -> SNS.REJECTED
  Alloc.Ignored -> SNS.IGNORED

addAllocationRequest :: forall m r. EsqDBFlow m r => ShortId Subscriber -> Id Booking -> m ()
addAllocationRequest subscriberId bookingId = do
  guid <- generateGUID
  currTime <- getCurrentTime
  let rideRequest =
        SRR.RideRequest
          { id = Id guid,
            bookingId = bookingId,
            subscriberId = subscriberId,
            createdAt = currTime,
            _type = SRR.ALLOCATION,
            info = Nothing
          }
  Esq.runTransaction $ QRR.create @m rideRequest

addNotificationStatuses ::
  forall m r.
  EsqDBFlow m r =>
  Id Booking ->
  NonEmpty (Id Driver) ->
  UTCTime ->
  m ()
addNotificationStatuses bookingId driverIds expiryTime = do
  notificationStatuses <- traverse createNotificationStatus driverIds
  Esq.runTransaction $ QNS.createMany @m $ toList notificationStatuses
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

logEvent :: forall m r. EsqDBFlow m r => AllocationEventType -> Id Booking -> m ()
logEvent eventType bookingId = Esq.runTransaction $ logAllocationEvent @m eventType bookingId Nothing

logDriverEvents :: forall m r. EsqDBFlow m r => AllocationEventType -> Id Booking -> NonEmpty (Id Driver) -> m ()
logDriverEvents eventType bookingId driverList = Esq.runTransaction $ traverse_ logDriverEvent driverList
  where
    logDriverEvent driver = logAllocationEvent @m eventType bookingId $ Just driver

getBooking :: forall m r. EsqDBFlow m r => Id Booking -> m Booking
getBooking bookingId = QRB.findById bookingId (Proxy @m) >>= fromMaybeM (BookingNotFound bookingId.getId)

incrementTaskCounter :: (TMetrics.HasAllocatorMetrics m r, CacheFlow m r, EsqDBFlow m r) => ShortId Subscriber -> m ()
incrementTaskCounter subscriberId = do
  org <- CQM.findBySubscriberId subscriberId >>= fromMaybeM (MerchantNotFound ("subscriberId-" <> subscriberId.getShortId))
  TMetrics.incrementTaskCounter org.name

incrementFailedTaskCounter :: (TMetrics.HasAllocatorMetrics m r, CacheFlow m r, EsqDBFlow m r) => ShortId Subscriber -> m ()
incrementFailedTaskCounter subscriberId = do
  org <- CQM.findBySubscriberId subscriberId >>= fromMaybeM (MerchantNotFound ("subscriberId-" <> subscriberId.getShortId))
  TMetrics.incrementFailedTaskCounter org.name

putTaskDuration :: (TMetrics.HasAllocatorMetrics m r, CacheFlow m r, EsqDBFlow m r) => ShortId Subscriber -> Milliseconds -> m ()
putTaskDuration subscriberId ms = do
  org <- CQM.findBySubscriberId subscriberId >>= fromMaybeM (MerchantNotFound ("subscriberId-" <> subscriberId.getShortId))
  TMetrics.putTaskDuration org.name ms

findAllocatorFCMConfigByMerchantId :: (MonadFlow m, HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id Merchant -> m FCM.FCMConfig
findAllocatorFCMConfigByMerchantId merchantId = do
  fcmConfig <- findFCMConfigByMerchantId merchantId
  pure fcmConfig{FCM.fcmTokenKeyPrefix = "transporter-allocator"}
