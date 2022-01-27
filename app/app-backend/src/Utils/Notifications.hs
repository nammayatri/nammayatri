module Utils.Notifications where

import qualified Beckn.External.FCM.Flow as FCM
import Beckn.External.FCM.Types as FCM
import Beckn.Types.Core.Taxi.Common.CancellationSource (CancellationSource (..))
import Beckn.Types.Error
import Beckn.Types.Id
import qualified Data.Text as T
import Domain.Types.Person as Person
import Domain.Types.RegistrationToken as RegToken
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import Domain.Types.SearchRequest as SearchRequest
import EulerHS.Prelude
import qualified Storage.Queries.Person as Person
import Types.Metrics
import Utils.Common

notifyOnRideAssigned ::
  ( EsqDBFlow m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SRide.Ride ->
  m ()
notifyOnRideAssigned rideBooking ride = do
  let personId = rideBooking.riderId
      rideId = ride.id
      driverName = ride.driverName
  person <- Person.findById personId >>= fromMaybeM PersonNotFound
  let notificationData =
        FCM.FCMAndroidData
          { fcmNotificationType = FCM.DRIVER_ASSIGNMENT,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = show $ getId rideId,
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.DRIVER_ASSIGNMENT
          }
      title = FCMNotificationTitle $ T.pack "Driver assigned!"
      body =
        FCMNotificationBody $
          unwords
            [ driverName,
              "will be your driver for this trip."
            ]
  FCM.notifyPerson notificationData $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken

notifyOnRideStarted ::
  ( EsqDBFlow m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SRide.Ride ->
  m ()
notifyOnRideStarted rideBooking ride = do
  let personId = rideBooking.riderId
      rideId = ride.id
      driverName = ride.driverName
  person <- Person.findById personId >>= fromMaybeM PersonNotFound
  let notificationData =
        FCM.FCMAndroidData
          { fcmNotificationType = FCM.TRIP_STARTED,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = show $ getId rideId,
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.TRIP_STARTED
          }
      title = FCMNotificationTitle $ T.pack "Trip started!"
      body =
        FCMNotificationBody $
          unwords
            [ driverName,
              "has started your trip. Please enjoy the ride!"
            ]
  FCM.notifyPerson notificationData $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken

notifyOnRideCompleted ::
  ( EsqDBFlow m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SRide.Ride ->
  m ()
notifyOnRideCompleted rideBooking ride = do
  let personId = rideBooking.riderId
      rideId = ride.id
      driverName = ride.driverName
  person <- Person.findById personId >>= fromMaybeM PersonNotFound
  let notificationData =
        FCM.FCMAndroidData
          { fcmNotificationType = FCM.TRIP_FINISHED,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = show $ getId rideId,
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.TRIP_FINISHED
          }
      title = FCMNotificationTitle $ T.pack "Trip finished!"
      body =
        FCMNotificationBody $
          unwords
            [ "Hope you enjoyed your trip with",
              driverName
            ]
  FCM.notifyPerson notificationData $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken

notifyOnExpiration ::
  ( FCMFlow m r,
    EsqDBFlow m r,
    CoreMetrics m
  ) =>
  SearchRequest ->
  m ()
notifyOnExpiration searchReq = do
  let searchRequestId = searchReq.id
  let personId = searchReq.riderId
  person <- Person.findById personId
  case person of
    Just p -> do
      let notificationData =
            FCM.FCMAndroidData
              { fcmNotificationType = FCM.EXPIRED_CASE,
                fcmShowNotification = FCM.SHOW,
                fcmEntityType = FCM.SearchRequest,
                fcmEntityIds = show $ getId searchRequestId,
                fcmNotificationJSON = FCM.createAndroidNotification title body FCM.EXPIRED_CASE
              }
          title = FCMNotificationTitle $ T.pack "Ride expired!"
          body =
            FCMNotificationBody $
              unwords
                [ "Your ride has expired as you did not confirm any offer.",
                  "Please book again to continue."
                ]
      FCM.notifyPerson notificationData $ FCM.FCMNotificationRecipient p.id.getId p.deviceToken
    _ -> pure ()

notifyOnRegistration ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  RegistrationToken ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyOnRegistration regToken personId mbDeviceToken =
  let tokenId = RegToken.id regToken
      notificationData =
        FCM.FCMAndroidData
          { fcmNotificationType = FCM.REGISTRATION_APPROVED,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Organization,
            fcmEntityIds = show tokenId,
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.REGISTRATION_APPROVED
          }
      title = FCMNotificationTitle $ T.pack "Registration Completed!"
      body =
        FCMNotificationBody $
          unwords
            [ "Welcome to Yatri.",
              "Click here to book your first ride with us."
            ]
   in FCM.notifyPerson notificationData $ FCM.FCMNotificationRecipient personId.getId mbDeviceToken

notifyOnRideBookingCancelled :: (CoreMetrics m, FCMFlow m r, EsqDBFlow m r) => SRB.RideBooking -> CancellationSource -> m ()
notifyOnRideBookingCancelled rideBooking cancellationSource = do
  person <- Person.findById rideBooking.riderId >>= fromMaybeM PersonNotFound
  FCM.notifyPerson (notificationData $ rideBooking.providerName) $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken
  where
    notificationData orgName =
      FCM.FCMAndroidData
        { fcmNotificationType = FCM.CANCELLED_PRODUCT,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = show $ getId rideBooking.requestId,
          fcmNotificationJSON = FCM.createAndroidNotification title (body orgName) FCM.CANCELLED_PRODUCT
        }
    title = FCMNotificationTitle $ T.pack "Ride cancelled!"
    body orgName =
      FCMNotificationBody $ getCancellationText orgName
    -- reasonMsg = encodeToText reason
    getCancellationText orgName = case cancellationSource of
      ByUser ->
        unwords
          [ "You have cancelled your ride for",
            showTimeIst (rideBooking.startTime) <> ".",
            "Check the app for details."
          ]
      ByOrganization ->
        unwords
          [ "\"" <> orgName <> "\" agency had to cancel the ride for",
            showTimeIst (rideBooking.startTime) <> ".",
            "Please book again to get another ride."
          ]
      ByDriver ->
        unwords
          [ "The driver had to cancel the ride for",
            showTimeIst (rideBooking.startTime) <> ".",
            "Please book again to get another ride."
          ]
      ByAllocator ->
        unwords
          [ "The ride for",
            showTimeIst (rideBooking.startTime),
            "was cancelled as we could not find a driver.",
            "Please book again to get another ride."
          ]

notifyOnRideBookingReallocated :: (CoreMetrics m, FCMFlow m r, EsqDBFlow m r) => SRB.RideBooking -> CancellationSource -> m ()
notifyOnRideBookingReallocated rideBooking cancellationSource = do
  person <- Person.findById rideBooking.riderId >>= fromMaybeM PersonNotFound
  notificationData <- buildNotificationData
  FCM.notifyPerson notificationData $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken
  where
    buildNotificationData = do
      body <- buildBody
      return $
        FCM.FCMAndroidData
          { fcmNotificationType = FCM.REALLOCATE_PRODUCT,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = show $ getId rideBooking.requestId,
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.REALLOCATE_PRODUCT
          }
    title = FCMNotificationTitle $ T.pack "Ride cancelled!"
    buildBody = do
      FCMNotificationBody <$> getReallocationText
    getReallocationText = case cancellationSource of
      ByDriver ->
        return $
          unwords
            [ "The driver had to cancel the ride for",
              showTimeIst (rideBooking.startTime) <> ".",
              "Please wait until we allocate other driver."
            ]
      _ -> throwError $ InternalError "Ride reallocation started when cancelled not by driver, which supposed to be impossible."
