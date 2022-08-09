module Utils.Notifications where

import qualified Beckn.External.FCM.Flow as FCM
import Beckn.External.FCM.Types as FCM
import Beckn.Types.Error
import Beckn.Types.Id
import qualified Data.Text as T
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import Domain.Types.Estimate (Estimate)
import Domain.Types.Person as Person
import Domain.Types.Quote (makeQuoteAPIEntity)
import qualified Domain.Types.Quote as DQuote
import Domain.Types.RegistrationToken as RegToken
import qualified Domain.Types.Ride as SRide
import Domain.Types.SearchRequest as SearchRequest
import EulerHS.Prelude
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.SearchRequest as QSearchReq
import Tools.Metrics
import Utils.Common

notifyOnDriverOfferIncoming ::
  ( EsqDBFlow m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id Estimate ->
  [DQuote.Quote] ->
  Person.Person ->
  m ()
notifyOnDriverOfferIncoming estimateId quotes person = do
  let notificationData =
        FCM.FCMData
          { fcmNotificationType = FCM.DRIVER_QUOTE_INCOMING,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = estimateId.getId,
            fcmEntityData = map makeQuoteAPIEntity quotes,
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.DRIVER_QUOTE_INCOMING
          }
      title = FCMNotificationTitle "New driver offers incoming!"
      body =
        FCMNotificationBody $
          unwords
            [ "There are new driver offers!",
              "Check the app for details"
            ]
  FCM.notifyPerson notificationData $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken

notifyOnRideAssigned ::
  ( EsqDBFlow m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnRideAssigned booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let notificationData =
        FCM.FCMData
          { fcmNotificationType = FCM.DRIVER_ASSIGNMENT,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = getId rideId,
            fcmEntityData = (),
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
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnRideStarted booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let notificationData =
        FCM.FCMData
          { fcmNotificationType = FCM.TRIP_STARTED,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = getId rideId,
            fcmEntityData = (),
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
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnRideCompleted booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let notificationData =
        FCM.FCMData
          { fcmNotificationType = FCM.TRIP_FINISHED,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = getId rideId,
            fcmEntityData = (),
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
            FCM.FCMData
              { fcmNotificationType = FCM.EXPIRED_CASE,
                fcmShowNotification = FCM.SHOW,
                fcmEntityType = FCM.SearchRequest,
                fcmEntityIds = getId searchRequestId,
                fcmEntityData = (),
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
        FCM.FCMData
          { fcmNotificationType = FCM.REGISTRATION_APPROVED,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Organization,
            fcmEntityIds = getId tokenId,
            fcmEntityData = (),
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

notifyOnBookingCancelled :: (CoreMetrics m, FCMFlow m r, EsqDBFlow m r) => SRB.Booking -> SBCR.CancellationSource -> m ()
notifyOnBookingCancelled booking cancellationSource = do
  person <- Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  FCM.notifyPerson (notificationData $ booking.providerName) $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken
  where
    notificationData orgName =
      FCM.FCMData
        { fcmNotificationType = FCM.CANCELLED_PRODUCT,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = getId booking.id,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title (body orgName) FCM.CANCELLED_PRODUCT
        }
    title = FCMNotificationTitle $ T.pack "Ride cancelled!"
    body orgName =
      FCMNotificationBody $ getCancellationText orgName
    -- reasonMsg = encodeToText reason
    getCancellationText orgName = case cancellationSource of
      SBCR.ByUser ->
        unwords
          [ "You have cancelled your ride for",
            showTimeIst (booking.startTime) <> ".",
            "Check the app for details."
          ]
      SBCR.ByOrganization ->
        unwords
          [ "\"" <> orgName <> "\" agency had to cancel the ride for",
            showTimeIst (booking.startTime) <> ".",
            "Please book again to get another ride."
          ]
      SBCR.ByDriver ->
        unwords
          [ "The driver had to cancel the ride for",
            showTimeIst (booking.startTime) <> ".",
            "Please book again to get another ride."
          ]
      SBCR.ByAllocator ->
        unwords
          [ "The ride for",
            showTimeIst (booking.startTime),
            "was cancelled as we could not find a driver.",
            "Please book again to get another ride."
          ]
      SBCR.ByApplication ->
        unwords
          [ "The ride for",
            showTimeIst (booking.startTime),
            "was cancelled because quote was not confirmed.",
            "Please book again to get another ride."
          ]

notifyOnBookingReallocated :: (CoreMetrics m, FCMFlow m r, EsqDBFlow m r) => SRB.Booking -> m ()
notifyOnBookingReallocated booking = do
  person <- Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  notificationData <- buildNotificationData
  FCM.notifyPerson notificationData $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken
  where
    buildNotificationData = do
      body <- buildBody
      return $
        FCM.FCMData
          { fcmNotificationType = FCM.REALLOCATE_PRODUCT,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Product,
            fcmEntityIds = getId booking.id,
            fcmEntityData = (),
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.REALLOCATE_PRODUCT
          }
    title = FCMNotificationTitle $ T.pack "Ride cancelled!"
    buildBody = do
      FCMNotificationBody <$> getReallocationText
    getReallocationText =
      return $
        unwords
          [ "The driver had to cancel the ride for",
            showTimeIst (booking.startTime) <> ".",
            "Please wait until we allocate other driver."
          ]

notifyOnQuoteReceived :: (CoreMetrics m, FCMFlow m r, EsqDBFlow m r) => DQuote.Quote -> m ()
notifyOnQuoteReceived quote = do
  searchRequest <- QSearchReq.findById quote.requestId >>= fromMaybeM (SearchRequestDoesNotExist quote.requestId.getId)
  person <- Person.findById searchRequest.riderId >>= fromMaybeM (PersonNotFound searchRequest.riderId.getId)
  let notificationData = mkNotificationData
  FCM.notifyPerson notificationData $ FCM.FCMNotificationRecipient person.id.getId person.deviceToken
  where
    mkNotificationData = do
      let title = FCMNotificationTitle $ T.pack "Quote received!"
          body =
            FCMNotificationBody $
              unwords
                [ "New quote recived with price",
                  show (quote.estimatedFare) <> "."
                ]
      FCM.FCMData
        { fcmNotificationType = FCM.QUOTE_RECEIVED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = getId quote.requestId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.REALLOCATE_PRODUCT
        }
