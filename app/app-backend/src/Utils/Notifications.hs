module Utils.Notifications where

import qualified Beckn.External.FCM.Flow as FCM
import Beckn.External.FCM.Types as FCM
import Beckn.Types.Core.Taxi.Cancel.CancellationSource (CancellationSource (..))
import Beckn.Types.Error
import Beckn.Types.Id
import qualified Data.Text as T
import EulerHS.Prelude
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.RideBooking as QRB
import Types.Metrics
import Types.Storage.Person as Person
import Types.Storage.RegistrationToken as RegToken
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRB
import Types.Storage.SearchRequest as SearchRequest
import Utils.Common

-- Note:
-- When customer searches case is created in the BA, and search request is
-- sent to BP, which creates a case in the BP also. When someone responds to
-- that saying, they can offer this ride, onSearch is called to BP for each
-- of these and product/caseProduct is created. Then the customer would
-- confirm one of these product. Which would be basically choosing on of
-- the offers. Only these are cancellable in the BP, which needs to send
-- a notification in BA as a part of onCancel. Similary, when BA is cancelling,
-- cancel should send a notification to the provider who had the ride.
-- This will be basically cancelling the product/caseProduct. Here, when the
-- onCancel comes, it would be ideal not to send a notification in BA. But
-- it's also okay to send this in the first cut. The BA also has a case
-- cancellation flow, which basically cancels the case with or without products.
-- If the case with product is being cancelled, you have to send notification
-- in the BP for each product. Here it would be mostly one product again.
-- When case doesn't have any product, there is no notification.
notifyOnStatusUpdate ::
  ( DBFlow m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  SRide.Ride ->
  SRide.RideStatus ->
  m ()
notifyOnStatusUpdate ride rideStatus =
  when (ride.status /= rideStatus) $ do
    rideBooking <- QRB.findById (ride.bookingId) >>= fromMaybeM RideBookingNotFound
    org <- QOrg.findOrganizationById rideBooking.providerId >>= fromMaybeM OrgNotFound
    let personId = rideBooking.requestorId
        rideId = ride.id
        providerName = org.name
        driverName = ride.driverName
    person <- Person.findById personId
    case person of
      Just p -> case rideStatus of
        SRide.CANCELLED -> do
          let notificationData =
                FCM.FCMAndroidData
                  { fcmNotificationType = FCM.CANCELLED_PRODUCT,
                    fcmShowNotification = FCM.SHOW,
                    fcmEntityType = FCM.Product,
                    fcmEntityIds = show $ getId rideId,
                    fcmNotificationJSON = FCM.createAndroidNotification title body FCM.CANCELLED_PRODUCT
                  }
              title = FCMNotificationTitle $ T.pack "Ride cancelled!"
              body =
                FCMNotificationBody $
                  unwords
                    [ providerName,
                      "had to cancel your ride for",
                      showTimeIst (rideBooking.startTime) <> ".",
                      "Check the app for more details."
                    ]
          FCM.notifyPerson notificationData $ FCM.FCMNotificationRecipient p.id.getId p.deviceToken
        SRide.INPROGRESS -> do
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
          FCM.notifyPerson notificationData $ FCM.FCMNotificationRecipient p.id.getId p.deviceToken
        SRide.COMPLETED -> do
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
          FCM.notifyPerson notificationData $ FCM.FCMNotificationRecipient p.id.getId p.deviceToken
        _ -> pure ()
      _ -> pure ()

notifyOnExpiration ::
  ( FCMFlow m r,
    DBFlow m r,
    CoreMetrics m
  ) =>
  SearchRequest ->
  m ()
notifyOnExpiration searchReq = do
  let searchRequestId = searchReq.id
  let personId = searchReq.requestorId
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

notifyOnCancel :: (CoreMetrics m, FCMFlow m r, DBFlow m r) => SRB.RideBooking -> Id Person -> Maybe FCM.FCMRecipientToken -> CancellationSource -> m ()
notifyOnCancel rideBooking personId mbDeviceToken cancellationSource = do
  org <- QOrg.findOrganizationById (rideBooking.providerId) >>= fromMaybeM OrgNotFound
  FCM.notifyPerson (notificationData $ org.name) $ FCM.FCMNotificationRecipient personId.getId mbDeviceToken
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
