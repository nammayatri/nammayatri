module Utils.Notifications where

import qualified Beckn.External.FCM.Flow as FCM
import Beckn.External.FCM.Types as FCM
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Core.Taxi.Common.CancellationSource (CancellationSource (..))
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Data.Text as T
import Domain.Types.Person as Person
import Domain.Types.RegistrationToken as RegToken
import Domain.Types.RideBooking (RideBooking)
import Domain.Types.SearchRequest as SearchRequest
import EulerHS.Prelude

-- | Send FCM "cancel" notification to driver
notifyOnCancel ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  SearchRequest ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  CancellationSource ->
  m ()
notifyOnCancel searchRequest personId mbDeviceToken cancellationSource = do
  cancellationText <- getCancellationText
  FCM.notifyPerson (notificationData cancellationText) $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    searchRequestId = SearchRequest.id searchRequest
    notificationData cancellationText =
      FCM.FCMAndroidData
        { fcmNotificationType = FCM.CANCELLED_PRODUCT,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = show $ getId searchRequestId,
          fcmNotificationJSON = FCM.createAndroidNotification title (body cancellationText) FCM.CANCELLED_PRODUCT
        }
    title = FCMNotificationTitle $ T.pack "Ride cancelled!"
    body text =
      FCMNotificationBody text
    getCancellationText = case cancellationSource of
      ByUser ->
        return $
          unwords
            [ "Customer had to cancel your ride for",
              showTimeIst (searchRequest.startTime) <> ".",
              "Check the app for more details."
            ]
      ByOrganization ->
        return $
          unwords
            [ "Your agency had to cancel the ride for",
              showTimeIst (searchRequest.startTime) <> ".",
              "Check the app for more details."
            ]
      ByDriver ->
        return $
          unwords
            [ "You have cancelled the ride for",
              showTimeIst (searchRequest.startTime) <> ".",
              "Check the app for more details."
            ]
      _ -> throwError (InternalError "Unexpected cancellation reason.")

notifyOnRegistration ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  RegistrationToken ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyOnRegistration regToken personId =
  FCM.notifyPerson notificationData . FCMNotificationRecipient personId.getId
  where
    tokenId = RegToken.id regToken
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
          [ "Welcome Yatri Partner!",
            "Click here to set up your account."
          ]

notifyDriver ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriver = sendNotificationToDriver FCM.SHOW

-- Send notification to device, i.e. notifications that should not be shown to the user,
-- but contains payload used by the app
notifyDevice ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDevice = sendNotificationToDriver FCM.DO_NOT_SHOW

sendNotificationToDriver ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  FCM.FCMShowNotification ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
sendNotificationToDriver displayOption notificationType notificationTitle message driverId =
  FCM.notifyPerson notificationData . FCMNotificationRecipient driverId.getId
  where
    notificationData =
      FCM.FCMAndroidData
        { fcmNotificationType = notificationType,
          fcmShowNotification = displayOption,
          fcmEntityIds = show . getId $ driverId,
          fcmEntityType = FCM.Person,
          fcmNotificationJSON = FCM.createAndroidNotification title body notificationType
        }
    title = FCM.FCMNotificationTitle notificationTitle
    body =
      FCMNotificationBody message

notifyDriverNewAllocation ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  Id rideBookingId ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriverNewAllocation rideBookingId personId =
  FCM.notifyPerson notificationData . FCMNotificationRecipient personId.getId
  where
    title = FCM.FCMNotificationTitle "New allocation request."
    body =
      FCM.FCMNotificationBody $
        unwords
          [ "New ride request!",
            "Check the app for more details."
          ]
    notificationData =
      FCM.FCMAndroidData
        { fcmNotificationType = FCM.ALLOCATION_REQUEST,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = getId rideBookingId,
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.ALLOCATION_REQUEST
        }

notifyRideNotAssigned ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  Id RideBooking ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyRideNotAssigned rideBookingId personId =
  FCM.notifyPerson notificationData . FCMNotificationRecipient personId.getId
  where
    title = FCM.FCMNotificationTitle "Ride not assigned."
    body =
      FCM.FCMNotificationBody $
        unwords
          [ "Could not assign the ride as it is no longer available",
            "Please wait for another request."
          ]
    notificationData =
      FCM.FCMAndroidData
        { fcmNotificationType = FCM.ALLOCATION_REQUEST_UNASSIGNED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = getId rideBookingId,
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.ALLOCATION_REQUEST_UNASSIGNED
        }
