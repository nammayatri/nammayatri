module Utils.Notifications where

import qualified Beckn.External.FCM.Flow as FCM
import Beckn.External.FCM.Types as FCM
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Data.Text as T
import Domain.Types.Booking (Booking)
import qualified Domain.Types.BookingCancellationReason as SBCR
import Domain.Types.Person as Person
import Domain.Types.RegistrationToken as RegToken
import EulerHS.Prelude

-- | Send FCM "cancel" notification to driver
notifyOnCancel ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  Booking ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  SBCR.CancellationSource ->
  m ()
notifyOnCancel booking personId mbDeviceToken cancellationSource = do
  cancellationText <- getCancellationText
  FCM.notifyPerson (notificationData cancellationText) $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notificationData cancellationText =
      FCM.FCMData
        { fcmNotificationType = FCM.CANCELLED_PRODUCT,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = getId booking.id,
          fcmNotificationJSON = FCM.createAndroidNotification title (body cancellationText) FCM.CANCELLED_PRODUCT
        }
    title = FCMNotificationTitle $ T.pack "Ride cancelled!"
    body text =
      FCMNotificationBody text
    getCancellationText = case cancellationSource of
      SBCR.ByUser ->
        return $
          unwords
            [ "Customer had to cancel your ride for",
              showTimeIst (booking.startTime) <> ".",
              "Check the app for more details."
            ]
      SBCR.ByOrganization ->
        return $
          unwords
            [ "Your agency had to cancel the ride for",
              showTimeIst (booking.startTime) <> ".",
              "Check the app for more details."
            ]
      SBCR.ByDriver ->
        return $
          unwords
            [ "You have cancelled the ride for",
              showTimeIst (booking.startTime) <> ".",
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
      FCM.FCMData
        { fcmNotificationType = FCM.REGISTRATION_APPROVED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Organization,
          fcmEntityIds = getId tokenId,
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
notifyDriver = sendNotificationToDriver FCM.SHOW Nothing

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
notifyDevice = sendNotificationToDriver FCM.DO_NOT_SHOW (Just FCM.HIGH)

sendNotificationToDriver ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  FCM.FCMShowNotification ->
  Maybe FCM.FCMAndroidMessagePriority ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
sendNotificationToDriver displayOption priority notificationType notificationTitle message driverId =
  FCM.notifyPersonWithPriority priority notificationData . FCMNotificationRecipient driverId.getId
  where
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notificationType,
          fcmShowNotification = displayOption,
          fcmEntityIds = getId driverId,
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
  Id bookingId ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriverNewAllocation bookingId personId =
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
      FCM.FCMData
        { fcmNotificationType = FCM.ALLOCATION_REQUEST,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = getId bookingId,
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.ALLOCATION_REQUEST
        }

notifyRideNotAssigned ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  Id Booking ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyRideNotAssigned bookingId personId =
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
      FCM.FCMData
        { fcmNotificationType = FCM.ALLOCATION_REQUEST_UNASSIGNED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = getId bookingId,
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.ALLOCATION_REQUEST_UNASSIGNED
        }

notifyFarePolicyChange ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  Id coordinatorId ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyFarePolicyChange coordinatorId =
  FCM.notifyPerson notificationData . FCMNotificationRecipient coordinatorId.getId
  where
    title = FCM.FCMNotificationTitle "Fare policy changed."
    body =
      FCM.FCMNotificationBody $
        unwords
          [ "Fare has been updated."
          ]
    notificationData =
      FCM.FCMData
        { fcmNotificationType = FCM.FARE_POLICY_CHANGED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId coordinatorId,
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.FARE_POLICY_CHANGED
        }

notifyDiscountChange ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  Id coordinatorId ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDiscountChange coordinatorId =
  FCM.notifyPerson notificationData . FCMNotificationRecipient coordinatorId.getId
  where
    title = FCM.FCMNotificationTitle "Discount updated."
    body =
      FCM.FCMNotificationBody $
        unwords
          [ "Discount has been changed."
          ]
    notificationData =
      FCM.FCMData
        { fcmNotificationType = FCM.DISCOUNT_CHANGED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId coordinatorId,
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.DISCOUNT_CHANGED
        }
