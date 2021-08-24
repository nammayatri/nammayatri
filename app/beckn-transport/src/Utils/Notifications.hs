module Utils.Notifications where

import qualified Beckn.External.FCM.Flow as FCM
import Beckn.External.FCM.Types as FCM
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.Mobility.Order (CancellationSource (..))
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetrics)
import Beckn.Utils.Common
import qualified Data.Text as T
import EulerHS.Prelude
import Types.Storage.Case as Case
import Types.Storage.Person as Person
import Types.Storage.ProductInstance as ProductInstance
import Types.Storage.RegistrationToken as RegToken

-- | Send FCM "cancel" notification to driver
notifyOnCancel ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  Case ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  CancellationSource ->
  m ()
notifyOnCancel c personId mbDeviceToken cancellationSource = do
  cancellationText <- getCancellationText
  FCM.notifyPerson (notificationData cancellationText) $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    caseId = Case.id c
    notificationData cancellationText =
      FCM.FCMAndroidData
        { fcmNotificationType = FCM.CANCELLED_PRODUCT,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = show $ getId caseId,
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
              showTimeIst (c.startTime) <> ".",
              "Check the app for more details."
            ]
      ByOrganization ->
        return $
          unwords
            [ "Your agency had to cancel the ride for",
              showTimeIst (c.startTime) <> ".",
              "Check the app for more details."
            ]
      ByDriver ->
        return $
          unwords
            [ "You have cancelled the ride for",
              showTimeIst (c.startTime) <> ".",
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

notifyTransporterOnExpiration ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  Case ->
  [Person] ->
  m ()
notifyTransporterOnExpiration c =
  traverse_ (\person -> FCM.notifyPerson notificationData $ FCMNotificationRecipient person.id.getId person.deviceToken)
  where
    notificationData =
      FCM.FCMAndroidData
        { fcmNotificationType = FCM.EXPIRED_CASE,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Case,
          fcmEntityIds = show . getId $ c.id,
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.EXPIRED_CASE
        }
    title = FCMNotificationTitle $ T.pack "Ride expired!"
    body =
      FCMNotificationBody $
        unwords
          [ "The ride request for",
            showTimeIst (Case.startTime c),
            "has expired as the customer failed to confirm.",
            "You can view more details in the app."
          ]

notifyCancelReqByBP ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  ProductInstance ->
  [Person] ->
  m ()
notifyCancelReqByBP p =
  traverse_ (\person -> FCM.notifyPerson notificationData $ FCMNotificationRecipient person.id.getId person.deviceToken)
  where
    notificationData =
      FCM.FCMAndroidData
        { fcmNotificationType = FCM.CANCELLED_PRODUCT,
          fcmShowNotification = FCM.SHOW,
          fcmEntityIds = show $ getId $ p.id,
          fcmEntityType = FCM.Organization,
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.CANCELLED_PRODUCT
        }
    title = FCM.FCMNotificationTitle $ T.pack "Driver has cancelled the ride!"
    body =
      FCMNotificationBody $
        unwords
          [ "The ride scheduled for",
            showTimeIst (ProductInstance.startTime p) <> ",",
            "has been cancelled. Check the app for more details."
          ]

notifyDriverCancelledRideRequest ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  ProductInstance ->
  [Person] ->
  m ()
notifyDriverCancelledRideRequest p =
  traverse_ (\person -> FCM.notifyPerson notificationData $ FCMNotificationRecipient person.id.getId person.deviceToken)
  where
    notificationData =
      FCM.FCMAndroidData
        { fcmNotificationType = FCM.DRIVER_UNASSIGNED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityIds = show $ getId $ p.id,
          fcmEntityType = FCM.Organization,
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.DRIVER_UNASSIGNED
        }
    title = FCM.FCMNotificationTitle $ T.pack "Driver has refused the ride!"
    body =
      FCMNotificationBody $
        unwords
          [ "The ride scheduled for",
            showTimeIst (ProductInstance.startTime p) <> ",",
            "has been refused by driver. Check the app for more details."
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
notifyDriver notificationType notificationTitle message driverId =
  FCM.notifyPerson notificationData . FCMNotificationRecipient driverId.getId
  where
    notificationData =
      FCM.FCMAndroidData
        { fcmNotificationType = notificationType,
          fcmShowNotification = FCM.SHOW,
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
  ProductInstance ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriverNewAllocation productInstance personId =
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
          fcmEntityIds = getId $ productInstance.id,
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.ALLOCATION_REQUEST
        }

notifyDriverUnassigned ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  ProductInstance ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriverUnassigned productInstance personId = FCM.notifyPerson notificationData . FCMNotificationRecipient personId.getId
  where
    title = FCM.FCMNotificationTitle "Ride not assigned."
    body =
      FCM.FCMNotificationBody $
        unwords
          [ "Ride could not be assigned to you.",
            "Please wait for another request."
          ]
    notificationData =
      FCM.FCMAndroidData
        { fcmNotificationType = FCM.ALLOCATION_REQUEST_UNASSIGNED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = getId $ productInstance.id,
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.ALLOCATION_REQUEST_UNASSIGNED
        }
