module Utils.Notifications where

import Beckn.External.FCM.Flow
import Beckn.External.FCM.Types as FCM
import Beckn.Types.Id
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetrics)
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Types.Storage.RegistrationToken as RegToken
import Beckn.Utils.Common
import qualified Data.Text as T
import EulerHS.Prelude

-- | Send FCM "cancel" notification to driver
notifyDriverOnCancel ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  Case ->
  Person ->
  m ()
notifyDriverOnCancel c =
  notifyPerson notificationData
  where
    caseId = Case.id c
    notificationData =
      FCM.FCMAndroidData
        { fcmNotificationType = FCM.CANCELLED_PRODUCT,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = show $ getId caseId,
          fcmNotificationJSON = createAndroidNotification title body FCM.CANCELLED_PRODUCT
        }
    title = FCMNotificationTitle $ T.pack "Ride cancelled!"
    body =
      FCMNotificationBody $
        unwords
          [ "Customer had to cancel your ride for",
            showTimeIst (Case.startTime c) <> ".",
            "Check the app for more details."
          ]

notifyOnRegistration ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  RegistrationToken ->
  Person ->
  m ()
notifyOnRegistration regToken =
  notifyPerson notificationData
  where
    tokenId = RegToken.id regToken
    notificationData =
      FCM.FCMAndroidData
        { fcmNotificationType = FCM.REGISTRATION_APPROVED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Organization,
          fcmEntityIds = show tokenId,
          fcmNotificationJSON = createAndroidNotification title body FCM.REGISTRATION_APPROVED
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
  traverse_ (notifyPerson notificationData)
  where
    notificationData =
      FCM.FCMAndroidData
        { fcmNotificationType = FCM.EXPIRED_CASE,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Case,
          fcmEntityIds = show . getId $ c.id,
          fcmNotificationJSON = createAndroidNotification title body FCM.EXPIRED_CASE
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
  traverse_ (notifyPerson notificationData)
  where
    notificationData =
      FCM.FCMAndroidData
        { fcmNotificationType = FCM.CANCELLED_PRODUCT,
          fcmShowNotification = FCM.SHOW,
          fcmEntityIds = show $ getId $ p.id,
          fcmEntityType = FCM.Organization,
          fcmNotificationJSON = createAndroidNotification title body FCM.CANCELLED_PRODUCT
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
notifyDriverCancelledRideRequest p = traverse_ (notifyPerson notificationData)
  where
    notificationData =
      FCM.FCMAndroidData
        { fcmNotificationType = FCM.DRIVER_UNASSIGNED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityIds = show $ getId $ p.id,
          fcmEntityType = FCM.Organization,
          fcmNotificationJSON = createAndroidNotification title body FCM.DRIVER_UNASSIGNED
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
  Person ->
  m ()
notifyDriver notificationType notificationTitle message driver =
  notifyPerson notificationData driver
  where
    notificationData =
      FCM.FCMAndroidData
        { fcmNotificationType = notificationType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityIds = show . getId $ driver.id,
          fcmEntityType = FCM.Person,
          fcmNotificationJSON = createAndroidNotification title body notificationType
        }
    title = FCM.FCMNotificationTitle notificationTitle
    body =
      FCMNotificationBody message

notifyDriverNewAllocation ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  ProductInstance ->
  Person ->
  m ()
notifyDriverNewAllocation productInstance = notifyPerson notificationData
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
          fcmNotificationJSON = createAndroidNotification title body FCM.ALLOCATION_REQUEST
        }

notifyDriverUnassigned ::
  ( FCMFlow m r,
    CoreMetrics m
  ) =>
  ProductInstance ->
  Person ->
  m ()
notifyDriverUnassigned productInstance = notifyPerson notificationData
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
          fcmNotificationJSON = createAndroidNotification title body FCM.ALLOCATION_REQUEST_UNASSIGNED
        }
