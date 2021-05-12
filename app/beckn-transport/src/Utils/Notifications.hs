{-# LANGUAGE OverloadedLabels #-}

module Utils.Notifications where

import App.Types
import Beckn.External.FCM.Flow
import Beckn.External.FCM.Types as FCM
import Beckn.Types.Id
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Types.Storage.RegistrationToken as RegToken
import Beckn.Utils.Common (showTimeIst)
import qualified Data.Text as T
import EulerHS.Prelude

-- | Send FCM "cancel" notification to driver
notifyDriverOnCancel :: Case -> Person -> Flow ()
notifyDriverOnCancel c =
  notifyPerson notificationData
  where
    caseId = Case._id c
    notificationData =
      FCM.FCMAndroidData
        { _fcmNotificationType = FCM.CANCELLED_PRODUCT,
          _fcmShowNotification = FCM.SHOW,
          _fcmEntityType = FCM.Product,
          _fcmEntityIds = show $ getId caseId,
          _fcmNotificationJSON = createAndroidNotification title body FCM.CANCELLED_PRODUCT
        }
    title = FCMNotificationTitle $ T.pack "Ride cancelled!"
    body =
      FCMNotificationBody $
        unwords
          [ "Customer had to cancel your ride for",
            showTimeIst (Case._startTime c) <> ".",
            "Check the app for more details."
          ]

notifyOnRegistration :: RegistrationToken -> Person -> Flow ()
notifyOnRegistration regToken =
  notifyPerson notificationData
  where
    tokenId = RegToken._id regToken
    notificationData =
      FCM.FCMAndroidData
        { _fcmNotificationType = FCM.REGISTRATION_APPROVED,
          _fcmShowNotification = FCM.SHOW,
          _fcmEntityType = FCM.Organization,
          _fcmEntityIds = show tokenId,
          _fcmNotificationJSON = createAndroidNotification title body FCM.REGISTRATION_APPROVED
        }
    title = FCMNotificationTitle $ T.pack "Registration Completed!"
    body =
      FCMNotificationBody $
        unwords
          [ "Welcome Yatri Partner!",
            "Click here to set up your account."
          ]

notifyTransporterOnExpiration :: Case -> [Person] -> Flow ()
notifyTransporterOnExpiration c =
  traverse_ (notifyPerson notificationData)
  where
    notificationData =
      FCM.FCMAndroidData
        { _fcmNotificationType = FCM.EXPIRED_CASE,
          _fcmShowNotification = FCM.SHOW,
          _fcmEntityType = FCM.Case,
          _fcmEntityIds = show . getId $ c ^. #_id,
          _fcmNotificationJSON = createAndroidNotification title body FCM.EXPIRED_CASE
        }
    title = FCMNotificationTitle $ T.pack "Ride expired!"
    body =
      FCMNotificationBody $
        unwords
          [ "The ride request for",
            showTimeIst (Case._startTime c),
            "has expired as the customer failed to confirm.",
            "You can view more details in the app."
          ]

notifyCancelReqByBP :: ProductInstance -> [Person] -> Flow ()
notifyCancelReqByBP p =
  traverse_ (notifyPerson notificationData)
  where
    notificationData =
      FCM.FCMAndroidData
        { _fcmNotificationType = FCM.CANCELLED_PRODUCT,
          _fcmShowNotification = FCM.SHOW,
          _fcmEntityIds = show $ getId $ p ^. #_id,
          _fcmEntityType = FCM.Organization,
          _fcmNotificationJSON = createAndroidNotification title body FCM.CANCELLED_PRODUCT
        }
    title = FCM.FCMNotificationTitle $ T.pack "Driver has cancelled the ride!"
    body =
      FCMNotificationBody $
        unwords
          [ "The ride scheduled for",
            showTimeIst (ProductInstance._startTime p) <> ",",
            "has been cancelled. Check the app for more details."
          ]

notifyDriverCancelledRideRequest :: ProductInstance -> [Person] -> Flow ()
notifyDriverCancelledRideRequest p = traverse_ (notifyPerson notificationData)
  where
    notificationData =
      FCM.FCMAndroidData
        { _fcmNotificationType = FCM.DRIVER_UNASSIGNED,
          _fcmShowNotification = FCM.SHOW,
          _fcmEntityIds = show $ getId $ p ^. #_id,
          _fcmEntityType = FCM.Organization,
          _fcmNotificationJSON = createAndroidNotification title body FCM.DRIVER_UNASSIGNED
        }
    title = FCM.FCMNotificationTitle $ T.pack "Driver has refused the ride!"
    body =
      FCMNotificationBody $
        unwords
          [ "The ride scheduled for",
            showTimeIst (ProductInstance._startTime p) <> ",",
            "has been refused by driver. Check the app for more details."
          ]

notifyDriver :: FCM.FCMNotificationType -> Text -> Text -> Person -> Flow ()
notifyDriver notificationType notificationTitle message driver =
  notifyPerson notificationData driver
  where
    notificationData =
      FCM.FCMAndroidData
        { _fcmNotificationType = notificationType,
          _fcmShowNotification = FCM.SHOW,
          _fcmEntityIds = show . getId $ driver ^. #_id,
          _fcmEntityType = FCM.Person,
          _fcmNotificationJSON = createAndroidNotification title body notificationType
        }
    title = FCM.FCMNotificationTitle notificationTitle
    body =
      FCMNotificationBody message

notifyDriverNewAllocation :: ProductInstance -> Person -> Flow ()
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
        { _fcmNotificationType = FCM.ALLOCATION_REQUEST,
          _fcmShowNotification = FCM.SHOW,
          _fcmEntityType = FCM.Product,
          _fcmEntityIds = getId $ productInstance ^. #_id,
          _fcmNotificationJSON = createAndroidNotification title body FCM.ALLOCATION_REQUEST
        }

notifyDriverUnassigned :: ProductInstance -> Person -> Flow ()
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
        { _fcmNotificationType = FCM.ALLOCATION_REQUEST_UNASSIGNED,
          _fcmShowNotification = FCM.SHOW,
          _fcmEntityType = FCM.Product,
          _fcmEntityIds = getId $ productInstance ^. #_id,
          _fcmNotificationJSON = createAndroidNotification title body FCM.ALLOCATION_REQUEST_UNASSIGNED
        }
