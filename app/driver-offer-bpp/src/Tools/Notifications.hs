module Tools.Notifications where

import qualified Beckn.External.FCM.Flow as FCM
import Beckn.External.FCM.Types as FCM
import Beckn.Storage.Hedis (HedisFlow)
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Data.Text as T
import Domain.Types.Booking (Booking)
import qualified Domain.Types.BookingCancellationReason as SBCR
import Domain.Types.Person as Person
import Domain.Types.RegistrationToken as RegToken
import Domain.Types.SearchRequest
import Domain.Types.SearchRequestForDriver
import EulerHS.Prelude

notifyOnNewSearchRequestAvailable ::
  ( FCMFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  SearchRequestForDriverAPIEntity ->
  m ()
notifyOnNewSearchRequestAvailable personId mbDeviceToken entityData = do
  FCM.notifyPersonWithPriorityDefault (Just FCM.HIGH) notificationData $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notifType = FCM.NEW_RIDE_AVAILABLE
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.SearchRequest,
          fcmEntityIds = entityData.searchRequestId.getId,
          fcmEntityData = Just entityData,
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType
        }
    title = FCMNotificationTitle "New ride available for offering"
    body =
      FCMNotificationBody $
        unwords
          [ "A new ride for",
            showTimeIst entityData.startTime,
            "is available",
            show entityData.distanceToPickup.getMeters,
            "meters away from you. Estimated base fare is",
            show entityData.baseFare <> " INR, estimated distance is",
            show $ entityData.distance,
            "meters"
          ]

-- | Send FCM "cancel" notification to driver
notifyOnCancel ::
  ( FCMFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  Booking ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  SBCR.CancellationSource ->
  m ()
notifyOnCancel booking personId mbDeviceToken cancellationSource = do
  cancellationText <- getCancellationText
  FCM.notifyPersonDefault (notificationData cancellationText) $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notificationData cancellationText =
      FCM.FCMData
        { fcmNotificationType = FCM.CANCELLED_PRODUCT,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = getId booking.id,
          fcmEntityData = (),
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
      SBCR.ByMerchant ->
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
      SBCR.ByApplication ->
        return $
          unwords
            [ "The ride for",
              showTimeIst (booking.startTime),
              "was cancelled because quote was not confirmed.",
              "Please book again to get another ride."
            ]
      _ -> throwError (InternalError "Unexpected cancellation reason.")

notifyOnRegistration ::
  ( FCMFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  RegistrationToken ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyOnRegistration regToken personId =
  FCM.notifyPersonDefault notificationData . FCMNotificationRecipient personId.getId
  where
    tokenId = RegToken.id regToken
    notificationData =
      FCM.FCMData
        { fcmNotificationType = FCM.REGISTRATION_APPROVED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Merchant,
          fcmEntityIds = getId tokenId,
          fcmEntityData = (),
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
    HedisFlow m r,
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
    HedisFlow m r,
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
    HedisFlow m r,
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
  FCM.notifyPersonWithPriorityDefault priority notificationData . FCMNotificationRecipient driverId.getId
  where
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notificationType,
          fcmShowNotification = displayOption,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId driverId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notificationType
        }
    title = FCM.FCMNotificationTitle notificationTitle
    body =
      FCMNotificationBody message

notifyDriverNewAllocation ::
  ( FCMFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  Id bookingId ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriverNewAllocation bookingId personId =
  FCM.notifyPersonWithPriorityDefault (Just FCM.HIGH) notificationData . FCMNotificationRecipient personId.getId
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
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.ALLOCATION_REQUEST
        }

notifyFarePolicyChange ::
  ( FCMFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  Id coordinatorId ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyFarePolicyChange coordinatorId =
  FCM.notifyPersonDefault notificationData . FCMNotificationRecipient coordinatorId.getId
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
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.FARE_POLICY_CHANGED
        }

notifyDiscountChange ::
  ( FCMFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  Id coordinatorId ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDiscountChange coordinatorId =
  FCM.notifyPersonDefault notificationData . FCMNotificationRecipient coordinatorId.getId
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
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.DISCOUNT_CHANGED
        }

notifyDriverClearedFare ::
  ( FCMFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  Id Person ->
  Id SearchRequest ->
  Money ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriverClearedFare driverId sReqId fare =
  FCM.notifyPersonDefault notificationData . FCMNotificationRecipient driverId.getId
  where
    title = FCM.FCMNotificationTitle "Clearing Fare!"
    body =
      FCM.FCMNotificationBody $
        unwords
          [ "Clearing fare - ",
            show fare.getMoney <> "."
          ]
    notificationData =
      FCM.FCMData
        { fcmNotificationType = FCM.CLEARED_FARE,
          fcmShowNotification = FCM.DO_NOT_SHOW,
          fcmEntityType = FCM.SearchRequest,
          fcmEntityIds = getId sReqId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.CLEARED_FARE
        }
