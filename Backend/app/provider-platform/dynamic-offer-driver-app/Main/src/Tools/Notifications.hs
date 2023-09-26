{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Notifications where

import qualified Data.Text as T
import Domain.Types.Booking (Booking)
import qualified Domain.Types.BookingCancellationReason as SBCR
import Domain.Types.Merchant
import Domain.Types.Merchant.PushNotification as MPN
import Domain.Types.Message.Message as Message
import Domain.Types.Person as Person
import Domain.Types.RegistrationToken as RegToken
import Domain.Types.SearchRequestForDriver
import Domain.Types.SearchTry
import EulerHS.Prelude
import qualified Kernel.External.Notification.FCM.Flow as FCM
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Storage.CachedQueries.Merchant.TransporterConfig

notifyOnNewSearchRequestAvailable ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  SearchRequestForDriverAPIEntity ->
  m ()
notifyOnNewSearchRequestAvailable merchantId personId mbDeviceToken entityData = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notifType = FCM.NEW_RIDE_AVAILABLE
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.SearchRequest,
          fcmEntityIds = entityData.searchTryId.getId,
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
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Booking ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  SBCR.CancellationSource ->
  m ()
notifyOnCancel merchantId booking personId mbDeviceToken cancellationSource = do
  cancellationText <- getCancellationText
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPerson transporterConfig.fcmConfig (notificationData cancellationText) $ FCMNotificationRecipient personId.getId mbDeviceToken
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
    body = FCMNotificationBody
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
            [ "Sorry your ride for",
              showTimeIst (booking.startTime),
              "was cancelled.",
              "Please try to book again"
            ]
      _ -> throwError (InternalError "Unexpected cancellation reason.")

notifyOnRegistration ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  RegistrationToken ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyOnRegistration merchantId regToken personId mbToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPerson transporterConfig.fcmConfig notificationData $ FCMNotificationRecipient personId.getId mbToken
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
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriver merchantId = sendNotificationToDriver merchantId FCM.SHOW Nothing

-- Send notification to device, i.e. notifications that should not be shown to the user,
-- but contains payload used by the app
notifyDevice ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDevice merchantId = sendNotificationToDriver merchantId FCM.DO_NOT_SHOW (Just FCM.HIGH)

sendNotificationToDriver ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  FCM.FCMShowNotification ->
  Maybe FCM.FCMAndroidMessagePriority ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
sendNotificationToDriver merchantId displayOption priority notificationType notificationTitle message driverId mbToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig priority False notificationData $ FCMNotificationRecipient driverId.getId mbToken
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

sendMessageToDriver ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  FCM.FCMShowNotification ->
  Maybe FCM.FCMAndroidMessagePriority ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Id Person ->
  Id Message.Message ->
  Maybe FCM.FCMRecipientToken ->
  m ()
sendMessageToDriver merchantId displayOption priority notificationType notificationTitle message driverId messageId mbToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig priority False notificationData $ FCMNotificationRecipient driverId.getId mbToken
  where
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notificationType,
          fcmShowNotification = displayOption,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId messageId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notificationType
        }
    title = FCM.FCMNotificationTitle notificationTitle
    body =
      FCMNotificationBody message

notifyDriverNewAllocation ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id bookingId ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriverNewAllocation merchantId bookingId personId mbToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData $ FCMNotificationRecipient personId.getId mbToken
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
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id coordinatorId ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyFarePolicyChange merchantId coordinatorId mbToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPerson transporterConfig.fcmConfig notificationData $ FCMNotificationRecipient coordinatorId.getId mbToken
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
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id coordinatorId ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDiscountChange merchantId coordinatorId mbToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPerson transporterConfig.fcmConfig notificationData $ FCMNotificationRecipient coordinatorId.getId mbToken
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
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Id SearchTry ->
  Money ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriverClearedFare merchantId driverId sReqId fare mbToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData $ FCMNotificationRecipient driverId.getId mbToken
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

notifyOnCancelSearchRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Id SearchTry ->
  m ()
notifyOnCancelSearchRequest merchantId personId mbDeviceToken searchTryId = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notifType = FCM.CANCELLED_SEARCH_REQUEST
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.SearchRequest,
          fcmEntityIds = searchTryId.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType
        }
    title = FCMNotificationTitle "Search Request cancelled!"
    body =
      FCMNotificationBody $
        unwords
          [ "Search request has been cancelled by customer"
          ]

notifyPaymentFailed ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Id DOrder.PaymentOrder ->
  m ()
notifyPaymentFailed merchantId personId mbDeviceToken orderId = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notifType = FCM.PAYMENT_FAILED
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.PaymentOrder,
          fcmEntityIds = orderId.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType
        }
    title = FCMNotificationTitle "Payment Failed!"
    body =
      FCMNotificationBody $
        unwords
          [ "Your payment attempt was unsuccessful."
          ]

notifyPaymentPending ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Id DOrder.PaymentOrder ->
  m ()
notifyPaymentPending merchantId personId mbDeviceToken orderId = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notifType = FCM.PAYMENT_PENDING
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.PaymentOrder,
          fcmEntityIds = orderId.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType
        }
    title = FCMNotificationTitle "Payment Pending!"
    body =
      FCMNotificationBody $
        unwords
          [ "To continue taking rides on Namma Yatri, clear you payment dues"
          ]

notifyPaymentSuccess ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Id DOrder.PaymentOrder ->
  m ()
notifyPaymentSuccess merchantId personId mbDeviceToken orderId = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notifType = FCM.PAYMENT_SUCCESS
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.PaymentOrder,
          fcmEntityIds = orderId.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType
        }
    title = FCMNotificationTitle "Payment Successful!"
    body =
      FCMNotificationBody $
        unwords
          [ "Your payment has been processed successfully. Start earning with Namma Yatri!"
          ]

notifyPaymentModeManualOnCancel ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyPaymentModeManualOnCancel merchantId personId mbDeviceToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notifType = FCM.PAYMENT_MODE_MANUAL
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = personId.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType
        }
    title = FCMNotificationTitle "Payment mode changed to manual"
    body =
      FCMNotificationBody $
        unwords
          [ "You have cancelled your UPI Autopay. You can clear your dues manually from the Plan page."
          ]

notifyPaymentModeManualOnPause ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyPaymentModeManualOnPause merchantId personId mbDeviceToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notifType = FCM.PAYMENT_MODE_MANUAL
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = personId.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType
        }
    title = FCMNotificationTitle "Payment mode changed to manual"
    body =
      FCMNotificationBody $
        unwords
          [ "You have paused your UPI Autopay. You can clear your dues manually from the Plan page."
          ]

notifyPaymentModeManualOnSuspend ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyPaymentModeManualOnSuspend merchantId personId mbDeviceToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notifType = FCM.PAYMENT_MODE_MANUAL
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = personId.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType
        }
    title = FCMNotificationTitle "Payment mode changed to manual"
    body =
      FCMNotificationBody $
        unwords
          [ "Your UPI Autopay has been suspended. You can clear your dues manually from the Plan page."
          ]

notifyPaymentNudge ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  MPN.NotificationSubType ->
  Text ->
  Text ->
  Maybe Text ->
  m ()
notifyPaymentNudge merchantId personId mbDeviceToken notificationSubType title_ body_ icon = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notifType = FCM.PAYMENT_NUDGE
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = personId.getId,
          fcmEntityData = Just notificationSubType,
          fcmNotificationJSON = FCM.createAndroidNotificationWithIcon title body notifType icon
        }
    title = FCMNotificationTitle title_
    body = FCMNotificationBody body_
