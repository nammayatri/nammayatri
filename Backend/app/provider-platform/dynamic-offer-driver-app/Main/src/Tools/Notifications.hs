{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Notifications where

import qualified Beckn.Types.Core.Taxi.Common.Location as Common
import qualified Data.Text as T
import Domain.Types.Booking (Booking)
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Message.Message as Message
import Domain.Types.Person as Person
import Domain.Types.RegistrationToken as RegToken
import qualified Domain.Types.Ride as DRide
import Domain.Types.SearchRequestForDriver
import Domain.Types.SearchTry
import EulerHS.Prelude
import qualified Kernel.External.Notification.FCM.Flow as FCM
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude hiding (unwords)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Storage.CachedQueries.Merchant.TransporterConfig

data EditLocationReq = EditLocationReq
  { rideId :: Id DRide.Ride,
    origin :: Maybe Common.Location,
    destination :: Maybe Common.Location
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

notifyOnNewSearchRequestAvailable ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  SearchRequestForDriverAPIEntity ->
  m ()
notifyOnNewSearchRequestAvailable merchantOpCityId personId mbDeviceToken entityData = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType,
          fcmOverlayNotificationJSON = Nothing
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
  Id DMOC.MerchantOperatingCity ->
  Booking ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  SBCR.CancellationSource ->
  m ()
notifyOnCancel merchantOpCityId booking personId mbDeviceToken cancellationSource = do
  cancellationText <- getCancellationText
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPerson transporterConfig.fcmConfig (notificationData cancellationText) $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notificationData cancellationText =
      FCM.FCMData
        { fcmNotificationType = FCM.CANCELLED_PRODUCT,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = getId booking.id,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title (body cancellationText) FCM.CANCELLED_PRODUCT,
          fcmOverlayNotificationJSON = Nothing
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
  Id DMOC.MerchantOperatingCity ->
  RegistrationToken ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyOnRegistration merchantOpCityId regToken personId mbToken = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.REGISTRATION_APPROVED,
          fcmOverlayNotificationJSON = Nothing
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
  Id DMOC.MerchantOperatingCity ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriver merchantOpCityId = sendNotificationToDriver merchantOpCityId FCM.SHOW Nothing

-- Send notification to device, i.e. notifications that should not be shown to the user,
-- but contains payload used by the app
notifyDevice ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDevice merchantOpCityId = sendNotificationToDriver merchantOpCityId FCM.DO_NOT_SHOW (Just FCM.HIGH)

sendNotificationToDriver ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  FCM.FCMShowNotification ->
  Maybe FCM.FCMAndroidMessagePriority ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
sendNotificationToDriver merchantOpCityId displayOption priority notificationType notificationTitle message driverId mbToken = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig priority False notificationData $ FCMNotificationRecipient driverId.getId mbToken
  where
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notificationType,
          fcmShowNotification = displayOption,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId driverId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notificationType,
          fcmOverlayNotificationJSON = Nothing
        }
    title = FCM.FCMNotificationTitle notificationTitle
    body =
      FCMNotificationBody message

sendMessageToDriver ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  FCM.FCMShowNotification ->
  Maybe FCM.FCMAndroidMessagePriority ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Id Person ->
  Id Message.Message ->
  Maybe FCM.FCMRecipientToken ->
  m ()
sendMessageToDriver merchantOpCityId displayOption priority notificationType notificationTitle message driverId messageId mbToken = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig priority False notificationData $ FCMNotificationRecipient driverId.getId mbToken
  where
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notificationType,
          fcmShowNotification = displayOption,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId messageId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notificationType,
          fcmOverlayNotificationJSON = Nothing
        }
    title = FCM.FCMNotificationTitle notificationTitle
    body =
      FCMNotificationBody message

notifyDriverNewAllocation ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id bookingId ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriverNewAllocation merchantOpCityId bookingId personId mbToken = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.ALLOCATION_REQUEST,
          fcmOverlayNotificationJSON = Nothing
        }

notifyFarePolicyChange ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id coordinatorId ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyFarePolicyChange merchantOpCityId coordinatorId mbToken = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.FARE_POLICY_CHANGED,
          fcmOverlayNotificationJSON = Nothing
        }

notifyDiscountChange ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id coordinatorId ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDiscountChange merchantOpCityId coordinatorId mbToken = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.DISCOUNT_CHANGED,
          fcmOverlayNotificationJSON = Nothing
        }

notifyDriverClearedFare ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Id SearchTry ->
  Money ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriverClearedFare merchantOpCityId driverId sReqId fare mbToken = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.CLEARED_FARE,
          fcmOverlayNotificationJSON = Nothing
        }

notifyOnCancelSearchRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Id SearchTry ->
  m ()
notifyOnCancelSearchRequest merchantOpCityId personId mbDeviceToken searchTryId = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType,
          fcmOverlayNotificationJSON = Nothing
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
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Id DOrder.PaymentOrder ->
  m ()
notifyPaymentFailed merchantOpCityId personId mbDeviceToken orderId = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType,
          fcmOverlayNotificationJSON = Nothing
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
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Id DOrder.PaymentOrder ->
  m ()
notifyPaymentPending merchantOpCityId personId mbDeviceToken orderId = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType,
          fcmOverlayNotificationJSON = Nothing
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
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Id DOrder.PaymentOrder ->
  m ()
notifyPaymentSuccess merchantOpCityId personId mbDeviceToken orderId = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType,
          fcmOverlayNotificationJSON = Nothing
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
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyPaymentModeManualOnCancel merchantOpCityId personId mbDeviceToken = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType,
          fcmOverlayNotificationJSON = Nothing
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
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyPaymentModeManualOnPause merchantOpCityId personId mbDeviceToken = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType,
          fcmOverlayNotificationJSON = Nothing
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
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyPaymentModeManualOnSuspend merchantOpCityId personId mbDeviceToken = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType,
          fcmOverlayNotificationJSON = Nothing
        }
    title = FCMNotificationTitle "Payment mode changed to manual"
    body =
      FCMNotificationBody $
        unwords
          [ "Your UPI Autopay has been suspended. You can clear your dues manually from the Plan page."
          ]

sendOverlay ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  [Text] ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Value ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe [Text] ->
  Maybe [FCM.FCMMediaLink] ->
  m ()
sendOverlay merchantOpCityId personId mbDeviceToken mbTitle description imageUrl okButtonText cancelButtonText actions link endPoint method reqBody delay contactSupportNumber toastMessage secondaryActions socialMediaLinks = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notifType = FCM.DRIVER_NOTIFY
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = if isJust mbTitle then FCM.SHOW else FCM.DO_NOT_SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = personId.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType,
          fcmOverlayNotificationJSON = Just $ FCM.createAndroidOverlayNotification mbTitle description imageUrl okButtonText cancelButtonText actions link endPoint method reqBody delay contactSupportNumber toastMessage secondaryActions socialMediaLinks
        }
    title = FCMNotificationTitle $ fromMaybe "Title" mbTitle -- if nothing then anyways fcmShowNotification is false
    body = FCMNotificationBody $ fromMaybe "Description" description

notifyPickupOrDropLocationChange ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  EditLocationReq ->
  m ()
notifyPickupOrDropLocationChange merchantOpCityId personId mbDeviceToken entityData = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notifType = FCM.EDIT_LOCATION
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.EditLocation,
          fcmEntityIds = entityData.rideId.getId,
          fcmEntityData = Just entityData,
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType,
          fcmOverlayNotificationJSON = Nothing
        }
    title = FCMNotificationTitle "Pickup and/or drop location has been changed by the customer"
    body =
      FCMNotificationBody $
        unwords
          [ "Customer has changed pickup or drop location. Please check the app for more details"
          ]
