{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Notifications where

import Data.Aeson
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Domain.Action.UI.SearchRequestForDriver
import Domain.Types.Booking (Booking)
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.BookingUpdateRequest as DBUR
import Domain.Types.Location
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Domain.Types.Merchant.Overlay as DTMO
import Domain.Types.Message.Message as Message
import Domain.Types.Person as Person
import Domain.Types.RegistrationToken as RegToken
import qualified Domain.Types.Ride as DRide
import Domain.Types.SearchTry
import qualified EulerHS.Prelude hiding (null)
import qualified Kernel.External.Notification as Notification
import qualified Kernel.External.Notification.FCM.Flow as FCM
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude hiding (unwords)
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Version as Version
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Storage.Cac.TransporterConfig
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import qualified Storage.Queries.Person as QPerson
import Utils.Common.Cac.KeyNameConstants

data EmptyDynamicParam = EmptyDynamicParam

instance ToJSON EmptyDynamicParam where
  toJSON EmptyDynamicParam = object []

data EditLocationReq = EditLocationReq
  { rideId :: Id DRide.Ride,
    origin :: Maybe Location,
    destination :: Maybe Location
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data UpdateLocationNotificationReq = UpdateLocationNotificationReq
  { rideId :: Id DRide.Ride,
    origin :: Maybe Location,
    destination :: Maybe Location,
    stops :: Maybe [Location],
    bookingUpdateRequestId :: Id DBUR.BookingUpdateRequest,
    newEstimatedDistance :: HighPrecMeters,
    newEstimatedFare :: HighPrecMoney,
    oldEstimatedDistance :: HighPrecMeters,
    oldEstimatedFare :: HighPrecMoney,
    validTill :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

templateText :: Text -> Text
templateText txt = "{#" <> txt <> "#}"

notifyOnNewSearchRequestAvailable ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  SearchRequestForDriverAPIEntity ->
  m ()
notifyOnNewSearchRequestAvailable merchantOpCityId personId mbDeviceToken entityData = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData (FCMNotificationRecipient personId.getId mbDeviceToken) EulerHS.Prelude.id
  where
    notifType = FCM.NEW_RIDE_AVAILABLE
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.SearchRequest,
          fcmEntityIds = entityData.searchTryId.getId,
          fcmEntityData = Just entityData,
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCMNotificationTitle "New ride available for offering"
    body =
      FCMNotificationBody $
        EulerHS.Prelude.unwords
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
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Booking ->
  Person ->
  SBCR.CancellationSource ->
  m ()
notifyOnCancel merchantOpCityId booking person cancellationSource = do
  cancellationText <- getCancellationText
  let newCityId = cityFallback person.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  transporterConfig <- findByMerchantOpCityId newCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPerson transporterConfig.fcmConfig (notificationData cancellationText) $ FCMNotificationRecipient person.id.getId person.deviceToken
  where
    notificationData cancellationText =
      FCM.FCMData
        { fcmNotificationType = FCM.CANCELLED_PRODUCT,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = getId booking.id,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title (body cancellationText) FCM.CANCELLED_PRODUCT Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCMNotificationTitle $ T.pack "Ride cancelled!"
    body = FCMNotificationBody
    getCancellationText = case cancellationSource of
      SBCR.ByUser ->
        return $
          EulerHS.Prelude.unwords
            [ "Customer had to cancel your ride for",
              showTimeIst (booking.startTime) <> ".",
              "Check the app for more details."
            ]
      SBCR.ByMerchant ->
        return $
          EulerHS.Prelude.unwords
            [ "Your agency had to cancel the ride for",
              showTimeIst (booking.startTime) <> ".",
              "Check the app for more details."
            ]
      SBCR.ByDriver ->
        return $
          EulerHS.Prelude.unwords
            [ "You have cancelled the ride for",
              showTimeIst (booking.startTime) <> ".",
              "Check the app for more details."
            ]
      SBCR.ByApplication ->
        return $
          EulerHS.Prelude.unwords
            [ "Sorry your ride for",
              showTimeIst (booking.startTime),
              "was cancelled.",
              "Please try to book again"
            ]
      _ -> throwError (InternalError "Unexpected cancellation reason.")

notifyOnRegistration ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  RegistrationToken ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyOnRegistration merchantOpCityId regToken personId mbToken = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.REGISTRATION_APPROVED Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCMNotificationTitle $ T.pack "Registration Completed!"
    body =
      FCMNotificationBody $
        EulerHS.Prelude.unwords
          [ "Welcome Yatri Partner!",
            "Click here to set up your account."
          ]

notifyDriver ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriver merchantOpCityId = sendNotificationToDriver merchantOpCityId FCM.SHOW Nothing

-- Send notification to device, i.e. notifications that should not be shown to the user,
-- but contains payload used by the app
notifyDevice ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDevice merchantOpCityId = sendNotificationToDriver merchantOpCityId FCM.DO_NOT_SHOW (Just FCM.HIGH)

sendNotificationToDriver ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  FCM.FCMShowNotification ->
  Maybe FCM.FCMAndroidMessagePriority ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
sendNotificationToDriver merchantOpCityId displayOption priority notificationType notificationTitle message driver mbToken = do
  let newCityId = cityFallback driver.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  transporterConfig <- findByMerchantOpCityId newCityId (Just (DriverId (cast driver.id))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig priority False notificationData (FCMNotificationRecipient driver.id.getId mbToken) EulerHS.Prelude.id
  where
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notificationType,
          fcmShowNotification = displayOption,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId driver.id,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notificationType Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCM.FCMNotificationTitle notificationTitle
    body =
      FCMNotificationBody message

sendMessageToDriver ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  FCM.FCMShowNotification ->
  Maybe FCM.FCMAndroidMessagePriority ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Person ->
  Id Message.Message ->
  m ()
sendMessageToDriver merchantOpCityId displayOption priority notificationType notificationTitle message driver messageId = do
  let newCityId = cityFallback driver.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  transporterConfig <- findByMerchantOpCityId newCityId (Just (DriverId (cast driver.id))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig priority False notificationData (FCMNotificationRecipient driver.id.getId driver.deviceToken) EulerHS.Prelude.id
  where
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notificationType,
          fcmShowNotification = displayOption,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId messageId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notificationType Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCM.FCMNotificationTitle notificationTitle
    body =
      FCMNotificationBody message

notifyDriverNewAllocation ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Id bookingId ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriverNewAllocation merchantOpCityId bookingId personId mbToken = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData (FCMNotificationRecipient personId.getId mbToken) EulerHS.Prelude.id
  where
    title = FCM.FCMNotificationTitle "New allocation request."
    body =
      FCM.FCMNotificationBody $
        EulerHS.Prelude.unwords
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
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.ALLOCATION_REQUEST Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }

notifyFarePolicyChange ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Id coordinatorId ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyFarePolicyChange merchantOpCityId coordinatorId mbToken = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPerson transporterConfig.fcmConfig notificationData $ FCMNotificationRecipient coordinatorId.getId mbToken
  where
    title = FCM.FCMNotificationTitle "Fare policy changed."
    body =
      FCM.FCMNotificationBody $
        EulerHS.Prelude.unwords
          [ "Fare has been updated."
          ]
    notificationData =
      FCM.FCMData
        { fcmNotificationType = FCM.FARE_POLICY_CHANGED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId coordinatorId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.FARE_POLICY_CHANGED Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }

notifyDiscountChange ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Id coordinatorId ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDiscountChange merchantOpCityId coordinatorId mbToken = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPerson transporterConfig.fcmConfig notificationData $ FCMNotificationRecipient coordinatorId.getId mbToken
  where
    title = FCM.FCMNotificationTitle "Discount updated."
    body =
      FCM.FCMNotificationBody $
        EulerHS.Prelude.unwords
          [ "Discount has been changed."
          ]
    notificationData =
      FCM.FCMData
        { fcmNotificationType = FCM.DISCOUNT_CHANGED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId coordinatorId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.DISCOUNT_CHANGED Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }

notifyDriverClearedFare ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  Id SearchTry ->
  Price ->
  m ()
notifyDriverClearedFare merchantOpCityId driver sReqId fare = do
  let newCityId = cityFallback driver.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  transporterConfig <- findByMerchantOpCityId newCityId (Just (DriverId (cast driver.id))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData (FCMNotificationRecipient driver.id.getId driver.deviceToken) EulerHS.Prelude.id
  where
    title = FCM.FCMNotificationTitle "Clearing Fare!"
    body =
      FCM.FCMNotificationBody $
        EulerHS.Prelude.unwords
          [ "Clearing fare - ",
            show fare.amount <> " " <> show fare.currency <> "."
          ]
    notificationData =
      FCM.FCMData
        { fcmNotificationType = FCM.CLEARED_FARE,
          fcmShowNotification = FCM.DO_NOT_SHOW,
          fcmEntityType = FCM.SearchRequest,
          fcmEntityIds = getId sReqId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.CLEARED_FARE Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }

notifyOnCancelSearchRequest ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  Id SearchTry ->
  m ()
notifyOnCancelSearchRequest merchantOpCityId person searchTryId = do
  let newCityId = cityFallback person.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  transporterConfig <- findByMerchantOpCityId newCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData (FCMNotificationRecipient person.id.getId person.deviceToken) EulerHS.Prelude.id
  where
    notifType = FCM.CANCELLED_SEARCH_REQUEST
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.SearchRequest,
          fcmEntityIds = searchTryId.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCMNotificationTitle "Search Request cancelled!"
    body =
      FCMNotificationBody $
        EulerHS.Prelude.unwords
          [ "Search request has been cancelled by customer"
          ]

notifyPaymentFailed ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Id DOrder.PaymentOrder ->
  m ()
notifyPaymentFailed merchantOpCityId personId mbDeviceToken orderId = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData (FCMNotificationRecipient personId.getId mbDeviceToken) EulerHS.Prelude.id
  where
    notifType = FCM.PAYMENT_FAILED
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.PaymentOrder,
          fcmEntityIds = orderId.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCMNotificationTitle "Payment Failed!"
    body =
      FCMNotificationBody $
        EulerHS.Prelude.unwords
          [ "Your payment attempt was unsuccessful."
          ]

notifyPaymentPending ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Id DOrder.PaymentOrder ->
  m ()
notifyPaymentPending merchantOpCityId personId mbDeviceToken orderId = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData (FCMNotificationRecipient personId.getId mbDeviceToken) EulerHS.Prelude.id
  where
    notifType = FCM.PAYMENT_PENDING
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.PaymentOrder,
          fcmEntityIds = orderId.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCMNotificationTitle "Payment Pending!"
    body =
      FCMNotificationBody $
        EulerHS.Prelude.unwords
          [ "To continue taking rides on Namma Yatri, clear you payment dues"
          ]

notifyPaymentSuccess ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  Id DOrder.PaymentOrder ->
  m ()
notifyPaymentSuccess merchantOpCityId person orderId = do
  let newCityId = cityFallback person.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  transporterConfig <- findByMerchantOpCityId newCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData (FCMNotificationRecipient person.id.getId person.deviceToken) EulerHS.Prelude.id
  where
    notifType = FCM.PAYMENT_SUCCESS
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.PaymentOrder,
          fcmEntityIds = orderId.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCMNotificationTitle "Payment Successful!"
    body =
      FCMNotificationBody $
        EulerHS.Prelude.unwords
          [ "Your payment has been processed successfully. Start earning with Namma Yatri!"
          ]

notifyPaymentModeManualOnCancel ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyPaymentModeManualOnCancel merchantOpCityId personId mbDeviceToken = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData (FCMNotificationRecipient personId.getId mbDeviceToken) EulerHS.Prelude.id
  where
    notifType = FCM.PAYMENT_MODE_MANUAL
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = personId.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCMNotificationTitle "Payment mode changed to manual"
    body =
      FCMNotificationBody $
        EulerHS.Prelude.unwords
          [ "You have cancelled your UPI Autopay. You can clear your dues manually from the Plan page."
          ]

notifyPaymentModeManualOnPause ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyPaymentModeManualOnPause merchantOpCityId personId mbDeviceToken = do
  transporterConfig <- findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData (FCMNotificationRecipient personId.getId mbDeviceToken) EulerHS.Prelude.id
  where
    notifType = FCM.PAYMENT_MODE_MANUAL
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = personId.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCMNotificationTitle "Payment mode changed to manual"
    body =
      FCMNotificationBody $
        EulerHS.Prelude.unwords
          [ "You have paused your UPI Autopay. You can clear your dues manually from the Plan page."
          ]

notifyPaymentModeManualOnSuspend ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  m ()
notifyPaymentModeManualOnSuspend merchantOpCityId person = do
  let newCityId = cityFallback person.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  transporterConfig <- findByMerchantOpCityId newCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData (FCMNotificationRecipient person.id.getId person.deviceToken) EulerHS.Prelude.id
  where
    notifType = FCM.PAYMENT_MODE_MANUAL
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = person.id.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCMNotificationTitle "Payment mode changed to manual"
    body =
      FCMNotificationBody $
        EulerHS.Prelude.unwords
          [ "Your UPI Autopay has been suspended. You can clear your dues manually from the Plan page."
          ]

sendOverlay ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  FCM.FCMOverlayReq ->
  m ()
sendOverlay merchantOpCityId person req@FCM.FCMOverlayReq {..} = do
  let newCityId = cityFallback person.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  transporterConfig <- findByMerchantOpCityId newCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData (FCMNotificationRecipient person.id.getId person.deviceToken) EulerHS.Prelude.id
  where
    notifType = FCM.DRIVER_NOTIFY
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = if isJust req.title then FCM.SHOW else FCM.DO_NOT_SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = person.id.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification notifTitle body notifType Nothing,
          fcmOverlayNotificationJSON = Just $ FCM.createAndroidOverlayNotification req,
          fcmNotificationId = Nothing
        }
    notifTitle = FCMNotificationTitle $ fromMaybe "Title" req.title -- if nothing then anyways fcmShowNotification is false
    body = FCMNotificationBody $ fromMaybe "Description" description

sendUpdateLocOverlay ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  FCM.FCMOverlayReq ->
  UpdateLocationNotificationReq ->
  m ()
sendUpdateLocOverlay merchantOpCityId person req@FCM.FCMOverlayReq {..} entityData = do
  let newCityId = cityFallback person.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  transporterConfig <- findByMerchantOpCityId newCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData (FCMNotificationRecipient person.id.getId person.deviceToken) EulerHS.Prelude.id
  where
    notifType = FCM.DRIVER_NOTIFY_LOCATION_UPDATE
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = if isJust req.title then FCM.SHOW else FCM.DO_NOT_SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = person.id.getId,
          fcmEntityData = Just entityData,
          fcmNotificationJSON = FCM.createAndroidNotification notifTitle body notifType Nothing,
          fcmOverlayNotificationJSON = Just $ FCM.createAndroidOverlayNotification req,
          fcmNotificationId = Nothing
        }
    notifTitle = FCMNotificationTitle $ fromMaybe "Title" req.title -- if nothing then anyways fcmShowNotification is false
    body = FCMNotificationBody $ fromMaybe "Description" description

notifyPickupOrDropLocationChange ::
  KvDbFlow m r =>
  Person ->
  EditLocationReq ->
  m ()
notifyPickupOrDropLocationChange person entityData = do
  let newCityId = cityFallback person.clientBundleVersion person.merchantOperatingCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  transporterConfig <- findByMerchantOpCityId newCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData (FCMNotificationRecipient person.id.getId person.deviceToken) EulerHS.Prelude.id
  where
    notifType = FCM.EDIT_LOCATION
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.EditLocation,
          fcmEntityIds = entityData.rideId.getId,
          fcmEntityData = Just entityData,
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCMNotificationTitle "Pickup and/or drop location has been changed by the customer"
    body =
      FCMNotificationBody $
        EulerHS.Prelude.unwords
          [ "Customer has changed pickup or drop location. Please check the app for more details"
          ]

mkOverlayReq :: DTMO.Overlay -> FCM.FCMOverlayReq
mkOverlayReq _overlay@DTMO.Overlay {..} =
  FCM.FCMOverlayReq
    { ..
    }

-- new function

buildSendSearchRequestNotificationData ::
  ( ServiceFlow m r,
    ToJSON SearchRequestForDriverAPIEntity,
    ToJSON EmptyDynamicParam
  ) =>
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  SearchRequestForDriverAPIEntity ->
  EmptyDynamicParam ->
  m (Notification.NotificationReq SearchRequestForDriverAPIEntity EmptyDynamicParam)
buildSendSearchRequestNotificationData driverId mbDeviceToken entityData dynamicParam =
  return $
    Notification.NotificationReq
      { category = Notification.NEW_RIDE_AVAILABLE,
        subCategory = Nothing,
        showNotification = Notification.SHOW,
        messagePriority = Just Notification.HIGH,
        entity = Notification.Entity Notification.SearchRequest entityData.searchRequestId.getId entityData,
        dynamicParams = dynamicParam,
        body = mkBody,
        title = title,
        auth = Notification.Auth driverId.getId ((.getFCMRecipientToken) <$> mbDeviceToken) Nothing,
        ttl = Just entityData.searchRequestValidTill,
        sound = Nothing
      }
  where
    title = "New ride available for offering"
    mkBody =
      cs $
        EulerHS.Prelude.unwords
          [ "A new ride for",
            cs $ showTimeIst entityData.startTime,
            "is available",
            show entityData.distanceToPickup.getMeters,
            "meters away from you. Estimated base fare is",
            show entityData.baseFare <> " INR, estimated distance is",
            show $ entityData.distance,
            "meters"
          ]

sendSearchRequestToDriverNotification ::
  ( ServiceFlow m r,
    ToJSON SearchRequestForDriverAPIEntity,
    ToJSON EmptyDynamicParam,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Notification.NotificationReq SearchRequestForDriverAPIEntity EmptyDynamicParam ->
  m ()
sendSearchRequestToDriverNotification merchantId merchantOpCityId req = Notification.notifyPersonWithAllProviders handler req False
  where
    handler = Notification.NotficationServiceHandler {..}

    getNotificationServiceList = do
      merchantServiceUsageConfig <- QMSUC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
      let sendSearchReqNotificationList = merchantServiceUsageConfig.sendSearchRequestToDriver
      when (null sendSearchReqNotificationList) $ throwError $ InternalError ("No notification service provider configured for the merchant Op city : " <> merchantOpCityId.getId)
      pure sendSearchReqNotificationList

    getServiceConfig service = do
      merchantNotificationServiceConfig <-
        QMSC.findByMerchantIdAndServiceWithCity merchantId (DMSC.NotificationService service) merchantOpCityId
          >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Notification" (show service))
      case merchantNotificationServiceConfig.serviceConfig of
        DMSC.NotificationServiceConfig nsc -> pure nsc
        _ -> throwError $ InternalError "Unknow Service Config"

    iosModifier (iosFCMdata :: (FCM.FCMData SearchRequestForDriverAPIEntity)) = iosFCMdata {fcmEntityData = modifyEntity iosFCMdata.fcmEntityData}

    modifyEntity SearchRequestForDriverAPIEntity {..} = IOSSearchRequestForDriverAPIEntity {..}

data StopReq = StopReq
  { bookingId :: Id Booking,
    stop :: Maybe LocationAPIEntity,
    isEdit :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

notifyStopModification ::
  KvDbFlow m r =>
  Person ->
  StopReq ->
  m ()
notifyStopModification person entityData = do
  let newCityId = cityFallback person.clientBundleVersion person.merchantOperatingCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  transporterConfig <- findByMerchantOpCityId newCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) False notificationData (FCMNotificationRecipient person.id.getId person.deviceToken) EulerHS.Prelude.id
  where
    notifType = if entityData.isEdit then FCM.EDIT_STOP else FCM.ADD_STOP
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = entityData.bookingId.getId,
          fcmEntityData = Just entityData,
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCMNotificationTitle (if entityData.isEdit then "Stop Edited" else "Stop Added")
    body =
      FCMNotificationBody $
        EulerHS.Prelude.unwords
          [ if entityData.isEdit then "Customer edited stop!" else "Customer added a stop!"
          ]

notifyOnRideStarted ::
  KvDbFlow m r =>
  DRide.Ride ->
  m ()
notifyOnRideStarted ride = do
  let personId = ride.driverId
      isAirConditioned = maybe False (>= 0.0) ride.vehicleServiceTierAirConditioned
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
      title =
        T.pack
          ( if isAirConditioned
              then "Your AC ride has started"
              else "Your ride has started"
          )
      body =
        T.pack
          ( if isAirConditioned
              then "Please turn on AC, offer sakkath service and have a safe ride!"
              else "Offer sakkath service and have a safe ride!"
          )
  notifyDriver merchantOperatingCityId FCM.TRIP_STARTED title body person person.deviceToken

----------------- we have to remove this once YATRI_PARTNER is migrated to new version ------------------

getNewMerchantOpCityId :: Maybe Version.Version -> Id DMOC.MerchantOperatingCity -> Id DMOC.MerchantOperatingCity
getNewMerchantOpCityId Nothing currentMerchantCityId = currentMerchantCityId
getNewMerchantOpCityId (Just version) currentMerchantCityId = do
  let clientBundleVersion = Version.versionToText version
  let getClientBundleVersion = clientBundleVersion `compare` "1.8.319" ----- 1.8.319 is current client bundle version of the apk in prod
  case getClientBundleVersion of
    GT -> "3a95be1d-9052-4715-8cf5-ea8f68ffc85a" ------------ Namma Yatri HyderaBad city Id as it is common in both
    _ -> currentMerchantCityId

cityFallback :: Maybe Version.Version -> Id DMOC.MerchantOperatingCity -> Id DMOC.MerchantOperatingCity
cityFallback version "2c618bb1-508d-90d6-c8f5-001e9d11c871" = getNewMerchantOpCityId version "2c618bb1-508d-90d6-c8f5-001e9d11c871" ---- PROD Kochi city ID
cityFallback version "1984f6b4-95eb-4683-b6b5-251b1b008566" = getNewMerchantOpCityId version "1984f6b4-95eb-4683-b6b5-251b1b008566" ---- MASTER Kochi city
cityFallback _ currentMerchantCityId = currentMerchantCityId
