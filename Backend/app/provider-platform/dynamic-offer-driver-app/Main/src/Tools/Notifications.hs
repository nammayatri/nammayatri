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
import Data.Default.Class
import Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Domain.Action.UI.CallFeedbackFCM as CallFeedbackFCM
import Domain.Action.UI.SearchRequestForDriver
import Domain.Types.AlertRequest as DAR
import Domain.Types.Booking (Booking)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.BookingUpdateRequest as DBUR
import Domain.Types.EmptyDynamicParam
import Domain.Types.Location
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantClientConfig as DMCC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Domain.Types.Message as Message
import qualified Domain.Types.Overlay as DTMO
import Domain.Types.Person as Person
import Domain.Types.RegistrationToken as RegToken
import qualified Domain.Types.Ride as DRide
import Domain.Types.SearchTry
import Domain.Types.ServiceTierType
import Domain.Types.Trip as Trip
import qualified Domain.Types.TripTransaction as DTT
import qualified EulerHS.Prelude hiding (null)
import qualified Kernel.External.Notification as Notification
import qualified Kernel.External.Notification.FCM.Flow as FCM
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Notification.Interface.GRPC as GRPC
import Kernel.External.Notification.Types (NotificationService (..))
import Kernel.External.Types (Language (..), ServiceFlow)
import Kernel.Prelude hiding (unwords)
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Version as Version
import Kernel.Utils.Common
import Lib.DriverCoins.Types
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Yudhishthira.Types as LYT
import qualified Storage.Cac.MerchantServiceUsageConfig as QMSUC
import Storage.Cac.TransporterConfig
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.MerchantClientConfig as QMCC
import qualified Storage.Queries.Person as QPerson
import Utils.Common.Cac.KeyNameConstants

clearDeviceToken :: (MonadFlow m, EsqDBFlow m r) => Id Person -> m ()
clearDeviceToken = QPerson.clearDeviceTokenByPersonId

templateText :: Text -> Text
templateText txt = "{#" <> txt <> "#}"

buildTemplate :: [(Text, Text)] -> Text -> Text
buildTemplate paramVars template =
  foldl'
    ( \msg (findKey, replaceVal) ->
        T.replace (templateText findKey) replaceVal msg
    )
    template
    paramVars

data EditPickupLocationReq = EditPickupLocationReq
  { rideId :: Id DRide.Ride,
    origin :: Maybe Location,
    hasAdvanceBooking :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data CancellationRateBaseNudgeData = CancellationRateBaseNudgeData
  { driverId :: Text,
    driverCancellationRate :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data DrunkAndDriveViolationWarningData = DrunkAndDriveViolationWarningData
  { driverId :: Text,
    drunkAndDriveViolationCount :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

-- FIXME is it correct?
data UpdateLocationNotificationReq = UpdateLocationNotificationReq
  { rideId :: Id DRide.Ride,
    origin :: Maybe Location,
    destination :: Maybe Location,
    stops :: Maybe [Location],
    bookingUpdateRequestId :: Id DBUR.BookingUpdateRequest,
    newEstimatedDistance :: HighPrecMeters,
    newEstimatedDistanceWithUnit :: Distance,
    newEstimatedFare :: HighPrecMoney,
    oldEstimatedDistance :: HighPrecMeters,
    oldEstimatedDistanceWithUnit :: Distance,
    oldEstimatedFare :: HighPrecMoney,
    validTill :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data FCMReq = FCMReq
  { notificationKey :: Text,
    subCategory :: Maybe Notification.SubCategory,
    showType :: FCM.FCMShowNotification,
    entityId :: Text,
    entityType :: FCM.FCMEntityType,
    sound :: Maybe Text,
    overlayReq :: Maybe FCM.FCMOverlayReq,
    priority :: Maybe FCM.FCMAndroidMessagePriority
  }

instance Default FCMReq where
  def =
    FCMReq
      { notificationKey = mempty,
        subCategory = Nothing,
        showType = FCM.SHOW,
        entityId = mempty,
        entityType = FCM.Product,
        sound = Nothing,
        overlayReq = Nothing,
        priority = Just FCM.HIGH
      }

createFCMReq :: Text -> Text -> FCM.FCMEntityType -> (FCMReq -> FCMReq) -> FCMReq
createFCMReq notificationKey entityId entityType modifier = modifier $ def {entityId = entityId, notificationKey = notificationKey, entityType = entityType}

findFCMConfigWithFallback :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Id Person -> m FCMConfig
findFCMConfigWithFallback merchantOpCityId personId = do
  driver <- QPerson.findById personId
  mbClientConfig <- QMCC.findByPackageOSAndService (ClientFCMService) (driver >>= (.clientDevice) <&> (.deviceType)) (fromMaybe "" (driver >>= (.clientId)))
  case mbClientConfig of
    Just clientConfig -> let (DMCC.ClientFCMServiceConfig fcmCfg) = clientConfig.clientServiceConfig in pure fcmCfg
    Nothing -> findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId) <&> (.fcmConfig)

dynamicFCMNotifyPerson ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    ToJSON a,
    FromJSON a
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Language ->
  Maybe Trip.TripCategory ->
  FCMReq ->
  Maybe a ->
  [(Text, Text)] ->
  Maybe [LYT.ConfigVersionMap] ->
  m ()
dynamicFCMNotifyPerson merchantOpCityId personId mbDeviceToken lang tripCategory fcmReq entityData dynamicParams mbConfigInExperimentVersions = do
  fcmConfig <- findFCMConfigWithFallback merchantOpCityId personId
  mbMerchantPN <- CPN.findMatchingMerchantPN merchantOpCityId fcmReq.notificationKey tripCategory fcmReq.subCategory (Just lang) mbConfigInExperimentVersions
  when (isNothing mbMerchantPN) $ logError $ "MISSED_FCM - " <> fcmReq.notificationKey
  whenJust mbMerchantPN $ \merchantPN -> do
    let title = FCMNotificationTitle $ buildTemplate dynamicParams merchantPN.title
        body = FCMNotificationBody $ buildTemplate dynamicParams merchantPN.body
        notificationData =
          FCM.FCMData
            { fcmNotificationType = merchantPN.fcmNotificationType,
              fcmShowNotification = fcmReq.showType,
              fcmEntityType = fcmReq.entityType,
              fcmEntityIds = fcmReq.entityId,
              fcmEntityData = entityData,
              fcmNotificationJSON = FCM.createAndroidNotification title body merchantPN.fcmNotificationType fcmReq.sound,
              fcmOverlayNotificationJSON = FCM.createAndroidOverlayNotification <$> fcmReq.overlayReq,
              fcmNotificationId = Nothing
            }
    FCM.notifyPersonWithPriority fcmConfig fcmReq.priority (clearDeviceToken personId) notificationData (FCMNotificationRecipient personId.getId mbDeviceToken) EulerHS.Prelude.id

notifyOnNewSearchRequestAvailable ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Language ->
  SearchRequestForDriverAPIEntity ->
  m ()
notifyOnNewSearchRequestAvailable merchantOpCityId personId mbDeviceToken language entityData = do
  dynamicFCMNotifyPerson
    merchantOpCityId
    personId
    mbDeviceToken
    language
    (Just entityData.tripCategory)
    (createFCMReq "NEW_RIDE_AVAILABLE" entityData.searchTryId.getId FCM.SearchRequest identity)
    (Just entityData)
    [ ("startTime", showTimeIst entityData.startTime),
      ("distanceToPickup", distanceToText entityData.distanceToPickupWithUnit),
      ("baseFare", show entityData.baseFare),
      ("distance", maybe "unknown" distanceToText entityData.distanceWithUnit)
    ]
    Nothing

data IssueBreachEntityData = IssueBreachEntityData
  { ibName :: Text,
    blockExpirationTime :: UTCTime,
    blockedReasonFlag :: Text,
    blockedSTiers :: [ServiceTierType]
  }
  deriving (Generic, ToJSON, Eq, FromJSON, Show)

data FeedbackBadgeEntityData = FeedbackBadgeEntityData
  { badgeKey :: Maybe Text,
    rating :: Int,
    badgeCount :: Maybe Int
  }
  deriving (Generic, ToJSON, Eq, FromJSON, Show)

notifySoftBlocked :: (CacheFlow m r, EsqDBFlow m r) => Person -> IssueBreachEntityData -> m ()
notifySoftBlocked person entity = do
  dynamicFCMNotifyPerson
    person.merchantOperatingCityId
    person.id
    person.deviceToken
    (fromMaybe ENGLISH person.language)
    Nothing
    (createFCMReq entity.ibName person.id.getId FCM.Person identity)
    (Just entity)
    [ ("blockExpirationTime", showTimeIst entity.blockExpirationTime),
      ("blockedSTiers", blockedSTiers)
    ]
    Nothing
  where
    blockedSTiers = T.intercalate ", " $ map show entity.blockedSTiers

sendFeedbackBadgeNotification ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  FeedbackBadgeEntityData ->
  m ()
sendFeedbackBadgeNotification merchantOpCityId driver entityData = do
  dynamicFCMNotifyPerson
    merchantOpCityId
    driver.id
    driver.deviceToken
    (fromMaybe ENGLISH driver.language)
    Nothing
    (createFCMReq "FEEDBACK_BADGE_PN" driver.id.getId FCM.Person identity)
    (Just entityData)
    [ ("rating", show entityData.rating),
      ("badgeCount", maybe "0" show entityData.badgeCount)
    ]
    Nothing

-- NEW_RIDE_AVAILABLE
-- title = FCMNotificationTitle "New ride available for offering"
-- body =
--   FCMNotificationBody $
--     EulerHS.Prelude.unwords
--       [ "A new ride for",
--         showTimeIst entityData.startTime,
--         "is available",
--         distanceToText entityData.distanceToPickupWithUnit,
--         "away from you. Estimated base fare is",
--         show entityData.baseFare <> " INR, estimated distance is", -- FIXME fix currency
--         maybe "unknown" distanceToText entityData.distanceWithUnit
--       ]

cancellationSourceToSubCategory :: SBCR.CancellationSource -> Notification.SubCategory
cancellationSourceToSubCategory = \case
  SBCR.ByUser -> Notification.ByUser
  SBCR.ByMerchant -> Notification.ByMerchant
  SBCR.ByDriver -> Notification.ByDriver
  SBCR.ByApplication -> Notification.ByApplication
  SBCR.ByAllocator -> Notification.ByAllocator

-- | Send FCM "cancel" notification to driver
notifyOnCancel ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Booking ->
  Person ->
  SBCR.CancellationSource ->
  m ()
notifyOnCancel merchantOpCityId booking person cancellationSource = do
  let newCityId = cityFallback person.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  subCategory <- getSubCategory cancellationSource
  dynamicFCMNotifyPerson
    newCityId
    person.id
    person.deviceToken
    (fromMaybe ENGLISH person.language)
    (Just booking.tripCategory)
    (createFCMReq "NOTIFY_DRIVER_ON_CANCEL" booking.id.getId FCM.Product (\r -> r {subCategory = Just subCategory, priority = Nothing}))
    (Just ())
    [ ("startTime", showTimeIst (booking.startTime))
    ]
    (Just booking.configInExperimentVersions)
  where
    getSubCategory = \case
      SBCR.ByAllocator -> throwError (InternalError "Unexpected cancellation reason.")
      _ -> return $ cancellationSourceToSubCategory cancellationSource

-- title = FCMNotificationTitle $ T.pack "Ride cancelled!"
-- body = FCMNotificationBody
-- SBCR.ByUser ->
--   return $
--     EulerHS.Prelude.unwords
--       [ "Customer had to cancel your ride for",
--         showTimeIst (booking.startTime) <> ".",
--         "Check the app for more details."
--       ]
-- SBCR.ByMerchant ->
--   return $
--     EulerHS.Prelude.unwords
--       [ "Your agency had to cancel the ride for",
--         showTimeIst (booking.startTime) <> ".",
--         "Check the app for more details."
--       ]
-- SBCR.ByDriver ->
--   return $
--     EulerHS.Prelude.unwords
--       [ "You have cancelled the ride for",
--         showTimeIst (booking.startTime) <> ".",
--         "Check the app for more details."
--       ]
-- SBCR.ByApplication ->
--   return $
--     EulerHS.Prelude.unwords
--       [ "Sorry your ride for",
--         showTimeIst (booking.startTime),
--         "was cancelled.",
--         "Please try to book again"
--       ]
--_ -> throwError (InternalError "Unexpected cancellation reason.")

notifyOnRegistration ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  RegistrationToken ->
  Id Person ->
  Language ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyOnRegistration merchantOpCityId regToken personId lang mbToken = do
  let tokenId = RegToken.id regToken
  dynamicFCMNotifyPerson
    merchantOpCityId
    personId
    mbToken
    lang
    Nothing
    (createFCMReq "REGISTRATION_APPROVED" (getId tokenId) FCM.Merchant (\r -> r {priority = Nothing}))
    (Just ())
    []
    Nothing

-- title = FCMNotificationTitle $ T.pack "Registration Completed!"
-- body =
--   FCMNotificationBody $
--     EulerHS.Prelude.unwords
--       [ "Welcome Yatri Partner!",
--         "Click here to set up your account."
--       ]

notifyDriver ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriver merchantOpCityId = sendNotificationToDriver merchantOpCityId FCM.SHOW Nothing

notifyDriverWithProviders ::
  ( ServiceFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    ToJSON a
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Notification.Category ->
  Text ->
  Text ->
  Person ->
  Maybe FCM.FCMRecipientToken ->
  a ->
  m ()
notifyDriverWithProviders merchantOpCityId category title body driver mbDeviceToken dataSend =
  runWithServiceConfigForProviders merchantOpCityId driver.clientId driver.clientDevice notificationData EulerHS.Prelude.id (clearDeviceToken driver.id)
  where
    notificationData =
      Notification.NotificationReq
        { category = category,
          subCategory = Nothing,
          showNotification = Notification.SHOW,
          messagePriority = Just Notification.HIGH,
          entity = Notification.Entity Notification.Merchant merchantOpCityId.getId dataSend,
          dynamicParams = EmptyDynamicParam,
          body = body,
          title = title,
          auth = Notification.Auth driver.id.getId ((.getFCMRecipientToken) <$> mbDeviceToken) Nothing,
          ttl = Nothing,
          sound = Nothing
        }

driverScheduledRideAcceptanceAlert ::
  ( ServiceFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Notification.Category ->
  Text ->
  Text ->
  Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
driverScheduledRideAcceptanceAlert merchantOpCityId category title body driver mbDeviceToken =
  runWithServiceConfigForProviders merchantOpCityId driver.clientId driver.clientDevice notificationData EulerHS.Prelude.id (clearDeviceToken driver.id)
  where
    notificationData =
      Notification.NotificationReq
        { category = category,
          subCategory = Nothing,
          showNotification = Notification.SHOW,
          messagePriority = Just Notification.HIGH,
          entity = Notification.Entity Notification.Merchant merchantOpCityId.getId EmptyDynamicParam,
          dynamicParams = EmptyDynamicParam,
          body = body,
          title = title,
          auth = Notification.Auth driver.id.getId ((.getFCMRecipientToken) <$> mbDeviceToken) Nothing,
          ttl = Nothing,
          sound = Nothing
        }

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
  Person ->
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
  Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
sendNotificationToDriver merchantOpCityId displayOption priority notificationType notificationTitle message driver mbToken = do
  let newCityId = cityFallback driver.clientBundleVersion merchantOpCityId
  fcmConfig <- findFCMConfigWithFallback newCityId driver.id -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  FCM.notifyPersonWithPriority fcmConfig priority (clearDeviceToken driver.id) notificationData (FCMNotificationRecipient driver.id.getId mbToken) EulerHS.Prelude.id
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
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
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
  let newCityId = cityFallback driver.clientBundleVersion merchantOpCityId
  fcmConfig <- findFCMConfigWithFallback newCityId driver.id -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  FCM.notifyPersonWithPriority fcmConfig priority (clearDeviceToken driver.id) notificationData (FCMNotificationRecipient driver.id.getId driver.deviceToken) EulerHS.Prelude.id
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
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Booking ->
  Id Person ->
  Language ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriverNewAllocation merchantOpCityId booking personId lang mbToken = do
  dynamicFCMNotifyPerson
    merchantOpCityId
    personId
    mbToken
    lang
    (Just booking.tripCategory)
    (createFCMReq "ALLOCATION_REQUEST" (getId booking.id) FCM.Product identity)
    (Just ())
    []
    Nothing

-- FCM.ALLOCATION_REQUEST
-- title = FCM.FCMNotificationTitle "New allocation request."
-- body =
--   FCM.FCMNotificationBody $
--     EulerHS.Prelude.unwords
--       [ "New ride request!",
--         "Check the app for more details."
--       ]

-- notifyFarePolicyChange ::
--   ( CacheFlow m r,
--     EsqDBFlow m r
--   ) =>
--   Id DMOC.MerchantOperatingCity ->
--   Id coordinatorId ->
--   Maybe FCM.FCMRecipientToken ->
--   m ()
-- notifyFarePolicyChange merchantOpCityId coordinatorId mbToken = do
--   transporterConfig <- findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
--   FCM.notifyPerson transporterConfig.fcmConfig (clearDeviceToken coordinatorId) notificationData $ FCMNotificationRecipient coordinatorId.getId mbToken
--   where
--     title = FCM.FCMNotificationTitle "Fare policy changed."
--     body =
--       FCM.FCMNotificationBody $
--         EulerHS.Prelude.unwords
--           [ "Fare has been updated."
--           ]
--     notificationData =
--       FCM.FCMData
--         { fcmNotificationType = FCM.FARE_POLICY_CHANGED,
--           fcmShowNotification = FCM.SHOW,
--           fcmEntityType = FCM.Person,
--           fcmEntityIds = getId coordinatorId,
--           fcmEntityData = (),
--           fcmNotificationJSON = FCM.createAndroidNotification title body FCM.FARE_POLICY_CHANGED Nothing,
--           fcmOverlayNotificationJSON = Nothing,
--           fcmNotificationId = Nothing
--        }

-- notifyDiscountChange ::
--   ( CacheFlow m r,
--     EsqDBFlow m r
--   ) =>
--   Id DMOC.MerchantOperatingCity ->
--   Id coordinatorId ->
--   Maybe FCM.FCMRecipientToken ->
--   m ()
-- notifyDiscountChange merchantOpCityId coordinatorId mbToken = do
--   transporterConfig <- findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
--   FCM.notifyPerson transporterConfig.fcmConfig (clearDeviceToken coordinatorId) notificationData $ FCMNotificationRecipient coordinatorId.getId mbToken
--   where
--     title = FCM.FCMNotificationTitle "Discount updated."
--     body =
--       FCM.FCMNotificationBody $
--         EulerHS.Prelude.unwords
--           [ "Discount has been changed."
--           ]
--     notificationData =
--       FCM.FCMData
--         { fcmNotificationType = FCM.DISCOUNT_CHANGED,
--           fcmShowNotification = FCM.SHOW,
--           fcmEntityType = FCM.Person,
--           fcmEntityIds = getId coordinatorId,
--           fcmEntityData = (),
--           fcmNotificationJSON = FCM.createAndroidNotification title body FCM.DISCOUNT_CHANGED Nothing,
--           fcmOverlayNotificationJSON = Nothing,
--           fcmNotificationId = Nothing
--         }

notifyDriverClearedFare ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    ServiceFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  Id SearchTry ->
  Price ->
  m ()
notifyDriverClearedFare merchantOpCityId driver sReqId _ = do
  notificationData <- buildClearedFareNotificationData merchantOpCityId driver.id driver.deviceToken sReqId
  runWithServiceConfigForProviders merchantOpCityId driver.clientId driver.clientDevice notificationData EulerHS.Prelude.id (clearDeviceToken driver.id)

buildClearedFareNotificationData ::
  ( ServiceFlow m r,
    ToJSON EmptyDynamicParam
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Id SearchTry ->
  m (Notification.NotificationReq EmptyDynamicParam EmptyDynamicParam)
buildClearedFareNotificationData merchantOpCityId driverId mbDeviceToken searchRequestId = do
  let params = [("", "")]
  mbMerchantPN <- CPN.findMatchingMerchantPN merchantOpCityId "CLEARED_FARE" Nothing Nothing Nothing Nothing >>= fromMaybeM (InternalError "MerchantPushNotification not found for CLEARED_FARE")
  return $
    Notification.NotificationReq
      { category = Notification.CLEARED_FARE,
        subCategory = Nothing,
        showNotification = Notification.DO_NOT_SHOW,
        messagePriority = Just Notification.HIGH,
        entity = Notification.Entity Notification.SearchRequest (getId searchRequestId) EmptyDynamicParam,
        dynamicParams = EmptyDynamicParam,
        body = buildTemplate params mbMerchantPN.body,
        title = buildTemplate params mbMerchantPN.title,
        auth = Notification.Auth driverId.getId ((.getFCMRecipientToken) <$> mbDeviceToken) Nothing,
        ttl = Nothing,
        sound = Nothing
      }

-- title = FCM.FCMNotificationTitle "Clearing Fare!"
-- body =
--   FCM.FCMNotificationBody $
--     EulerHS.Prelude.unwords
--       [ "Clearing fare - ",
--         show fare.amount <> " " <> show fare.currency <> "."
--       ]

notifyOnCancelSearchRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    ServiceFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  Id SearchTry ->
  Trip.TripCategory ->
  m ()
notifyOnCancelSearchRequest merchantOpCityId driver searchTryId tripCategory = do
  notificationData <- buildCancelSearchNotificationData merchantOpCityId driver.id driver.deviceToken searchTryId tripCategory
  runWithServiceConfigForProviders merchantOpCityId driver.clientId driver.clientDevice notificationData EulerHS.Prelude.id (clearDeviceToken driver.id)

buildCancelSearchNotificationData ::
  ( ServiceFlow m r,
    ToJSON EmptyDynamicParam
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Id SearchTry ->
  Trip.TripCategory ->
  m (Notification.NotificationReq EmptyDynamicParam EmptyDynamicParam)
buildCancelSearchNotificationData merchantOpCityId driverId mbDeviceToken searchRequestId tripCategory = do
  let params = [("", "")]
  mbMerchantPN <- CPN.findMatchingMerchantPN merchantOpCityId "CANCELLED_SEARCH_REQUEST" (Just tripCategory) Nothing Nothing Nothing >>= fromMaybeM (InternalError "MerchantPushNotification not found for CANCELLED_SEARCH_REQUEST")
  return $
    Notification.NotificationReq
      { category = Notification.CANCELLED_SEARCH_REQUEST,
        subCategory = Nothing,
        showNotification = Notification.DO_NOT_SHOW,
        messagePriority = Just Notification.HIGH,
        entity = Notification.Entity Notification.SearchRequest (getId searchRequestId) EmptyDynamicParam,
        dynamicParams = EmptyDynamicParam,
        body = buildTemplate params mbMerchantPN.body,
        title = buildTemplate params mbMerchantPN.title,
        auth = Notification.Auth driverId.getId ((.getFCMRecipientToken) <$> mbDeviceToken) Nothing,
        ttl = Nothing,
        sound = Nothing
      }

--notifType = FCM.CANCELLED_SEARCH_REQUEST
-- title = FCMNotificationTitle "Search Request cancelled!"
-- body =
--   FCMNotificationBody $
--     EulerHS.Prelude.unwords
--       [ "Search request has been cancelled by customer"
--       ]

notifyPaymentFailed ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  Maybe FCM.FCMRecipientToken ->
  Id DOrder.PaymentOrder ->
  m ()
notifyPaymentFailed merchantOpCityId person mbDeviceToken orderId = do
  dynamicFCMNotifyPerson
    merchantOpCityId
    person.id
    mbDeviceToken
    (fromMaybe ENGLISH person.language)
    Nothing
    (createFCMReq "PAYMENT_FAILED" orderId.getId FCM.PaymentOrder identity)
    (pure ())
    []
    Nothing

-- PAYMENT_FAILED
-- title = FCMNotificationTitle "Payment Failed!"
-- body =
--   FCMNotificationBody $
--     EulerHS.Prelude.unwords
--       [ "Your payment attempt was unsuccessful."
--       ]

notifyPaymentPending ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  Maybe FCM.FCMRecipientToken ->
  Id DOrder.PaymentOrder ->
  m ()
notifyPaymentPending merchantOpCityId person mbDeviceToken orderId = do
  dynamicFCMNotifyPerson
    merchantOpCityId
    person.id
    mbDeviceToken
    (fromMaybe ENGLISH person.language)
    Nothing
    (createFCMReq "PAYMENT_PENDING" orderId.getId FCM.PaymentOrder identity)
    (pure ())
    []
    Nothing

-- FCM.PAYMENT_PENDING
-- title = FCMNotificationTitle "Payment Pending!"
-- body =
--   FCMNotificationBody $
--     EulerHS.Prelude.unwords
--       [ "To continue taking rides on Namma Yatri, clear you payment dues"
--       ]

notifyPaymentSuccess ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  Id DOrder.PaymentOrder ->
  m ()
notifyPaymentSuccess merchantOpCityId person orderId = do
  let newCityId = cityFallback person.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  dynamicFCMNotifyPerson
    newCityId
    person.id
    person.deviceToken
    (fromMaybe ENGLISH person.language)
    Nothing
    (createFCMReq "PAYMENT_SUCCESS" orderId.getId FCM.PaymentOrder identity)
    (pure ())
    []
    Nothing

-- notifType = FCM.PAYMENT_SUCCESS
-- notificationData =
--   FCM.FCMData
--     { fcmNotificationType = notifType,
--       fcmShowNotification = FCM.SHOW,
--       fcmEntityType = FCM.PaymentOrder,
--       fcmEntityIds = orderId.getId,
--       fcmEntityData = (),
--       fcmNotificationJSON = FCM.createAndroidNotification title body notifType Nothing,
--       fcmOverlayNotificationJSON = Nothing,
--       fcmNotificationId = Nothing
--     }
-- title = FCMNotificationTitle "Payment Successful!"
-- body =
--   FCMNotificationBody $
--     EulerHS.Prelude.unwords
--       [ "Your payment has been processed successfully. Start earning with Namma Yatri!"
--       ]

notifyPaymentModeManualOnCancel ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Language ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyPaymentModeManualOnCancel merchantOpCityId personId lang mbDeviceToken = do
  dynamicFCMNotifyPerson
    merchantOpCityId
    personId
    mbDeviceToken
    lang
    Nothing
    (createFCMReq "PAYMENT_MODE_MANUAL_ON_CANCEL" personId.getId FCM.Person identity)
    (pure ())
    []
    Nothing

-- PAYMENT_MODE_MANUAL_ON_CANCEL
-- notifType = FCM.PAYMENT_MODE_MANUAL
-- title = FCMNotificationTitle "Payment mode changed to manual"
-- body =
--   FCMNotificationBody $
--     EulerHS.Prelude.unwords
--       [ "You have cancelled your UPI Autopay. You can clear your dues manually from the Plan page."
--       ]

notifyPaymentModeManualOnPause ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Language ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyPaymentModeManualOnPause merchantOpCityId personId lang mbDeviceToken = do
  dynamicFCMNotifyPerson
    merchantOpCityId
    personId
    mbDeviceToken
    lang
    Nothing
    (createFCMReq "PAYMENT_MODE_MANUAL_ON_PAUSE" personId.getId FCM.Person identity)
    (pure ())
    []
    Nothing

-- PAYMENT_MODE_MANUAL_ON_PAUSE
-- notifType = FCM.PAYMENT_MODE_MANUAL
-- title = FCMNotificationTitle "Payment mode changed to manual"
-- body =
--   FCMNotificationBody $
--     EulerHS.Prelude.unwords
--       [ "You have paused your UPI Autopay. You can clear your dues manually from the Plan page."
--       ]

notifyPaymentModeManualOnSuspend ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  m ()
notifyPaymentModeManualOnSuspend merchantOpCityId person = do
  let newCityId = cityFallback person.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  dynamicFCMNotifyPerson
    newCityId
    person.id
    person.deviceToken
    (fromMaybe ENGLISH person.language)
    Nothing
    (createFCMReq "PAYMENT_MODE_MANUAL_ON_SUSPEND" person.id.getId FCM.Person identity)
    (pure ())
    []
    Nothing

-- notifType = FCM.PAYMENT_MODE_MANUAL
-- title = FCMNotificationTitle "Payment mode changed to manual"
-- body =
--   FCMNotificationBody $
--     EulerHS.Prelude.unwords
--       [ "Your UPI Autopay has been suspended. You can clear your dues manually from the Plan page."
--       ]

sendOverlay ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  FCM.FCMOverlayReq ->
  m ()
sendOverlay merchantOpCityId person req@FCM.FCMOverlayReq {..} = do
  let newCityId = cityFallback person.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  fcmConfig <- findFCMConfigWithFallback newCityId person.id
  FCM.notifyPersonWithPriority fcmConfig (Just FCM.HIGH) (clearDeviceToken person.id) notificationData (FCMNotificationRecipient person.id.getId person.deviceToken) EulerHS.Prelude.id
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
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  FCM.FCMOverlayReq ->
  UpdateLocationNotificationReq ->
  m ()
sendUpdateLocOverlay merchantOpCityId person req@FCM.FCMOverlayReq {..} entityData = do
  let newCityId = cityFallback person.clientBundleVersion merchantOpCityId
  -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  fcmConfig <- findFCMConfigWithFallback newCityId person.id
  FCM.notifyPersonWithPriority fcmConfig (Just FCM.HIGH) (clearDeviceToken person.id) notificationData (FCMNotificationRecipient person.id.getId person.deviceToken) EulerHS.Prelude.id
  where
    notifType = FCM.DRIVER_NOTIFY_LOCATION_UPDATE
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = if isJust req.title then FCM.SHOW else FCM.DO_NOT_SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = person.id.getId,
          fcmEntityData = Just entityData,
          fcmNotificationJSON = FCM.createAndroidNotification notifTitle body notifType (Just "driver_trip_update_pop_up.mp3"),
          fcmOverlayNotificationJSON = Just $ FCM.createAndroidOverlayNotification req,
          fcmNotificationId = Nothing
        }
    notifTitle = FCMNotificationTitle $ fromMaybe "Title" req.title -- if nothing then anyways fcmShowNotification is false
    body = FCMNotificationBody $ fromMaybe "Description" description

sendPickupLocationChangedOverlay ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Person ->
  FCM.FCMOverlayReq ->
  EditPickupLocationReq ->
  m ()
sendPickupLocationChangedOverlay person req entityData = do
  let newCityId = cityFallback person.clientBundleVersion person.merchantOperatingCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  fcmConfig <- findFCMConfigWithFallback newCityId person.id
  FCM.notifyPersonWithPriority fcmConfig (Just FCM.HIGH) (clearDeviceToken person.id) notificationData (FCMNotificationRecipient person.id.getId person.deviceToken) EulerHS.Prelude.id
  where
    notifType = FCM.EDIT_LOCATION
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.EditLocation,
          fcmEntityIds = entityData.rideId.getId,
          fcmEntityData = Just entityData,
          fcmNotificationJSON = FCM.createAndroidNotification notifTitle body notifType Nothing,
          fcmOverlayNotificationJSON = Just $ FCM.createAndroidOverlayNotification req,
          fcmNotificationId = Nothing
        }
    notifTitle = FCMNotificationTitle $ fromMaybe "Title" req.title -- if nothing then anyways fcmShowNotification is false
    body = FCMNotificationBody $ fromMaybe "Description" req.description

sendCancellationRateNudgeOverlay ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  FCM.FCMNotificationType ->
  FCM.FCMOverlayReq ->
  CancellationRateBaseNudgeData ->
  m ()
sendCancellationRateNudgeOverlay mOpCityId person fcmType req entityData = do
  fcmConfig <- findFCMConfigWithFallback mOpCityId person.id
  FCM.notifyPersonWithPriority fcmConfig (Just FCM.HIGH) (clearDeviceToken person.id) notificationData (FCMNotificationRecipient person.id.getId person.deviceToken) EulerHS.Prelude.id
  where
    notificationData =
      FCM.FCMData
        { fcmNotificationType = fcmType,
          fcmShowNotification = FCM.DO_NOT_SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = entityData.driverId,
          fcmEntityData = Just entityData,
          fcmNotificationJSON = FCM.createAndroidNotification notifTitle body fcmType Nothing,
          fcmOverlayNotificationJSON = Just $ FCM.createAndroidOverlayNotification req,
          fcmNotificationId = Nothing
        }
    notifTitle = FCMNotificationTitle $ fromMaybe "Title" req.title
    body = FCMNotificationBody $ fromMaybe "Description" req.description

drunkAndDriveViolationWarningOverlay ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  FCM.FCMOverlayReq ->
  DrunkAndDriveViolationWarningData ->
  m ()
drunkAndDriveViolationWarningOverlay mOpCityId person req entityData = do
  fcmConfig <- findFCMConfigWithFallback mOpCityId person.id
  FCM.notifyPersonWithPriority fcmConfig (Just FCM.HIGH) (clearDeviceToken person.id) notificationData (FCMNotificationRecipient person.id.getId person.deviceToken) EulerHS.Prelude.id
  where
    notificationData =
      FCM.FCMData
        { fcmNotificationType = FCM.DRUNK_AND_DRIVE_VIOLATION_WARNING,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = entityData.driverId,
          fcmEntityData = Just entityData,
          fcmNotificationJSON = FCM.createAndroidNotification notifTitle body FCM.DRUNK_AND_DRIVE_VIOLATION_WARNING Nothing,
          fcmOverlayNotificationJSON = Just $ FCM.createAndroidOverlayNotification req,
          fcmNotificationId = Nothing
        }
    notifTitle = FCMNotificationTitle $ fromMaybe "Title" req.title
    body = FCMNotificationBody $ fromMaybe "Description" req.description

driverStopDetectionAlert ::
  ( ServiceFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Notification.Category ->
  Text ->
  Text ->
  Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
driverStopDetectionAlert merchantOpCityId category title body driver mbDeviceToken =
  runWithServiceConfigForProviders merchantOpCityId driver.clientId driver.clientDevice notificationData EulerHS.Prelude.id (clearDeviceToken driver.id)
  where
    notificationData =
      Notification.NotificationReq
        { category = category,
          subCategory = Nothing,
          showNotification = Notification.DO_NOT_SHOW,
          messagePriority = Just Notification.HIGH,
          entity = Notification.Entity Notification.Person driver.id.getId EmptyDynamicParam,
          dynamicParams = EmptyDynamicParam,
          title = title,
          body = body,
          auth = Notification.Auth driver.id.getId ((.getFCMRecipientToken) <$> mbDeviceToken) Nothing,
          ttl = Nothing,
          sound = Nothing
        }

mkOverlayReq :: DTMO.Overlay -> FCM.FCMOverlayReq -- handle mod Title
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
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  SearchRequestForDriverAPIEntity ->
  EmptyDynamicParam ->
  Maybe TripCategory ->
  m (Notification.NotificationReq SearchRequestForDriverAPIEntity EmptyDynamicParam)
buildSendSearchRequestNotificationData merchantOpCityId driverId mbDeviceToken entityData dynamicParam tripCategory = do
  mbMerchantPN <- CPN.findMatchingMerchantPN merchantOpCityId "NEW_RIDE_AVAILABLE" tripCategory Nothing Nothing Nothing >>= fromMaybeM (InternalError "MerchantPushNotification not found for NEW_RIDE_AVAILABLE")
  return $
    Notification.NotificationReq
      { category = Notification.NEW_RIDE_AVAILABLE,
        subCategory = Nothing,
        showNotification = Notification.SHOW,
        messagePriority = Just Notification.HIGH,
        entity = Notification.Entity Notification.SearchRequest entityData.searchRequestId.getId entityData,
        dynamicParams = dynamicParam,
        body = buildTemplate params mbMerchantPN.body,
        title = buildTemplate params mbMerchantPN.title,
        auth = Notification.Auth driverId.getId ((.getFCMRecipientToken) <$> mbDeviceToken) Nothing,
        ttl = Just entityData.searchRequestValidTill,
        sound = Nothing
      }
  where
    params =
      [ ("startTime", cs $ showTimeIst entityData.startTime),
        ("distanceToPickup", distanceToText entityData.distanceToPickupWithUnit),
        ("baseFare", show entityData.baseFare),
        ("distance", maybe "unknown" distanceToText entityData.distanceWithUnit)
      ]

-- title = "New ride available for offering"
-- mkBody =
--   cs $
--     EulerHS.Prelude.unwords
--       [ "A new ride for",
--         cs $ showTimeIst entityData.startTime,
--         "is available",
--         distanceToText entityData.distanceToPickupWithUnit,
--         "away from you. Estimated base fare is",
--         show entityData.baseFare <> " INR, estimated distance is", -- FIXME currency
--         maybe "unknown" distanceToText entityData.distanceWithUnit
--       ]

sendSearchRequestToDriverNotification ::
  ( ServiceFlow m r,
    ToJSON SearchRequestForDriverAPIEntity,
    ToJSON EmptyDynamicParam,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Notification.NotificationReq SearchRequestForDriverAPIEntity EmptyDynamicParam ->
  m ()
sendSearchRequestToDriverNotification _merchantId merchantOpCityId driverId req = do
  --logDebug $ "DFCM - NEW_RIDE_AVAILABLE  Title -> " <> show req.title <> " body - " <> show req.body
  driver <- QPerson.findById driverId
  runWithServiceConfigForProviders merchantOpCityId (driver >>= (.clientId)) (driver >>= (.clientDevice)) req iosModifier (clearDeviceToken driverId)
  where
    iosModifier (iosFCMdata :: (FCM.FCMData SearchRequestForDriverAPIEntity)) = iosFCMdata {fcmEntityData = modifyEntity iosFCMdata.fcmEntityData}
    modifyEntity SearchRequestForDriverAPIEntity {..} = IOSSearchRequestForDriverAPIEntity {..}

data StopReq = StopReq
  { bookingId :: Id Booking,
    stop :: Maybe LocationAPIEntity,
    isEdit :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

notifyStopModification ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  DRide.RideStatus ->
  Person ->
  StopReq ->
  Trip.TripCategory ->
  m ()
notifyStopModification rideStatus person entityData tripCategory = do
  let newCityId = cityFallback person.clientBundleVersion person.merchantOperatingCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  if rideStatus == DRide.NEW
    then do
      fcmConfig <- findFCMConfigWithFallback newCityId person.id
      let notificationType = if entityData.isEdit then FCM.EDIT_STOP else FCM.ADD_STOP
          titleText = if entityData.isEdit then "Stop Edited" else "Stop Added"
          bodyText = if entityData.isEdit then "Customer has edited the stop." else "Customer has added a new stop."
          title = FCM.FCMNotificationTitle titleText
          body = FCM.FCMNotificationBody bodyText
      let notificationData =
            FCM.FCMData
              { fcmNotificationType = notificationType,
                fcmShowNotification = FCM.SHOW,
                fcmEntityType = FCM.Product,
                fcmEntityIds = entityData.bookingId.getId,
                fcmEntityData = Just entityData,
                fcmNotificationJSON = FCM.createAndroidNotification title body notificationType Nothing,
                fcmOverlayNotificationJSON = Nothing,
                fcmNotificationId = Nothing
              }
      FCM.notifyPersonWithPriority fcmConfig (Just FCM.HIGH) (clearDeviceToken person.id) notificationData (FCMNotificationRecipient person.id.getId person.deviceToken) EulerHS.Prelude.id
    else do
      let notificationKey = if entityData.isEdit then "EDIT_STOP" else "ADD_STOP"
      dynamicFCMNotifyPerson
        newCityId
        person.id
        person.deviceToken
        (fromMaybe ENGLISH person.language)
        (Just tripCategory)
        (createFCMReq notificationKey entityData.bookingId.getId FCM.Product identity)
        (Just entityData)
        []
        Nothing

-- notifType = if entityData.isEdit then FCM.EDIT_STOP else FCM.ADD_STOP
-- title = FCMNotificationTitle (if entityData.isEdit then "Stop Edited" else "Stop Added")
-- body =
--   FCMNotificationBody $
--     EulerHS.Prelude.unwords
--       [ if entityData.isEdit then "Customer edited stop!" else "Customer added a stop!"
--       ]

notifyOnRideStarted ::
  ( ServiceFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  ) =>
  DRide.Ride ->
  DBooking.Booking ->
  m ()
notifyOnRideStarted ride booking = do
  let personId = ride.driverId
      isAirConditioned = fromMaybe False ride.isAirConditioned
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  merchantOperatingCity <- CQMOC.findById merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  let offerAdjective = if merchantOperatingCity.language == KANNADA then "sakkath" else "great"
  let notificationKey = if isAirConditioned then "AC_RIDE_STARTED" else "RIDE_STARTED"
  mbMerchantPN <- CPN.findMatchingMerchantPNInRideFlow merchantOperatingCityId notificationKey (Just ride.tripCategory) Nothing person.language booking.configInExperimentVersions >>= fromMaybeM (InternalError "MerchantPushNotification not found for notifyOnRideStarted")
  let dynamicParams = [("offerAdjective", offerAdjective)]
  let title = buildTemplate dynamicParams mbMerchantPN.title
      body = buildTemplate dynamicParams mbMerchantPN.body
  notifyDriverWithProviders merchantOperatingCityId Notification.TRIP_STARTED title body person person.deviceToken EmptyDynamicParam

data WMBTripAssignedData = WMBTripAssignedData
  { tripTransactionId :: Id DTT.TripTransaction,
    routeCode :: Text,
    routeShortname :: Text,
    vehicleNumber :: Text,
    vehicleServiceTierType :: ServiceTierType,
    roundRouteCode :: Maybe Text,
    isFirstBatchTrip :: Bool,
    status :: DTT.TripStatus
  }
  deriving (Generic, Show, FromJSON, ToJSON)

notifyWmbOnRide ::
  ( ServiceFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    ToJSON a
  ) =>
  Id Person ->
  Id DMOC.MerchantOperatingCity ->
  DTT.TripStatus ->
  Text ->
  Text ->
  a ->
  m ()
notifyWmbOnRide driverId merchantOperatingCityId status title body entityData = do
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  case status of
    DTT.TRIP_ASSIGNED -> notifyDriverWithProviders merchantOperatingCityId Notification.WMB_TRIP_ASSIGNED title body person person.deviceToken entityData
    DTT.COMPLETED -> notifyDriverWithProviders merchantOperatingCityId Notification.WMB_TRIP_FINISHED title body person person.deviceToken entityData
    DTT.CANCELLED -> notifyDriverWithProviders merchantOperatingCityId Notification.WMB_TRIP_FINISHED title body person person.deviceToken entityData -- TODO :: Change this to WMB_TRIP_CANCELLED
    DTT.IN_PROGRESS -> notifyDriverWithProviders merchantOperatingCityId Notification.WMB_TRIP_STARTED title body person person.deviceToken entityData
    _ -> pure ()

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

data NotifReq = NotifReq
  { entityId :: Text,
    title :: Text,
    message :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

notifyDriverOnEvents ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  NotifReq ->
  FCM.FCMNotificationType ->
  m ()
notifyDriverOnEvents merchantOpCityId personId mbDeviceToken entityData notifType = do
  fcmConfig <- findFCMConfigWithFallback merchantOpCityId personId
  FCM.notifyPersonWithPriority fcmConfig (Just FCM.HIGH) (clearDeviceToken personId) notificationData (FCMNotificationRecipient personId.getId mbDeviceToken) EulerHS.Prelude.id
  where
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = entityData.entityId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCMNotificationTitle entityData.title
    body =
      FCMNotificationBody $
        EulerHS.Prelude.unwords
          [ entityData.message
          ]

{- Run this to trigger realtime GRPC notifications -}
notifyFleetWithGRPCProvider ::
  ( ServiceFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    ToJSON a
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Notification.Category ->
  Text ->
  Text ->
  Id Person ->
  a ->
  m ()
notifyFleetWithGRPCProvider merchantOpCityId category title body driverId entityData = do
  clientId <-
    QFDA.findByDriverId driverId True
      >>= \case
        Just fleetDriverAssociation -> pure (Id fleetDriverAssociation.fleetOwnerId)
        Nothing -> pure driverId
  notifyWithGRPCProvider merchantOpCityId category title body clientId entityData

{- Run this to trigger realtime GRPC notifications -}
notifyWithGRPCProvider ::
  ( ServiceFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    ToJSON a
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Notification.Category ->
  Text ->
  Text ->
  Id Person ->
  a ->
  m ()
notifyWithGRPCProvider merchantOpCityId category title body clientId entityData = do
  merchantNotificationServiceConfig <-
    QMSC.findByServiceAndCity (DMSC.NotificationService Notification.GRPC) merchantOpCityId
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "Notification" "GRPC")
  case merchantNotificationServiceConfig.serviceConfig of
    DMSC.NotificationServiceConfig (Notification.GRPCConfig cfg) -> do
      notificationId <- generateGUID
      GRPC.notifyPerson cfg notificationData notificationId
    _ -> throwError $ InternalError "Unknow Service Config"
  where
    notificationData =
      Notification.NotificationReq
        { category = category,
          subCategory = Nothing,
          showNotification = Notification.SHOW,
          messagePriority = Just Notification.HIGH,
          entity = Notification.Entity Notification.Merchant merchantOpCityId.getId entityData,
          dynamicParams = EmptyDynamicParam,
          body = body,
          title = title,
          auth = Notification.Auth clientId.getId Nothing Nothing,
          ttl = Nothing,
          sound = Nothing
        }

{- Run with service Providers can be used to trigger Critical Notifications over multiple channels FCM & GRPC -}
runWithServiceConfigForProviders ::
  ( ServiceFlow m r,
    ToJSON a,
    ToJSON b,
    ToJSON c,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  Maybe Version.Device ->
  Notification.NotificationReq a b ->
  (FCMData a -> FCMData c) ->
  m () ->
  m ()
runWithServiceConfigForProviders merchantOpCityId clientId clientDevice req iosModifier = Notification.notifyPersonWithAllProviders handler req Nothing
  where
    handler = Notification.NotficationServiceHandler {..}

    getNotificationServiceList = do
      merchantServiceUsageConfig <- QMSUC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
      let sendSearchReqNotificationList = merchantServiceUsageConfig.sendSearchRequestToDriver
      when (null sendSearchReqNotificationList) $ throwError $ InternalError ("No notification service provider configured for the merchant Op city : " <> merchantOpCityId.getId)
      pure sendSearchReqNotificationList

    getServiceConfig FCM = do
      mbClientConfig <- QMCC.findByPackageOSAndService (ClientFCMService) (clientDevice <&> (.deviceType)) (fromMaybe "" clientId)
      case mbClientConfig of
        Just clientConfig -> let (DMCC.ClientFCMServiceConfig fcmCfg) = clientConfig.clientServiceConfig in pure $ Notification.FCMConfig fcmCfg
        Nothing -> do
          merchantNotificationServiceConfig <-
            QMSC.findByServiceAndCity (DMSC.NotificationService FCM) merchantOpCityId
              >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "Notification" (show FCM))
          case merchantNotificationServiceConfig.serviceConfig of
            DMSC.NotificationServiceConfig nsc -> pure nsc
            _ -> throwError $ InternalError "Unknow Service Config"
    getServiceConfig service = do
      merchantNotificationServiceConfig <-
        QMSC.findByServiceAndCity (DMSC.NotificationService service) merchantOpCityId
          >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "Notification" (show service))
      case merchantNotificationServiceConfig.serviceConfig of
        DMSC.NotificationServiceConfig nsc -> pure nsc
        _ -> throwError $ InternalError "Unknow Service Config"

data CoinsNotificationData = CoinsNotificationData
  { coins :: Int,
    event :: DCT.DriverCoinsFunctionType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

sendCoinsNotification ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Text ->
  Person ->
  Maybe FCM.FCMRecipientToken ->
  CoinsNotificationData ->
  m ()
sendCoinsNotification merchantOpCityId notificationTitle message driver mbToken entityData = do
  let newCityId = cityFallback driver.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  fcmConfig <- findFCMConfigWithFallback newCityId driver.id
  FCM.notifyPersonWithPriority fcmConfig Nothing (clearDeviceToken driver.id) notificationData (FCMNotificationRecipient driver.id.getId mbToken) EulerHS.Prelude.id
  where
    notificationData =
      FCM.FCMData
        { fcmNotificationType = FCM.COINS_SUCCESS,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId driver.id,
          fcmEntityData = entityData,
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.COINS_SUCCESS Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCM.FCMNotificationTitle notificationTitle
    body =
      FCMNotificationBody message

requestRejectionNotification ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Text ->
  Person ->
  Maybe FCM.FCMRecipientToken ->
  DAR.AlertRequest ->
  m ()
requestRejectionNotification merchantOpCityId notificationTitle message driver mbToken entityData = do
  let newCityId = cityFallback driver.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  fcmConfig <- findFCMConfigWithFallback newCityId driver.id
  FCM.notifyPersonWithPriority fcmConfig Nothing (clearDeviceToken driver.id) notificationData (FCMNotificationRecipient driver.id.getId mbToken) EulerHS.Prelude.id
  where
    notificationData =
      FCM.FCMData
        { fcmNotificationType = FCM.DRIVER_REQUEST_REJECTED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId driver.id,
          fcmEntityData = entityData,
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.DRIVER_REQUEST_REJECTED Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCM.FCMNotificationTitle notificationTitle
    body =
      FCMNotificationBody message

-- This function is to be removed after next apk deployment
sendCoinsNotificationV3 ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Text ->
  Person ->
  Maybe FCM.FCMRecipientToken ->
  CoinsNotificationData ->
  MetroRideType ->
  m ()
sendCoinsNotificationV3 merchantOpCityId notificationTitle message driver mbToken entityData metroRideType = do
  logDebug $ "We are in metro notification"
  let newCityId = cityFallback driver.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  fcmConfig <- findFCMConfigWithFallback newCityId driver.id
  FCM.notifyPersonWithPriority fcmConfig Nothing (clearDeviceToken driver.id) notificationData (FCMNotificationRecipient driver.id.getId mbToken) EulerHS.Prelude.id
  where
    fcmNotificationType = case metroRideType of
      ToMetro -> FCM.TO_METRO_COINS
      FromMetro -> FCM.FROM_METRO_COINS
      _ -> COINS_SUCCESS
    notificationData =
      FCM.FCMData
        { fcmNotificationType = fcmNotificationType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId driver.id,
          fcmEntityData = entityData,
          fcmNotificationJSON = FCM.createAndroidNotification title body fcmNotificationType Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCM.FCMNotificationTitle notificationTitle
    body =
      FCMNotificationBody message

sendDriverEKDLiveFCM ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Language ->
  CallFeedbackFCM.CallFeedbackEntity ->
  m ()
sendDriverEKDLiveFCM merchantOpCityId driverId mbDeviceToken language entityData = do
  logDebug $ "We are in EKD live call feedback"
  let fcmReq = createFCMReq "EKD_LIVE_CALL_FEEDBACK" driverId.getId FCM.Person (\r -> r {showType = FCM.DO_NOT_SHOW})
  dynamicFCMNotifyPerson
    merchantOpCityId
    driverId
    mbDeviceToken
    language
    Nothing
    fcmReq
    (pure entityData)
    []
    Nothing

notifyEditDestination ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyEditDestination merchantOpCityId personId mbDeviceToken = do
  logDebug $ "We are in edit destination when driver accepted edited location"
  fcmConfig <- findFCMConfigWithFallback merchantOpCityId personId
  FCM.notifyPersonWithPriority fcmConfig (Just FCM.HIGH) (clearDeviceToken personId) notificationData (FCMNotificationRecipient personId.getId mbDeviceToken) EulerHS.Prelude.id
  where
    notificationData =
      FCM.FCMData
        { fcmNotificationType = FCM.DRIVER_ACCEPTED_EDITED_LOCATION,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId personId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.DRIVER_ACCEPTED_EDITED_LOCATION Nothing,
          fcmOverlayNotificationJSON = Nothing,
          fcmNotificationId = Nothing
        }
    title = FCM.FCMNotificationTitle "Edit Destination"
    body = FCMNotificationBody "Driver accepted edited destination."

data PickupInstructionEntityData = PickupInstructionEntityData
  { instruction :: Maybe Text,
    audioUrl :: Maybe Text,
    customerName :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, Show)

sendPickupInstructionNotification ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Person ->
  PickupInstructionEntityData ->
  m ()
sendPickupInstructionNotification merchantOpCityId driver entityData = do
  let newCityId = cityFallback driver.clientBundleVersion merchantOpCityId -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  fcmConfig <- findFCMConfigWithFallback newCityId driver.id
  -- Build notification data
  let notificationData =
        FCM.FCMData
          { fcmNotificationType = FCM.PICKUP_INSTRUCTIONS,
            fcmShowNotification = FCM.SHOW,
            fcmEntityType = FCM.Person,
            fcmEntityIds = getId driver.id,
            fcmEntityData = entityData,
            fcmNotificationJSON = FCM.createAndroidNotification title body FCM.PICKUP_INSTRUCTIONS Nothing,
            fcmOverlayNotificationJSON = Nothing,
            fcmNotificationId = Nothing
          }
      title = FCM.FCMNotificationTitle "Pickup Instructions"
      body = FCMNotificationBody $ fromMaybe "You have received pickup instructions" entityData.instruction
  logInfo $ "PickupInstructionNotification: Notification data: " <> show notificationData
  -- Send notification
  FCM.notifyPersonWithPriority fcmConfig (Just FCM.HIGH) (clearDeviceToken driver.id) notificationData (FCMNotificationRecipient driver.id.getId driver.deviceToken) EulerHS.Prelude.id
