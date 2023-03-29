{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.FCM.Types where

import Control.Lens.TH
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Default.Class
import EulerHS.Prelude hiding (id, (.=))
import Kernel.Storage.Esqueleto (PersistField, PersistFieldSql)
import Kernel.Types.App
import Kernel.Utils.GenericPretty
import Kernel.Utils.TH
import Kernel.Utils.Text (decodeFromText, encodeToText)

data FCMConfig = FCMConfig
  { fcmUrl :: BaseUrl,
    fcmServiceAccount :: Text,
    fcmTokenKeyPrefix :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (PrettyShow, FromJSON, ToJSON)

data FCMNotificationRecipient = FCMNotificationRecipient
  { id :: Text,
    token :: Maybe FCMRecipientToken
  }

-- | Device token
newtype FCMRecipientToken = FCMRecipientToken
  { getFCMRecipientToken :: Text
  }
  deriving newtype (PersistField, PersistFieldSql, Show, PrettyShow)

deriveIdentifierInstances ''FCMRecipientToken

-- | FCM authorization token
newtype FCMAuthToken = FCMAuthToken
  { getFCMAuthToken :: Text
  }
  deriving (Show)
  deriving newtype (PrettyShow)

deriveIdentifierInstances ''FCMAuthToken

-- | FCM notification title
newtype FCMNotificationTitle = FCMNotificationTitle
  { getFCMNotificationTitle :: Text
  }
  deriving (Show)
  deriving newtype (PrettyShow)

deriveIdentifierInstances ''FCMNotificationTitle

-- | FCM notification body
newtype FCMNotificationBody = FCMNotificationBody
  { getFCMNotificationBody :: Text
  }
  deriving (Show)
  deriving newtype (PrettyShow)

deriveIdentifierInstances ''FCMNotificationBody

-- | Notification image / icon path
newtype FCMNotificationIconUrl = FCMNotificationIconUrl
  { getFCMNotificationIconUrl :: Text
  }
  deriving (Show)
  deriving newtype (PrettyShow)

deriveIdentifierInstances ''FCMNotificationIconUrl

-- | Notification types
data FCMNotificationType
  = REGISTRATION_APPROVED
  | EXPIRED_CASE
  | CANCELLED_PRODUCT
  | REALLOCATE_PRODUCT
  | DRIVER_ASSIGNMENT
  | TRIP_STARTED
  | TRIP_FINISHED
  | ALLOCATION_REQUEST
  | ALLOCATION_REQUEST_UNASSIGNED
  | ACCOUNT_DISABLED
  | TRIGGER_SERVICE
  | FARE_POLICY_CHANGED
  | DISCOUNT_CHANGED
  | QUOTE_RECEIVED
  | NEW_RIDE_AVAILABLE
  | DRIVER_QUOTE_INCOMING
  | DRIVER_ON_THE_WAY
  | DRIVER_HAS_REACHED
  | CLEARED_FARE
  | CANCELLED_SEARCH_REQUEST
  | NEW_MESSAGE
  | REFERRAL_ACTIVATED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable FCMNotificationType

-- | Entity types types
data FCMEntityType = SearchRequest | Product | Merchant | Person
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable FCMEntityType

-- | Priority of a message to send to Android devices
data FCMAndroidMessagePriority = NORMAL | HIGH
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable FCMAndroidMessagePriority

-- | Priority levels of a notification
data FCMNotificationPriority
  = PRIORITY_UNSPECIFIED
  | PRIORITY_MIN
  | PRIORITY_LOW
  | PRIORITY_DEFAULT
  | PRIORITY_HIGH
  | PRIORITY_MAX
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable FCMNotificationPriority

-- | Different visibility levels of a notification
data FCMNotificationVisibility = VISIBILITY_UNSPECIFIED | PRIVATE | PUBLIC | SECRET
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable FCMNotificationVisibility

data FCMShowNotification = SHOW | DO_NOT_SHOW
  deriving (Show, Eq, Read, Generic)
  deriving (PrettyShow) via Showable FCMShowNotification

instance ToJSON FCMShowNotification where
  toJSON SHOW = "true"
  toJSON _ = "false"

instance FromJSON FCMShowNotification where
  parseJSON = withText "FCMShowNotification" \case
    "true" -> pure SHOW
    "false" -> pure DO_NOT_SHOW
    str -> typeMismatch "FCMShowNotification" (String str)

-- | HTTP request headers
type FCMHeaders = Map Text Text

-- | Target to send a message to. Target can be only one of the following:
-- data FCMTarget = FCMTopic Text | FCMToken Text | FCMCondition Text

-- | Represents a color in the RGBA color space
data FCMColor = FCMColor
  { fcmRed :: Int,
    fcmGreen :: Int,
    fcmBlue :: Int,
    fcmAlpha :: Int
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMColor)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMColor)

-- | Options for features provided by the FCM SDK for Android.
newtype FCMAndroidOptions = FCMAndroidOptions
  { fcmdAnalyticsLabel :: Maybe Text
  }
  deriving (Eq, Show)
  deriving newtype (PrettyShow)

$(makeLenses ''FCMAndroidOptions)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMAndroidOptions)

instance Default FCMAndroidOptions where
  def = FCMAndroidOptions Nothing

-- | Options for features provided by the FCM SDK for iOS
data FCMApnsOptions = FCMApnsOptions
  { fcmaAnalyticsLabel :: !(Maybe Text),
    fcmaImage :: !(Maybe FCMNotificationIconUrl)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMApnsOptions)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMApnsOptions)

instance Default FCMApnsOptions where
  def = FCMApnsOptions Nothing Nothing

-- | Options for features provided by the FCM SDK for iOS
data FCMWebpushOptions = FCMWebpushOptions
  { fcmwAnalyticsLabel :: !(Maybe Text),
    fcmwLink :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMWebpushOptions)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMWebpushOptions)

instance Default FCMWebpushOptions where
  def = FCMWebpushOptions Nothing Nothing

-- | Settings to control notification LED
data FCMLightSettings = FCMLightSettings
  { fcmLightOnDuration :: !(Maybe Text),
    fcmLightOffDuration :: !(Maybe Text),
    fcmColor :: !(Maybe FCMColor)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMLightSettings)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMLightSettings)

instance Default FCMLightSettings where
  def = FCMLightSettings Nothing Nothing Nothing

-- | Basic notification template to use across all platforms
data FCMNotification = FCMNotification
  { fcmTitle :: !(Maybe FCMNotificationTitle),
    fcmBody :: !(Maybe FCMNotificationBody),
    fcmImage :: !(Maybe FCMNotificationIconUrl)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMNotification)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMNotification)

instance Default FCMNotification where
  def = FCMNotification Nothing Nothing Nothing

-- | Notification to send to android devices
data FCMAndroidNotification = FCMAndroidNotification
  { fcmdTitle :: !(Maybe FCMNotificationTitle),
    fcmdBody :: !(Maybe FCMNotificationBody),
    fcmdIcon :: !(Maybe FCMNotificationIconUrl),
    fcmdColor :: !(Maybe Text),
    fcmdSound :: !(Maybe Text),
    fcmdTag :: !(Maybe FCMNotificationType),
    fcmdClickAction :: !(Maybe Text),
    fcmdBodyLocKey :: !(Maybe Text),
    fcmdBodyLockArgs :: !(Maybe [Text]),
    fcmdTitleLocKey :: !(Maybe Text),
    fcmdTitleLockArgs :: !(Maybe [Text]),
    fcmdChannelId :: !(Maybe Text),
    fcmdTicker :: !(Maybe Text),
    fcmdSticky :: !(Maybe Bool),
    fcmdEventTime :: !(Maybe Text),
    fcmdLocalOnly :: !(Maybe Bool),
    fcmdNotificationPriority :: !(Maybe FCMNotificationPriority),
    fcmdDefaultSound :: !(Maybe Bool),
    fcmdDefalutVibrateTimings :: !(Maybe Bool),
    fcmdDefaultLightSettings :: !(Maybe Bool),
    fcmdVibrateTimings :: !(Maybe [Text])
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMAndroidNotification)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMAndroidNotification)

instance Default FCMAndroidNotification where
  def =
    let sound = Just "default"
        channelId = Just "General"
     in FCMAndroidNotification
          { fcmdTitle = Nothing,
            fcmdBody = Nothing,
            fcmdIcon = Nothing,
            fcmdColor = Nothing,
            fcmdSound = sound,
            fcmdTag = Nothing,
            fcmdClickAction = Nothing,
            fcmdBodyLocKey = Nothing,
            fcmdBodyLockArgs = Nothing,
            fcmdTitleLocKey = Nothing,
            fcmdTitleLockArgs = Nothing,
            fcmdChannelId = channelId,
            fcmdTicker = Nothing,
            fcmdSticky = Nothing,
            fcmdEventTime = Nothing,
            fcmdLocalOnly = Nothing,
            fcmdNotificationPriority = Nothing,
            fcmdDefaultSound = Nothing,
            fcmdDefalutVibrateTimings = Nothing,
            fcmdDefaultLightSettings = Nothing,
            fcmdVibrateTimings = Nothing
          }

-- | FCM payload
data FCMData a = FCMData
  { fcmNotificationType :: FCMNotificationType,
    fcmShowNotification :: FCMShowNotification,
    fcmEntityType :: FCMEntityType,
    fcmEntityIds :: Text,
    fcmEntityData :: a,
    fcmNotificationJSON :: FCMAndroidNotification
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMData)

instance (ToJSON a) => ToJSON (FCMData a) where
  toJSON FCMData {..} =
    object
      [ "notification_type" .= fcmNotificationType,
        "show_notification" .= fcmShowNotification,
        "entity_type" .= fcmEntityType,
        "entity_ids" .= fcmEntityIds,
        "entity_data" .= encodeToText fcmEntityData,
        "notification_json" .= encodeToText fcmNotificationJSON
      ]

instance (FromJSON a) => FromJSON (FCMData a) where
  parseJSON = withObject "FCMData" \o ->
    FCMData
      <$> o .: "notification_type"
      <*> o .: "show_notification"
      <*> o .: "entity_type"
      <*> o .: "entity_ids"
      <*> (o .: "entity_data" >>= parseNotificationJson)
      <*> (o .: "notification_json" >>= parseNotificationJson)
    where
      parseNotificationJson str =
        maybe (typeMismatch "Json string" (String str)) pure $ decodeFromText str

-- | Android specific options for messages sent through FCM connection server
data FCMAndroidConfig a = FCMAndroidConfig
  { fcmdCollapseKey :: !(Maybe Text),
    fcmdPriority :: !(Maybe FCMAndroidMessagePriority),
    fcmdTtl :: !(Maybe Text),
    fcmdRestrictedPackageName :: !(Maybe Text),
    fcmdData :: !(Maybe (FCMData a)),
    fcmdOptions :: !(Maybe FCMAndroidOptions),
    fcmdDirectBootOk :: !(Maybe Bool)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMAndroidConfig)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMAndroidConfig)

instance (Default a) => Default (FCMAndroidConfig a) where
  def =
    let z = Nothing
     in FCMAndroidConfig z z z z z z z

-- | Apple Push Notification Service specific options
data FCMAlert = FCMAlert
  { fcmTitle :: !(Maybe Text),
    fcmBody :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMAlert)

instance ToJSON FCMAlert where
  toJSON FCMAlert {..} =
    object
      [ "title" .= fcmTitle,
        "body" .= fcmBody
      ]

instance FromJSON FCMAlert where
  parseJSON = withObject "FCMAlert" \o ->
    FCMAlert
      <$> o .: "title"
      <*> o .: "body"

instance Default FCMAlert where
  def = FCMAlert Nothing Nothing

-----------------------------------------

data FCMaps a = FCMaps
  { fcmAlert :: !(Maybe FCMAlert),
    fcmData :: !(Maybe (FCMData a)),
    fcmCategory :: !(Maybe FCMNotificationType)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMaps)

instance (ToJSON a) => ToJSON (FCMaps a) where
  toJSON FCMaps {..} =
    object
      [ "alert" .= fcmAlert,
        "data" .= fcmData,
        "category" .= fcmCategory
      ]

instance (FromJSON a) => FromJSON (FCMaps a) where
  parseJSON = withObject "FCMaps" \o ->
    FCMaps
      <$> o .: "alert"
      <*> o .: "data"
      <*> o .: "category"

instance Default (FCMaps a) where
  def = FCMaps Nothing Nothing Nothing

newtype FCMApnPayload a = FCMApnPayload
  { fcmAps :: Maybe (FCMaps a)
  }
  deriving (Eq, Show)
  deriving newtype (PrettyShow)

$(makeLenses ''FCMApnPayload)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMApnPayload)

instance Default (FCMApnPayload a) where
  def = FCMApnPayload Nothing

newtype FCMApnHeaders = FCMApnHeaders
  { fcmApnsPriority :: Maybe Text
  }
  deriving (Eq, Show)
  deriving newtype (PrettyShow)

$(makeLenses ''FCMApnHeaders)

instance ToJSON FCMApnHeaders where
  toJSON FCMApnHeaders {..} =
    object
      [ "apns-priority" .= fcmApnsPriority
      ]

instance FromJSON FCMApnHeaders where
  parseJSON = withObject "FCMApnHeaders" \o ->
    FCMApnHeaders
      <$> o .: "apns-priority"

instance Default FCMApnHeaders where
  def = FCMApnHeaders Nothing

data FCMApnsConfig a = FCMApnsConfig
  { fcmaHeaders :: !(Maybe FCMApnHeaders),
    fcmaPayload :: !(Maybe (FCMApnPayload a)),
    fcmaOptions :: !(Maybe FCMApnsOptions)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMApnsConfig)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMApnsConfig)

instance Default (FCMApnsConfig a) where
  def = FCMApnsConfig Nothing Nothing Nothing

-- | Webpush protocol specific options
data FCMWebpushConfig a = FCMWebpushConfig
  { fcmwHeaders :: !(Maybe FCMHeaders),
    fcmwData :: !(Maybe (FCMData a)),
    fcmwNotification :: !(Maybe Value),
    fcmwOptions :: !(Maybe FCMWebpushOptions)
  }
  deriving (Eq, Show, Generic)
  deriving (PrettyShow) via Showable (FCMWebpushConfig a)

$(makeLenses ''FCMWebpushConfig)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMWebpushConfig)

instance Default (FCMWebpushConfig a) where
  def = FCMWebpushConfig Nothing Nothing Nothing Nothing

-- | Message to send by Firebase Cloud Messaging Service
data FCMMessage a = FCMMessage
  { fcmToken :: !(Maybe FCMRecipientToken),
    fcmTopic :: !(Maybe Text),
    fcmCondition :: !(Maybe Text),
    fcmNotification :: !(Maybe FCMNotification),
    fcmAndroid :: !(Maybe (FCMAndroidConfig a)),
    fcmWebpush :: !(Maybe (FCMWebpushConfig a)),
    fcmApns :: !(Maybe (FCMApnsConfig a)),
    fcmOptions :: !(Maybe FCMAndroidOptions)
  }
  deriving (Eq, Show, Generic, PrettyShow)

$(makeLenses ''FCMMessage)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMMessage)

instance Default (FCMMessage a) where
  def =
    let z = Nothing
     in FCMMessage z z z z z z z z

newtype FCMRequest a = FCMRequest
  { fcmeMessage :: FCMMessage a
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (PrettyShow)

$(makeLenses ''FCMRequest)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMRequest)

-- | Priority levels of a notification
data FCMErrorCode
  = -- No more information is available about this error
    UNSPECIFIED_ERROR
  | -- (HTTP error code = 400) Request parameters were invalid.
    -- An extension of type google.rpc.BadRequest is returned to specify
    -- which field was invalid
    INVALID_ARGUMENT
  | -- (HTTP error code = 404) App instance was unregistered from FCM. This usually means that the token used is no longer valid and a new one must be used
    UNREGISTERED
  | -- (HTTP error code = 403) The authenticated sender Id is different from the sender Id for the registration token
    SENDER_ID_MISMATCH
  | -- (HTTP error code = 429) Sending limit exceeded for the message target. An extension of type google.rpc.QuotaFailure is returned to specify which quota got exceeded
    QUOTA_EXCEEDED
  | -- (HTTP error code = 401) APNs certificate or auth key was invalid or missing. Deprecated. Use THIRD_PARTY_AUTH_ERROR
    APNS_AUTH_ERROR
  | -- (HTTP error code = 503) The server is overloaded
    UNAVAILABLE
  | -- (HTTP error code = 500) An unknown internal error occurred
    INTERNAL
  | -- (HTTP error code = 401) APNs certificate or web push auth key was invalid or missing
    THIRD_PARTY_AUTH_ERROR
  | PERMISSION_DENIED
  | UNAUTHENTICATED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable FCMErrorCode

data FCMError = FCMError
  { fcmerrCode :: Int,
    fcmerrStatus :: FCMErrorCode,
    fcmerrMessage :: !Text
  }
  deriving (Show, Eq, Read, Generic, PrettyShow)

$(makeLenses ''FCMError)

$(deriveJSON (aesonPrefix snakeCase) ''FCMError)

-- | Message to send by Firebase Cloud Messaging Service
data FCMResponse = FCMResponse
  { fcmName :: Maybe Text,
    fcmerrError :: Maybe FCMError
  }
  deriving (Show, Eq, Read, Generic, PrettyShow)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMResponse)
