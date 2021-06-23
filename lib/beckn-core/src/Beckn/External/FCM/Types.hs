{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.External.FCM.Types where

import Beckn.Types.App
import Beckn.Types.Field ((:::))
import Beckn.Utils.TH
import Beckn.Utils.Text (encodeToText)
import Control.Lens.TH
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Default.Class
import EulerHS.Prelude hiding ((.=))

type FCMFlow m r = (HasFlowEnv m r ["fcmUrl" ::: BaseUrl, "fcmJsonPath" ::: Maybe Text])

-- | Device token
newtype FCMRecipientToken = FCMRecipientToken
  { getFCMRecipientToken :: Text
  }
  deriving (Show)

deriveIdentifierInstances ''FCMRecipientToken

-- | FCM authorization token
newtype FCMAuthToken = FCMAuthToken
  { getFCMAuthToken :: Text
  }
  deriving (Show)

deriveIdentifierInstances ''FCMAuthToken

-- | FCM notification title
newtype FCMNotificationTitle = FCMNotificationTitle
  { getFCMNotificationTitle :: Text
  }
  deriving (Show)

deriveIdentifierInstances ''FCMNotificationTitle

-- | FCM notification body
newtype FCMNotificationBody = FCMNotificationBody
  { getFCMNotificationBody :: Text
  }
  deriving (Show)

deriveIdentifierInstances ''FCMNotificationBody

-- | Notification image / icon path
newtype FCMNotificationIconUrl = FCMNotificationIconUrl
  { getFCMNotificationIconUrl :: Text
  }
  deriving (Show)

deriveIdentifierInstances ''FCMNotificationIconUrl

-- | Notification types
data FCMNotificationType
  = REGISTRATION_APPROVED
  | TRACKING_CALLBACK
  | EXPIRED_CASE
  | CANCELLED_PRODUCT
  | DRIVER_ASSIGNMENT
  | DRIVER_UNASSIGNED
  | TRIP_STARTED
  | TRIP_FINISHED
  | ALLOCATION_REQUEST
  | ALLOCATION_REQUEST_UNASSIGNED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- | Entity types types
data FCMEntityType = Case | Product | Organization | Person
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- | Priority of a message to send to Android devices
data FCMAndroidMessagePriority = NORMAL | HIGH
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- | Priority levels of a notification
data FCMNotificationPriority
  = PRIORITY_UNSPECIFIED
  | PRIORITY_MIN
  | PRIORITY_LOW
  | PRIORITY_DEFAULT
  | PRIORITY_HIGH
  | PRIORITY_MAX
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- | Different visibility levels of a notification
data FCMNotificationVisibility = VISIBILITY_UNSPECIFIED | PRIVATE | PUBLIC | SECRET
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data FCMShowNotification = SHOW | DO_NOT_SHOW
  deriving (Show, Eq, Read, Generic)

instance ToJSON FCMShowNotification where
  toJSON SHOW = "true"
  toJSON _ = "false"

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
  deriving (Eq, Show)

$(makeLenses ''FCMColor)

$(deriveToJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMColor)

-- | Options for features provided by the FCM SDK for Android.
newtype FCMAndroidOptions = FCMAndroidOptions
  { fcmdAnalyticsLabel :: Maybe Text
  }
  deriving (Eq, Show)

$(makeLenses ''FCMAndroidOptions)

$(deriveToJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMAndroidOptions)

instance Default FCMAndroidOptions where
  def = FCMAndroidOptions Nothing

-- | Options for features provided by the FCM SDK for iOS
data FCMApnsOptions = FCMApnsOptions
  { fcmaAnalyticsLabel :: !(Maybe Text),
    fcmaImage :: !(Maybe FCMNotificationIconUrl)
  }
  deriving (Eq, Show)

$(makeLenses ''FCMApnsOptions)

$(deriveToJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMApnsOptions)

instance Default FCMApnsOptions where
  def = FCMApnsOptions Nothing Nothing

-- | Options for features provided by the FCM SDK for iOS
data FCMWebpushOptions = FCMWebpushOptions
  { fcmwAnalyticsLabel :: !(Maybe Text),
    fcmwLink :: !(Maybe Text)
  }
  deriving (Eq, Show)

$(makeLenses ''FCMWebpushOptions)

$(deriveToJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMWebpushOptions)

instance Default FCMWebpushOptions where
  def = FCMWebpushOptions Nothing Nothing

-- | Settings to control notification LED
data FCMLightSettings = FCMLightSettings
  { fcmLightOnDuration :: !(Maybe Text),
    fcmLightOffDuration :: !(Maybe Text),
    fcmColor :: !(Maybe FCMColor)
  }
  deriving (Eq, Show)

$(makeLenses ''FCMLightSettings)

$(deriveToJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMLightSettings)

instance Default FCMLightSettings where
  def = FCMLightSettings Nothing Nothing Nothing

-- | Basic notification template to use across all platforms
data FCMNotification = FCMNotification
  { fcmTitle :: !(Maybe FCMNotificationTitle),
    fcmBody :: !(Maybe FCMNotificationBody),
    fcmImage :: !(Maybe FCMNotificationIconUrl)
  }
  deriving (Eq, Show)

$(makeLenses ''FCMNotification)

$(deriveToJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMNotification)

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
  deriving (Eq, Show)

$(makeLenses ''FCMAndroidNotification)

$(deriveToJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMAndroidNotification)

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
data FCMAndroidData = FCMAndroidData
  { fcmNotificationType :: FCMNotificationType,
    fcmShowNotification :: FCMShowNotification,
    fcmEntityType :: FCMEntityType,
    fcmEntityIds :: Text,
    fcmNotificationJSON :: FCMAndroidNotification
  }
  deriving (Eq, Show)

$(makeLenses ''FCMAndroidData)

instance ToJSON FCMAndroidData where
  toJSON FCMAndroidData {..} =
    object
      [ "notification_type" .= fcmNotificationType,
        "show_notification" .= fcmShowNotification,
        "entity_type" .= fcmEntityType,
        "entity_ids" .= fcmEntityIds,
        "notification_json" .= encodeToText fcmNotificationJSON
      ]

-- | Android specific options for messages sent through FCM connection server
data FCMAndroidConfig = FCMAndroidConfig
  { fcmdCollapseKey :: !(Maybe Text),
    fcmdPriority :: !(Maybe FCMAndroidMessagePriority),
    fcmdTtl :: !(Maybe Text),
    fcmdRestrictedPackageName :: !(Maybe Text),
    fcmdData :: !(Maybe FCMAndroidData),
    fcmdOptions :: !(Maybe FCMAndroidOptions),
    fcmdDirectBootOk :: !(Maybe Bool)
  }
  deriving (Eq, Show)

$(makeLenses ''FCMAndroidConfig)

$(deriveToJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMAndroidConfig)

instance Default FCMAndroidConfig where
  def =
    let z = Nothing
     in FCMAndroidConfig z z z z z z z

-- | Apple Push Notification Service specific options
data FCMApnsConfig = FCMApnsConfig
  { fcmaHeaders :: !(Maybe FCMHeaders),
    fcmaPayload :: !(Maybe Value),
    fcmaOptions :: !(Maybe FCMApnsOptions)
  }
  deriving (Eq, Show)

$(makeLenses ''FCMApnsConfig)

$(deriveToJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMApnsConfig)

instance Default FCMApnsConfig where
  def = FCMApnsConfig Nothing Nothing Nothing

-- | Webpush protocol specific options
data FCMWebpushConfig = FCMWebpushConfig
  { fcmwHeaders :: !(Maybe FCMHeaders),
    fcmwData :: !(Maybe FCMAndroidData),
    fcmwNotification :: !(Maybe Value),
    fcmwOptions :: !(Maybe FCMWebpushOptions)
  }
  deriving (Eq, Show)

$(makeLenses ''FCMWebpushConfig)

$(deriveToJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMWebpushConfig)

instance Default FCMWebpushConfig where
  def = FCMWebpushConfig Nothing Nothing Nothing Nothing

-- | Message to send by Firebase Cloud Messaging Service
data FCMMessage = FCMMessage
  { fcmToken :: !(Maybe FCMRecipientToken),
    fcmTopic :: !(Maybe Text),
    fcmCondition :: !(Maybe Text),
    fcmNotification :: !(Maybe FCMNotification),
    fcmAndroid :: !(Maybe FCMAndroidConfig),
    fcmWebpush :: !(Maybe FCMWebpushConfig),
    fcmApns :: !(Maybe FCMApnsConfig),
    fcmOptions :: !(Maybe FCMAndroidOptions)
  }
  deriving (Eq, Show)

$(makeLenses ''FCMMessage)

$(deriveToJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMMessage)

instance Default FCMMessage where
  def =
    let z = Nothing
     in FCMMessage z z z z z z z z

newtype FCMRequest = FCMRequest
  { fcmeMessage :: FCMMessage
  }
  deriving (Eq, Show)

$(makeLenses ''FCMRequest)

$(deriveToJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMRequest)

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

data FCMError = FCMError
  { fcmerrCode :: Int,
    fcmerrStatus :: FCMErrorCode,
    fcmerrMessage :: !Text
  }
  deriving (Show, Eq, Read, Generic)

$(makeLenses ''FCMError)

$(deriveFromJSON (aesonPrefix snakeCase) ''FCMError)

-- | Message to send by Firebase Cloud Messaging Service
data FCMResponse = FCMResponse
  { fcmName :: Maybe Text,
    fcmerrError :: Maybe FCMError
  }
  deriving (Show, Eq, Read, Generic)

$(deriveFromJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMResponse)
