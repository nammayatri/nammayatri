{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.External.FCM.Types where

import Beckn.Utils.TH
import Control.Lens.TH
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Default.Class
import Data.Map (Map)
import Data.Text (Text)
import EulerHS.Prelude

-- | Device token
newtype FCMRecipientToken
  = FCMRecipientToken
      { getFCMRecipientToken :: Text
      }
  deriving (Show)

deriveIdentifierInstances ''FCMRecipientToken

-- | FCM authorization token
newtype FCMAuthToken
  = FCMAuthToken
      { getFCMAuthToken :: Text
      }
  deriving (Show)

deriveIdentifierInstances ''FCMAuthToken

-- | Priority of a message to send to Android devices
data FCMAndroidMessagePriority = NORMAL | HIGH
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- | Priority levels of a notification
data FCMNotificationPriority = PRIORITY_UNSPECIFIED | PRIORITY_MIN | PRIORITY_LOW | PRIORITY_DEFAULT | PRIORITY_HIGH | PRIORITY_MAX
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- | Different visibility levels of a notification
data FCMNotificationVisibility = VISIBILITY_UNSPECIFIED | PRIVATE | PUBLIC | SECRET
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- | FCM payload
data FCMData
  = FCMData
      { _fcmNotificationType :: Text,
        _fcmShowNotification :: Text,
        _fcmEntityIds :: Text,
        _fcmEntityType :: Text
      }
  deriving (Eq, Show)

$(makeLenses ''FCMData)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMData)

-- | HTTP request headers
type FCMHeaders = Map Text Text

-- | Target to send a message to. Target can be only one of the following:
data FCMTarget = FCMTopic Text | FCMToken Text | FCMCondition Text

-- | Represents a color in the RGBA color space
data FCMColor
  = FCMColor
      { _fcmRed :: Int,
        _fcmGreen :: Int,
        _fcmBlue :: Int,
        _fcmAlpha :: Int
      }
  deriving (Eq, Show)

$(makeLenses ''FCMColor)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMColor)

-- | Options for features provided by the FCM SDK for Android.
data FCMAndroidOptions
  = FCMAndroidOptions
      { _fcmdAnalyticsLabel :: !(Maybe Text)
      }
  deriving (Eq, Show)

$(makeLenses ''FCMAndroidOptions)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMAndroidOptions)

instance Default FCMAndroidOptions where
  def = FCMAndroidOptions Nothing

-- | Options for features provided by the FCM SDK for iOS
data FCMApnsOptions
  = FCMApnsOptions
      { _fcmaAnalyticsLabel :: !(Maybe Text),
        _fcmaImage :: !(Maybe Text)
      }
  deriving (Eq, Show)

$(makeLenses ''FCMApnsOptions)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMApnsOptions)

instance Default FCMApnsOptions where
  def = FCMApnsOptions Nothing Nothing

-- | Options for features provided by the FCM SDK for iOS
data FCMWebpushOptions
  = FCMWebpushOptions
      { _fcmwAnalyticsLabel :: !(Maybe Text),
        _fcmwLink :: !(Maybe Text)
      }
  deriving (Eq, Show)

$(makeLenses ''FCMWebpushOptions)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMWebpushOptions)

instance Default FCMWebpushOptions where
  def = FCMWebpushOptions Nothing Nothing

-- | Settings to control notification LED
data FCMLightSettings
  = FCMLightSettings
      { _fcmLightOnDuration :: !(Maybe Text),
        _fcmLightOffDuration :: !(Maybe Text),
        _fcmColor :: !(Maybe FCMColor)
      }
  deriving (Eq, Show)

$(makeLenses ''FCMLightSettings)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMLightSettings)

instance Default FCMLightSettings where
  def = FCMLightSettings Nothing Nothing Nothing

-- | Basic notification template to use across all platforms
data FCMNotification
  = FCMNotification
      { _fcmTitle :: !(Maybe Text),
        _fcmBody :: !(Maybe Text),
        _fcmImage :: !(Maybe Text)
      }
  deriving (Eq, Show)

$(makeLenses ''FCMNotification)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMNotification)

instance Default FCMNotification where
  def = FCMNotification Nothing Nothing Nothing

-- | Notification to send to android devices
data FCMAndroidNotification
  = FCMAndroidNotification
      { _fcmdTitle :: !(Maybe Text),
        _fcmdBody :: !(Maybe Text),
        _fcmdIcon :: !(Maybe Text),
        _fcmdColor :: !(Maybe Text),
        _fcmdSound :: !(Maybe Text),
        _fcmdTag :: !(Maybe Text),
        _fcmdClickAction :: !(Maybe Text),
        _fcmdBodyLocKey :: !(Maybe Text),
        _fcmdBodyLockArgs :: !(Maybe [Text]),
        _fcmdTitleLocKey :: !(Maybe Text),
        _fcmdTitleLockArgs :: !(Maybe [Text]),
        _fcmdChannelId :: !(Maybe Text),
        _fcmdTicker :: !(Maybe Text),
        _fcmdSticky :: !(Maybe Bool),
        _fcmdEventTime :: !(Maybe Text),
        _fcmdLocalOnly :: !(Maybe Bool),
        _fcmdNotificationPriority :: !(Maybe FCMNotificationPriority),
        _fcmdDefaultSound :: !(Maybe Bool),
        _fcmdDefalutVibrateTimings :: !(Maybe Bool),
        _fcmdDefaultLightSettings :: !(Maybe Bool),
        _fcmdVibrateTimings :: !(Maybe [Text])
      }
  deriving (Eq, Show)

$(makeLenses ''FCMAndroidNotification)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMAndroidNotification)

instance Default FCMAndroidNotification where
  def =
    let z = Nothing
     in FCMAndroidNotification z z z z z z z z z z z z z z z z z z z z z

-- | Android specific options for messages sent through FCM connection server
data FCMAndroidConfig
  = FCMAndroidConfig
      { _fcmdCollapseKey :: !(Maybe Text),
        _fcmdPriority :: !(Maybe FCMAndroidMessagePriority),
        _fcmdTtl :: !(Maybe Text),
        _fcmdRestrictedPackageName :: !(Maybe Text),
        _fcmdData :: !(Maybe FCMData),
        _fcmdNotification :: !(Maybe FCMAndroidNotification),
        _fcmdOptions :: !(Maybe FCMAndroidOptions),
        _fcmdDirectBootOk :: !(Maybe Bool)
      }
  deriving (Eq, Show)

$(makeLenses ''FCMAndroidConfig)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMAndroidConfig)

instance Default FCMAndroidConfig where
  def =
    let z = Nothing
     in FCMAndroidConfig z z z z z z z z

-- | Apple Push Notification Service specific options
data FCMApnsConfig
  = FCMApnsConfig
      { _fcmaHeaders :: !(Maybe FCMHeaders),
        _fcmaPayload :: !(Maybe Value),
        _fcmaOptions :: !(Maybe FCMApnsOptions)
      }
  deriving (Eq, Show)

$(makeLenses ''FCMApnsConfig)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMApnsConfig)

instance Default FCMApnsConfig where
  def = FCMApnsConfig Nothing Nothing Nothing

-- | Webpush protocol specific options
data FCMWebpushConfig
  = FCMWebpushConfig
      { _fcmwHeaders :: !(Maybe FCMHeaders),
        _fcmwData :: !(Maybe FCMData),
        _fcmwNotification :: !(Maybe Value),
        _fcmwOptions :: !(Maybe FCMWebpushOptions)
      }
  deriving (Eq, Show)

$(makeLenses ''FCMWebpushConfig)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMWebpushConfig)

instance Default FCMWebpushConfig where
  def = FCMWebpushConfig Nothing Nothing Nothing Nothing

-- | Message to send by Firebase Cloud Messaging Service
data FCMMessage
  = FCMMessage
      { _fcmToken :: !(Maybe Text),
        _fcmTopic :: !(Maybe Text),
        _fcmCondition :: !(Maybe Text),
        _fcmName :: !(Maybe Text),
        _fcmData :: !(Maybe FCMData),
        _fcmNotification :: !(Maybe FCMNotification),
        _fcmAndroid :: !(Maybe FCMAndroidConfig),
        _fcmWebpush :: !(Maybe FCMWebpushConfig),
        _fcmApns :: !(Maybe FCMApnsConfig),
        _fcmOptions :: !(Maybe FCMAndroidOptions)
      }
  deriving (Eq, Show)

$(makeLenses ''FCMMessage)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMMessage)

instance Default FCMMessage where
  def =
    let z = Nothing
     in FCMMessage z z z z z z z z z z

data FCMRequest
  = FCMRequest
      { _fcmeMessage :: FCMMessage
      }
  deriving (Eq, Show)

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
  | -- (HTTP error code = 403) The authenticated sender ID is different from the sender ID for the registration token
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

data FCMError
  = FCMError
      { _fcmerrCode :: !Int,
        _fcmerrStatus :: !FCMErrorCode,
        _fcmerrMessage :: !Text
      }
  deriving (Eq, Show)

$(makeLenses ''FCMError)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMError)

data FCMClientError
  = FCMClientError
      { _fcmerrError :: FCMError
      }
  deriving (Eq, Show)

$(makeLenses ''FCMClientError)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMClientError)

data FCMResponse
  = FCMResponseSuccess !FCMMessage
  | FCMResponseError !FCMClientError
  deriving (Show)

$(makeLenses ''FCMResponse)

$(makePrisms ''FCMResponse)

$(deriveJSON (aesonPrefix snakeCase) {omitNothingFields = True} ''FCMResponse)
