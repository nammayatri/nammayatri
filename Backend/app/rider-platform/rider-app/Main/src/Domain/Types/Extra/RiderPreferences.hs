module Domain.Types.Extra.RiderPreferences where

import Data.Aeson
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH

-- PreferenceType is the discriminator stored in the DB column `preference_type`.
-- for that category is defined in PreferenceData below.
-- mkBeamInstancesForEnum generates the Beam/DB read-write instances.
-- mkHttpInstancesForEnum generates the Servant query-param / path-param instances.
data PreferenceType
  = LOCATION_PICKUP
  | NOTIFICATION_PREFERENCE
  deriving (Show, Read, Eq, Ord, Generic, ToSchema)

instance ToJSON PreferenceType where
  toJSON = Data.Aeson.String . T.pack . show

instance FromJSON PreferenceType where
  parseJSON = Data.Aeson.withText "PreferenceType" $ \t ->
    case readMaybe (T.unpack t) of
      Just v -> pure v
      Nothing -> fail $ "Unknown PreferenceType: " <> T.unpack t

$(mkBeamInstancesForEnum ''PreferenceType)

$(mkHttpInstancesForEnum ''PreferenceType)

-- PreferenceData is stored as JSONB in the DB.
-- The tagged JSON encoding {"tag": "LocationPickupPreference", "contents": {...}}
-- lets us decode the right constructor without a separate type column.
data PreferenceData
  = LocationPickupPreference LocationPickupData
  | NotificationPreference NotificationPreferenceData
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- Holds everything needed to auto-fill a pickup point for a given source location.
-- sourceGeohash is derived from sourceLat/sourceLon on the backend (8-char, ~38m precision)
-- and used as the lookup key so we can match "same area" without exact coordinate equality.
data LocationPickupData = LocationPickupData
  { sourceGeohash :: Text,
    sourceLat :: Double,
    sourceLon :: Double,
    sourceAddress :: Maybe Text,
    pickupLat :: Double,
    pickupLon :: Double,
    pickupAddress :: Maybe Text,
    pickupAddressSubtitle :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- osPermissionGranted reflects the client's last-reported OS-level push permission
-- state. It is informational only — the send-path gate in Tools.Notifications checks
-- only enabledCategories, since a denied OS permission already blocks delivery at the
-- FCM/APNs layer and doesn't need a second enforcement point here.
data NotificationPreferenceData = NotificationPreferenceData
  { osPermissionGranted :: Bool,
    enabledCategories :: [NotificationCategory]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- User-facing grouping shown in the notification-permission popup. Deliberately kept
-- separate from Kernel.External.Notification.Interface.Types.Category (an external,
-- FCM-level enum we don't own) — every MerchantPushNotification.key is mapped to one
-- of these via MerchantPushNotification.notificationCategory.
data NotificationCategory
  = RIDE_RELATED
  | PROMOTIONAL
  | OFFERS
  | PAYMENTS
  | SAFETY
  | ACCOUNT
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''NotificationCategory)
