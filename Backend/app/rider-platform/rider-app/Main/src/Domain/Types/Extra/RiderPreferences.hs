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
