{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.RiderPreferences where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Extra.RiderPreferences
import qualified Domain.Types.RiderPreferences
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data AllRiderPreferencesResp = AllRiderPreferencesResp {locationPickups :: [LocationPickupRespData], notificationPreference :: Kernel.Prelude.Maybe NotificationPreferenceRespData}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LocationPickupReqData = LocationPickupReqData
  { pickupAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupAddressSubtitle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupLat :: Kernel.Prelude.Double,
    pickupLon :: Kernel.Prelude.Double,
    sourceAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sourceLat :: Kernel.Prelude.Double,
    sourceLon :: Kernel.Prelude.Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LocationPickupRespData = LocationPickupRespData
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.RiderPreferences.RiderPreferences,
    pickupAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupAddressSubtitle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupLat :: Kernel.Prelude.Double,
    pickupLon :: Kernel.Prelude.Double,
    sourceAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sourceGeohash :: Kernel.Prelude.Text,
    sourceLat :: Kernel.Prelude.Double,
    sourceLon :: Kernel.Prelude.Double,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NotificationPreferenceReqData = NotificationPreferenceReqData {enabledCategories :: [Domain.Types.Extra.RiderPreferences.NotificationCategory], osPermissionGranted :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NotificationPreferenceRespData = NotificationPreferenceRespData
  { createdAt :: Kernel.Prelude.UTCTime,
    enabledCategories :: [Domain.Types.Extra.RiderPreferences.NotificationCategory],
    osPermissionGranted :: Kernel.Prelude.Bool,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderPreferenceReq = RiderPreferenceReq
  { locationData :: Kernel.Prelude.Maybe LocationPickupReqData,
    notificationPreferenceData :: Kernel.Prelude.Maybe NotificationPreferenceReqData,
    preferenceType :: Domain.Types.Extra.RiderPreferences.PreferenceType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderPreferencesResp = RiderPreferencesResp {locationPickups :: [LocationPickupRespData]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
