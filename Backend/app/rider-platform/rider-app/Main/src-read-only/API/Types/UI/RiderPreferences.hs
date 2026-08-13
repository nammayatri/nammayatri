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

data AllRiderPreferencesResp = AllRiderPreferencesResp {locationPickups :: [LocationPickupRespData], rideConfig :: Kernel.Prelude.Maybe RideConfigData}
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

data RideConfigData = RideConfigData
  { isAutoAssign :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isBusiness :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isPetRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isTransitEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderPreferenceReq = RiderPreferenceReq
  { locationData :: Kernel.Prelude.Maybe LocationPickupReqData,
    preferenceType :: Domain.Types.Extra.RiderPreferences.PreferenceType,
    rideConfigData :: Kernel.Prelude.Maybe RideConfigData
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderPreferencesResp = RiderPreferencesResp {locationPickups :: [LocationPickupRespData]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
