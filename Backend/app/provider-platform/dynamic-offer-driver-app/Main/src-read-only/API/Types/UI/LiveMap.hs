{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.LiveMap where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data DriverStatus
  = TO_PICKUP
  | ON_TRIP
  | ONLINE
  | SILENT
  | OFFLINE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MapDriverInfoRes = MapDriverInfoRes
  { driverName :: Kernel.Prelude.Text,
    driverStatus :: DriverStatus,
    vehicleNumber :: Kernel.Prelude.Text,
    vehicleStatus :: VehicleStatus,
    position :: Kernel.External.Maps.Types.LatLong,
    source :: Kernel.Prelude.Text,
    destination :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleStatus
  = Active
  | InActive
  | Valid
  | Invalid
  | Pending
  | OnRide
  | TripAssigned
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
