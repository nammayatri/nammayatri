{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.NearbyDrivers where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.VehicleVariant
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data NearbyDriver = NearbyDriver
  { bearing :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    id :: Data.Text.Text,
    location :: Kernel.External.Maps.Types.LatLong,
    vehicleType :: Kernel.Prelude.Maybe Domain.Types.VehicleVariant.VehicleVariant
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyDriverReq = NearbyDriverReq {location :: Kernel.External.Maps.Types.LatLong, radius :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyDriverRes = NearbyDriverRes {drivers :: [NearbyDriver]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
