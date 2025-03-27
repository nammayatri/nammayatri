{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.NearbyDrivers where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.VehicleVariant
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Distance
import Servant
import Tools.Auth

data DriverInfo = DriverInfo
  { applicableServiceTierTypes :: [Domain.Types.ServiceTierType.ServiceTierType],
    bearing :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    distance :: Kernel.Types.Distance.Meters,
    driverId :: Data.Text.Text,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearByDriversBucket = NearByDriversBucket {driverInfo :: [DriverInfo], radius :: Kernel.Types.Distance.Meters, variant :: Domain.Types.VehicleVariant.VehicleVariant}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyDriverReq = NearbyDriverReq {location :: Kernel.External.Maps.Types.LatLong, radius :: Kernel.Types.Distance.Meters}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyDriverRes = NearbyDriverRes {buckets :: [NearByDriversBucket], serviceTierTypeToVehicleVariant :: Data.Aeson.Value, variantLevelDriverCount :: Data.Aeson.Value}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
