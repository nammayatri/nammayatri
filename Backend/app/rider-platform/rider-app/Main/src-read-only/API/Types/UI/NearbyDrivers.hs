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

data BusRideInfo = BusRideInfo
  { busNumber :: Data.Text.Text,
    destination :: Kernel.External.Maps.Types.LatLong,
    driverName :: Kernel.Prelude.Maybe Data.Text.Text,
    groupId :: Kernel.Prelude.Maybe Data.Text.Text,
    routeCode :: Data.Text.Text,
    routeLongName :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CarRideInfo = CarRideInfo {pickupLocation :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverInfo = DriverInfo
  { applicableServiceTierTypes :: [Domain.Types.ServiceTierType.ServiceTierType],
    bearing :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    distance :: Kernel.Types.Distance.Meters,
    driverId :: Data.Text.Text,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    rideDetails :: Kernel.Prelude.Maybe RideDetails
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearByDriversBucket = NearByDriversBucket {driverInfo :: [DriverInfo], radius :: Kernel.Types.Distance.Meters, variant :: Domain.Types.VehicleVariant.VehicleVariant}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyDriverReq = NearbyDriverReq {location :: Kernel.External.Maps.Types.LatLong, radius :: Kernel.Types.Distance.Meters, vehicleVariants :: Kernel.Prelude.Maybe [Domain.Types.VehicleVariant.VehicleVariant]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearbyDriverRes = NearbyDriverRes {buckets :: [NearByDriversBucket], serviceTierTypeToVehicleVariant :: Data.Aeson.Value, variantLevelDriverCount :: Data.Aeson.Value}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PilotRideInfo = PilotRideInfo
  { destination :: Kernel.External.Maps.Types.LatLong,
    driverName :: Kernel.Prelude.Maybe Data.Text.Text,
    dutyType :: Kernel.Prelude.Maybe Data.Text.Text,
    endAddress :: Kernel.Prelude.Maybe Data.Text.Text,
    groupId :: Kernel.Prelude.Maybe Data.Text.Text,
    pilotNumber :: Data.Text.Text,
    scheduledTripTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    source :: Kernel.External.Maps.Types.LatLong,
    startAddress :: Kernel.Prelude.Maybe Data.Text.Text,
    vipName :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideDetails = RideDetails {rideId :: Data.Text.Text, rideInfo :: Kernel.Prelude.Maybe RideInfo}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideInfo
  = Bus BusRideInfo
  | Car CarRideInfo
  | Pilot PilotRideInfo
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
