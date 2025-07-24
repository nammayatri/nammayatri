{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SharedRide where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.SharedBooking
import qualified Domain.Types.VehicleVariant
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data SharedRide = SharedRide
  { bppSharedRideId :: Kernel.Prelude.Text,
    chargeableDistanceValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.SharedRide.SharedRide,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    rideEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rideIds :: [Kernel.Types.Id.Id Domain.Types.Ride.Ride],
    rideStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    sharedBookingId :: Kernel.Types.Id.Id Domain.Types.SharedBooking.SharedBooking,
    status :: Domain.Types.SharedRide.SharedRideStatus,
    totalFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    trackingUrl :: Kernel.Prelude.Maybe Kernel.Types.Common.BaseUrl,
    traveledDistanceValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleModel :: Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text,
    vehicleServiceTierType :: Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType,
    vehicleVariant :: Domain.Types.VehicleVariant.VehicleVariant,
    waypoints :: Data.Aeson.Value
  }
  deriving (Generic, Show)

data SharedRideStatus = UPCOMING | NEW | INPROGRESS | COMPLETED | CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''SharedRideStatus))

$(mkHttpInstancesForEnum (''SharedRideStatus))
