{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleServiceTier where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleVariant
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VehicleServiceTier = VehicleServiceTier
  { airConditionedThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    allowedAreas :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    allowedVehicleVariant :: [Domain.Types.VehicleVariant.VehicleVariant],
    autoSelectedVehicleVariant :: [Domain.Types.VehicleVariant.VehicleVariant],
    baseVehicleServiceTier :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    defaultForVehicleVariant :: [Domain.Types.VehicleVariant.VehicleVariant],
    driverRating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    fareAdditionPerKmOverBaseServiceTier :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.VehicleServiceTier.VehicleServiceTier,
    isAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isIntercityEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isRentalsEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    longDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    luggageCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    oxygen :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    priority :: Kernel.Prelude.Int,
    scheduleBookingListEligibilityTags :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    seatingCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    serviceTierType :: Domain.Types.Common.ServiceTierType,
    shortDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    stopFcmSuppressCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    stopFcmThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    vehicleIconUrl :: Kernel.Prelude.Maybe Kernel.Types.Common.BaseUrl,
    vehicleRating :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    ventilator :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
