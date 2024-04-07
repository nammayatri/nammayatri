{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleServiceTier where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.Vehicle
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VehicleServiceTier = VehicleServiceTier
  { airConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    allowedVehicleVariant :: [Domain.Types.Vehicle.Variant],
    defaultForVehicleVariant :: [Domain.Types.Vehicle.Variant],
    driverRating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    id :: Kernel.Types.Id.Id Domain.Types.VehicleServiceTier.VehicleServiceTier,
    longDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    luggageCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    seatingCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    serviceTierType :: Domain.Types.ServiceTierType.ServiceTierType,
    shortDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleRating :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
