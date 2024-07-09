{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleServiceTier where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.Vehicle
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VehicleServiceTier = VehicleServiceTier
  { airConditionedThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    allowedVehicleVariant :: [Domain.Types.Vehicle.Variant],
    autoSelectedVehicleVariant :: [Domain.Types.Vehicle.Variant],
    defaultForVehicleVariant :: [Domain.Types.Vehicle.Variant],
    driverRating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    id :: Kernel.Types.Id.Id Domain.Types.VehicleServiceTier.VehicleServiceTier,
    isAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    longDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    luggageCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    oxygen :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    priority :: Kernel.Prelude.Int,
    seatingCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    selectedByDefaul :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    serviceTierType :: Domain.Types.ServiceTierType.ServiceTierType,
    shortDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleImageUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleRating :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    ventilator :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
