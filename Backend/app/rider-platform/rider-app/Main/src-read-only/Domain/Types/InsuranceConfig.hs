{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.InsuranceConfig where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.VehicleCategory
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data InsuranceConfig = InsuranceConfig
  { allowedVehicleServiceTiers :: Kernel.Prelude.Maybe [Domain.Types.ServiceTierType.ServiceTierType],
    city :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    driverInsuredAmount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    hours :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.InsuranceConfig.InsuranceConfig,
    insuredAmount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    partnerId :: Kernel.Prelude.Text,
    plan :: Kernel.Prelude.Text,
    planType :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    state :: Kernel.Prelude.Text,
    tripCategory :: Domain.Types.Common.TripCategory,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
