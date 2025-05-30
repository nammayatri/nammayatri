{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RCValidationRules where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RCValidationRules = RCValidationRules
  { fuelType :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    id :: Kernel.Types.Id.Id Domain.Types.RCValidationRules.RCValidationRules,
    maxVehicleAge :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    vehicleClass :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    vehicleOEM :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
