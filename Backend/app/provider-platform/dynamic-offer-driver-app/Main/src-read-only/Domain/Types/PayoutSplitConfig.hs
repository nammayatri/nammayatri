{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PayoutSplitConfig where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleVariant
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Types.SpecialLocation
import qualified Tools.Beam.UtilsTH

data PayoutSplitConfig = PayoutSplitConfig
  { area :: Lib.Types.SpecialLocation.Area,
    bankDetails :: Data.Aeson.Value,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    vehicleVariant :: Domain.Types.VehicleVariant.VehicleVariant,
    vendorId :: Data.Text.Text,
    vendorSplitAmount :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
