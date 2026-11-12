{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.NavigationPointConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data NavigationPointConfig = NavigationPointConfig
  { bucketGeohashPrecision :: Kernel.Prelude.Int,
    enabled :: Kernel.Prelude.Bool,
    maxPointsToShow :: Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    pointGeohashPrecision :: Kernel.Prelude.Int,
    radiusInMeters :: Kernel.Prelude.Double,
    requiredFieldMask :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ttlInSeconds :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
