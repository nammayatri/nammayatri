{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DomainDiscountConfig where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified SharedLogic.Type
import qualified Tools.Beam.UtilsTH

data DomainDiscountConfig = DomainDiscountConfig
  { billingCategory :: SharedLogic.Type.BillingCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    discountPercentage :: Kernel.Prelude.Double,
    domain :: Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleServiceTier :: Domain.Types.Common.ServiceTierType,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
  }
  deriving (Generic, Show, ToJSON, FromJSON)
