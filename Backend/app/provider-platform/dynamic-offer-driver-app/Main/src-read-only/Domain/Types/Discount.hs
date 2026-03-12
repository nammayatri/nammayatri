{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Discount where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Plan
import qualified Domain.Types.VehicleCategory
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Discount = Discount
  { config :: Kernel.Prelude.Maybe Data.Aeson.Value,
    discountType :: Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.Discount.Discount,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    paymentMode :: Kernel.Prelude.Maybe Domain.Types.Plan.PaymentMode,
    planId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Plan.Plan),
    validFrom :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    validTo :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
