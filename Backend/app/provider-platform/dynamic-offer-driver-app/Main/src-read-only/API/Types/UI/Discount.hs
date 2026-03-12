{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Discount where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Discount
import qualified Domain.Types.DiscountTier
import qualified Domain.Types.Plan
import qualified Domain.Types.VehicleCategory
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data DiscountInfo = DiscountInfo
  { description :: Kernel.Prelude.Text,
    discountId :: Kernel.Types.Id.Id Domain.Types.Discount.Discount,
    discountType :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    paymentMode :: Kernel.Prelude.Maybe Domain.Types.Plan.PaymentMode,
    planId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Plan.Plan),
    tiers :: [DiscountTierInfo],
    validFrom :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    validTo :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DiscountTierInfo = DiscountTierInfo
  { description :: Kernel.Prelude.Text,
    discountValue :: Kernel.Prelude.Double,
    discountValueType :: Domain.Types.DiscountTier.DiscountValueType,
    name :: Kernel.Prelude.Text,
    thresholdValue :: Kernel.Prelude.Double,
    tierOrder :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GetDiscountsRes = GetDiscountsRes {discounts :: [DiscountInfo]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
