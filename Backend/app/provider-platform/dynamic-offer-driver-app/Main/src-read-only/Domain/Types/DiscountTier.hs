{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DiscountTier where

import Data.Aeson
import qualified Domain.Types.Discount
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data DiscountTier = DiscountTier
  { discountId :: Kernel.Types.Id.Id Domain.Types.Discount.Discount,
    discountValue :: Kernel.Prelude.Double,
    discountValueType :: Domain.Types.DiscountTier.DiscountValueType,
    id :: Kernel.Types.Id.Id Domain.Types.DiscountTier.DiscountTier,
    thresholdValue :: Kernel.Prelude.Double,
    tierOrder :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, (Show), (Eq))

data DiscountValueType = PERCENTAGE | FIXED deriving (Generic, (Show), (ToJSON), (FromJSON), (ToSchema), (Eq), (Ord), (Read), (ToParamSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''DiscountValueType))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''DiscountValueType))
