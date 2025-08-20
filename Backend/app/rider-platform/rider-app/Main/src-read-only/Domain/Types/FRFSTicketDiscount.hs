{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSTicketDiscount where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSTicketDiscount = FRFSTicketDiscount
  { code :: Kernel.Prelude.Text,
    currency :: Kernel.Types.Common.Currency,
    description :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSTicketDiscount.FRFSTicketDiscount,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    title :: Kernel.Prelude.Text,
    tnc :: Kernel.Prelude.Text,
    value :: Domain.Types.FRFSTicketDiscount.DiscountValue,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data DiscountValue = FixedAmount Kernel.Types.Common.HighPrecMoney | Percentage Kernel.Prelude.Double deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''DiscountValue)
