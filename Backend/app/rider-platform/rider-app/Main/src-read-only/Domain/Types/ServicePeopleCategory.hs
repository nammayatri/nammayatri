{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ServicePeopleCategory where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import qualified Tools.Beam.UtilsTH
import qualified Tools.Payment

data ServicePeopleCategory = ServicePeopleCategory
  { cancellationCharges :: Kernel.Prelude.Maybe [Domain.Types.ServicePeopleCategory.CancellationCharge],
    description :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory,
    name :: Kernel.Prelude.Text,
    pricePerUnit :: Kernel.Types.Common.Price,
    pricingType :: Domain.Types.ServicePeopleCategory.PricingType,
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    vendorSplitDetails :: Kernel.Prelude.Maybe [Tools.Payment.VendorSplitDetails],
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data CancelCharge = FlatFee Kernel.Types.Common.HighPrecMoney | Percentage Kernel.Prelude.Int deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data CancellationCharge = CancellationCharge {cancelCharge :: Domain.Types.ServicePeopleCategory.CancelCharge, time :: Kernel.Types.Common.Seconds}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord, Read)

data PricingType = SameDay | AllDays deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CancelCharge)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CancellationCharge)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PricingType)
