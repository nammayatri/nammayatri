{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ScheduledPayoutConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.Common
import qualified Tools.Beam.UtilsTH

data ScheduledPayoutConfig = ScheduledPayoutConfig
  { batchSize :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    dayOfMonth :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    dayOfWeek :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    defaultPayoutRail :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    discontinuedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    frequency :: Domain.Types.ScheduledPayoutConfig.ScheduledPayoutFrequency,
    isEnabled :: Kernel.Prelude.Bool,
    itemsPerBatch :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxRetriesPerDriver :: Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    minimumPayoutAmount :: Kernel.Types.Common.HighPrecMoney,
    orderType :: Kernel.Prelude.Text,
    pausedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    payoutCategory :: Lib.Payment.Domain.Types.Common.EntityName,
    remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementGatedEligibility :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    timeDiffFromUtc :: Kernel.Types.Common.Seconds,
    timeOfDay :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving (Generic, Show, ToJSON, FromJSON, Eq)

data ScheduledPayoutFrequency = DAILY | WEEKLY | MONTHLY deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ScheduledPayoutFrequency)
