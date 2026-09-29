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
import qualified Lib.Payment.Domain.Types.PayoutBatch
import qualified Tools.Beam.UtilsTH

data ScheduledPayoutConfig = ScheduledPayoutConfig
  { batchSize :: Kernel.Prelude.Int,
    bufferCheckEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    dayOfMonth :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    dayOfWeek :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    defaultPayoutRail :: Kernel.Prelude.Maybe Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchRail,
    frequency :: Domain.Types.ScheduledPayoutConfig.ScheduledPayoutFrequency,
    intervalDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    intervalHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    isEnabled :: Kernel.Prelude.Bool,
    itemsPerBatchLimit :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxRetriesPerDriver :: Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    minimumPayoutAmount :: Kernel.Types.Common.HighPrecMoney,
    orderType :: Kernel.Prelude.Text,
    payoutCategory :: Lib.Payment.Domain.Types.Common.EntityName,
    remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rescheduleBufferMinutes :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    timeDiffFromUtc :: Kernel.Types.Common.Seconds,
    timeOfDay :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving (Generic, Show, ToJSON, FromJSON, Eq)

data ScheduledPayoutFrequency = DAILY | WEEKLY | MONTHLY | HOURLY | EVERY_N_DAYS deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ScheduledPayoutFrequency)
