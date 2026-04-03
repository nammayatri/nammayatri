{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.ScheduledPayoutConfig where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.Common
import qualified Domain.Types.VehicleCategory
import qualified Tools.Beam.UtilsTH



data ScheduledPayoutConfig
    = ScheduledPayoutConfig {batchSize :: Kernel.Prelude.Int,
                             createdAt :: Kernel.Prelude.UTCTime,
                             dayOfMonth :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                             dayOfWeek :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                             frequency :: Domain.Types.ScheduledPayoutConfig.ScheduledPayoutFrequency,
                             isEnabled :: Kernel.Prelude.Bool,
                             maxRetriesPerDriver :: Kernel.Prelude.Int,
                             merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                             merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                             minimumPayoutAmount :: Kernel.Types.Common.HighPrecMoney,
                             orderType :: Kernel.Prelude.Text,
                             payoutCategory :: Lib.Payment.Domain.Types.Common.EntityName,
                             remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             timeDiffFromUtc :: Kernel.Types.Common.Seconds,
                             timeOfDay :: Kernel.Prelude.Text,
                             updatedAt :: Kernel.Prelude.UTCTime,
                             vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory}
    deriving (Generic, Show, ToJSON, FromJSON, Eq)
data ScheduledPayoutFrequency = DAILY | WEEKLY | MONTHLY deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''ScheduledPayoutFrequency))

