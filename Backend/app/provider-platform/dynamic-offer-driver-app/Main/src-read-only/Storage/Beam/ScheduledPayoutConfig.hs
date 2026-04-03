{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.ScheduledPayoutConfig where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.ScheduledPayoutConfig
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.Common
import qualified Domain.Types.VehicleCategory
import qualified Database.Beam as B



data ScheduledPayoutConfigT f
    = ScheduledPayoutConfigT {batchSize :: (B.C f Kernel.Prelude.Int),
                              createdAt :: (B.C f Kernel.Prelude.UTCTime),
                              dayOfMonth :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                              dayOfWeek :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                              frequency :: (B.C f Domain.Types.ScheduledPayoutConfig.ScheduledPayoutFrequency),
                              isEnabled :: (B.C f Kernel.Prelude.Bool),
                              maxRetriesPerDriver :: (B.C f Kernel.Prelude.Int),
                              merchantId :: (B.C f Kernel.Prelude.Text),
                              merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                              minimumPayoutAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                              orderType :: (B.C f Kernel.Prelude.Text),
                              payoutCategory :: (B.C f Lib.Payment.Domain.Types.Common.EntityName),
                              remark :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              timeDiffFromUtc :: (B.C f Kernel.Types.Common.Seconds),
                              timeOfDay :: (B.C f Kernel.Prelude.Text),
                              updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                              vehicleCategory :: (B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory))}
    deriving (Generic, B.Beamable)
instance B.Table ScheduledPayoutConfigT
    where data PrimaryKey ScheduledPayoutConfigT f = ScheduledPayoutConfigId (B.C f Kernel.Prelude.Text) (B.C f Lib.Payment.Domain.Types.Common.EntityName) deriving (Generic, B.Beamable)
          primaryKey = ScheduledPayoutConfigId <$> merchantOperatingCityId <*> payoutCategory
type ScheduledPayoutConfig = ScheduledPayoutConfigT Identity

$(enableKVPG (''ScheduledPayoutConfigT) [('merchantOperatingCityId), ('payoutCategory)] [])

$(mkTableInstances (''ScheduledPayoutConfigT) "scheduled_payout_config")

