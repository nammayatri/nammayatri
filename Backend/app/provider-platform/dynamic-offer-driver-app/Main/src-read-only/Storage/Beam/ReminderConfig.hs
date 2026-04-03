{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.ReminderConfig where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.DocumentVerificationConfig
import qualified Database.Beam as B



data ReminderConfigT f
    = ReminderConfigT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                       daysThreshold :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                       documentType :: (B.C f Domain.Types.DocumentVerificationConfig.DocumentType),
                       enabled :: (B.C f Kernel.Prelude.Bool),
                       isMandatory :: (B.C f Kernel.Prelude.Bool),
                       merchantId :: (B.C f Kernel.Prelude.Text),
                       merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                       reminderIntervals :: (B.C f [Kernel.Prelude.Int]),
                       reminderOnRideRescheduleIntervalSeconds :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                       reminderRescheduleIntervalSeconds :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                       ridesThreshold :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                       updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table ReminderConfigT
    where data PrimaryKey ReminderConfigT f = ReminderConfigId (B.C f Domain.Types.DocumentVerificationConfig.DocumentType) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = ReminderConfigId <$> documentType <*> merchantOperatingCityId
type ReminderConfig = ReminderConfigT Identity

$(enableKVPG (''ReminderConfigT) [('documentType), ('merchantOperatingCityId)] [])

$(mkTableInstances (''ReminderConfigT) "reminder_config")

