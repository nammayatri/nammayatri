{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ReminderConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DocumentVerificationConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ReminderConfigT f = ReminderConfigT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    daysThreshold :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    documentType :: (B.C f Domain.Types.DocumentVerificationConfig.DocumentType),
    enabled :: (B.C f Kernel.Prelude.Bool),
    id :: (B.C f Kernel.Prelude.Text),
    isMandatory :: (B.C f Kernel.Prelude.Bool),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    reminderIntervals :: (B.C f [Kernel.Prelude.Int]),
    ridesThreshold :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table ReminderConfigT where
  data PrimaryKey ReminderConfigT f = ReminderConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ReminderConfigId . id

type ReminderConfig = ReminderConfigT Identity

$(enableKVPG (''ReminderConfigT) [('id)] [])

$(mkTableInstances (''ReminderConfigT) "reminder_config")
