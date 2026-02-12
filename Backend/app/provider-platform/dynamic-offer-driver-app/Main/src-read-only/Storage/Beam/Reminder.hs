{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Reminder where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Reminder
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ReminderT f = ReminderT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    currentIntervalIndex :: B.C f Kernel.Prelude.Int,
    documentType :: B.C f Domain.Types.DocumentVerificationConfig.DocumentType,
    driverId :: B.C f Kernel.Prelude.Text,
    dueDate :: B.C f Kernel.Prelude.UTCTime,
    entityId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    metadata :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    reminderDate :: B.C f Kernel.Prelude.UTCTime,
    status :: B.C f Domain.Types.Reminder.ReminderStatus,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ReminderT where
  data PrimaryKey ReminderT f = ReminderId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ReminderId . id

type Reminder = ReminderT Identity

$(enableKVPG ''ReminderT ['id] [['documentType], ['driverId]])

$(mkTableInstances ''ReminderT "reminder")
