{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.NotificationSoundsConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Notification.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data NotificationSoundsConfigT f = NotificationSoundsConfigT
  { blindSound :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    defaultSound :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    notificationType :: B.C f Kernel.External.Notification.Interface.Types.Category,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table NotificationSoundsConfigT where
  data PrimaryKey NotificationSoundsConfigT f = NotificationSoundsConfigId (B.C f Kernel.Prelude.Text) (B.C f Kernel.External.Notification.Interface.Types.Category) deriving (Generic, B.Beamable)
  primaryKey = NotificationSoundsConfigId <$> merchantOperatingCityId <*> notificationType

type NotificationSoundsConfig = NotificationSoundsConfigT Identity

$(enableKVPG ''NotificationSoundsConfigT ['merchantOperatingCityId, 'notificationType] [])

$(mkTableInstances ''NotificationSoundsConfigT "notification_sounds_config")
