{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Yudhishthira.Storage.Beam.TagActionNotificationConfig where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import qualified Lib.Yudhishthira.Types.TagActionNotificationConfig
import qualified Lib.Yudhishthira.Types
import qualified Kernel.Prelude
import qualified Database.Beam as B



data TagActionNotificationConfigT f
    = TagActionNotificationConfigT {id :: (B.C f Kernel.Prelude.Text),
                                    merchantId :: (B.C f Kernel.Prelude.Text),
                                    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                                    notificationKey :: (B.C f Kernel.Prelude.Text),
                                    notificationType :: (B.C f Lib.Yudhishthira.Types.TagActionNotificationConfig.NotificationType),
                                    notifyAt :: (B.C f Kernel.Prelude.TimeOfDay),
                                    createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                    updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table TagActionNotificationConfigT
    where data PrimaryKey TagActionNotificationConfigT f = TagActionNotificationConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = TagActionNotificationConfigId . id
type TagActionNotificationConfig = TagActionNotificationConfigT Identity

$(enableKVPG (''TagActionNotificationConfigT) [('id)] [])

$(mkTableInstancesGenericSchema (''TagActionNotificationConfigT) "tag_action_notification_config")

