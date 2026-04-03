{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.Notification where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.Prelude
import qualified Domain.Types.Notification
import qualified Database.Beam as B



data NotificationT f
    = NotificationT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                     id :: (B.C f Kernel.Prelude.Text),
                     merchantShortId :: (B.C f Kernel.Prelude.Text),
                     metadata :: (B.C f Kernel.Prelude.Text),
                     notificationCategory :: (B.C f Domain.Types.Notification.NotificationCategory),
                     notificationCount :: (B.C f Kernel.Prelude.Int),
                     readStatus :: (B.C f Kernel.Prelude.Bool),
                     receiverId :: (B.C f Kernel.Prelude.Text),
                     senderId :: (B.C f Kernel.Prelude.Text),
                     updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                     merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table NotificationT
    where data PrimaryKey NotificationT f = NotificationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = NotificationId . id
type Notification = NotificationT Identity

$(enableKVPG (''NotificationT) [('id)] [])

$(mkTableInstances (''NotificationT) "notification")

