{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.MessageReport where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.MessageReport
import qualified Data.Time
import qualified Data.Aeson
import qualified Database.Beam as B



data MessageReportT f
    = MessageReportT {createdAt :: (B.C f Data.Time.LocalTime),
                      deliveryStatus :: (B.C f Domain.Types.MessageReport.DeliveryStatus),
                      driverId :: (B.C f Kernel.Prelude.Text),
                      likeStatus :: (B.C f Kernel.Prelude.Bool),
                      messageDynamicFields :: (B.C f Data.Aeson.Value),
                      messageId :: (B.C f Kernel.Prelude.Text),
                      readStatus :: (B.C f Kernel.Prelude.Bool),
                      reply :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      updatedAt :: (B.C f Data.Time.LocalTime)}
    deriving (Generic, B.Beamable)
instance B.Table MessageReportT
    where data PrimaryKey MessageReportT f = MessageReportId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = MessageReportId <$> driverId <*> messageId
type MessageReport = MessageReportT Identity

$(enableKVPG (''MessageReportT) [('driverId), ('messageId)] [])

$(mkTableInstances (''MessageReportT) "message_report")

