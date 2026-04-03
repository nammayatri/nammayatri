{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.DocumentReminderHistory where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.DocumentReminderHistory
import qualified Database.Beam as B



data DocumentReminderHistoryT f
    = DocumentReminderHistoryT {completionDate :: (B.C f Kernel.Prelude.UTCTime),
                                documentType :: (B.C f Domain.Types.DocumentVerificationConfig.DocumentType),
                                entityId :: (B.C f Kernel.Prelude.Text),
                                entityType :: (B.C f Domain.Types.DocumentReminderHistory.EntityType),
                                id :: (B.C f Kernel.Prelude.Text),
                                merchantId :: (B.C f Kernel.Prelude.Text),
                                merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                                rideCountAtCompletion :: (B.C f Kernel.Prelude.Int),
                                updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                                createdAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table DocumentReminderHistoryT
    where data PrimaryKey DocumentReminderHistoryT f = DocumentReminderHistoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = DocumentReminderHistoryId . id
type DocumentReminderHistory = DocumentReminderHistoryT Identity

$(enableKVPG (''DocumentReminderHistoryT) [('id)] [[('documentType)], [('entityId)], [('entityType)]])

$(mkTableInstances (''DocumentReminderHistoryT) "document_reminder_history")

