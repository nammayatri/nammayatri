{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DocumentAuditLog where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DocumentAuditLog
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DocumentAuditLogT f = DocumentAuditLogT
  { action :: B.C f Domain.Types.DocumentAuditLog.AuditAction,
    actorId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    actorRole :: B.C f Kernel.Prelude.Text,
    actorSource :: B.C f Domain.Types.DocumentAuditLog.ActorSource,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    details :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    documentRefId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    documentRefType :: B.C f Domain.Types.DocumentAuditLog.DocumentRefType,
    documentType :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    entityId :: B.C f Kernel.Prelude.Text,
    entityType :: B.C f Domain.Types.DocumentAuditLog.AuditEntityType,
    eventId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fieldName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    newStatus :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    previousStatus :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    reason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DocumentAuditLogT where
  data PrimaryKey DocumentAuditLogT f = DocumentAuditLogId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DocumentAuditLogId . id

type DocumentAuditLog = DocumentAuditLogT Identity

$(enableKVPG ''DocumentAuditLogT ['id] [['entityId]])

$(mkTableInstances ''DocumentAuditLogT "document_audit_log")
