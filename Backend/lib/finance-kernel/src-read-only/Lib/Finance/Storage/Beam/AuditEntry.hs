{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.AuditEntry where

import qualified Data.Aeson
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Finance.Domain.Types.AuditEntry

data AuditEntryT f = AuditEntryT
  { action :: (B.C f Lib.Finance.Domain.Types.AuditEntry.AuditAction),
    actorId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    actorType :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    entityId :: (B.C f Kernel.Prelude.Text),
    entityType :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    ipAddress :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    metadata :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    newState :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    previousState :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table AuditEntryT where
  data PrimaryKey AuditEntryT f = AuditEntryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AuditEntryId . id

type AuditEntry = AuditEntryT Identity

$(enableKVPG (''AuditEntryT) [('id)] [[('entityId)]])

$(mkTableInstancesGenericSchema (''AuditEntryT) "finance_audit_entry")
