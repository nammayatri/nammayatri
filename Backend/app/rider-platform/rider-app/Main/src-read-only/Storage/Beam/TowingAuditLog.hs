{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TowingAuditLog where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.TowingRequest
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TowingAuditLogT f = TowingAuditLogT
  { action :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    lat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    lon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    newStatus :: B.C f (Kernel.Prelude.Maybe Domain.Types.TowingRequest.TowingStatus),
    note :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    performedBy :: B.C f Kernel.Prelude.Text,
    performedByName :: B.C f Kernel.Prelude.Text,
    towingRequestId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TowingAuditLogT where
  data PrimaryKey TowingAuditLogT f = TowingAuditLogId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TowingAuditLogId . id

type TowingAuditLog = TowingAuditLogT Identity

$(enableKVPG ''TowingAuditLogT ['id] [['towingRequestId]])

$(mkTableInstances ''TowingAuditLogT "towing_audit_log")
