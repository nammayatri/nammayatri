{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TowingAuditLog where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.TowingRequest
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TowingAuditLogT f = TowingAuditLogT
  { action :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    ipAddress :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    lat :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    lon :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    metadata :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    newStatus :: (B.C f (Kernel.Prelude.Maybe Domain.Types.TowingRequest.TowingStatus)),
    performedBy :: (B.C f Kernel.Prelude.Text),
    performedByName :: (B.C f Kernel.Prelude.Text),
    performedByRole :: (B.C f Kernel.Prelude.Text),
    previousStatus :: (B.C f (Kernel.Prelude.Maybe Domain.Types.TowingRequest.TowingStatus)),
    towingRequestId :: (B.C f Kernel.Prelude.Text),
    userAgent :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table TowingAuditLogT where
  data PrimaryKey TowingAuditLogT f = TowingAuditLogId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TowingAuditLogId . id

type TowingAuditLog = TowingAuditLogT Identity

$(enableKVPG (''TowingAuditLogT) [('id)] [[('towingRequestId)]])

$(mkTableInstances (''TowingAuditLogT) "towing_audit_log")
