{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.AuditEntry where

import qualified Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Finance.Audit
import qualified Kernel.Types.Id

-- | AuditEntry is an immutable (append-only) record.
--   There is no updatedAt field by design -- audit entries must never be modified.
--   See LAW 2: Immutability of History.
data AuditEntry = AuditEntry
  { action :: Kernel.Types.Finance.Audit.AuditAction,
    actorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    actorType :: Kernel.Types.Finance.Audit.AuditActorType,
    createdAt :: Kernel.Prelude.UTCTime,
    entityId :: Kernel.Prelude.Text,
    entityType :: Kernel.Types.Finance.Audit.AuditEntityType,
    hashChain :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.AuditEntry.AuditEntry,
    ipAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    metadata :: Kernel.Prelude.Maybe Data.Aeson.Value,
    newState :: Kernel.Prelude.Maybe Data.Aeson.Value,
    previousState :: Kernel.Prelude.Maybe Data.Aeson.Value
  }
  deriving (Generic)
