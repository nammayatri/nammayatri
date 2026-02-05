{-
  Finance.Audit.Service

  Concrete audit trail operations for domain use.
  Implements LAW 2: Immutability of History (append-only).
  Uses generated Beam queries internally.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Audit.Service
  ( -- * Audit operations
    logAudit,
    getAuditLog,
    getAuditByAction,
    getAuditByActor,

    -- * Input types (re-export from Interface)
    module Lib.Finance.Audit.Interface,
  )
where

import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Audit.Interface
import Lib.Finance.Domain.Types.AuditEntry
import Lib.Finance.Error.Types
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.AuditEntry as QAudit

-- | Log an audit entry (append-only, never modify)
logAudit ::
  (BeamFlow.BeamFlow m r) =>
  AuditInput ->
  m (Either FinanceError AuditEntry)
logAudit input = do
  now <- getCurrentTime
  auditId <- generateGUID

  let entry =
        AuditEntry
          { id = Id auditId,
            entityType = input.entityType,
            entityId = input.entityId,
            action = input.action,
            actorType = input.actorType,
            actorId = input.actorId,
            previousState = input.beforeState,
            newState = input.afterState,
            metadata = Nothing,
            ipAddress = Nothing,
            merchantId = input.merchantId,
            merchantOperatingCityId = input.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

  QAudit.create entry
  pure $ Right entry

-- | Get audit log for an entity
getAuditLog ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- Entity type
  Text -> -- Entity ID
  m [AuditEntry]
getAuditLog = QAudit.findByEntity

-- | Get audit entries by action type
getAuditByAction ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- Entity type
  AuditAction ->
  m [AuditEntry]
getAuditByAction = QAudit.findByAction

-- | Get audit entries by actor
getAuditByActor ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- Actor type (e.g., "DRIVER", "SYSTEM", "ADMIN")
  Maybe Text -> -- Actor ID (optional)
  m [AuditEntry]
getAuditByActor = QAudit.findByActor
