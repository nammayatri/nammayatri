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
    logAuditWithMetadata,
    getAuditLog,
    getAuditByAction,
    getAuditByActor,
    getAuditEntry,

    -- * Financial audit middleware
    withFinancialAudit,
    FinancialAuditContext (..),

    -- * Paginated/filtered queries
    listAuditEntries,
    countAuditEntries,
    listAdminAuditActions,
    getAuditEntriesByDateRange,

    -- * Input types (re-export from Interface)
    module Lib.Finance.Audit.Interface,
  )
where

import Data.Aeson (Value)
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Audit.Interface
import Lib.Finance.Domain.Types.AuditEntry
import Lib.Finance.Error.Types
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.AuditEntry as QAudit
import qualified Lib.Finance.Storage.Queries.AuditEntryExtra as QAuditExtra

-- | Context for the financial audit middleware
data FinancialAuditContext = FinancialAuditContext
  { entityType :: Text,
    entityId :: Text,
    action :: AuditAction,
    actorType :: Text,
    actorId :: Maybe Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    ipAddress :: Maybe Text,
    metadata :: Maybe Value
  }
  deriving (Eq, Show, Generic)

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
            hashChain = Nothing,
            ipAddress = Nothing,
            merchantId = input.merchantId,
            merchantOperatingCityId = input.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

  QAudit.create entry
  pure $ Right entry

-- | Log an audit entry with additional metadata, IP address, and hash chain
logAuditWithMetadata ::
  (BeamFlow.BeamFlow m r) =>
  AuditInput ->
  Maybe Value -> -- metadata
  Maybe Text -> -- ipAddress
  Maybe Text -> -- hashChain
  m (Either FinanceError AuditEntry)
logAuditWithMetadata input mbMetadata mbIpAddress mbHashChain = do
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
            metadata = mbMetadata,
            hashChain = mbHashChain,
            ipAddress = mbIpAddress,
            merchantId = input.merchantId,
            merchantOperatingCityId = input.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

  QAudit.create entry
  pure $ Right entry

-- | Composable audit wrapper for any financial operation.
--   Captures before/after state automatically.
--
--   Usage:
--   @
--     withFinancialAudit auditCtx (Just beforeStateJson) $ do
--       -- perform the mutation
--       updateInvoiceStatus invoiceId newStatus
--       -- return the new state as JSON
--       pure afterStateJson
--   @
withFinancialAudit ::
  (BeamFlow.BeamFlow m r) =>
  FinancialAuditContext ->
  Maybe Value -> -- before-state captured by caller
  m (a, Maybe Value) -> -- operation returning (result, after-state)
  m a
withFinancialAudit ctx beforeState operation = do
  (result, afterState) <- operation
  let auditInput =
        AuditInput
          { entityType = ctx.entityType,
            entityId = ctx.entityId,
            action = ctx.action,
            actorType = ctx.actorType,
            actorId = ctx.actorId,
            beforeState = beforeState,
            afterState = afterState,
            merchantId = ctx.merchantId,
            merchantOperatingCityId = ctx.merchantOperatingCityId
          }
  _ <- logAuditWithMetadata auditInput ctx.metadata ctx.ipAddress Nothing
  pure result

-- | Get a single audit entry by ID
getAuditEntry ::
  (BeamFlow.BeamFlow m r) =>
  Id AuditEntry ->
  m (Maybe AuditEntry)
getAuditEntry = QAudit.findById

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

-- | Paginated audit entries with optional filters
listAuditEntries ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- merchantId
  Maybe Text -> -- entityType filter
  Maybe AuditAction -> -- action filter
  Maybe Text -> -- actorType filter
  Maybe UTCTime -> -- dateFrom
  Maybe UTCTime -> -- dateTo
  Int -> -- limit
  Int -> -- offset
  m [AuditEntry]
listAuditEntries = QAuditExtra.findAllWithFilters

-- | Count audit entries with optional filters
countAuditEntries ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- merchantId
  Maybe Text -> -- entityType filter
  Maybe AuditAction -> -- action filter
  Maybe Text -> -- actorType filter
  Maybe UTCTime -> -- dateFrom
  Maybe UTCTime -> -- dateTo
  m Int
countAuditEntries = QAuditExtra.countWithFilters

-- | List admin-specific audit actions
listAdminAuditActions ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- merchantId
  Maybe UTCTime -> -- dateFrom
  Maybe UTCTime -> -- dateTo
  Int -> -- limit
  Int -> -- offset
  m [AuditEntry]
listAdminAuditActions = QAuditExtra.findAdminActions

-- | Get audit entries within a date range for a merchant
getAuditEntriesByDateRange ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- merchantId
  UTCTime -> -- from
  UTCTime -> -- to
  m [AuditEntry]
getAuditEntriesByDateRange = QAuditExtra.findByDateRange
