{-
  Finance.Audit.Service

  Concrete audit trail operations for domain use.
  Implements LAW 2: Immutability of History (append-only).
  Uses generated Beam queries internally.
  Implements cryptographic hash chain for tamper-evidence.
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

    -- * Hash chain
    computeAuditHash,

    -- * Input types (re-export from Interface)
    module Lib.Finance.Audit.Interface,
  )
where

import Crypto.Hash (SHA256, hashWith)
import Data.Aeson (Value, encode)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as TE
import Kernel.Prelude
import Kernel.Types.Finance.Audit (AuditAction, AuditActorType, AuditEntityType)
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
  { entityType :: AuditEntityType,
    entityId :: Text,
    action :: AuditAction,
    actorType :: AuditActorType,
    actorId :: Maybe Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    ipAddress :: Maybe Text,
    metadata :: Maybe Value
  }
  deriving (Eq, Show, Generic)

-- | Compute SHA-256 hash for an audit entry, chaining with the previous entry's hash.
--   Hash = SHA256(previousHash + entityType + entityId + action + newState + timestamp)
computeAuditHash ::
  Maybe Text -> -- previous hash (Nothing for first entry in chain)
  AuditEntityType -> -- entityType
  Text -> -- entityId
  AuditAction -> -- action
  Maybe Value -> -- newState
  UTCTime -> -- timestamp
  Text
computeAuditHash mbPrevHash entityType entityId action newState timestamp =
  let prevHashBytes = maybe BS.empty TE.encodeUtf8 mbPrevHash
      entityTypeBytes = TE.encodeUtf8 (show entityType)
      entityIdBytes = TE.encodeUtf8 entityId
      actionBytes = TE.encodeUtf8 (show action)
      newStateBytes = maybe BS.empty (toStrict . encode) newState
      timestampBytes = TE.encodeUtf8 (show timestamp)
      combined = prevHashBytes <> entityTypeBytes <> entityIdBytes <> actionBytes <> newStateBytes <> timestampBytes
      digest = hashWith SHA256 combined
   in TE.decodeUtf8 $ Base16.encode (BA.convert digest :: BS.ByteString)

-- | Log an audit entry (append-only, never modify).
--   Automatically computes hash chain by fetching the previous entry's hash.
logAudit ::
  (BeamFlow.BeamFlow m r) =>
  AuditInput ->
  m (Either FinanceError AuditEntry)
logAudit input = do
  now <- getCurrentTime
  auditId <- generateGUID

  -- Fetch previous hash for chain computation
  mbPrevEntry <- QAuditExtra.findLatestForEntity input.entityType input.entityId
  let mbPrevHash = mbPrevEntry >>= (.hashChain)
      entryHash = computeAuditHash mbPrevHash input.entityType input.entityId input.action input.afterState now

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
            hashChain = Just entryHash,
            ipAddress = Nothing,
            merchantId = input.merchantId,
            merchantOperatingCityId = input.merchantOperatingCityId,
            createdAt = now
          }

  QAudit.create entry
  pure $ Right entry

-- | Log an audit entry with additional metadata and IP address.
--   Automatically computes hash chain by fetching the previous entry's hash.
logAuditWithMetadata ::
  (BeamFlow.BeamFlow m r) =>
  AuditInput ->
  Maybe Value -> -- metadata
  Maybe Text -> -- ipAddress
  m (Either FinanceError AuditEntry)
logAuditWithMetadata input mbMetadata mbIpAddress = do
  now <- getCurrentTime
  auditId <- generateGUID

  -- Fetch previous hash for chain computation
  mbPrevEntry <- QAuditExtra.findLatestForEntity input.entityType input.entityId
  let mbPrevHash = mbPrevEntry >>= (.hashChain)
      entryHash = computeAuditHash mbPrevHash input.entityType input.entityId input.action input.afterState now

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
            hashChain = Just entryHash,
            ipAddress = mbIpAddress,
            merchantId = input.merchantId,
            merchantOperatingCityId = input.merchantOperatingCityId,
            createdAt = now
          }

  QAudit.create entry
  pure $ Right entry

-- | Composable audit wrapper for any financial operation.
--   Captures before/after state automatically.
--   Computes hash chain for tamper-evidence.
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
  _ <- logAuditWithMetadata auditInput ctx.metadata ctx.ipAddress
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
  AuditEntityType -> -- Entity type
  Text -> -- Entity ID
  m [AuditEntry]
getAuditLog = QAudit.findByEntity

-- | Get audit entries by action type
getAuditByAction ::
  (BeamFlow.BeamFlow m r) =>
  AuditEntityType -> -- Entity type
  AuditAction ->
  m [AuditEntry]
getAuditByAction = QAudit.findByAction

-- | Get audit entries by actor
getAuditByActor ::
  (BeamFlow.BeamFlow m r) =>
  AuditActorType -> -- Actor type
  Maybe Text -> -- Actor ID (optional)
  m [AuditEntry]
getAuditByActor = QAudit.findByActor

-- | Paginated audit entries with optional filters
listAuditEntries ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- merchantId
  Maybe AuditEntityType -> -- entityType filter
  Maybe AuditAction -> -- action filter
  Maybe AuditActorType -> -- actorType filter
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
  Maybe AuditEntityType -> -- entityType filter
  Maybe AuditAction -> -- action filter
  Maybe AuditActorType -> -- actorType filter
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

-- | Get audit entries within a date range for a merchant (with pagination)
getAuditEntriesByDateRange ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- merchantId
  UTCTime -> -- from
  UTCTime -> -- to
  Int -> -- limit
  Int -> -- offset
  m [AuditEntry]
getAuditEntriesByDateRange = QAuditExtra.findByDateRange
