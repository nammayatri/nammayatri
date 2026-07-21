-- | Domain event emitted whenever a DVC document status or a tracked driver/fleet/RC flag changes.
-- Published as a JSON payload on Redis Stream "audit.events.shard<N>" where
-- shardId = hash(entityId) mod shardCount. A kafka-consumers DOCUMENT_AUDIT_CONSUMER
-- deserializes this, applies the per-city gate, and persists into document_audit_log.
--
-- Schema versioning: v1. Add new optional fields only; never remove or rename — in-flight
-- consumers may still be reading the old shape. IDs/enums are carried as Text to keep the
-- cross-package JSON contract simple.
module Domain.Types.Event.DocumentAuditEvent
  ( DocumentAuditEvent (..),
    schemaVersion,
  )
where

import Data.Aeson (Value)
import Kernel.Prelude

data DocumentAuditEvent = DocumentAuditEvent
  { schemaVer :: Int,
    auditId :: Text, -- the future row id; the consumer's idempotency key
    eventTimestamp :: UTCTime,
    entityType :: Text,
    entityId :: Text,
    documentType :: Maybe Text,
    fieldName :: Maybe Text,
    documentRefType :: Text,
    documentRefId :: Maybe Text,
    eventId :: Maybe Text, -- provider/transaction id linking a request-time row to its async webhook result
    action :: Text,
    previousStatus :: Maybe Text,
    newStatus :: Maybe Text,
    actorId :: Maybe Text,
    actorRole :: Text,
    actorSource :: Text,
    reason :: Maybe Text,
    details :: Maybe Value,
    merchantId :: Maybe Text,
    merchantOperatingCityId :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

schemaVersion :: Int
schemaVersion = 1
