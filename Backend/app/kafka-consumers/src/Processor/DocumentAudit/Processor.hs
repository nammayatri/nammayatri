{-# LANGUAGE PackageImports #-}

-- | DOCUMENT_AUDIT_CONSUMER — persists document/flag audit events off the request path.
--
-- The driver-app writer ('SharedLogic.DriverOnboarding.Audit') publishes a thin
-- 'DocumentAuditEvent' to Redis Stream "audit.events.shard<N>" (one xAdd, no DB write).
-- This consumer applies the per-city gate (TransporterConfig.enableDocumentAuditLog) and
-- persists into document_audit_log. Idempotent on auditId so re-delivery (PEL claim /
-- restart) is a no-op.
module Processor.DocumentAudit.Processor
  ( processDocumentAudit,
  )
where

import qualified Data.Text as T
import "dynamic-offer-driver-app" Domain.Types.DocumentAuditLog (DocumentAuditLog (..))
import "dynamic-offer-driver-app" Domain.Types.Event.DocumentAuditEvent (DocumentAuditEvent)
import Environment (Flow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (logError, logInfo, withLogTag)
import qualified "dynamic-offer-driver-app" Storage.Cac.TransporterConfig as SCTC
import qualified "dynamic-offer-driver-app" Storage.Queries.DocumentAuditLog as Q

-- 24h far exceeds any reasonable Redis-Stream max-delivery-delay (PEL idle, claim, retry).
idempotencyTTLSeconds :: Int
idempotencyTTLSeconds = 86400

processDocumentAudit :: DocumentAuditEvent -> Flow ()
processDocumentAudit event =
  withLogTag ("auditId-" <> event.auditId) $ do
    -- City gate FIRST: the writer publishes for every city (config is off the hot path), so most
    -- events are dropped here. Evaluating the gate before claiming the idempotency key avoids writing
    -- a useless 24h Redis key per disabled-city event (the gate is an in-mem cached read in steady state).
    enabled <- cityAuditEnabled event.merchantOperatingCityId
    if not enabled
      then logInfo "document-audit.city-disabled-drop"
      else do
        let processedKey = "docaudit:processed:" <> event.auditId
        -- atomic SETNX-with-TTL: only the first delivery within the TTL proceeds.
        fresh <- Hedis.setNxExpire processedKey idempotencyTTLSeconds ("1" :: Text)
        if not fresh
          then logInfo "document-audit.idempotency-skip"
          else -- If persist throws (transient DB/cache/Redis), release the claim so the stream runner's PEL
          -- redelivery can retry — otherwise the 24h claim would turn a retriable failure into a permanent
          -- silent drop. A parse-fail completes normally and keeps the claim (terminal, not reprocessed).

            ( case rowFromEvent event of
                Left err -> logError $ "document-audit.parse-failed " <> err
                Right row -> Q.create row
            )
              `onException` Hedis.del processedKey

-- The city master switch. Off (or unknown city) ⇒ drop the event.
cityAuditEnabled :: Maybe Text -> Flow Bool
cityAuditEnabled Nothing = pure False
cityAuditEnabled (Just cityId) = do
  mbCfg <- SCTC.findByMerchantOpCityId (Id cityId) Nothing
  pure $ maybe False (fromMaybe False . (.enableDocumentAuditLog)) mbCfg

-- Wire event (Text/enums-as-text) → the persisted row. The four enum fields (action, entityType,
-- documentRefType, actorSource) show↔read round-trip because writer and consumer share the exact
-- enum definitions from the driver-app lib. actorRole is free-form Text (no enum) — any dashboard
-- role is persisted verbatim; the writer maps a blank role to "ADMIN" so it is never empty.
rowFromEvent :: DocumentAuditEvent -> Either Text DocumentAuditLog
rowFromEvent ev = do
  action <- readEnum "action" ev.action
  entityType <- readEnum "entityType" ev.entityType
  documentRefType <- readEnum "documentRefType" ev.documentRefType
  -- actorRole is free-form Text (any dashboard role flows through verbatim); no enum parse.
  let actorRole = ev.actorRole
  actorSource <- readEnum "actorSource" ev.actorSource
  pure
    DocumentAuditLog
      { id = Id ev.auditId,
        action,
        actorId = ev.actorId,
        actorRole,
        actorSource,
        createdAt = ev.eventTimestamp,
        details = ev.details,
        documentRefId = ev.documentRefId,
        documentRefType,
        documentType = ev.documentType,
        entityId = ev.entityId,
        entityType,
        eventId = ev.eventId,
        fieldName = ev.fieldName,
        merchantId = Id <$> ev.merchantId,
        merchantOperatingCityId = Id <$> ev.merchantOperatingCityId,
        newStatus = ev.newStatus,
        previousStatus = ev.previousStatus,
        reason = ev.reason,
        updatedAt = ev.eventTimestamp
      }

readEnum :: Read a => Text -> Text -> Either Text a
readEnum fieldNm t = maybe (Left $ fieldNm <> "=" <> t <> " unparseable") Right (readMaybe (T.unpack t))
