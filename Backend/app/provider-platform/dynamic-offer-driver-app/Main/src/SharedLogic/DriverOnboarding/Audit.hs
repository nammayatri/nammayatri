-- | Document audit log — best-effort, off-the-request-path writer.
--
-- The typed wrappers (auditImageStatus, auditDocStatus, auditFlagChange, …) build an
-- 'AuditLogEntry' and hand it to the single sink 'recordAudit', which PUBLISHES a thin
-- event (one 'Hedis.xAdd', no DB write, no fork). A kafka-consumers DOCUMENT_AUDIT_CONSUMER
-- gates (per-city flag) and persists into document_audit_log off the request path.
--
-- Gate: city flag ON  AND  (source ≠ DASHBOARD  OR  actorId present). The cheap actor
-- half is the in-memory short-circuit here; the city-flag read happens in the consumer.
module SharedLogic.DriverOnboarding.Audit
  ( Requestor (..),
    driverAppPerson,
    dashboardActor,
    systemScheduler,
    systemActor,
    externalProvider,
    actorRoleFromText,
    toActorRole,
    entityTypeFromRole,
    imageEntity,
    auditActorFromPersonOrRequestor,
    auditActorFromIdOrRequestor,
    dashboardActorFromForwarded,
    dashboardActorOrFallback,
    dashboardActorOrSystem,
    entityTypeFromMbPerson,
    AuditFlow,
    AuditLogEntry (..),
    auditImageUpload,
    auditImageStatus,
    auditImageStatusById,
    auditImageStatusByIdWithEvent,
    auditDocStatus,
    auditDocStatusWithEvent,
    auditBotApproval,
    auditDelete,
    auditFlagChange,
    auditEnabled,
    fetchForAudit,
    fetchForAuditByCity,
    auditConfigByCity,
  )
where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable (hash)
import qualified Domain.Types.DocumentAuditLog as DAL
import qualified Domain.Types.Event.DocumentAuditEvent as Ev
import qualified Domain.Types.Image as Image
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import Environment (RideEventsPublisherCfg)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Storage.Hedis.Config (HedisFlow)
import Kernel.Types.Common (MonadTime (getCurrentTime))
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.GuidLike (generateGUID)
import Kernel.Types.Id
import Kernel.Types.MonadGuid (MonadGuid)
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, logError)
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Cac.TransporterConfig as SCTC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))

-- ─────────────────────────── Actor ───────────────────────────

data Requestor = Requestor
  { actorId :: Maybe Text,
    -- Free-form role text (NOT a closed enum): dashboard roles are added over time, so any new
    -- role (e.g. DOC_VERIFIER) is recorded verbatim without a code change to writer or consumer.
    actorRole :: Text,
    actorSource :: DAL.ActorSource
  }

driverAppPerson :: Id DP.Person -> Text -> Requestor
driverAppPerson personId role = Requestor (Just personId.getId) role DAL.DRIVER_APP

dashboardActor :: Maybe (Id DP.Person) -> Text -> Requestor
dashboardActor mbId role = Requestor (getId <$> mbId) role DAL.DASHBOARD

-- SCHEDULER: ONLY for code that runs inside a scheduler Job (reached from SharedLogic/Allocator/Jobs/**).
systemScheduler :: Requestor
systemScheduler = Requestor Nothing "SYSTEM" DAL.SCHEDULER

-- SYSTEM: the generic no-actor/system default for everything that is NOT a scheduler Job
-- (dashboard fallbacks, event-driven status recompute, self-serve/auto-delete defaults, …).
systemActor :: Requestor
systemActor = Requestor Nothing "SYSTEM" DAL.SYSTEM

externalProvider :: Requestor
externalProvider = Requestor Nothing "SYSTEM" DAL.WEBHOOK

-- dashboard DashboardAccessType token → actor role text. Known tokens are normalised to a
-- canonical role; any OTHER role (e.g. a newly-added DOC_VERIFIER) flows through verbatim.
-- A blank role (requestorId present but requestorRole omitted) defaults to ADMIN — preserving
-- the prior closed-enum behaviour where unknown/absent roles fell back to ADMIN.
actorRoleFromText :: Text -> Text
actorRoleFromText = \case
  "" -> "ADMIN"
  "RENTAL_FLEET_OWNER" -> "FLEET_OWNER"
  "DASHBOARD_OPERATOR" -> "OPERATOR"
  other -> other

-- driver-app: map the already-fetched person.role to the actor role text (no extra query)
toActorRole :: DP.Role -> Text
toActorRole = \case
  DP.FLEET_OWNER -> "FLEET_OWNER"
  DP.FLEET_BUSINESS -> "FLEET_OWNER"
  DP.OPERATOR -> "OPERATOR"
  DP.ADMIN -> "ADMIN"
  _ -> "DRIVER"

-- ────────────────────────── Entity ──────────────────────────

-- Person-doc ENTITY (whose document the row is about — independent of the actor):
-- derive from the document owner's person.role instead of hardcoding DRIVER.
entityTypeFromRole :: DP.Role -> DAL.AuditEntityType
entityTypeFromRole = \case
  DP.FLEET_OWNER -> DAL.FLEET_OWNER
  DP.FLEET_BUSINESS -> DAL.FLEET_OWNER
  DP.OPERATOR -> DAL.OPERATOR
  _ -> DAL.DRIVER

-- Entity for an image row: vehicle-doc images carry the RC they belong to (rcId is set at
-- upload for insurance/PUC/permit/fitness/NOC/vehicle photos, where the RC is mandatory) and
-- belong to the VEHICLE entity; person-doc images (and the RC image itself, uploaded before
-- the RC row exists) belong to the owner with their role-derived entity type.
imageEntity :: DP.Role -> Image.Image -> (DAL.AuditEntityType, Text)
imageEntity ownerRole image = case image.rcId of
  Just rcId -> (DAL.VEHICLE, rcId)
  Nothing -> (entityTypeFromRole ownerRole, image.personId.getId)

-- Resolve the audit actor: the supplied dashboard/operator requestor, else the person acting on their own
-- document (role derived from person.role). Centralizes the driver-self fallback repeated across verify flows.
auditActorFromPersonOrRequestor :: DP.Person -> Maybe Requestor -> Requestor
auditActorFromPersonOrRequestor person mbRequestor =
  fromMaybe (driverAppPerson person.id (toActorRole person.role)) mbRequestor

-- Same fallback when only the person id (and a fixed "DRIVER" role) is in scope.
auditActorFromIdOrRequestor :: Id DP.Person -> Maybe Requestor -> Requestor
auditActorFromIdOrRequestor personId mbRequestor =
  fromMaybe (driverAppPerson personId "DRIVER") mbRequestor

-- Build a dashboard-operator actor from the forwarded (gate-controlled) requestorId + role. Nothing when the
-- role is absent (merchant.sendDocumentAuditActorDetails was off) so the caller falls back to the driver.
dashboardActorFromForwarded :: Maybe Text -> Maybe Text -> Maybe Requestor
dashboardActorFromForwarded mbRequestorId mbRequestorRole =
  (\role -> dashboardActor (Id <$> mbRequestorId) (actorRoleFromText role)) <$> mbRequestorRole

-- For dashboard surfaces where the acting person id is ALWAYS forwarded (so the actor is known) but the role is
-- only forwarded when the merchant opted in: build the dashboard requestor with the real forwarded role, else
-- keep the same person id + DASHBOARD source with the given fallback role. No hardcoded role baked in here — the
-- caller supplies the fallback that fits its surface.
dashboardActorOrFallback :: Text -> Maybe Text -> Text -> Requestor
dashboardActorOrFallback personId mbRole fallbackRole =
  fromMaybe (dashboardActor (Just (Id personId)) fallbackRole) (dashboardActorFromForwarded (Just personId) mbRole)

-- For INLINE dashboard audits (emitted directly in a BPP handler, no shared function with a driver-self
-- fallback): use the forwarded dashboard actor when present, else the SYSTEM actor. Critically this NEVER
-- yields a DASHBOARD actor with a Nothing actorId (which recordAudit silently drops) — the merchant-opted-out
-- case degrades to SYSTEM, which passes the producer gate. This is the canonical actor for dashboard-only
-- mutation-side audits; do not hand-roll `dashboardActor (Id <$> mbId) …` for these.
dashboardActorOrSystem :: Maybe Text -> Maybe Text -> Requestor
dashboardActorOrSystem mbRequestorId mbRequestorRole =
  fromMaybe systemActor (dashboardActorFromForwarded mbRequestorId mbRequestorRole)

-- Audit ENTITY type from an optionally-fetched owner person: FLEET_OWNER/OPERATOR/… derived from the role,
-- DRIVER when the owner could not be resolved (or the gated fetch was skipped). Pairs with a gated owner read
-- (fetchForAuditByCity) at sites where only a personId is in scope: mbOwner <- fetchForAuditByCity city (find pid);
-- let eType = entityTypeFromMbPerson mbOwner. Kept pure so Audit.hs stays free of Storage.Queries imports.
entityTypeFromMbPerson :: Maybe DP.Person -> DAL.AuditEntityType
entityTypeFromMbPerson = maybe DAL.DRIVER (entityTypeFromRole . (.role))

-- ─────────────────────────── Entry ───────────────────────────

data AuditLogEntry = AuditLogEntry
  { entityType :: DAL.AuditEntityType,
    entityId :: Text,
    documentType :: Maybe Text,
    fieldName :: Maybe Text,
    documentRefType :: DAL.DocumentRefType,
    documentRefId :: Maybe Text,
    eventId :: Maybe Text,
    action :: DAL.AuditAction,
    previousStatus :: Maybe Text,
    newStatus :: Maybe Text,
    reason :: Maybe Text,
    details :: Maybe Value,
    merchantId :: Maybe (Id DM.Merchant),
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity)
  }

-- ─────────────────────── Sink (publish) ───────────────────────

-- Gated by a dedicated 'documentAuditPublisherCfg' (reuses the RideEventsPublisherCfg shape:
-- streamPrefix + shardCount). Nothing = audit publishing OFF. The city-flag gate + persistence
-- happen in the DOCUMENT_AUDIT_CONSUMER, off the request path.
type AuditFlow m r =
  ( HedisFlow m r,
    HasField "documentAuditPublisherCfg" r (Maybe RideEventsPublisherCfg),
    MonadTime m,
    MonadIO m,
    MonadGuid m
  )

-- The ONE sink. Publishes a thin event (single xAdd) — NO DB write, NO fork. Best-effort.
recordAudit :: AuditFlow m r => Requestor -> AuditLogEntry -> m ()
recordAudit requestor entry = do
  mbCfg <- asks (.documentAuditPublisherCfg)
  whenJust mbCfg $ \cfg ->
    -- cheap actor short-circuit; a merchant-off dashboard event is never even published
    when (requestor.actorSource /= DAL.DASHBOARD || isJust requestor.actorId) $ do
      result <- try @_ @SomeException $ do
        now <- getCurrentTime
        guid <- generateGUID
        let event = buildAuditEvent guid now requestor entry
            shardId = hash entry.entityId `mod` cfg.shardCount
            streamName = cfg.streamPrefix <> show shardId
        void $ Hedis.xAdd streamName "*" [("payload", BSL.toStrict (A.encode event))]
      case result of
        Right () -> pure ()
        Left e -> logError $ "document-audit.publish-failed entityId=" <> entry.entityId <> " err=" <> show e

-- | Run a read done SOLELY to populate an audit field (e.g. the pre-update row for
-- 'previousStatus') only when document-audit logging is enabled for the city — pass
-- @fromMaybe False transporterConfig.enableDocumentAuditLog@ (the same per-city flag the
-- DOCUMENT_AUDIT_CONSUMER gates persistence on). Skips the extra read (returns Nothing) when
-- off, keeping it off the hot onboarding path.
fetchForAudit :: Applicative m => Bool -> m (Maybe a) -> m (Maybe a)
fetchForAudit enabled act = if enabled then act else pure Nothing

-- | Is document-audit logging enabled for this city? Reads the per-city 'enableDocumentAuditLog'
-- flag off an already-fetched 'TransporterConfig' (the same flag the DOCUMENT_AUDIT_CONSUMER gates
-- persistence on). Use at sites that already have the config in scope.
auditEnabled :: DTC.TransporterConfig -> Bool
auditEnabled = fromMaybe False . (.enableDocumentAuditLog)

-- | Gate a purely-audit read on the city's 'enableDocumentAuditLog' flag, fetched from the
-- in-memory config-pilot 'TransporterConfig'. BEST-EFFORT: if no config is found the read is
-- skipped (no throw) — it only feeds audit fields, so a missing config must not fail the request.
-- Use at the async webhook/handler sites that have no 'TransporterConfig' already in scope.
fetchForAuditByCity :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m (Maybe a) -> m (Maybe a)
fetchForAuditByCity opCityId act = do
  mbTc <- getConfig (TransporterConfigDimensions {merchantOperatingCityId = opCityId.getId}) (Just (SCTC.findByMerchantOpCityId opCityId Nothing))
  fetchForAudit (maybe False auditEnabled mbTc) act

-- | The city's 'TransporterConfig' but ONLY when document-audit is enabled (else Nothing). For an
-- audit-only prefetch that needs the config value itself (e.g. to bound a query), not just the
-- on/off flag. Same in-memory config-pilot read + best-effort (no-throw) semantics as 'fetchForAuditByCity'.
auditConfigByCity :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m (Maybe DTC.TransporterConfig)
auditConfigByCity opCityId = do
  mbTc <- getConfig (TransporterConfigDimensions {merchantOperatingCityId = opCityId.getId}) (Just (SCTC.findByMerchantOpCityId opCityId Nothing))
  pure $ mbTc >>= \tc -> if auditEnabled tc then Just tc else Nothing

buildAuditEvent :: Id DAL.DocumentAuditLog -> UTCTime -> Requestor -> AuditLogEntry -> Ev.DocumentAuditEvent
buildAuditEvent guid now Requestor {..} AuditLogEntry {merchantId = mId, merchantOperatingCityId = cId, ..} =
  Ev.DocumentAuditEvent
    { schemaVer = Ev.schemaVersion,
      auditId = guid.getId,
      eventTimestamp = now,
      entityType = show entityType,
      entityId = entityId,
      documentType = documentType,
      fieldName = fieldName,
      documentRefType = show documentRefType,
      documentRefId = documentRefId,
      eventId = eventId,
      action = show action,
      previousStatus = previousStatus,
      newStatus = newStatus,
      actorId = actorId,
      actorRole = actorRole,
      actorSource = show actorSource,
      reason = reason,
      details = details,
      merchantId = getId <$> mId,
      merchantOperatingCityId = getId <$> cId
    }

boolTxt :: Bool -> Text
boolTxt b = if b then "true" else "false"

-- empty entry to override per wrapper
base :: AuditLogEntry
base =
  AuditLogEntry
    { entityType = DAL.DRIVER,
      entityId = "",
      documentType = Nothing,
      fieldName = Nothing,
      documentRefType = DAL.IMAGE,
      documentRefId = Nothing,
      eventId = Nothing,
      action = DAL.STATUS_CHANGED,
      previousStatus = Nothing,
      newStatus = Nothing,
      reason = Nothing,
      details = Nothing,
      merchantId = Nothing,
      merchantOperatingCityId = Nothing
    }

-- ───────────────────────── Wrappers ─────────────────────────

-- Image upload (UPLOADED). documentRefId = the image id. ownerRole = the image owner's
-- person.role; the entity is derived from it (or the VEHICLE when the image carries an rcId).
auditImageUpload :: AuditFlow m r => Requestor -> DP.Role -> Image.Image -> m ()
auditImageUpload requestor ownerRole image =
  recordAudit requestor $
    base
      { entityType = eType,
        entityId = eId,
        documentType = Just (show image.imageType),
        documentRefType = DAL.IMAGE,
        documentRefId = Just image.id.getId,
        action = DAL.UPLOADED,
        newStatus = show <$> image.verificationStatus,
        merchantId = Just image.merchantId,
        merchantOperatingCityId = image.merchantOperatingCityId
      }
  where
    (eType, eId) = imageEntity ownerRole image

-- Image status change with the full Image in scope. prev is the image's current
-- (Maybe) verificationStatus, new is the status we are transitioning to.
-- ownerRole = the image owner's person.role (entity derived; rcId-carrying images → VEHICLE).
auditImageStatus :: AuditFlow m r => Requestor -> DP.Role -> Image.Image -> Maybe Documents.VerificationStatus -> Documents.VerificationStatus -> DAL.AuditAction -> m ()
auditImageStatus requestor ownerRole image prev new act =
  recordAudit requestor $
    base
      { entityType = eType,
        entityId = eId,
        documentType = Just (show image.imageType),
        documentRefType = DAL.IMAGE,
        documentRefId = Just image.id.getId,
        action = act,
        previousStatus = show <$> prev,
        newStatus = Just (show new),
        merchantId = Just image.merchantId,
        merchantOperatingCityId = image.merchantOperatingCityId
      }
  where
    (eType, eId) = imageEntity ownerRole image

-- Image status when only the id is in scope. eType/entityId = the entity the image belongs to
-- (owner person with role-derived type, or VEHICLE + rcId for vehicle-doc images).
-- merchant/city MUST be threaded so the consumer's per-city gate can match (a Nothing city is dropped).
auditImageStatusById :: (AuditFlow m r, Show s) => Requestor -> DAL.AuditEntityType -> Text -> Text -> Id Image.Image -> Maybe Text -> s -> DAL.AuditAction -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
auditImageStatusById requestor eType entityId' docTypeTxt imgId prev new act =
  auditImageStatusByIdWithEvent requestor eType entityId' docTypeTxt imgId prev new act Nothing

-- Like auditImageStatusById but also stamps an eventId (provider/transaction id) so a
-- request-time row and its later async webhook result can be correlated on event_id.
auditImageStatusByIdWithEvent :: (AuditFlow m r, Show s) => Requestor -> DAL.AuditEntityType -> Text -> Text -> Id Image.Image -> Maybe Text -> s -> DAL.AuditAction -> Maybe Text -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
auditImageStatusByIdWithEvent requestor eType entityId' docTypeTxt imgId prev new act eventId mId cId =
  recordAudit requestor $
    base
      { entityType = eType,
        entityId = entityId',
        documentType = Just docTypeTxt,
        documentRefType = DAL.IMAGE,
        documentRefId = Just imgId.getId,
        eventId = eventId,
        action = act,
        previousStatus = prev,
        newStatus = Just (show new),
        merchantId = Just mId,
        merchantOperatingCityId = Just cId
      }

-- Generic domain/derived doc status (RC, DL, inspection reject, …).
-- merchant/city MUST be threaded so the consumer's per-city gate can match (a Nothing city is dropped).
auditDocStatus :: AuditFlow m r => Requestor -> DAL.AuditEntityType -> Text -> Text -> DAL.DocumentRefType -> Maybe Text -> Maybe Text -> Maybe Text -> DAL.AuditAction -> Maybe Text -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
auditDocStatus requestor eType eId docTypeTxt refType refId prev new act reason' =
  auditDocStatusWithEvent requestor eType eId docTypeTxt refType refId prev new act reason' Nothing

-- Like auditDocStatus but also stamps an eventId (provider/transaction/request id) so a
-- request-time VERIFICATION_REQUESTED row and its async STATUS_CHANGED result correlate on event_id.
auditDocStatusWithEvent :: AuditFlow m r => Requestor -> DAL.AuditEntityType -> Text -> Text -> DAL.DocumentRefType -> Maybe Text -> Maybe Text -> Maybe Text -> DAL.AuditAction -> Maybe Text -> Maybe Text -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
auditDocStatusWithEvent requestor eType eId docTypeTxt refType refId prev new act reason' eventId mId cId =
  recordAudit requestor $
    base
      { entityType = eType,
        entityId = eId,
        documentType = Just docTypeTxt,
        documentRefType = refType,
        documentRefId = refId,
        eventId = eventId,
        action = act,
        previousStatus = prev,
        newStatus = new,
        reason = reason',
        merchantId = Just mId,
        merchantOperatingCityId = Just cId
      }

-- BOT review (BotApproval → ReviewRequest). botCheck tags the two phases in details.
auditBotApproval :: AuditFlow m r => Requestor -> DAL.AuditEntityType -> Text -> Text -> DAL.AuditAction -> Maybe Text -> Maybe Text -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Text -> m ()
auditBotApproval requestor eType eId reviewReqId act prev new mId cId botCheck =
  recordAudit requestor $
    base
      { entityType = eType,
        entityId = eId,
        documentType = Just "BotApproval",
        documentRefType = DAL.REVIEW_REQUEST,
        documentRefId = Just reviewReqId,
        action = act,
        previousStatus = prev,
        newStatus = new,
        details = (\l -> object ["botCheck" .= l]) <$> botCheck,
        merchantId = Just mId,
        merchantOperatingCityId = Just cId
      }

-- Document delete / unlink. merchant/city MUST be threaded so the consumer's per-city gate can match.
auditDelete :: AuditFlow m r => Requestor -> DAL.AuditEntityType -> Text -> Text -> DAL.DocumentRefType -> Maybe Text -> Maybe Text -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
auditDelete requestor eType eId docTypeTxt refType refId reason' mId cId =
  recordAudit requestor $
    base {entityType = eType, entityId = eId, documentType = Just docTypeTxt, documentRefType = refType, documentRefId = refId, action = DAL.DELETED, reason = reason', merchantId = Just mId, merchantOperatingCityId = Just cId}

-- Flag flip (verified / enabled / approved). documentType = Nothing; action = FLAG_CHANGED.
auditFlagChange :: AuditFlow m r => Requestor -> DAL.AuditEntityType -> Text -> Text -> DAL.DocumentRefType -> Bool -> Bool -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
auditFlagChange requestor eType eId fieldNm refType prev new mId cId =
  recordAudit requestor $
    base
      { entityType = eType,
        entityId = eId,
        fieldName = Just fieldNm,
        documentRefType = refType,
        documentRefId = Just eId,
        action = DAL.FLAG_CHANGED,
        previousStatus = Just (boolTxt prev),
        newStatus = Just (boolTxt new),
        merchantId = Just mId,
        merchantOperatingCityId = Just cId
      }
