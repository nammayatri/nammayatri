module SharedLogic.Finance.SubscriptionPurchase
  ( createSubscriptionPurchase,
    updateSubscriptionPurchase,
    updateSubscriptionPurchaseStatus,
    activateSubscriptionPurchaseExpiry,
  )
where

import qualified Data.Aeson as Aeson
import Domain.Types.SubscriptionPurchase
import Kernel.Beam.Functions (ToTType' (..))
import Kernel.Prelude
import Kernel.Utils.Common
import Lib.Finance.Audit.Interface (AuditInput (..))
import qualified Lib.Finance.Audit.Service as Audit
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Domain.Types.AuditEntry (AuditAction (..))
import qualified Lib.Finance.Domain.Types.AuditEntry as AuditDomain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Storage.Beam.SubscriptionPurchase as Beam
import qualified Storage.Queries.SubscriptionPurchase as QSP
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSPE

--------------------------------------------------------------------------------
-- AUDIT HELPERS
--------------------------------------------------------------------------------

subscriptionPurchaseToAuditValue :: SubscriptionPurchase -> Aeson.Value
subscriptionPurchaseToAuditValue =
  Aeson.toJSON . toTType' @Beam.SubscriptionPurchase . hideSubscriptionPurchaseSensitiveFields
  where
    -- No sensitive fields identified in SubscriptionPurchase as of now
    hideSubscriptionPurchaseSensitiveFields :: SubscriptionPurchase -> SubscriptionPurchase
    hideSubscriptionPurchaseSensitiveFields = identity

logSubscriptionPurchaseAudit ::
  BeamFlow m r =>
  Finance.ActorInfo ->
  AuditAction ->
  Maybe SubscriptionPurchase ->
  SubscriptionPurchase ->
  m ()
logSubscriptionPurchaseAudit actorInfo action mbBefore afterPurchase = do
  auditResult <-
    Audit.logAudit
      AuditInput
        { entityType = AuditDomain.SubscriptionPurchase,
          entityId = afterPurchase.id.getId,
          action = action,
          actorType = actorInfo.actorType,
          actorId = actorInfo.actorId,
          beforeState = subscriptionPurchaseToAuditValue <$> mbBefore,
          afterState = Just $ subscriptionPurchaseToAuditValue afterPurchase,
          merchantId = afterPurchase.merchantId.getId,
          merchantOperatingCityId = afterPurchase.merchantOperatingCityId.getId
        }
  case auditResult of
    Left err -> logWarning $ "Failed to audit subscription purchase (" <> show action <> "): " <> show err
    Right _ -> pure ()

subscriptionPurchaseAuditAction :: SubscriptionPurchase -> SubscriptionPurchase -> AuditAction
subscriptionPurchaseAuditAction before after
  | before.status /= after.status = StatusChanged
  | otherwise = Updated

withUpdatedActorFields :: Finance.ActorInfo -> UTCTime -> SubscriptionPurchase -> SubscriptionPurchase
withUpdatedActorFields actorInfo now purchase =
  purchase
    { updatedBy = Just actorInfo.actorType,
      updatedById = actorInfo.actorId,
      updatedAt = now
    }

--------------------------------------------------------------------------------
-- CREATE/UPDATE OPERATIONS
--------------------------------------------------------------------------------

createSubscriptionPurchase ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  SubscriptionPurchase ->
  m ()
createSubscriptionPurchase purchase = do
  actorInfo <- asks (.actorInfo)
  QSP.create purchase
  logSubscriptionPurchaseAudit actorInfo Created Nothing purchase

updateSubscriptionPurchase ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  SubscriptionPurchase ->
  SubscriptionPurchase ->
  m ()
updateSubscriptionPurchase before after = do
  actorInfo <- asks (.actorInfo)
  now <- getCurrentTime
  let updated = withUpdatedActorFields actorInfo now after
      action = subscriptionPurchaseAuditAction before updated
  QSP.updateByPrimaryKey updated
  logSubscriptionPurchaseAudit actorInfo action (Just before) updated

updateSubscriptionPurchaseStatus ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  SubscriptionPurchase ->
  SubscriptionPurchaseStatus ->
  m ()
updateSubscriptionPurchaseStatus before newStatus = do
  actorInfo <- asks (.actorInfo)
  now <- getCurrentTime
  let after = withUpdatedActorFields actorInfo now (before {status = newStatus})
  QSP.updateStatusById newStatus (Just actorInfo.actorType) actorInfo.actorId before.id
  logSubscriptionPurchaseAudit actorInfo StatusChanged (Just before) after

activateSubscriptionPurchaseExpiry ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  SubscriptionPurchase ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m ()
activateSubscriptionPurchaseExpiry before expiryDate startDate = do
  actorInfo <- asks (.actorInfo)
  now <- getCurrentTime
  let after = withUpdatedActorFields actorInfo now (before {expiryDate = expiryDate, startDate = startDate})
  QSPE.updateExpiryAndStartDateById expiryDate startDate actorInfo before.id
  logSubscriptionPurchaseAudit actorInfo Updated (Just before) after
