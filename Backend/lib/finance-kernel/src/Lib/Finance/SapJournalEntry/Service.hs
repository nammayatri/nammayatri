{-
  Finance.SapJournalEntry.Service

  Concrete SAP journal entry operations for domain use.
  Uses generated Beam queries internally.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.SapJournalEntry.Service
  ( createSapJournalEntry,
    module Lib.Finance.SapJournalEntry.Interface,
  )
where

import qualified Data.Aeson as Aeson
import Kernel.Beam.Functions (ToTType' (..))
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Audit.Interface (AuditInput (..))
import qualified Lib.Finance.Audit.Service as Audit
import Lib.Finance.Domain.Types.AuditEntry (AuditAction (..))
import qualified Lib.Finance.Domain.Types.AuditEntry as AuditDomain
import Lib.Finance.Domain.Types.SapJournalEntry
import Lib.Finance.SapJournalEntry.Interface
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Beam.SapJournalEntry as BeamSJE
import qualified Lib.Finance.Storage.Queries.SapJournalEntry as QSJE

--------------------------------------------------------------------------------
-- AUDIT HELPERS
--------------------------------------------------------------------------------

sapJournalEntryToAuditValue :: SapJournalEntry -> Aeson.Value
sapJournalEntryToAuditValue =
  Aeson.toJSON . toTType' @BeamSJE.SapJournalEntry . hideSapJournalEntrySensitiveFields
  where
    hideSapJournalEntrySensitiveFields :: SapJournalEntry -> SapJournalEntry
    hideSapJournalEntrySensitiveFields SapJournalEntry {..} =
      SapJournalEntry
        { rawResponse = Nothing,
          ..
        }

auditSapJournalEntryCreate ::
  BeamFlow.BeamFlow m r =>
  ActorInfo ->
  SapJournalEntry ->
  m ()
auditSapJournalEntryCreate actorInfo entry = do
  auditResult <-
    Audit.logAudit
      AuditInput
        { entityType = AuditDomain.SapJournalEntry,
          entityId = entry.id.getId,
          action = Created,
          actorType = actorInfo.actorType,
          actorId = actorInfo.actorId,
          beforeState = Nothing,
          afterState = Just $ sapJournalEntryToAuditValue entry,
          merchantId = entry.merchantId,
          merchantOperatingCityId = entry.merchantOperatingCityId
        }
  case auditResult of
    Left err ->
      logWarning $
        "Failed to audit SapJournalEntry (Created): " <> show err
    Right _ -> pure ()

--------------------------------------------------------------------------------
-- CREATE OPERATIONS
--------------------------------------------------------------------------------

createSapJournalEntry ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  SapJournalEntryInput ->
  m ()
createSapJournalEntry input = do
  actorInfo <- asks (.actorInfo)
  now <- getCurrentTime
  entryId <- generateGUID
  let entry =
        SapJournalEntry
          { id = Id entryId,
            belnr = input.belnr,
            batchId = input.batchId,
            blart = input.blart,
            transactionType = input.transactionType,
            description = input.description,
            budat = input.budat,
            bldat = input.bldat,
            gjahr = input.gjahr,
            totalDebitAmount = input.totalDebitAmount,
            totalCreditAmount = input.totalCreditAmount,
            currency = input.currency,
            transactionCount = input.transactionCount,
            glNumber = input.glNumber,
            glName = input.glName,
            sapMessage = input.sapMessage,
            status = input.status,
            periodStartTime = input.periodStartTime,
            periodEndTime = input.periodEndTime,
            rawResponse = input.rawResponse,
            merchantId = input.merchantId,
            merchantOperatingCityId = input.merchantOperatingCityId,
            createdBy = actorInfo.actorType,
            createdById = actorInfo.actorId,
            updatedBy = actorInfo.actorType,
            updatedById = actorInfo.actorId,
            createdAt = now,
            updatedAt = now
          }
  QSJE.create entry
  auditSapJournalEntryCreate actorInfo entry
