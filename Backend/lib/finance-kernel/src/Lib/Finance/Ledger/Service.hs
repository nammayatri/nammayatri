{-
  Finance.Ledger.Service

  Concrete ledger entry operations for domain use.
  Implements double-entry bookkeeping (LAW 1: Conservation of Money).
  Uses generated Beam queries internally.

  Domain code only needs to:
  1. Decide what type of entry to create
  2. Calculate amounts
  3. Pick the right accounts

  All generic operations (status, voiding, querying, summing) are here.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Ledger.Service
  ( -- * Create operations
    createEntry,
    createEntryWithBalanceUpdate,
    createReversal,

    -- * Status management
    updateEntryStatus,
    settleEntry,
    settleEntryWithBalancesAndAmount,
    voidEntry,
    markEntriesAsPaidOut,
    markEntriesAsClaimed,

    -- * Query by ID/reference
    getEntry,
    getEntriesByReference,
    getEntriesByEntityReference,
    getEntriesByReferenceAndToAccount,
    getEntriesByReferenceAndFromAccount,
    getEntriesByAccount,
    getLatestEntryByAccount,
    getEntriesBetween,

    -- * Query by account (the main way domain queries)
    findByAccountAndStatus,
    findByAccountWithFilters,
    findByAccountWithFiltersAndConcernedIndividual,

    -- * Aggregations (common for domain use)
    sumByAccountAndStatus,
    countByAccountAndStatus,

    -- * Payout-specific queries (efficient DB-level filtering)
    findCreditsByAccountAfterTime,
    findUnsettledByAccountAfterTime,
    findUnsettledByAccountBeforeTime,
    findBySettlementId,

    -- * Payout eligibility (shared by driver wallet and rider cashback payouts)
    PayoutEligibility (..),
    getPayoutEligibilityData,
    entryNetForAccount,

    -- * Settlement reservation (Option A — DB-level in-flight guard)
    markEntriesAsProcessing,
    markEntriesAsUnsettled,

    -- * Input types (re-export from Interface)
    module Lib.Finance.Ledger.Interface,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Kernel.Beam.Functions (ToTType' (..), findAllWithKV, findAllWithOptionsKV, updateWithKV)
import Kernel.Prelude
import Kernel.Types.Common ()
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Audit.Interface (AuditInput (..))
import qualified Lib.Finance.Audit.Service as Audit
import Lib.Finance.Core.Types (TimeRange (..))
import Lib.Finance.Domain.Types.Account (Account)
import qualified Lib.Finance.Domain.Types.Account as Account
import Lib.Finance.Domain.Types.AuditEntry (AuditAction (..))
import qualified Lib.Finance.Domain.Types.AuditEntry as AuditDomain
import Lib.Finance.Domain.Types.LedgerEntry
import Lib.Finance.Error.Types
import Lib.Finance.Ledger.Interface
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Beam.LedgerEntry as BeamLE
import qualified Lib.Finance.Storage.Queries.Account as QAccount
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedger
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerExtra
import qualified Sequelize as Se

--------------------------------------------------------------------------------
-- AUDIT HELPERS
--------------------------------------------------------------------------------

ledgerEntryToAuditValue :: LedgerEntry -> Aeson.Value
ledgerEntryToAuditValue = Aeson.toJSON . toTType' @BeamLE.LedgerEntry . hideLedgerEntrySensitiveFields
  where
    -- No sensitive fields identified in LedgerEntry as of now
    hideLedgerEntrySensitiveFields :: LedgerEntry -> LedgerEntry
    hideLedgerEntrySensitiveFields = identity

logLedgerAudit ::
  BeamFlow.BeamFlow m r =>
  ActorInfo ->
  AuditAction ->
  Maybe LedgerEntry ->
  LedgerEntry ->
  m ()
logLedgerAudit actorInfo action mbBefore afterEntry = do
  auditResult <-
    Audit.logAudit
      AuditInput
        { entityType = AuditDomain.LedgerEntry,
          entityId = afterEntry.id.getId,
          action = action,
          actorType = actorInfo.actorType,
          actorId = actorInfo.actorId,
          beforeState = ledgerEntryToAuditValue <$> mbBefore,
          afterState = Just $ ledgerEntryToAuditValue afterEntry,
          merchantId = afterEntry.merchantId,
          merchantOperatingCityId = afterEntry.merchantOperatingCityId
        }
  case auditResult of
    Left err -> logWarning $ "Failed to audit ledger entry (" <> show action <> "): " <> show err
    Right _ -> pure ()

auditLedgerCreate ::
  BeamFlow.BeamFlow m r =>
  ActorInfo ->
  LedgerEntry ->
  m ()
auditLedgerCreate actorInfo = logLedgerAudit actorInfo Created Nothing

auditLedgerReversal ::
  BeamFlow.BeamFlow m r =>
  ActorInfo ->
  LedgerEntry ->
  m ()
auditLedgerReversal actorInfo = logLedgerAudit actorInfo Reversed Nothing

auditLedgerUpdate ::
  BeamFlow.BeamFlow m r =>
  ActorInfo ->
  AuditAction ->
  LedgerEntry ->
  LedgerEntry ->
  m ()
auditLedgerUpdate actorInfo action before after =
  logLedgerAudit actorInfo action (Just before) after

settlementSnapshotChanged :: LedgerEntry -> LedgerEntry -> Bool
settlementSnapshotChanged before after =
  before.settlementStatus /= after.settlementStatus
    || before.settlementId /= after.settlementId
    || before.settlementTimestamp /= after.settlementTimestamp

settlementAuditPairs ::
  [LedgerEntry] ->
  [LedgerEntry] ->
  [(LedgerEntry, LedgerEntry)]
settlementAuditPairs beforeEntries afterEntries =
  let afterById = Map.fromList [(entry.id, entry) | entry <- afterEntries]
   in [ (before, after)
        | before <- beforeEntries,
          Just after <- [Map.lookup before.id afterById],
          settlementSnapshotChanged before after
      ]

auditBatchSettlementUpdates ::
  BeamFlow.BeamFlow m r =>
  ActorInfo ->
  AuditAction ->
  [LedgerEntry] ->
  m ()
auditBatchSettlementUpdates _ _ [] = pure ()
auditBatchSettlementUpdates actorInfo action beforeEntries = do
  afterEntries <- QLedgerExtra.findByIds (map (.id) beforeEntries)
  forM_ (settlementAuditPairs beforeEntries afterEntries) $ \(before, after) ->
    auditLedgerUpdate actorInfo action before after

--------------------------------------------------------------------------------
-- CREATE OPERATIONS
--------------------------------------------------------------------------------

-- | Create a ledger entry WITHOUT updating account balances
-- Use this for entries that don't immediately affect balances (e.g., PENDING)
createEntry ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  LedgerEntryInput ->
  m (Either FinanceError LedgerEntry)
createEntry input = do
  actorInfo <- asks (.actorInfo)
  now <- getCurrentTime
  entryId <- generateGUID
  let entry =
        LedgerEntry
          { id = Id entryId,
            fromAccountId = input.fromAccountId,
            toAccountId = input.toAccountId,
            concernedIndividualId = input.concernedIndividualId,
            amount = input.amount,
            currency = input.currency,
            entryType = input.entryType,
            status = input.status,
            referenceType = input.referenceType,
            referenceId = input.referenceId,
            entityReferenceId = input.entityReferenceId,
            entityReferenceType = input.entityReferenceType,
            reversalOf = Nothing,
            voidReason = Nothing,
            settledAt = Nothing,
            metadataV2 = input.metadata,
            reconciliationStatus = Nothing,
            fromStartingBalance = Nothing,
            fromEndingBalance = Nothing,
            toStartingBalance = Nothing,
            toEndingBalance = Nothing,
            settlementStatus = input.settlementStatus,
            settlementId = Nothing,
            settlementTimestamp = Nothing,
            timestamp = now,
            merchantId = input.merchantId,
            merchantOperatingCityId = input.merchantOperatingCityId,
            createdBy = Just actorInfo.actorType,
            createdById = actorInfo.actorId,
            updatedBy = Just actorInfo.actorType,
            updatedById = actorInfo.actorId,
            createdAt = now,
            updatedAt = now
          }

  QLedger.create entry
  auditLedgerCreate actorInfo entry
  pure $ Right entry

-- | Create a ledger entry AND update account balances atomically
-- LAW 1: Credits = Debits (fromAccount debited, toAccount credited)
createEntryWithBalanceUpdate ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  LedgerEntryInput ->
  m (Either FinanceError LedgerEntry)
createEntryWithBalanceUpdate input = do
  actorInfo <- asks (.actorInfo)
  mbFromAccount <- QAccount.findById input.fromAccountId
  mbToAccount <- QAccount.findById input.toAccountId

  case (mbFromAccount, mbToAccount) of
    (Nothing, _) -> pure $ Left $ AccountError AccountNotFound (show input.fromAccountId)
    (_, Nothing) -> pure $ Left $ AccountError AccountNotFound (show input.toAccountId)
    (Just fromAccount, Just toAccount) -> do
      now <- getCurrentTime
      entryId <- generateGUID
      let amount = input.amount
          fromStartBal = fromAccount.balance
          toStartBal = toAccount.balance
          isAssetOrExpenseAccount acc = acc.accountType == Account.Asset || acc.accountType == Account.Expense
          fromEndBal =
            if isAssetOrExpenseAccount fromAccount
              then fromStartBal + amount
              else fromStartBal - amount
          toEndBal =
            if isAssetOrExpenseAccount toAccount
              then toStartBal - amount
              else toStartBal + amount
      let entry =
            LedgerEntry
              { id = Id entryId,
                fromAccountId = input.fromAccountId,
                toAccountId = input.toAccountId,
                concernedIndividualId = input.concernedIndividualId,
                amount = amount,
                currency = input.currency,
                entryType = input.entryType,
                status = input.status,
                referenceType = input.referenceType,
                referenceId = input.referenceId,
                entityReferenceId = input.entityReferenceId,
                entityReferenceType = input.entityReferenceType,
                reversalOf = Nothing,
                voidReason = Nothing,
                settledAt = Nothing,
                metadataV2 = input.metadata,
                reconciliationStatus = Nothing,
                fromStartingBalance = Just fromStartBal,
                fromEndingBalance = Just fromEndBal,
                toStartingBalance = Just toStartBal,
                toEndingBalance = Just toEndBal,
                settlementStatus = input.settlementStatus,
                settlementId = Nothing,
                settlementTimestamp = Nothing,
                timestamp = now,
                merchantId = input.merchantId,
                merchantOperatingCityId = input.merchantOperatingCityId,
                createdBy = Just actorInfo.actorType,
                createdById = actorInfo.actorId,
                updatedBy = Just actorInfo.actorType,
                updatedById = actorInfo.actorId,
                createdAt = now,
                updatedAt = now
              }
      QLedger.create entry
      _ <- QAccount.updateBalance fromEndBal input.fromAccountId
      _ <- QAccount.updateBalance toEndBal input.toAccountId
      auditLedgerCreate actorInfo entry
      pure $ Right entry

-- | Create a reversal entry for an existing entry
-- LAW 2: History is immutable - we create a new entry, not modify the old one
createReversal ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  Id LedgerEntry -> -- Entry to reverse
  Text -> -- Reason for reversal
  m (Either FinanceError LedgerEntry)
createReversal originalId reason = do
  actorInfo <- asks (.actorInfo)
  mbOriginal <- QLedger.findById originalId

  case mbOriginal of
    Nothing -> pure $ Left $ LedgerError InvalidReversal (show originalId)
    Just original -> do
      now <- getCurrentTime
      reversalId <- generateGUID

      let reversal =
            LedgerEntry
              { id = Id reversalId,
                fromAccountId = original.toAccountId, -- Swap
                toAccountId = original.fromAccountId,
                concernedIndividualId = original.concernedIndividualId,
                amount = original.amount,
                currency = original.currency,
                entryType = Reversal,
                status = SETTLED, -- Reversals are immediately settled
                referenceType = original.referenceType,
                referenceId = original.referenceId,
                entityReferenceId = original.entityReferenceId,
                entityReferenceType = original.entityReferenceType,
                reversalOf = Just originalId,
                voidReason = Nothing,
                settledAt = Just now,
                metadataV2 =
                  Just $
                    LedgerEntryMetadata
                      { subscriptionAllocations = Nothing,
                        reason = Just reason,
                        driverPayable = Nothing,
                        payoutOrderId = Nothing,
                        d2cReferralEarnings = Nothing,
                        d2dReferralEarnings = Nothing,
                        dailyStatsId = Nothing
                      },
                reconciliationStatus = original.reconciliationStatus,
                fromStartingBalance = Nothing,
                fromEndingBalance = Nothing,
                toStartingBalance = Nothing,
                toEndingBalance = Nothing,
                settlementStatus = Nothing,
                settlementId = Nothing,
                settlementTimestamp = Nothing,
                timestamp = now,
                merchantId = original.merchantId,
                merchantOperatingCityId = original.merchantOperatingCityId,
                createdBy = Just actorInfo.actorType,
                createdById = actorInfo.actorId,
                updatedBy = Just actorInfo.actorType,
                updatedById = actorInfo.actorId,
                createdAt = now,
                updatedAt = now
              }

      QLedger.create reversal

      -- Update account balances: undo the original's type-aware delta.
      -- createEntryWithBalanceUpdate applies from +amount / to -amount for Asset|Expense
      -- accounts and the opposite for the rest, so the reversal must invert exactly that
      -- per account type (a flat from +/to - doubles Asset|Expense balances).
      mbFrom <- QAccount.findById original.fromAccountId
      mbTo <- QAccount.findById original.toAccountId
      let isAssetOrExpenseAccount acc = acc.accountType == Account.Asset || acc.accountType == Account.Expense
      forM_ mbFrom $ \a -> QAccount.updateBalance (if isAssetOrExpenseAccount a then a.balance - original.amount else a.balance + original.amount) original.fromAccountId
      forM_ mbTo $ \a -> QAccount.updateBalance (if isAssetOrExpenseAccount a then a.balance + original.amount else a.balance - original.amount) original.toAccountId

      auditLedgerReversal actorInfo reversal
      pure $ Right reversal

--------------------------------------------------------------------------------
-- STATUS MANAGEMENT (These are generic, domain just calls them)
--------------------------------------------------------------------------------

-- | Update entry status (generic operation)
updateEntryStatus ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  Id LedgerEntry ->
  EntryStatus ->
  m ()
updateEntryStatus entryId newStatus = do
  actorInfo <- asks (.actorInfo)
  mbBefore <- QLedger.findById entryId
  forM_ mbBefore $ \before ->
    when (before.status /= newStatus) $ do
      QLedger.updateStatus newStatus (Just actorInfo.actorType) actorInfo.actorId entryId
      mbAfter <- QLedger.findById entryId
      forM_ mbAfter $ \after ->
        auditLedgerUpdate actorInfo StatusChanged before after

-- | Settle an entry (mark as SETTLED with timestamp).
-- Idempotently posts balance deltas to from/to accounts the first time an
-- entry transitions out of PENDING/DUE, using the same Asset/Expense rule as
-- 'createEntryWithBalanceUpdate'. Writes the resulting balance snapshots onto
-- the entry. If the entry is already SETTLED (or not found / VOIDED), no-op.
settleEntry ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  Id LedgerEntry ->
  m ()
settleEntry entryId = do
  actorInfo <- asks (.actorInfo)
  mbEntry <- QLedger.findById entryId
  forM_ mbEntry $ \entry ->
    when (entry.status == PENDING || entry.status == DUE) $ do
      mbFrom <- QAccount.findById entry.fromAccountId
      mbTo <- QAccount.findById entry.toAccountId
      case (mbFrom, mbTo) of
        (Just fromAccount, Just toAccount) -> do
          now <- getCurrentTime
          let amount = entry.amount
              isAssetOrExpenseAccount acc = acc.accountType == Account.Asset || acc.accountType == Account.Expense
              fromStartBal = fromAccount.balance
              toStartBal = toAccount.balance
              fromEndBal =
                if isAssetOrExpenseAccount fromAccount
                  then fromStartBal + amount
                  else fromStartBal - amount
              toEndBal =
                if isAssetOrExpenseAccount toAccount
                  then toStartBal - amount
                  else toStartBal + amount
          _ <- QAccount.updateBalance fromEndBal entry.fromAccountId
          _ <- QAccount.updateBalance toEndBal entry.toAccountId
          let updatedEntry =
                entry
                  { status = SETTLED,
                    settledAt = Just now,
                    fromStartingBalance = Just fromStartBal,
                    fromEndingBalance = Just fromEndBal,
                    toStartingBalance = Just toStartBal,
                    toEndingBalance = Just toEndBal,
                    updatedBy = Just actorInfo.actorType,
                    updatedById = actorInfo.actorId
                  }
          QLedger.updateByPrimaryKey updatedEntry
          auditLedgerUpdate actorInfo StatusChanged entry updatedEntry
        _ -> pure ()

-- | Field-wise merge: prefer non-Nothing from the left (new) side.
mergeLedgerEntryMetadata :: LedgerEntryMetadata -> LedgerEntryMetadata -> LedgerEntryMetadata
mergeLedgerEntryMetadata new old =
  LedgerEntryMetadata
    { subscriptionAllocations = new.subscriptionAllocations <|> old.subscriptionAllocations,
      reason = new.reason <|> old.reason,
      driverPayable = new.driverPayable <|> old.driverPayable,
      payoutOrderId = new.payoutOrderId <|> old.payoutOrderId,
      d2cReferralEarnings = new.d2cReferralEarnings <|> old.d2cReferralEarnings,
      d2dReferralEarnings = new.d2dReferralEarnings <|> old.d2dReferralEarnings,
      dailyStatsId = new.dailyStatsId <|> old.dailyStatsId
    }

mergeMaybeLedgerEntryMetadata :: Maybe LedgerEntryMetadata -> Maybe LedgerEntryMetadata -> Maybe LedgerEntryMetadata
mergeMaybeLedgerEntryMetadata (Just new) (Just old) = Just $ mergeLedgerEntryMetadata new old
mergeMaybeLedgerEntryMetadata new old = new <|> old

-- | Settle an entry, update its amount, AND write balance snapshots.
-- Use when the final settled amount differs from the original hold amount (e.g., fare recalculation).
settleEntryWithBalancesAndAmount ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  Id LedgerEntry ->
  HighPrecMoney -> -- settledAmount (the actual/final amount)
  HighPrecMoney -> -- fromStartingBalance
  HighPrecMoney -> -- fromEndingBalance
  HighPrecMoney -> -- toStartingBalance
  HighPrecMoney -> -- toEndingBalance
  Maybe LedgerEntryMetadata ->
  m ()
settleEntryWithBalancesAndAmount entryId settledAmount fromStartBal fromEndBal toStartBal toEndBal mbMetadata = do
  actorInfo <- asks (.actorInfo)
  now <- getCurrentTime
  mbEntry <- QLedger.findById entryId
  forM_ mbEntry $ \entry -> do
    let updatedEntry =
          entry
            { amount = settledAmount,
              status = SETTLED,
              settledAt = Just now,
              fromStartingBalance = Just fromStartBal,
              fromEndingBalance = Just fromEndBal,
              toStartingBalance = Just toStartBal,
              toEndingBalance = Just toEndBal,
              metadataV2 = mergeMaybeLedgerEntryMetadata mbMetadata entry.metadataV2,
              updatedBy = Just actorInfo.actorType,
              updatedById = actorInfo.actorId
            }
    QLedger.updateByPrimaryKey updatedEntry
    auditLedgerUpdate actorInfo Updated entry updatedEntry

-- | Void an entry (mark as VOIDED with reason)
voidEntry ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  Id LedgerEntry ->
  Text -> -- Reason for voiding
  m ()
voidEntry entryId reason = do
  actorInfo <- asks (.actorInfo)
  mbBefore <- QLedger.findById entryId
  forM_ mbBefore $ \before ->
    when (before.status /= VOIDED) $ do
      QLedger.updateVoided VOIDED (Just reason) (Just actorInfo.actorType) actorInfo.actorId entryId
      mbAfter <- QLedger.findById entryId
      forM_ mbAfter $ \after ->
        auditLedgerUpdate actorInfo StatusChanged before after

--------------------------------------------------------------------------------
-- QUERY BY ID/REFERENCE
--------------------------------------------------------------------------------

-- | Get a ledger entry by ID
getEntry ::
  (BeamFlow.BeamFlow m r) =>
  Id LedgerEntry ->
  m (Maybe LedgerEntry)
getEntry = QLedger.findById

-- | Get all entries for a reference (e.g., rideId)
getEntriesByReference ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- Reference type (e.g., "RIDE")
  Text -> -- Reference ID
  m [LedgerEntry]
getEntriesByReference = QLedger.findByReference

-- | Every entry for a sub-domain entity (e.g. one refund request), reversals included —
--   a reversal inherits the entity reference of the entry it reverses.
getEntriesByEntityReference ::
  (BeamFlow.BeamFlow m r) =>
  EntityReferenceType ->
  Text -> -- Entity reference ID (e.g. refundRequestId)
  m [LedgerEntry]
getEntriesByEntityReference entityRefType entityRefId =
  QLedger.findByEntityReference (Just entityRefType) (Just entityRefId)

-- | Entries for (refType, refId) where toAccountId matches — i.e. credits
-- landing in the given account. Use for earn-side lookups.
getEntriesByReferenceAndToAccount ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- Reference type
  Text -> -- Reference ID
  Id Account ->
  m [LedgerEntry]
getEntriesByReferenceAndToAccount = QLedger.findByReferenceAndToAccount

-- | Entries for (refType, refId) where fromAccountId matches — i.e. debits
-- leaving the given account. Use for burn-side lookups.
getEntriesByReferenceAndFromAccount ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- Reference type
  Text -> -- Reference ID
  Id Account ->
  m [LedgerEntry]
getEntriesByReferenceAndFromAccount = QLedger.findByReferenceAndFromAccount

-- | Get all entries for an account
getEntriesByAccount ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  m [LedgerEntry]
getEntriesByAccount accountId = do
  fromEntries <- QLedger.findByFromAccount accountId
  toEntries <- QLedger.findByToAccount accountId
  pure $ fromEntries <> toEntries

-- | Latest ledger entry for an account (by createdAt)
getLatestEntryByAccount ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  m (Maybe LedgerEntry)
getLatestEntryByAccount = QLedgerExtra.findLatestByAccount

-- | Get entries for an account within a time range
getEntriesBetween ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  TimeRange ->
  m [LedgerEntry]
getEntriesBetween accountId timeRange = do
  allEntries <- getEntriesByAccount accountId
  pure $
    filter
      ( \e ->
          e.timestamp >= timeRange.rangeFrom
            && e.timestamp <= timeRange.rangeTo
      )
      allEntries

--------------------------------------------------------------------------------
-- QUERY BY ACCOUNT (Main way domain code queries)
--------------------------------------------------------------------------------

-- | Find entries for an account with specific status
findByAccountAndStatus ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  EntryStatus ->
  m [LedgerEntry]
findByAccountAndStatus accountId status = do
  entries <- getEntriesByAccount accountId
  pure $ filter (\e -> e.status == status) entries

findByAccountWithFilters ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe EntryStatus ->
  Maybe [Text] ->
  m [LedgerEntry]
findByAccountWithFilters accountId mbFrom mbTo mbMin mbMax mbStatus mbReferenceTypes = do
  entries <- getEntriesByAccount accountId
  pure $
    filter
      ( \e ->
          and
            [ maybe True (\from -> e.timestamp >= from) mbFrom,
              maybe True (\to -> e.timestamp <= to) mbTo,
              maybe True (\minAmt -> e.amount >= minAmt) mbMin,
              maybe True (\maxAmt -> e.amount <= maxAmt) mbMax,
              maybe True (\status -> e.status == status) mbStatus,
              maybe True (\refs -> e.referenceType `elem` refs) mbReferenceTypes
            ]
      )
      entries

-- | Newest-first (createdAt DESC) entries touching an account, with limit/offset
--   applied by the query itself. Callers must NOT slice the result themselves:
--   the plain findAll has no ORDER BY, so slicing it hands back an arbitrary
--   window of the account's history (physical row order in practice, i.e. the
--   oldest entries) and recent entries never surface on the first page.
findByAccountWithFiltersAndConcernedIndividual ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe EntryStatus ->
  Maybe [Text] ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  m [LedgerEntry]
findByAccountWithFiltersAndConcernedIndividual accountId mbFrom mbTo mbMin mbMax mbStatus mbReferenceTypes mbConcernedIndividualId mbLimit mbOffset =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Or
            [ Se.Is BeamLE.toAccountId $ Se.Eq (getId accountId),
              Se.Is BeamLE.fromAccountId $ Se.Eq (getId accountId)
            ]
        ]
          <> [Se.Is BeamLE.timestamp $ Se.GreaterThanOrEq from | Just from <- [mbFrom]]
          <> [Se.Is BeamLE.timestamp $ Se.LessThanOrEq to | Just to <- [mbTo]]
          <> [Se.Is BeamLE.amount $ Se.GreaterThanOrEq minAmt | Just minAmt <- [mbMin]]
          <> [Se.Is BeamLE.amount $ Se.LessThanOrEq maxAmt | Just maxAmt <- [mbMax]]
          <> [Se.Is BeamLE.status $ Se.Eq status | Just status <- [mbStatus]]
          <> [Se.Is BeamLE.referenceType $ Se.In refs | Just refs <- [mbReferenceTypes]]
          <> [Se.Is BeamLE.concernedIndividualId $ Se.Eq (Just cid) | Just cid <- [mbConcernedIndividualId]]
    ]
    (Se.Desc BeamLE.createdAt)
    mbLimit
    mbOffset

--------------------------------------------------------------------------------
-- AGGREGATIONS (Common operations domain needs)
--------------------------------------------------------------------------------

-- | Sum amounts for an account by status
sumByAccountAndStatus ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  EntryStatus ->
  m HighPrecMoney
sumByAccountAndStatus accountId status = do
  entries <- findByAccountAndStatus accountId status
  pure $ sum $ map (.amount) entries

-- | Count entries for an account by status
countByAccountAndStatus ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  EntryStatus ->
  m Int
countByAccountAndStatus accountId status = do
  entries <- findByAccountAndStatus accountId status
  pure $ length entries

--------------------------------------------------------------------------------
-- PAYOUT-SPECIFIC QUERIES (efficient DB-level filtering)
--------------------------------------------------------------------------------

-- | Find credit entries (toAccountId = accountId) after a given time.
--   Used for computing non-redeemable balance (recent credits that can't be paid out yet).
findCreditsByAccountAfterTime ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  UTCTime -> -- from (cutoff)
  UTCTime -> -- to (now)
  m [LedgerEntry]
findCreditsByAccountAfterTime accountId from to =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamLE.toAccountId $ Se.Eq (getId accountId),
          Se.Is BeamLE.status $ Se.Eq SETTLED,
          Se.Is BeamLE.timestamp $ Se.GreaterThanOrEq from,
          Se.Is BeamLE.timestamp $ Se.LessThanOrEq to,
          Se.Or
            [ Se.Is BeamLE.settlementStatus $ Se.Eq (Just UNSETTLED),
              Se.Is BeamLE.settlementStatus $ Se.Eq Nothing
            ]
        ]
    ]

-- | Find unsettled entries (both credits and debits) for an account after a given time.
--   Used for computing non-redeemable balance (recent net movement that can't be paid out yet).
findUnsettledByAccountAfterTime ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  UTCTime -> -- from (cutoff)
  UTCTime -> -- to (now)
  m [LedgerEntry]
findUnsettledByAccountAfterTime accountId from to =
  findAllWithKV
    [ Se.And
        [ Se.Or
            [ Se.Is BeamLE.toAccountId $ Se.Eq (getId accountId),
              Se.Is BeamLE.fromAccountId $ Se.Eq (getId accountId)
            ],
          Se.Is BeamLE.status $ Se.Eq SETTLED,
          Se.Is BeamLE.timestamp $ Se.GreaterThanOrEq from,
          Se.Is BeamLE.timestamp $ Se.LessThanOrEq to,
          Se.Or
            [ Se.Is BeamLE.settlementStatus $ Se.Eq (Just UNSETTLED),
              Se.Is BeamLE.settlementStatus $ Se.Eq Nothing
            ]
        ]
    ]

-- | Find unsettled entries (both credits and debits) for an account before a given time.
--   Returns entries where settlementStatus = UNSETTLED OR settlementStatus IS NULL,
--   Used for collecting redeemable entry IDs for payout settlement.
findUnsettledByAccountBeforeTime ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  UTCTime -> -- before (cutoff)
  m [LedgerEntry]
findUnsettledByAccountBeforeTime accountId before =
  findAllWithKV
    [ Se.And
        [ Se.Or
            [ Se.Is BeamLE.toAccountId $ Se.Eq (getId accountId),
              Se.Is BeamLE.fromAccountId $ Se.Eq (getId accountId)
            ],
          Se.Is BeamLE.timestamp $ Se.LessThan before,
          Se.Is BeamLE.status $ Se.Eq SETTLED,
          Se.Or
            [ Se.Is BeamLE.settlementStatus $ Se.Eq (Just UNSETTLED),
              Se.Is BeamLE.settlementStatus $ Se.Eq Nothing
            ]
        ]
    ]

-- | Entries stamped with a settlementId (PayoutRequest id) while reserved or paid out.
findBySettlementId ::
  (BeamFlow.BeamFlow m r) =>
  Text ->
  m [LedgerEntry]
findBySettlementId settlementId =
  findAllWithKV [Se.Is BeamLE.settlementId $ Se.Eq (Just settlementId)]

--------------------------------------------------------------------------------
-- PAYOUT ELIGIBILITY
--------------------------------------------------------------------------------

data PayoutEligibility = PayoutEligibility
  { walletBalance :: HighPrecMoney,
    nonRedeemableBalance :: HighPrecMoney,
    redeemableBalance :: HighPrecMoney,
    redeemableEntries :: [LedgerEntry]
  }

-- | Signed impact of an entry on the given account: +amount when it credits the
--   account (to side), -amount when it debits it (from side).
entryNetForAccount :: Id Account -> LedgerEntry -> HighPrecMoney
entryNetForAccount accountId e
  | e.toAccountId == accountId = e.amount
  | e.fromAccountId == accountId = negate e.amount
  | otherwise = 0

{- Note [Redeemable payout calculation]
Only entries *below* the payout cut-off are redeemable. Anything earned after
the cut-off must stay in the wallet to fund the next cycle, so we hold it back:

    nonRedeemable = max 0 (min walletBalance (creditsAboveCutOff - debitsAboveCutOff))
                  = max 0 (min walletBalance netAbove)
    payout        = max 0 (walletBalance - nonRedeemable)

Each clamp matters: `max 0` so that post-cut-off deductions exceeding
post-cut-off earnings never *add* to the payout (Scenario 2); `min walletBalance`
so that debt carried across the cut-off cannot hold back more than the wallet
holds (Scenario 4), which keeps `payout + nonRedeemable == walletBalance` for
any non-negative balance (a negative balance reports 0 / 0).

Note this consumes only the *sums* above the cut-off, so it is path-independent:
the interleaving of debits and credits inside the post-cut-off window cannot
change the result (see Scenario 3, where the running balance dips negative).
It also never sums the entries below the cut-off: rows there that lack a
settlementStatus (e.g. payout debits posted before the hold model) would
otherwise net against fresh earnings. Those rows are only used to pick which
entries a payout stamps PAID_OUT.

Scenario 1 — cash commission on top, wallet stays positive
----------------------------------------------------------
  nonRedeemable = max 0 (min 150 (200 - 150)) = 50
  payout        = max 0 (150 - 50)            = 100      (expected 100)
  wallet = 150

  above cut-off:
    debits   Online Ride 3  charge + commission   50
             Online Ride 4  charge + commission   50
             Cash   Ride 5  charge + commission   50 -> 150
    credits  Online Ride 3                       100
             Online Ride 4                       100   -> 200
    net                                                 +50   (held back)

  --- payout cut-off ---

    debits   Online Ride 1  charge + commission   50
             Online Ride 2  charge + commission   50   -> 100
    credits  Online Ride 1                       100
             Online Ride 2                       100   -> 200
    net                                                +100   (redeemable)

Scenario 2 — cash commissions exceed online earnings, netAbove negative
------------------------------------------------------------------------
  nonRedeemable = max 0 (min 50 (200 - 250)) = 0
  payout        = max 0 (50 - 0)             = 50        (expected 50)
  wallet = 50

  above cut-off:
    debits   Online Ride 3  charge + commission   50
             Online Ride 4  charge + commission   50
             Cash   Ride 5  charge + commission   50
             Cash   Ride 6  charge + commission   50
             Cash   Ride 7  charge + commission   50   -> 250
    credits  Online Ride 3                       100
             Online Ride 4                       100   -> 200
    net                                                 -50   (clamped to 0,
                                                               already eaten
                                                               into wallet)

  --- payout cut-off ---

    debits   Online Ride 1  charge + commission   50
             Online Ride 2  charge + commission   50   -> 100
    credits  Online Ride 1                       100
             Online Ride 2                       100   -> 200
    net                                                +100

Scenario 3 — cash commissions land first, wallet dips negative, then recovers
------------------------------------------------------------------------------
  nonRedeemable = max 0 (min 150 (400 - 350)) = 50
  payout        = max 0 (150 - 50)            = 100      (expected 100)
  wallet = 150

  above cut-off (running balance shown, starting from 100 below the cut-off):
    debits   Cash   Ride 3  charge + commission   50   ->   50
             Cash   Ride 4  charge + commission   50   ->    0
             Cash   Ride 5  charge + commission   50   ->  -50  NEGATIVE
             Online Ride 6  charge + commission   50
             Online Ride 7  charge + commission   50
             Online Ride 8  charge + commission   50
             Online Ride 9  charge + commission   50   -> 350
    credits  Online Ride 6                       100   ->    0
             Online Ride 7                       100   ->   50
             Online Ride 8                       100   ->  100
             Online Ride 9                       100   ->  150
                                                       -> 400
    net                                                 +50   (held back)

  --- payout cut-off ---

    debits   Online Ride 1  charge + commission   50
             Online Ride 2  charge + commission   50   -> 100
    credits  Online Ride 1                       100
             Online Ride 2                       100   -> 200
    net                                                +100

  The transient -50 never reaches the formula: it sees only the totals
  (350 debits / 400 credits), so Scenario 3 and Scenario 1 agree.

Scenario 4 — debt carried across the cut-off
---------------------------------------------
  Cash commissions charged *before* the cut-off leave the driver closing the
  previous cycle owing 50. There is nothing redeemable, but the wallet is
  positive because post-cut-off online rides funded it.

  nonRedeemable = max 0 (min 50 (200 - 100)) = 50
  payout        = max 0 (50 - 50)            = 0         (expected 0)
  wallet = 50

  above cut-off:
    debits   Online Ride 3  charge + commission   50
             Online Ride 4  charge + commission   50   -> 100
    credits  Online Ride 3                       100
             Online Ride 4                       100   -> 200
    net                                                +100   (held back)

  --- payout cut-off ---

    debits   Online Ride 1  charge + commission   50
             Online Ride 2  charge + commission   50
             Cash   Ride A  charge + commission   50
             Cash   Ride B  charge + commission   50
             Cash   Ride C  charge + commission   50   -> 250
    credits  Online Ride 1                       100
             Online Ride 2                       100   -> 200
    net                                                 -50   (debt, NOT
                                                               redeemable)

  Without `min walletBalance` the hold-back would be reported as 100 against a
  wallet of 50; with it the whole 50 is held back, the payout is 0, and the
  post-cut-off earnings become payable next cycle once they have absorbed the
  debt.
-}

-- | Payout eligibility for one wallet account, see Note [Redeemable payout calculation].
--   PROCESSING entries (in-flight payout holds and their reservations) are excluded by the
--   unsettled queries, and PAID_OUT entries never come back, so a payout in flight or already
--   settled cannot be counted again.
getPayoutEligibilityData ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  HighPrecMoney -> -- current wallet balance
  UTCTime -> -- payout cutoff time
  UTCTime -> -- current time (upper bound)
  m PayoutEligibility
getPayoutEligibilityData accountId walletBalance cutoff now = do
  unsettledAbove <- findUnsettledByAccountAfterTime accountId cutoff now
  redeemableEntries <- findUnsettledByAccountBeforeTime accountId cutoff
  let netAbove = sum (map (entryNetForAccount accountId) unsettledAbove)
      nonRedeemableBalance = max 0 (min walletBalance netAbove)
  pure
    PayoutEligibility
      { walletBalance,
        nonRedeemableBalance,
        redeemableBalance = max 0 (walletBalance - nonRedeemableBalance),
        redeemableEntries
      }

--------------------------------------------------------------------------------
-- SETTLEMENT (Mark entries as paid out)
--------------------------------------------------------------------------------

-- | Mark a batch of ledger entries as paid out.
-- Uses a single batch UPDATE query for performance.
-- Used by the wallet payout webhook handler after a successful disbursement.
markEntriesAsPaidOut ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  [Id LedgerEntry] -> -- Entry IDs to mark
  Text -> -- Settlement ID (PayoutRequest ID)
  m ()
markEntriesAsPaidOut [] _ = pure ()
markEntriesAsPaidOut entryIds payoutRequestId = do
  actorInfo <- asks (.actorInfo)
  beforeEntries <- QLedgerExtra.findByIds entryIds
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamLE.settlementStatus (Just PAID_OUT),
      Se.Set BeamLE.settlementId (Just payoutRequestId),
      Se.Set BeamLE.settlementTimestamp (Just now),
      Se.Set BeamLE.updatedAt now
    ]
    [Se.Is BeamLE.id $ Se.In (map (.getId) entryIds)]
  auditBatchSettlementUpdates actorInfo StatusChanged beforeEntries

-- | Mark entries as PAID_OUT without a settlementId: legs that move money rather than
--   accrue it (payout holds, settlements, reversals, airport cash withdrawals), so they are
--   never counted as unsettled or picked up by a later payout.
markEntriesAsClaimed ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  [Id LedgerEntry] ->
  m ()
markEntriesAsClaimed [] = pure ()
markEntriesAsClaimed entryIds = do
  actorInfo <- asks (.actorInfo)
  beforeEntries <- QLedgerExtra.findByIds entryIds
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamLE.settlementStatus (Just PAID_OUT),
      Se.Set BeamLE.settlementTimestamp (Just now),
      Se.Set BeamLE.updatedAt now
    ]
    [Se.Is BeamLE.id $ Se.In (map (.getId) entryIds)]
  auditBatchSettlementUpdates actorInfo StatusChanged beforeEntries

-- | Reserve a batch of ledger entries for an in-flight payout.
--   Sets settlementStatus = PROCESSING (so subsequent eligibility queries
--   skip them) and stamps the optional settlementId. Only flips entries
--   that are still UNSETTLED / NULL — already-PAID_OUT or already-PROCESSING
--   entries are left untouched.
markEntriesAsProcessing ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  [Id LedgerEntry] ->
  Maybe Text -> -- optional settlementId (PayoutRequest id once known)
  m ()
markEntriesAsProcessing [] _ = pure ()
markEntriesAsProcessing entryIds mbSettlementId = do
  actorInfo <- asks (.actorInfo)
  beforeEntries <- QLedgerExtra.findByIds entryIds
  now <- getCurrentTime
  updateWithKV
    ( [ Se.Set BeamLE.settlementStatus (Just PROCESSING),
        Se.Set BeamLE.settlementTimestamp (Just now),
        Se.Set BeamLE.updatedAt now
      ]
        <> maybe [] (\sid -> [Se.Set BeamLE.settlementId (Just sid)]) mbSettlementId
    )
    [ Se.And
        [ Se.Is BeamLE.id $ Se.In (map (.getId) entryIds),
          Se.Or
            [ Se.Is BeamLE.settlementStatus $ Se.Eq Nothing,
              Se.Is BeamLE.settlementStatus $ Se.Eq (Just UNSETTLED),
              -- already-PROCESSING entries are allowed so callers can
              -- idempotently stamp the PayoutRequest id once it's known.
              Se.Is BeamLE.settlementStatus $ Se.Eq (Just PROCESSING)
            ]
        ]
    ]
  auditBatchSettlementUpdates actorInfo StatusChanged beforeEntries

-- | Revert a batch of PROCESSING ledger entries back to UNSETTLED.
--   Used when a payout submission fails (sync or via webhook) to release
--   the reservation so the entries become eligible again.
--   Only flips PROCESSING entries — PAID_OUT entries are not touched.
markEntriesAsUnsettled ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  [Id LedgerEntry] ->
  m ()
markEntriesAsUnsettled [] = pure ()
markEntriesAsUnsettled entryIds = do
  actorInfo <- asks (.actorInfo)
  beforeEntries <- QLedgerExtra.findByIds entryIds
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamLE.settlementStatus (Just UNSETTLED),
      Se.Set BeamLE.settlementId Nothing,
      Se.Set BeamLE.settlementTimestamp Nothing,
      Se.Set BeamLE.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamLE.id $ Se.In (map (.getId) entryIds),
          Se.Or
            [ Se.Is BeamLE.settlementStatus $ Se.Eq (Just PROCESSING),
              Se.Is BeamLE.settlementStatus $ Se.Eq Nothing
            ]
        ]
    ]
  auditBatchSettlementUpdates actorInfo StatusChanged beforeEntries
