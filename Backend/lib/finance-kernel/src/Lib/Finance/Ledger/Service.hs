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
    voidEntry,

    -- * Query by ID/reference
    getEntry,
    getEntriesByReference,
    getEntriesByAccount,
    getEntriesBetween,

    -- * Query by account (the main way domain queries)
    findByAccountAndStatus,
    findByAccountWithFilters,

    -- * Aggregations (common for domain use)
    sumByAccountAndStatus,
    countByAccountAndStatus,

    -- * Input types (re-export from Interface)
    module Lib.Finance.Ledger.Interface,
  )
where

import qualified Data.Aeson as Aeson
import Kernel.Prelude
import Kernel.Types.Common ()
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Core.Types (TimeRange (..))
import Lib.Finance.Domain.Types.Account (Account)
import qualified Lib.Finance.Domain.Types.Account as Account
import Lib.Finance.Domain.Types.LedgerEntry
import Lib.Finance.Error.Types
import Lib.Finance.Ledger.Interface
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.Account as QAccount
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedger

--------------------------------------------------------------------------------
-- CREATE OPERATIONS
--------------------------------------------------------------------------------

-- | Create a ledger entry WITHOUT updating account balances
-- Use this for entries that don't immediately affect balances (e.g., PENDING)
createEntry ::
  (BeamFlow.BeamFlow m r) =>
  LedgerEntryInput ->
  m (Either FinanceError LedgerEntry)
createEntry input = do
  now <- getCurrentTime
  entryId <- generateGUID
  let entry =
        LedgerEntry
          { id = Id entryId,
            fromAccountId = input.fromAccountId,
            toAccountId = input.toAccountId,
            amount = input.amount,
            currency = input.currency,
            entryType = input.entryType,
            status = input.status,
            referenceType = input.referenceType,
            referenceId = input.referenceId,
            reversalOf = Nothing,
            voidReason = Nothing,
            settledAt = Nothing,
            metadata = input.metadata,
            reconciliationStatus = Nothing,
            fromStartingBalance = Nothing,
            fromEndingBalance = Nothing,
            toStartingBalance = Nothing,
            toEndingBalance = Nothing,
            timestamp = now,
            merchantId = input.merchantId,
            merchantOperatingCityId = input.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

  QLedger.create entry
  pure $ Right entry

-- | Create a ledger entry AND update account balances atomically
-- LAW 1: Credits = Debits (fromAccount debited, toAccount credited)
createEntryWithBalanceUpdate ::
  (BeamFlow.BeamFlow m r) =>
  LedgerEntryInput ->
  m (Either FinanceError LedgerEntry)
createEntryWithBalanceUpdate input = do
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
          isAssetAccount acc = acc.accountType == Account.Asset
          fromEndBal =
            if isAssetAccount fromAccount
              then fromStartBal + amount
              else fromStartBal - amount
          toEndBal =
            if isAssetAccount toAccount
              then toStartBal - amount
              else toStartBal + amount
      let entry =
            LedgerEntry
              { id = Id entryId,
                fromAccountId = input.fromAccountId,
                toAccountId = input.toAccountId,
                amount = amount,
                currency = input.currency,
                entryType = input.entryType,
                status = input.status,
                referenceType = input.referenceType,
                referenceId = input.referenceId,
                reversalOf = Nothing,
                voidReason = Nothing,
                settledAt = Nothing,
                metadata = input.metadata,
                reconciliationStatus = Nothing,
                fromStartingBalance = Just fromStartBal,
                fromEndingBalance = Just fromEndBal,
                toStartingBalance = Just toStartBal,
                toEndingBalance = Just toEndBal,
                timestamp = now,
                merchantId = input.merchantId,
                merchantOperatingCityId = input.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
      QLedger.create entry
      _ <- QAccount.updateBalance fromEndBal input.fromAccountId
      _ <- QAccount.updateBalance toEndBal input.toAccountId
      pure $ Right entry

-- | Create a reversal entry for an existing entry
-- LAW 2: History is immutable - we create a new entry, not modify the old one
createReversal ::
  (BeamFlow.BeamFlow m r) =>
  Id LedgerEntry -> -- Entry to reverse
  Text -> -- Reason for reversal
  m (Either FinanceError LedgerEntry)
createReversal originalId reason = do
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
                amount = original.amount,
                currency = original.currency,
                entryType = Reversal,
                status = SETTLED, -- Reversals are immediately settled
                referenceType = original.referenceType,
                referenceId = original.referenceId,
                reversalOf = Just originalId,
                voidReason = Nothing,
                settledAt = Just now,
                metadata = Just $ Aeson.String reason,
                reconciliationStatus = original.reconciliationStatus,
                fromStartingBalance = Nothing,
                fromEndingBalance = Nothing,
                toStartingBalance = Nothing,
                toEndingBalance = Nothing,
                timestamp = now,
                merchantId = original.merchantId,
                merchantOperatingCityId = original.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }

      QLedger.create reversal

      -- Update account balances (reverse the original transaction)
      mbFrom <- QAccount.findById original.fromAccountId
      mbTo <- QAccount.findById original.toAccountId
      forM_ mbFrom $ \a -> QAccount.updateBalance (a.balance + original.amount) original.fromAccountId
      forM_ mbTo $ \a -> QAccount.updateBalance (a.balance - original.amount) original.toAccountId

      pure $ Right reversal

--------------------------------------------------------------------------------
-- STATUS MANAGEMENT (These are generic, domain just calls them)
--------------------------------------------------------------------------------

-- | Update entry status (generic operation)
updateEntryStatus ::
  (BeamFlow.BeamFlow m r) =>
  Id LedgerEntry ->
  EntryStatus ->
  m ()
updateEntryStatus entryId newStatus = do
  QLedger.updateStatus newStatus entryId

-- | Settle an entry (mark as SETTLED with timestamp)
settleEntry ::
  (BeamFlow.BeamFlow m r) =>
  Id LedgerEntry ->
  m ()
settleEntry entryId = do
  now <- getCurrentTime
  QLedger.updateSettled SETTLED (Just now) entryId

-- | Void an entry (mark as VOIDED with reason)
voidEntry ::
  (BeamFlow.BeamFlow m r) =>
  Id LedgerEntry ->
  Text -> -- Reason for voiding
  m ()
voidEntry entryId reason = do
  QLedger.updateVoided VOIDED (Just reason) entryId

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

-- | Get all entries for an account
getEntriesByAccount ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  m [LedgerEntry]
getEntriesByAccount accountId = do
  fromEntries <- QLedger.findByFromAccount accountId
  toEntries <- QLedger.findByToAccount accountId
  pure $ fromEntries <> toEntries

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
