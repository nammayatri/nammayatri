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

    -- * Query by owner (the main way domain queries)
    findByOwner,
    findByOwnerAndStatus,

    -- * Aggregations (common for domain use)
    sumByOwnerAndStatus,
    countByOwnerAndStatus,

    -- * Input types (re-export from Interface)
    module Lib.Finance.Ledger.Interface,
  )
where

import qualified Data.Aeson as Aeson
import Kernel.Prelude
import Kernel.Types.Common ()
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Lib.Finance.Core.Types (TimeRange (..))
import Lib.Finance.Domain.Types.Account (Account)
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
  let entryNum = truncate (utcTimeToPOSIXSeconds now * 1000) -- Unique timestamp-based entry number
  let entry =
        LedgerEntry
          { id = Id entryId,
            fromAccountId = input.fromAccountId,
            toAccountId = input.toAccountId,
            amount = input.amount,
            currency = input.currency,
            entryType = input.entryType,
            entryNumber = entryNum,
            status = input.status, -- NEW: Use input status
            ownerType = input.ownerType, -- NEW
            ownerId = input.ownerId, -- NEW
            referenceType = input.referenceType,
            referenceId = input.referenceId,
            reversalOf = Nothing,
            voidReason = Nothing,
            settledAt = Nothing,
            metadata = input.metadata,
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
      entryResult <- createEntry input

      case entryResult of
        Left err -> pure $ Left err
        Right entry -> do
          let amount = input.amount
          _ <- QAccount.updateBalance (fromAccount.balance - amount) input.fromAccountId
          _ <- QAccount.updateBalance (toAccount.balance + amount) input.toAccountId
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
      let entryNum = truncate (utcTimeToPOSIXSeconds now * 1000)

      let reversal =
            LedgerEntry
              { id = Id reversalId,
                fromAccountId = original.toAccountId, -- Swap
                toAccountId = original.fromAccountId,
                amount = original.amount,
                currency = original.currency,
                entryType = Reversal,
                entryNumber = entryNum,
                status = SETTLED, -- Reversals are immediately settled
                ownerType = original.ownerType,
                ownerId = original.ownerId,
                referenceType = original.referenceType,
                referenceId = original.referenceId,
                reversalOf = Just originalId,
                voidReason = Nothing,
                settledAt = Just now,
                metadata = Just $ Aeson.String reason,
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
-- QUERY BY OWNER (Main way domain code queries)
--------------------------------------------------------------------------------

-- | Find all entries for an owner
findByOwner ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- Owner type (e.g., "DRIVER")
  Text -> -- Owner ID
  m [LedgerEntry]
findByOwner = QLedger.findByOwner

-- | Find entries for an owner with specific status
-- This is THE main query for domain code
findByOwnerAndStatus ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- Owner type
  Text -> -- Owner ID
  EntryStatus -> -- Status to filter
  m [LedgerEntry]
findByOwnerAndStatus = QLedger.findByOwnerAndStatus

--------------------------------------------------------------------------------
-- AGGREGATIONS (Common operations domain needs)
--------------------------------------------------------------------------------

-- | Sum amounts for an owner by status
-- Example: sumByOwnerAndStatus "DRIVER" driverId PENDING
sumByOwnerAndStatus ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- Owner type
  Text -> -- Owner ID
  EntryStatus ->
  m HighPrecMoney
sumByOwnerAndStatus ownerType ownerId status = do
  entries <- findByOwnerAndStatus ownerType ownerId status
  pure $ sum $ map (.amount) entries

-- | Count entries for an owner by status
countByOwnerAndStatus ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- Owner type
  Text -> -- Owner ID
  EntryStatus ->
  m Int
countByOwnerAndStatus ownerType ownerId status = do
  entries <- findByOwnerAndStatus ownerType ownerId status
  pure $ length entries
