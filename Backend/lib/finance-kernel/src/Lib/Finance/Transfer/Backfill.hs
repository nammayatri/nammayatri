{-
  Lib.Finance.Transfer.Backfill

  Mint a Transfer row for a money movement that started BEFORE the kernel owned the
  flow (a pre-migration payout request or payment order still in flight). This is what
  lets domains delete their legacy settle paths outright: on webhook/poll, a missing
  row is backfilled from the domain's existing process record and the one kernel path
  handles it. Idempotent on (referenceType, referenceId) like every transfer.

  Backfilled OUTBOUND rows start IN_FLIGHT (the rail call demonstrably happened —
  that's why a webhook arrived); INBOUND rows start CREATED. Ledger legs the legacy
  initiate already posted are attached via 'knownEntryIds' so settlement claims them.
-}
module Lib.Finance.Transfer.Backfill
  ( BackfillSpec (..),
    backfillTransfer,
    backfilledPreMigrationMarker,
  )
where

import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Domain.Types.Account (Account)
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Domain.Types.Transfer as DT
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.Transfer as QT

-- | Persistent marker (metadata.reason) identifying rows minted by backfill: their
--   createdAt is the backfill moment, NOT when the money moved, so time-window logic
--   (e.g. the per-day cap) must exclude them. lastError carries the same text but is
--   cleared on later status updates — metadata survives.
backfilledPreMigrationMarker :: Text
backfilledPreMigrationMarker = "BACKFILLED_PRE_MIGRATION"

data BackfillSpec = BackfillSpec
  { direction :: DT.TransferDirection,
    railKey :: DT.RailKey,
    railOrderId :: Maybe Text,
    accountId :: Id Account,
    amount :: HighPrecMoney,
    feeAmount :: HighPrecMoney,
    currency :: Currency,
    referenceType :: Text,
    referenceId :: Text,
    -- | legs the legacy initiate already posted (e.g. the live hold), so the kernel
    --   settle path claims them instead of re-deriving
    knownEntryIds :: [Id LE.LedgerEntry],
    -- | earning entries the legacy flow reserved (stash contents), to be PAID_OUT /
    --   released by the kernel on outcome
    coveredEntryIds :: [Id LE.LedgerEntry],
    merchantId :: Text,
    merchantOperatingCityId :: Text
  }
  deriving (Generic)

-- | Get-or-create: an existing row always wins; a minted row records why it exists.
backfillTransfer ::
  (BeamFlow.BeamFlow m r) =>
  BackfillSpec ->
  m DT.Transfer
backfillTransfer spec =
  QT.findByReference spec.referenceType spec.referenceId >>= \case
    Just existing -> pure existing
    Nothing -> do
      transferId <- generateGUID
      now <- getCurrentTime
      let row =
            DT.Transfer
              { id = Id transferId,
                direction = spec.direction,
                status = case spec.direction of
                  DT.OUTBOUND -> DT.IN_FLIGHT
                  _ -> DT.CREATED,
                railKey = Just spec.railKey,
                railStatus = Nothing,
                railOrderId = spec.railOrderId,
                accountId = spec.accountId,
                counterAccountId = Nothing,
                requestedAmount = Just spec.amount,
                amount = spec.amount,
                feeAmount = spec.feeAmount,
                currency = spec.currency,
                referenceType = spec.referenceType,
                referenceId = spec.referenceId,
                holdEntryId = listToMaybe spec.knownEntryIds,
                entryIds = Just (map (.getId) spec.knownEntryIds),
                coveredEntryIds = Just (map (.getId) spec.coveredEntryIds),
                metadata = Just DT.emptyTransferMetadata {DT.reason = Just backfilledPreMigrationMarker},
                scheduledFor = Nothing,
                attemptCount = 1,
                lastError = Just backfilledPreMigrationMarker,
                merchantId = spec.merchantId,
                merchantOperatingCityId = spec.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
      QT.create row
      pure row
