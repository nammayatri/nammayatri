{-
  Lib.Finance.Transfer.Outcome

  The single idempotent handler for an OUTBOUND transfer's outcome. Webhooks, status
  polls, bulk batch checks, and dashboards all funnel here; every ledger leg is looked
  up before it is posted (via settlePayoutLedger), so the same outcome can be applied
  any number of times, and a success arriving after a failure re-opens the hold and
  settles — exactly the semantics the driver wallet and rider cashback rely on today.

  Inbound outcomes live in Lib.Finance.Transfer.Inbound (they need the CreditRecipe).

  Design: docs/backend/design/finance-kernel-unification-plan.md §5.3.
-}
module Lib.Finance.Transfer.Outcome
  ( applyTransferOutcome,
    applyRailStatus,
    checkTransferStatus,
    bumpNonLedgerStatus,
    guardCtxMatchesTransfer,
  )
where

import Data.List (nub)
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Balance (withAccountBalanceLock)
import Lib.Finance.Domain.Types.LedgerEntry (EntryType (Reversal), LedgerEntry)
import qualified Lib.Finance.Domain.Types.Transfer as DT
import Lib.Finance.Error.Types
import Lib.Finance.FinanceM (FinanceCtx (..))
import Lib.Finance.Ledger.PayoutSettlement (PayoutLedgerRefs (..), PayoutOutcome (..), RunFinance, findActivePayoutHold, settlePayoutLedger)
import Lib.Finance.Ledger.Service (createReversal, getEntriesByReference, markEntriesAsPaidOut, markEntriesAsUnsettled)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.Transfer as QT
import qualified Lib.Finance.Storage.Queries.TransferExtra as QTE
import Lib.Finance.Transfer.Rail (TransferOutcome (..), TransferRail (..))

-- | ctx.referenceId is what every ledger leg is keyed on; a mismatch would settle the
--   wrong payout's legs. Fail loudly instead.
guardCtxMatchesTransfer :: FinanceCtx -> DT.Transfer -> Either FinanceError ()
guardCtxMatchesTransfer ctx t
  | ctx.referenceId == t.referenceId = Right ()
  | otherwise = Left $ TransferError CTX_REFERENCE_MISMATCH ("ctx " <> ctx.referenceId <> " vs transfer " <> t.referenceId)

applyTransferOutcome ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  RunFinance m ->
  PayoutLedgerRefs ->
  FinanceCtx ->
  Id DT.Transfer ->
  Maybe Text -> -- raw rail status to stamp alongside, verbatim
  TransferOutcome ->
  m (Either FinanceError DT.Transfer)
applyTransferOutcome runFinance refs ctx transferId mbRaw outcome =
  QT.findById transferId >>= \case
    Nothing -> pure $ Left $ TransferError TRANSFER_NOT_FOUND transferId.getId
    Just t -> case guardCtxMatchesTransfer ctx t of
      Left err -> pure (Left err)
      Right () -> case t.direction of
        DT.INTERNAL -> pure (Right t) -- terminal at creation; nothing to apply
        DT.INBOUND -> case outcome of
          OutcomeSucceeded -> pure $ Left $ TransferError INBOUND_REQUIRES_RECIPE "use Lib.Finance.Transfer.Inbound.applyInboundTransferOutcome"
          _ -> bumpNonLedgerStatus t mbRaw outcome
        DT.OUTBOUND -> withAccountBalanceLock t.accountId 10 $ case outcome of
          OutcomeSucceeded -> settleOutboundSuccess runFinance refs ctx t mbRaw
          OutcomeFailed reason -> settleOutboundFailure runFinance refs ctx t mbRaw reason
          OutcomeReversed reason -> settleOutboundReversed refs ctx t mbRaw reason
          OutcomePending -> bumpNonLedgerStatus t mbRaw outcome
          OutcomeNeedsReview _ -> bumpNonLedgerStatus t mbRaw outcome

settleOutboundSuccess ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  RunFinance m ->
  PayoutLedgerRefs ->
  FinanceCtx ->
  DT.Transfer ->
  Maybe Text ->
  m (Either FinanceError DT.Transfer)
settleOutboundSuccess runFinance refs ctx t mbRaw
  | t.status == DT.SUCCEEDED = pure (Right t)
  | otherwise = do
    res <- settlePayoutLedger runFinance refs ctx t.amount Nothing PayoutSucceeded
    case res of
      Left err -> pure (Left err)
      Right legIds -> do
        -- settlementId = the DOMAIN process id (transfer.referenceId, e.g. the
        -- PayoutRequest id) — dashboards and the stash-fallback lookup resolve it
        -- against domain tables, so the kernel row id would dangle.
        markEntriesAsPaidOut (coveredIds t) t.referenceId
        QTE.updateLedgerLegsById Nothing (mergeLegIds t legIds) t.id
        QTE.updateStatusById DT.SUCCEEDED mbRaw Nothing t.id
        QTE.clearLastErrorById t.id
        refetch t.id

settleOutboundFailure ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  RunFinance m ->
  PayoutLedgerRefs ->
  FinanceCtx ->
  DT.Transfer ->
  Maybe Text ->
  Text ->
  m (Either FinanceError DT.Transfer)
settleOutboundFailure runFinance refs ctx t mbRaw reason
  -- a failure arriving after settlement is a contradiction, not a failure to record. The money
  -- was paid, so nothing is compensated; SUCCEEDED is terminal for the ordinary status bumps, so
  -- park the row for ops explicitly (a plain bump would silently do nothing here)
  | t.status == DT.SUCCEEDED = do
    QTE.updateStatusById DT.NEEDS_REVIEW mbRaw (Just ("failure reported after success: " <> reason)) t.id
    refetch t.id
  | t.status `elem` [DT.FAILED, DT.REVERSED] = pure (Right t)
  | otherwise = do
    settlementLegs <- getEntriesByReference refs.settlementReferenceType ctx.referenceId
    -- settlement legs mean the payout was paid and the row was parked afterwards: a repeated
    -- failure must not reverse money that already left
    if not (null settlementLegs)
      then pure (Right t)
      else do
        res <- settlePayoutLedger runFinance refs ctx t.amount Nothing (PayoutFailed reason)
        case res of
          Left err -> pure (Left err)
          Right legIds -> do
            markEntriesAsUnsettled (coveredIds t)
            QTE.updateLedgerLegsById Nothing (mergeLegIds t legIds) t.id
            QTE.updateStatusById DT.FAILED mbRaw (Just reason) t.id
            refetch t.id

-- | Rail reversed an already-settled transfer: reverse every unreversed settlement and
--   hold leg (money returns to the owner account) and release the covered entries.
settleOutboundReversed ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  PayoutLedgerRefs ->
  FinanceCtx ->
  DT.Transfer ->
  Maybe Text ->
  Text ->
  m (Either FinanceError DT.Transfer)
settleOutboundReversed refs ctx t mbRaw reason
  | t.status == DT.REVERSED = pure (Right t)
  | otherwise = do
    settlementLegs <- getEntriesByReference refs.settlementReferenceType ctx.referenceId
    -- inline (not a where-bound helper): a signature-less local binding would be
    -- generalised and lose the enclosing HasActorInfo m r constraint
    let reversedIds = mapMaybe (.reversalOf) settlementLegs
        toReverse = [e | e <- settlementLegs, e.entryType /= Reversal, e.id `notElem` reversedIds]
    reversalResults <- mapM (\e -> createReversal e.id reason) toReverse
    let reversalRes = map (.id) <$> sequenceEither reversalResults
    case reversalRes of
      Left err -> pure (Left err)
      Right settlementReversals -> do
        mbHold <- findActivePayoutHold refs.holdReferenceType ctx.referenceId
        holdRes <- case mbHold of
          Nothing -> pure (Right [])
          Just hold -> fmap (\rev -> [rev.id]) <$> createReversal hold.id reason
        case holdRes of
          Left err -> pure (Left err)
          Right holdReversals -> do
            markEntriesAsUnsettled (coveredIds t)
            QTE.updateLedgerLegsById Nothing (mergeLegIds t (settlementReversals <> holdReversals)) t.id
            QTE.updateStatusById DT.REVERSED mbRaw (Just reason) t.id
            refetch t.id

-- | Status-only moves: pending bumps and review flags. Never downgrades a terminal
--   status and never moves NEEDS_REVIEW back to IN_FLIGHT on a mere pending poll.
bumpNonLedgerStatus ::
  (BeamFlow.BeamFlow m r) =>
  DT.Transfer ->
  Maybe Text ->
  TransferOutcome ->
  m (Either FinanceError DT.Transfer)
bumpNonLedgerStatus t mbRaw outcome = do
  let terminal = t.status `elem` [DT.SUCCEEDED, DT.FAILED, DT.REVERSED]
  case outcome of
    OutcomePending
      | not terminal && t.status /= DT.NEEDS_REVIEW ->
        QTE.updateStatusById DT.IN_FLIGHT mbRaw Nothing t.id
    OutcomeNeedsReview reason
      | not terminal ->
        QTE.updateStatusById DT.NEEDS_REVIEW mbRaw (Just reason) t.id
    OutcomeFailed reason
      | not terminal ->
        -- INBOUND failure: nothing was posted, status is the whole story
        QTE.updateStatusById DT.FAILED mbRaw (Just reason) t.id
    _ -> pure ()
  refetch t.id

-- | Stamp a raw rail status and apply what it means. The webhook/poll entry point:
--   domain decodes the payload, looks the rail up in its registry, and calls this.
applyRailStatus ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  RunFinance m ->
  TransferRail m ->
  PayoutLedgerRefs ->
  FinanceCtx ->
  Id DT.Transfer ->
  Text ->
  m (Either FinanceError DT.Transfer)
applyRailStatus runFinance rail refs ctx transferId raw =
  applyTransferOutcome runFinance refs ctx transferId (Just raw) (rail.classifyStatus raw)

-- | Poll the rail and apply the fresh status. No-op (Right t) when the rail can't
--   poll this transfer (e.g. bulk items awaiting the batch check).
checkTransferStatus ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  RunFinance m ->
  TransferRail m ->
  PayoutLedgerRefs ->
  FinanceCtx ->
  Id DT.Transfer ->
  m (Either FinanceError DT.Transfer)
checkTransferStatus runFinance rail refs ctx transferId =
  QT.findById transferId >>= \case
    Nothing -> pure $ Left $ TransferError TRANSFER_NOT_FOUND transferId.getId
    Just t ->
      rail.pollStatus t >>= \case
        Nothing -> pure (Right t)
        Just raw -> applyRailStatus runFinance rail refs ctx transferId raw

-- helpers ---------------------------------------------------------------------

coveredIds :: DT.Transfer -> [Id LedgerEntry]
coveredIds t = map Id (fromMaybe [] t.coveredEntryIds)

mergeLegIds :: DT.Transfer -> [Id LedgerEntry] -> [Text]
mergeLegIds t newIds = nub (fromMaybe [] t.entryIds <> map (.getId) newIds)

refetch :: (BeamFlow.BeamFlow m r) => Id DT.Transfer -> m (Either FinanceError DT.Transfer)
refetch transferId =
  maybe (Left $ TransferError TRANSFER_NOT_FOUND transferId.getId) Right <$> QT.findById transferId

sequenceEither :: [Either e a] -> Either e [a]
sequenceEither = sequence
