module Lib.Finance.Ledger.PayoutSettlement
  ( PayoutLedgerRefs (..),
    PayoutOutcome (..),
    RunFinance,
    findActivePayoutHold,
    postOnceByReference,
    settlePayoutLedger,
  )
where

import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common (HighPrecMoney)
import Lib.Finance.Core.Types (HasActorInfo)
import Lib.Finance.Domain.Types.LedgerEntry (EntryType (Reversal), LedgerEntry, LedgerEntryMetadata)
import Lib.Finance.Error.Types (FinanceError)
import Lib.Finance.FinanceM (AccountRole (..), FinanceCtx (..), FinanceM, transfer, transferInProcessing)
import Lib.Finance.Ledger.Service (createReversal, getEntriesByReference, markEntriesAsClaimed)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow

-- | The two reference types that identify one payout's legs; every leg carries the PayoutRequest id
--   as referenceId, which is what makes the handler re-runnable.
data PayoutLedgerRefs = PayoutLedgerRefs
  { holdReferenceType :: Text,
    settlementReferenceType :: Text,
    settlementToRole :: AccountRole
  }

data PayoutOutcome = PayoutSucceeded | PayoutFailed Text

type RunFinance m = FinanceCtx -> FinanceM m () -> m (Either FinanceError ((), [Id LedgerEntry]))

-- | The hold (OwnerLiability → OwnerPayoutLiability) that has not been reversed yet, if any.
findActivePayoutHold :: (BeamFlow.BeamFlow m r) => Text -> Text -> m (Maybe LedgerEntry)
findActivePayoutHold holdReferenceType payoutRequestId = do
  legs <- getEntriesByReference holdReferenceType payoutRequestId
  let reversedIds = mapMaybe (.reversalOf) legs
  pure $ find (\e -> e.entryType /= Reversal && e.id `notElem` reversedIds) legs

-- | Post a leg only when nothing with the same (referenceType, ctx.referenceId) exists yet.
postOnceByReference ::
  (BeamFlow.BeamFlow m r) =>
  Text ->
  FinanceCtx ->
  m (Either FinanceError [Id LedgerEntry]) ->
  m (Either FinanceError [Id LedgerEntry])
postOnceByReference referenceType ctx post = do
  existing <- getEntriesByReference referenceType ctx.referenceId
  if null existing then post else pure (Right (map (.id) existing))

-- | The single ledger handler for a payout outcome, shared by driver wallet and rider cashback payouts.
--   ctx.referenceId must be the PayoutRequest id. Success: makes sure the hold exists (posting it
--   here if initiate never did) and then the settlement leg; failure: reverses the active hold.
--   Every leg is looked up before it is posted, so the same outcome can be applied any number of
--   times and a success arriving after a reversal simply opens a fresh hold.
settlePayoutLedger ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  RunFinance m ->
  PayoutLedgerRefs ->
  FinanceCtx ->
  HighPrecMoney ->
  Maybe LedgerEntryMetadata ->
  PayoutOutcome ->
  m (Either FinanceError [Id LedgerEntry])
settlePayoutLedger runFinance refs ctx amount metadata outcome = do
  mbHold <- findActivePayoutHold refs.holdReferenceType ctx.referenceId
  settlementLegs <- getEntriesByReference refs.settlementReferenceType ctx.referenceId
  case outcome of
    PayoutSucceeded
      | not (null settlementLegs) -> pure (Right [])
      | otherwise -> do
        holdRes <- case mbHold of
          Just hold -> pure (Right [hold.id])
          Nothing -> post (transferInProcessing OwnerLiability OwnerPayoutLiability amount refs.holdReferenceType metadata)
        case holdRes of
          Left err -> pure (Left err)
          Right holdIds -> do
            settlementRes <- post (transfer OwnerPayoutLiability refs.settlementToRole (maybe amount (.amount) mbHold) refs.settlementReferenceType Nothing)
            forM settlementRes $ \settlementIds -> do
              markEntriesAsClaimed (holdIds <> settlementIds)
              pure (holdIds <> settlementIds)
    PayoutFailed reason -> case mbHold of
      Just hold | null settlementLegs -> do
        res <- createReversal hold.id reason
        forM res $ \reversal -> do
          markEntriesAsClaimed [hold.id, reversal.id]
          pure [hold.id, reversal.id]
      _ -> pure (Right [])
  where
    post action = fmap snd <$> runFinance ctx (void action)
