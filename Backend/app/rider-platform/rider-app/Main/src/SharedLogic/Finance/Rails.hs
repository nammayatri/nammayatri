{-
  SharedLogic.Finance.Rails

  The rider app's TransferRail records for the finance-kernel transfer verbs.
  lib/payment stays the rail layer (PayoutRequest / PayoutOrder rows are still
  created); these records adapt it to the kernel's RailSubmitResult / TransferOutcome
  vocabulary. classifyJuspayPayoutStatus is THE one rider-side mapping from Juspay's
  raw payout statuses — the per-caller cast tables derive from kernel status now.
-}
module SharedLogic.Finance.Rails
  ( riderJuspayPayoutRailKey,
    classifyJuspayPayoutStatus,
    riderJuspayStatusRail,
    cashbackSubmitRail,
    awardPayoutRail,
    runAwardPayoutTransfer,
    settleRiderPayoutViaKernel,
    transferStatusToPayoutStatus,
    payoutOrderFallbackStatus,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.PersonStats as DPS
import qualified Kernel.External.Payout.Interface as IPayout
import qualified Kernel.External.Payout.Juspay.Types.Payout as JPayout
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import qualified Lib.Finance.Balance as FinanceBalance
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Domain.Types.Account (CounterpartyType (RIDER))
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Domain.Types.Transfer as DT
import Lib.Finance.FinanceM (runFinance)
import Lib.Finance.Ledger.PayoutSettlement (PayoutLedgerRefs (..), findActivePayoutHold)
import qualified Lib.Finance.Ledger.Service as LedgerService
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Finance.Storage.Queries.Transfer as QTransfer
import qualified Lib.Finance.Transfer.Backfill as Backfill
import Lib.Finance.Transfer.Outbound (OutboundSpec (..), initiateTransferOut)
import Lib.Finance.Transfer.Outcome (applyRailStatus)
import Lib.Finance.Transfer.Rail
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.PayoutOrder as PayoutOrder
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance
import Storage.Beam.Payment ()

riderJuspayPayoutRailKey :: Text
riderJuspayPayoutRailKey = "JUSPAY_PAYOUT"

-- | The ONE rider-side mapping from Juspay's raw payout-status vocabulary to the
--   kernel's canonical outcomes. Unparseable text goes to review, never to a guess.
classifyJuspayPayoutStatus :: Text -> TransferOutcome
classifyJuspayPayoutStatus raw = case readMaybe @JPayout.PayoutOrderStatus (T.unpack raw) of
  Nothing -> OutcomeNeedsReview ("unrecognised payout status: " <> raw)
  Just st -> case st of
    JPayout.SUCCESS -> OutcomeSucceeded
    JPayout.FULFILLMENTS_SUCCESSFUL -> OutcomeSucceeded
    JPayout.ERROR -> OutcomeFailed raw
    JPayout.FAILURE -> OutcomeFailed raw
    JPayout.FULFILLMENTS_FAILURE -> OutcomeFailed raw
    JPayout.CANCELLED -> OutcomeFailed raw
    JPayout.FULFILLMENTS_CANCELLED -> OutcomeFailed raw
    JPayout.DISCARDED -> OutcomeFailed raw
    JPayout.INVALID -> OutcomeFailed raw
    JPayout.REVERSED -> OutcomeReversed raw
    JPayout.MANUAL_REVIEW -> OutcomeNeedsReview raw
    JPayout.FULFILLMENTS_MANUAL_REVIEW -> OutcomeNeedsReview raw
    JPayout.CONFLICTED -> OutcomeNeedsReview raw
    _ -> OutcomePending

-- | Status-only rail used at webhook / settlement time: classify + poll (the persisted
--   PayoutOrder row; the PayoutStatusCheck job keeps refreshing it from the gateway).
--   Its submit is never invoked on this path.
riderJuspayStatusRail :: (PaymentBeamFlow.BeamFlow m r) => TransferRail m
riderJuspayStatusRail =
  TransferRail
    { railKey = riderJuspayPayoutRailKey,
      holdOrdering = SubmitThenHold,
      submit = \_ -> pure (RailAmbiguous "status-only rail: submit not supported"),
      pollStatus = \t -> case t.railOrderId of
        Nothing -> pure Nothing
        Just oid -> fmap (show . (.status)) <$> QPayoutOrder.findByOrderId oid,
      classifyStatus = classifyJuspayPayoutStatus,
      collectPayload = Nothing
    }

-- | Submit rail for the cashback payout: executes a pre-created PayoutRequest so its id
--   anchors every ledger leg (the kernel transfer's referenceId is the request id).
cashbackSubmitRail ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r, Finance.HasActorInfo m r, EncFlow m r) =>
  PayoutRequest.PayoutSubmission ->
  PayoutRequest.PayoutRequest ->
  (DPayment.CreatePayoutServiceReq -> m IPayout.CreatePayoutOrderResp) ->
  (PayoutOrder.PayoutOrder -> m ()) ->
  TransferRail m
cashbackSubmitRail submission payoutRequest payoutCall afterPayoutOrderCreated =
  TransferRail
    { railKey = riderJuspayPayoutRailKey,
      holdOrdering = SubmitThenHold,
      submit = \_ -> do
        outcome <- PayoutRequest.executePayoutRequestWithOutcome submission.transferAmount submission.currency submission.payoutServiceFlow payoutRequest payoutCall afterPayoutOrderCreated
        pure $ case outcome of
          PayoutRequest.Executed po -> RailAccepted {railOrderId = po.orderId, railStatus = show po.status}
          PayoutRequest.ConfirmedFailure msg -> RailRejected msg
          PayoutRequest.AmbiguousFailure msg -> RailAmbiguous msg
          -- a prior run already handed it to the rail; the status check resolves it
          PayoutRequest.NotExecutable status -> RailDeferred (show status),
      pollStatus = \t -> case t.railOrderId of
        Nothing -> pure Nothing
        Just oid -> fmap (show . (.status)) <$> QPayoutOrder.findByOrderId oid,
      classifyStatus = classifyJuspayPayoutStatus,
      collectPayload = Nothing
    }

-- | Submit rail for referral / metro awards: the closure performs today's
--   createPayoutService call; the kernel owns the hold and the outcome.
awardPayoutRail :: (PaymentBeamFlow.BeamFlow m r) => m RailSubmitResult -> TransferRail m
awardPayoutRail submitAction =
  TransferRail
    { railKey = riderJuspayPayoutRailKey,
      holdOrdering = SubmitThenHold,
      submit = const submitAction,
      pollStatus = \t -> case t.railOrderId of
        Nothing -> pure Nothing
        Just oid -> fmap (show . (.status)) <$> QPayoutOrder.findByOrderId oid,
      classifyStatus = classifyJuspayPayoutStatus,
      collectPayload = Nothing
    }

-- | The shared award-payout shape (referral ride award, backlog award, metro cashback):
--   accrue the award into the rider's wallet (BuyerExpense → OwnerLiability, settled),
--   then run an OUTBOUND kernel transfer whose submit performs the existing
--   createPayoutService call. Money moves ONLY through the kernel: when the path cannot
--   start or the rail confirms a rejection, 'onNotSubmitted' lets the caller do its
--   retry bookkeeping — it must never move money itself. NEEDS_REVIEW keeps the hold
--   and waits for ops/poll, deliberately without a retry hook (retrying an unknown
--   outcome is how double-pays happen).
runAwardPayoutTransfer ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  Currency ->
  Text -> -- riderId
  Text -> -- payout order id (uid); the transfer's referenceId
  Text -> -- accrual anchor: stable across retries of the same award (booking id / uid)
  HighPrecMoney ->
  Text -> -- accrual reference type
  Text -> -- transfer (hold) reference type
  PayoutLedgerRefs ->
  m RailSubmitResult -> -- adapted createPayoutService closure
  (Text -> m ()) -> -- retry/bookkeeping hook when nothing was submitted (reason)
  m ()
runAwardPayoutTransfer merchantId merchantOpCityId currency riderId orderId accrualReferenceId amount accrualRefType transferRefType refs submitAction onNotSubmitted = do
  let ctx = RidePaymentFinance.buildRiderFinanceCtx merchantId merchantOpCityId currency True riderId orderId Nothing Nothing Nothing
      accrualCtx = RidePaymentFinance.buildRiderFinanceCtx merchantId merchantOpCityId currency True riderId accrualReferenceId Nothing Nothing Nothing
  -- Idempotent accrual: a retry of the same award (same anchor) reuses the existing
  -- leg instead of granting the amount twice.
  existingAccrual <- filter (\e -> isNothing e.reversalOf) <$> LedgerService.getEntriesByReference accrualRefType accrualReferenceId
  accrualRes <-
    if null existingAccrual
      then RidePaymentFinance.createSettledAwardAccrualLeg accrualCtx amount accrualRefType
      else pure (Right (map (.id) existingAccrual))
  case accrualRes of
    Left err -> do
      logError $ "award payout " <> orderId <> ": accrual leg failed: " <> show err
      onNotSubmitted ("ACCRUAL_FAILED: " <> show err)
    Right accrualIds ->
      RidePaymentFinance.getWalletAccountByOwner RIDER riderId >>= \case
        Nothing -> do
          logError $ "award payout " <> orderId <> ": no wallet account after accrual"
          onNotSubmitted "NO_WALLET_ACCOUNT"
        Just account -> do
          res <-
            initiateTransferOut
              runFinance
              (awardPayoutRail submitAction)
              OutboundSpec
                { ctx = ctx,
                  account = account,
                  amount = FinanceBalance.Exact amount,
                  referenceType = transferRefType,
                  referenceId = orderId,
                  ledgerRefs = refs,
                  instant = True,
                  legacyOfferHoldKeys = [],
                  coveredEntryIds = Just accrualIds,
                  holdLedgerMetadata = Nothing,
                  metadata = DT.emptyTransferMetadata {DT.coveredRefs = Just [DT.CoveredRef {refType = accrualRefType, amount = amount}]}
                }
          case res of
            Left err -> do
              logError $ "award payout " <> orderId <> ": kernel transfer rejected: " <> show err
              onNotSubmitted ("KERNEL_REJECTED: " <> show err)
            Right t -> do
              logInfo $ "award payout " <> orderId <> " transfer " <> t.id.getId <> " status " <> show t.status
              when (t.status == DT.FAILED) $
                -- rail confirmed nothing was sent; hold already reversed by the kernel
                onNotSubmitted ("RAIL_REJECTED: " <> fromMaybe "-" t.lastError)

-- | Settle a payout outcome through the kernel transfer. Pre-migration payouts have no
--   transfer row; one is minted from the domain's process record (backfill-on-read) so
--   the single kernel handler covers them too. Backfill is impossible only when the
--   rider has no wallet account at all — then nothing was ever ledgered for this payout
--   and there is nothing to settle.
settleRiderPayoutViaKernel ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  Text -> -- transfer reference type
  Text -> -- transfer reference id
  PayoutLedgerRefs ->
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  Currency ->
  Text -> -- riderId
  Maybe Text -> -- rail order id, stamped on a backfilled row
  HighPrecMoney -> -- payout amount, used for a backfilled row
  [Id LE.LedgerEntry] -> -- covered accrual entries, used for a backfilled row
  Text -> -- raw Juspay payout status
  m ()
settleRiderPayoutViaKernel refType refId refs merchantId merchantOpCityId currency riderId mbRailOrderId amount coveredIds rawStatus = do
  mbTransfer <-
    QTransfer.findByReference refType refId >>= \case
      Just t -> pure (Just t)
      Nothing ->
        RidePaymentFinance.getWalletAccountByOwner RIDER riderId >>= \case
          Nothing -> do
            logError $ "payout settle " <> refId <> ": no wallet account for rider " <> riderId <> "; nothing ledgered to settle"
            pure Nothing
          Just account -> do
            mbHold <- findActivePayoutHold refs.holdReferenceType refId
            Just
              <$> Backfill.backfillTransfer
                Backfill.BackfillSpec
                  { direction = DT.OUTBOUND,
                    railKey = riderJuspayPayoutRailKey,
                    railOrderId = mbRailOrderId,
                    accountId = account.id,
                    amount = amount,
                    feeAmount = 0,
                    currency = currency,
                    referenceType = refType,
                    referenceId = refId,
                    knownEntryIds = maybeToList ((.id) <$> mbHold),
                    coveredEntryIds = coveredIds,
                    merchantId = merchantId,
                    merchantOperatingCityId = merchantOpCityId
                  }
  whenJust mbTransfer $ \transfer -> do
    let ctx = RidePaymentFinance.buildRiderFinanceCtx merchantId merchantOpCityId currency True riderId refId Nothing Nothing Nothing
    applyRailStatus runFinance riderJuspayStatusRail refs ctx transfer.id rawStatus
      >>= either (\err -> logError $ "kernel settlement failed for transfer " <> transfer.id.getId <> ": " <> show err) (const (pure ()))

-- | Presentation mapping: kernel transfer status → the rider API's PayoutStatus.
transferStatusToPayoutStatus :: DT.TransferStatus -> DPS.PayoutStatus
transferStatusToPayoutStatus = \case
  DT.SUCCEEDED -> DPS.Success
  DT.FAILED -> DPS.Failed
  DT.REVERSED -> DPS.Failed
  DT.NEEDS_REVIEW -> DPS.ManualReview
  _ -> DPS.Processing -- CREATED / SUBMITTED / IN_FLIGHT

-- | Fallback for rows with no kernel transfer (read-only history): derive the API
--   status from the raw order status through the ONE classify mapping.
payoutOrderFallbackStatus :: JPayout.PayoutOrderStatus -> DPS.PayoutStatus
payoutOrderFallbackStatus st = case classifyJuspayPayoutStatus (show st) of
  OutcomeSucceeded -> DPS.Success
  OutcomeFailed _ -> DPS.Failed
  OutcomeReversed _ -> DPS.Failed
  OutcomeNeedsReview _ -> DPS.ManualReview
  OutcomePending -> DPS.Processing
