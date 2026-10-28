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
    rideRefundTransferRefType,
    refundStatusToTransferStatus,
    refundRequestStatusToTransferStatus,
    mintRideRefundTransfer,
    updateRideRefundTransferStatus,
    refundPollPacing,
  )
where

import qualified Domain.Types.PersonStats as DPS
import qualified Domain.Types.RefundRequest as DRefundRequest
import qualified Kernel.External.Payment.Interface.Types as PaymentI
import qualified Kernel.External.Payout.Interface as IPayout
import qualified Kernel.External.Payout.Juspay.Types.Payout as JPayout
import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import qualified Lib.Finance.AccountConfig as FinanceConfig
import qualified Lib.Finance.Balance as FinanceBalance
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Domain.Types.Account (AccountType (Liability), CounterpartyType (RIDER))
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Domain.Types.Transfer as DT
import Lib.Finance.FinanceM (runFinance)
import qualified Lib.Finance.FinanceM as FinM
import Lib.Finance.Ledger.PayoutSettlement (PayoutLedgerRefs (..), findActivePayoutHold)
import qualified Lib.Finance.Ledger.Service as LedgerService
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Finance.Storage.Queries.Transfer as QTransfer
import qualified Lib.Finance.Storage.Queries.TransferExtra as QTransferExtra
import qualified Lib.Finance.Transfer.Backfill as Backfill
import Lib.Finance.Transfer.Outbound (OutboundAccrual (..), OutboundSpec (..), initiateTransferOutWithAccrual)
import Lib.Finance.Transfer.Outcome (applyRailStatus)
import Lib.Finance.Transfer.Rail
import qualified Lib.Finance.Transfer.RailStatus as RailStatus
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.PayoutOrder as PayoutOrder
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance
import Storage.Beam.Payment ()

riderJuspayPayoutRailKey :: DT.RailKey
riderJuspayPayoutRailKey = DT.JUSPAY_PAYOUT

-- | The ONE rider-side mapping from Juspay's raw payout-status vocabulary to the
--   kernel's canonical outcomes. Unparseable text goes to review, never to a guess.
-- | Delegates to the kernel's canonical mapping (Lib.Finance.Transfer.RailStatus) —
--   one interpreter for Juspay's payout vocabulary across both apps.
classifyJuspayPayoutStatus :: Text -> TransferOutcome
classifyJuspayPayoutStatus = RailStatus.classifyJuspayPayoutStatus

-- | Status-only rail used at webhook / settlement time: classify + poll (the persisted
--   PayoutOrder row; the PayoutStatusCheck job keeps refreshing it from the gateway).
--   Its submit is never invoked on this path.
riderJuspayStatusRail :: (PaymentBeamFlow.BeamFlow m r) => TransferRail m
riderJuspayStatusRail =
  TransferRail
    { railKey = riderJuspayPayoutRailKey,
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
  (Text -> m ()) -> -- escalation hook when the transfer PARKED (NEEDS_REVIEW): money may be in motion, so no retry — surface to ops/product instead
  m ()
runAwardPayoutTransfer merchantId merchantOpCityId currency riderId orderId accrualReferenceId amount accrualRefType transferRefType refs submitAction onNotSubmitted onParked = do
  let ctx = RidePaymentFinance.buildRiderFinanceCtx merchantId merchantOpCityId currency True riderId orderId Nothing Nothing Nothing
      accrualCtx = RidePaymentFinance.buildRiderFinanceCtx merchantId merchantOpCityId currency True riderId accrualReferenceId Nothing Nothing Nothing
  -- The wallet account is resolved (created on a rider's first award) before the transfer so
  -- the kernel can lock it; the award credit itself is posted by the kernel inside that lock.
  runFinance ctx (FinM.account FinM.OwnerLiability) >>= \case
    Left err -> do
      logError $ "award payout " <> orderId <> ": no wallet account: " <> show err
      onNotSubmitted ("NO_WALLET_ACCOUNT: " <> show err)
    Right (account, _) -> do
      let -- Idempotent: a retry of the same award (same anchor) reuses the existing leg instead
          -- of granting the amount twice. A credit minted by this call is taken back by the
          -- kernel if the transfer is rejected or fails, so it is never left spendable
          -- (booking deposits read the same wallet); a reused one stays with the retry chain.
          accrual =
            OutboundAccrual
              { postAccrual = do
                  entries <- LedgerService.getEntriesByReference accrualRefType accrualReferenceId
                  -- a credit the kernel already took back (rejected earlier attempt) does not count
                  let reversedIds = mapMaybe (.reversalOf) entries
                      liveAccrual = [e | e <- entries, isNothing e.reversalOf, e.id `notElem` reversedIds]
                  if null liveAccrual
                    then fmap (\ids -> (ids, True)) <$> RidePaymentFinance.createSettledAwardAccrualLeg accrualCtx amount accrualRefType
                    else pure (Right (map (.id) liveAccrual, False))
              }
      res <-
        initiateTransferOutWithAccrual
          runFinance
          (awardPayoutRail submitAction)
          (Just accrual)
          OutboundSpec
            { ctx = ctx,
              account = account,
              amount = FinanceBalance.Exact amount,
              referenceType = transferRefType,
              referenceId = orderId,
              ledgerRefs = refs,
              instant = True,
              legacyOfferHoldKeys = [],
              alwaysRedeemableRefs = [],
              -- the kernel covers the credit it posts
              coveredEntryIds = Nothing,
              holdLedgerMetadata = Nothing,
              metadata = DT.emptyTransferMetadata {DT.coveredRefs = Just [DT.CoveredRef {refType = accrualRefType, amount = amount}]}
            }
      case res of
        Left err -> do
          -- Nothing was sent and no hold exists; the kernel already took back a credit it minted.
          logError $ "award payout " <> orderId <> ": kernel transfer rejected: " <> show err
          onNotSubmitted ("KERNEL_REJECTED: " <> show err)
        Right t -> do
          logInfo $ "award payout " <> orderId <> " transfer " <> t.id.getId <> " status " <> show t.status
          when (t.status == DT.FAILED) $
            -- rail confirmed nothing was sent; hold already reversed by the kernel, and so was the credit
            onNotSubmitted ("RAIL_REJECTED: " <> fromMaybe "-" t.lastError)
          when (t.status == DT.NEEDS_REVIEW) $
            -- parked: the submit outcome is unknown, money may be with the partner.
            -- Never retried (that's how double-pays happen) — escalate instead.
            onParked ("PARKED_FOR_REVIEW: " <> fromMaybe "-" t.lastError)

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
            if isNothing mbHold && null coveredIds
              then do
                -- Pre-migration UNLEDGERED payout (awards had no ledger on main: no hold,
                -- no covered accruals). Settling via the kernel here would hold-repair a
                -- hold that never existed and debit a wallet that was never credited —
                -- driving it negative. Keep main's behaviour: presentation updates only.
                logInfo $ "payout settle " <> refId <> ": pre-migration unledgered payout (no hold, no covered entries); skipping kernel ledger settlement"
                pure Nothing
              else
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

--------------------------------------------------------------------------------
-- Ride-refund transfer tracking
--------------------------------------------------------------------------------

-- | Transfer-row reference type for ride refunds. NOT a ledger refType: the refund's
--   ledger legs keep their own vocabulary (RideFareRefund, ...) on the existing
--   createRefundRaisedLedger / Succeeded / void path; this row is only the canonical
--   money-process status for the gateway attempt.
rideRefundTransferRefType :: Text
rideRefundTransferRefType = "RideRefund"

riderJuspayRefundRailKey :: DT.RailKey
riderJuspayRefundRailKey = DT.JUSPAY_REFUND

-- | THE gateway refund-status mapping (canonical, total).
refundStatusToTransferStatus :: PaymentI.RefundStatus -> DT.TransferStatus
refundStatusToTransferStatus = \case
  PaymentI.REFUND_SUCCESS -> DT.SUCCEEDED
  PaymentI.REFUND_FAILURE -> DT.FAILED
  PaymentI.REFUND_CANCELED -> DT.FAILED
  PaymentI.MANUAL_REVIEW -> DT.NEEDS_REVIEW
  PaymentI.REFUND_REQUIRES_ACTION -> DT.NEEDS_REVIEW
  PaymentI.REFUND_PENDING -> DT.IN_FLIGHT

-- | RefundRequest-status view of the same lifecycle, for the funnel that only sees the
--   cast status (processRefundResult). Nothing = not a money transition (OPEN/REJECTED).
refundRequestStatusToTransferStatus :: DRefundRequest.RefundRequestStatus -> Maybe DT.TransferStatus
refundRequestStatusToTransferStatus = \case
  DRefundRequest.REFUNDED -> Just DT.SUCCEEDED
  DRefundRequest.FAILED -> Just DT.FAILED
  DRefundRequest.APPROVED -> Just DT.IN_FLIGHT
  DRefundRequest.OPEN -> Nothing
  DRefundRequest.REJECTED -> Nothing

-- | Mint the money-process row for one gateway refund attempt, idempotent on the
--   Refunds row id (each retry attempt = one Refunds row = one transfer). A rider with
--   no wallet account was never ledgered — skip with a log, never fabricate an account.
mintRideRefundTransfer ::
  (FinanceBeamFlow.BeamFlow m r) =>
  Text -> -- rider id
  Text -> -- merchant id
  Text -> -- merchant operating city id
  Currency ->
  HighPrecMoney ->
  Text -> -- Refunds row id (gateway requestId)
  m ()
mintRideRefundTransfer riderId merchantId merchantOpCityId currency amount refundsId =
  RidePaymentFinance.getWalletAccountByOwner RIDER riderId >>= \case
    Nothing ->
      logInfo $ "ride refund " <> refundsId <> ": no wallet account for rider " <> riderId <> "; transfer tracking skipped"
    Just account ->
      void $
        Backfill.backfillTransfer
          Backfill.BackfillSpec
            { direction = DT.OUTBOUND,
              railKey = riderJuspayRefundRailKey,
              railOrderId = Just refundsId,
              accountId = account.id,
              amount = amount,
              feeAmount = 0,
              currency = currency,
              referenceType = rideRefundTransferRefType,
              referenceId = refundsId,
              knownEntryIds = [],
              coveredEntryIds = [],
              merchantId = merchantId,
              merchantOperatingCityId = merchantOpCityId
            }

-- | Status landing for a refund attempt. Terminal rows are never downgraded by a late
--   pending poll; a missing row (pre-migration refund, account-less rider) is a skip.
updateRideRefundTransferStatus ::
  (FinanceBeamFlow.BeamFlow m r) =>
  Text -> -- Refunds row id
  Maybe Text -> -- raw gateway status, verbatim
  DT.TransferStatus ->
  m ()
updateRideRefundTransferStatus refundsId mbRaw newStatus =
  QTransfer.findByReference rideRefundTransferRefType refundsId >>= \case
    Nothing -> pure ()
    Just t -> do
      let terminal = t.status `elem` [DT.SUCCEEDED, DT.FAILED, DT.REVERSED]
      unless (terminal && newStatus == DT.IN_FLIGHT) $
        QTransferExtra.updateStatusById newStatus mbRaw Nothing t.id

-- | Refund status-poll pacing for a city: the kernel (Liability, RIDER) class config's
--   statusCheckIntervalSec / statusCheckMaxAttempts, with the historical hard defaults
--   (12h base interval, 5 attempts) when the class carries none. This REPLACES
--   RiderConfig.refundStatusUpdateInterval/Retries — the class config is the only
--   configurable source now (seeded per city by rider migration 0006).
refundPollPacing ::
  (FinanceBeamFlow.BeamFlow m r) =>
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  m (NominalDiffTime, Int)
refundPollPacing merchantId merchantOperatingCityId = do
  cfg <-
    FinanceConfig.getAccountConfig
      FinanceConfig.AccountClass
        { merchantId = merchantId,
          merchantOperatingCityId = merchantOperatingCityId,
          accountType = Liability,
          counterpartyType = Just RIDER,
          subLedger = Nothing
        }
  pure
    ( maybe defaultRefundPollInterval fromIntegral cfg.statusCheckIntervalSec,
      fromMaybe defaultRefundPollAttempts cfg.statusCheckMaxAttempts
    )
  where
    -- the defaults RiderConfig's fromTType historically applied (43200s, 5 retries)
    defaultRefundPollInterval :: NominalDiffTime
    defaultRefundPollInterval = 43200
    defaultRefundPollAttempts :: Int
    defaultRefundPollAttempts = 5
