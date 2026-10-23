{-
  SharedLogic.Finance.Rails

  The driver app's TransferRail records: how the finance-kernel transfer orchestration
  talks to the actual payout rails. The PayoutRequest / PayoutOrder rows stay — they
  are the rail layer — but the hold/submit ordering, compensation and canonical status
  now live in the kernel, and 'classifyPayoutOrderStatus' is THE one place Juspay's
  payout status vocabulary is interpreted for wallet money.
-}
module SharedLogic.Finance.Rails
  ( juspayPayoutRailKey,
    stripePayoutRailKey,
    hdfcBulkRailKey,
    juspayPgRailKey,
    railKeyForFlow,
    classifyPayoutOrderStatus,
    classifyPaymentTransactionStatus,
    mkWalletPayoutRail,
    settlementRailByKey,
    juspayPgInboundRail,
  )
where

import qualified Data.Text as T
import Kernel.External.Encryption (EncFlow)
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Payout.Interface as IPayout
import qualified Kernel.External.Payout.Juspay.Types.Payout as JPayout
import Kernel.Prelude
import Kernel.Utils.Common (Currency, HighPrecMoney)
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Finance.Transfer.Rail as Rail
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow

juspayPayoutRailKey, stripePayoutRailKey, hdfcBulkRailKey, juspayPgRailKey :: Text
juspayPayoutRailKey = "JUSPAY_PAYOUT"
stripePayoutRailKey = "STRIPE_PAYOUT"
hdfcBulkRailKey = "HDFC_BULK"
juspayPgRailKey = "JUSPAY_PG"

railKeyForFlow :: IPayout.PayoutServiceFlow -> Text
railKeyForFlow = \case
  IPayout.JuspayFlow -> juspayPayoutRailKey
  IPayout.StripeFlow -> stripePayoutRailKey
  IPayout.BulkFlow -> hdfcBulkRailKey

-- | THE Juspay payout status mapping. Total: an unparseable status is nobody's guess,
--   so it goes to review instead of being treated as pending forever.
classifyPayoutOrderStatus :: Text -> Rail.TransferOutcome
classifyPayoutOrderStatus raw = case readMaybe @JPayout.PayoutOrderStatus (T.unpack raw) of
  Nothing -> Rail.OutcomeNeedsReview ("unrecognised payout status: " <> raw)
  Just status -> case status of
    JPayout.SUCCESS -> Rail.OutcomeSucceeded
    JPayout.FULFILLMENTS_SUCCESSFUL -> Rail.OutcomeSucceeded
    JPayout.ERROR -> Rail.OutcomeFailed raw
    JPayout.FAILURE -> Rail.OutcomeFailed raw
    JPayout.FULFILLMENTS_FAILURE -> Rail.OutcomeFailed raw
    JPayout.CANCELLED -> Rail.OutcomeFailed raw
    JPayout.FULFILLMENTS_CANCELLED -> Rail.OutcomeFailed raw
    JPayout.DISCARDED -> Rail.OutcomeFailed raw
    JPayout.INVALID -> Rail.OutcomeFailed raw
    JPayout.REVERSED -> Rail.OutcomeReversed raw
    JPayout.MANUAL_REVIEW -> Rail.OutcomeNeedsReview raw
    JPayout.FULFILLMENTS_MANUAL_REVIEW -> Rail.OutcomeNeedsReview raw
    JPayout.CONFLICTED -> Rail.OutcomeNeedsReview raw
    -- INITIATED, PROCESSING, DEBITED, AWAITING_APPROVAL, READY_FOR_FULFILLMENT,
    -- FULFILLMENTS_SCHEDULED, FULFILLED_PARTIALLY, GATEWAY_SWITCHED, VALID, ...
    _ -> Rail.OutcomePending

-- | Payment (collection) status mapping for inbound transfers (wallet topup).
classifyPaymentTransactionStatus :: Payment.TransactionStatus -> Rail.TransferOutcome
classifyPaymentTransactionStatus = \case
  Payment.CHARGED -> Rail.OutcomeSucceeded
  Payment.AUTHENTICATION_FAILED -> Rail.OutcomeFailed "AUTHENTICATION_FAILED"
  Payment.AUTHORIZATION_FAILED -> Rail.OutcomeFailed "AUTHORIZATION_FAILED"
  Payment.JUSPAY_DECLINED -> Rail.OutcomeFailed "JUSPAY_DECLINED"
  Payment.CANCELLED -> Rail.OutcomeFailed "CANCELLED"
  Payment.AUTO_REFUNDED -> Rail.OutcomeFailed "AUTO_REFUNDED"
  Payment.CLIENT_AUTH_TOKEN_EXPIRED -> Rail.OutcomeFailed "CLIENT_AUTH_TOKEN_EXPIRED"
  Payment.PARTIAL_CHARGED -> Rail.OutcomeNeedsReview "PARTIAL_CHARGED"
  _ -> Rail.OutcomePending

-- | The wallet payout rail for one specific PayoutRequest: the submit closure executes
--   that request through lib/payment exactly as before (PayoutOrder row, PR status
--   history), so dashboards keep working — those statuses are now derived, not
--   authoritative. The kernel decides ordering from 'holdOrdering'.
mkWalletPayoutRail ::
  ( EncFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    FinanceBeamFlow.BeamFlow m r,
    Finance.HasActorInfo m r
  ) =>
  IPayout.PayoutServiceFlow ->
  PayoutRequest.PayoutRequest ->
  Maybe HighPrecMoney -> -- explicit transferAmount override
  Currency ->
  (DPayment.CreatePayoutServiceReq -> m IPayout.CreatePayoutOrderResp) ->
  (DPayoutOrder.PayoutOrder -> m ()) -> -- afterPayoutOrderCreated
  (Text -> m ()) -> -- on confirmed rejection (fail the bulk order, etc.)
  Rail.TransferRail m
mkWalletPayoutRail payoutServiceFlow payoutRequest mbTransferAmount currency payoutCall afterOrderCreated onConfirmedRejection =
  Rail.TransferRail
    { railKey = railKeyForFlow payoutServiceFlow,
      holdOrdering = case payoutServiceFlow of
        IPayout.BulkFlow -> Rail.HoldThenSubmit
        _ -> Rail.SubmitThenHold,
      submit = \_submission -> do
        outcome <- PayoutRequest.executePayoutRequestWithOutcome mbTransferAmount currency payoutServiceFlow payoutRequest payoutCall afterOrderCreated
        case outcome of
          PayoutRequest.Executed order ->
            pure Rail.RailAccepted {railOrderId = order.orderId, railStatus = show order.status}
          PayoutRequest.ConfirmedFailure msg -> do
            onConfirmedRejection msg
            pure (Rail.RailRejected msg)
          PayoutRequest.AmbiguousFailure msg ->
            pure (Rail.RailAmbiguous msg)
          -- Request already in flight (or terminal): nothing new was sent; the money must
          -- stay parked and the batch/status check resolves it.
          PayoutRequest.NotExecutable status ->
            pure (Rail.RailDeferred ("REQUEST_" <> show status)),
      -- Webhooks and the existing status-check jobs push fresh raw statuses through
      -- applyRailStatus; nothing in the driver app pulls via the kernel yet.
      pollStatus = \_ -> pure Nothing,
      classifyStatus = classifyPayoutOrderStatus,
      collectPayload = Nothing
    }

-- | Settlement-time rail: webhooks only need classifyStatus (and the key must round-trip
--   from the transfer row). Its submit must never be reached; if it is, the transfer
--   parks in NEEDS_REVIEW rather than guessing.
settlementRailByKey :: (Applicative m) => Text -> Rail.TransferRail m
settlementRailByKey key =
  Rail.TransferRail
    { railKey = key,
      holdOrdering = Rail.SubmitThenHold,
      submit = \_ -> pure (Rail.RailAmbiguous "settlement-time rail cannot submit"),
      pollStatus = \_ -> pure Nothing,
      classifyStatus = classifyPayoutOrderStatus,
      collectPayload = Nothing
    }

-- | Inbound topup rail: the PaymentOrder (and its SDK payload) is created by the
--   existing SharedLogic.Payment.createWalletTopupOrder out of band, so this rail only
--   names the flow; outcomes arrive typed through the payment webhook.
juspayPgInboundRail :: (Applicative m) => Rail.TransferRail m
juspayPgInboundRail =
  Rail.TransferRail
    { railKey = juspayPgRailKey,
      holdOrdering = Rail.SubmitThenHold,
      submit = \_ -> pure (Rail.RailDeferred "ORDER_CREATED_OUT_OF_BAND"),
      pollStatus = \_ -> pure Nothing,
      classifyStatus = \_ -> Rail.OutcomePending,
      collectPayload = Nothing
    }
