{-
  Lib.Finance.Transfer.RailStatus

  Canonical rail-status -> TransferOutcome mappings for the rails the platform
  actually runs on. finance-kernel may depend on Kernel.External.* (mobility-core),
  so these live HERE, once — the apps' Rails modules re-export them instead of each
  maintaining their own copy of the same 20-constructor case.

  Rule unchanged from Transfer.Rail: raw statuses ride along verbatim in
  Transfer.railStatus; these functions are the only interpreters.
-}
module Lib.Finance.Transfer.RailStatus
  ( classifyJuspayPayoutStatus,
    classifyJuspayTransactionStatus,
  )
where

import qualified Data.Text as T
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Payout.Juspay.Types.Payout as JPayout
import Kernel.Prelude
import Lib.Finance.Transfer.Rail (TransferOutcome (..))

-- | Juspay payout-order statuses (24 constructors) -> canonical outcome. Total:
--   unparseable text is NEEDS_REVIEW, never a guess.
classifyJuspayPayoutStatus :: Text -> TransferOutcome
classifyJuspayPayoutStatus raw = case readMaybe @JPayout.PayoutOrderStatus (T.unpack raw) of
  Nothing -> OutcomeNeedsReview ("unrecognised payout status: " <> raw)
  Just status -> case status of
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
    -- INITIATED, PROCESSING, DEBITED, AWAITING_APPROVAL, READY_FOR_FULFILLMENT,
    -- FULFILLMENTS_SCHEDULED, FULFILLED_PARTIALLY, GATEWAY_SWITCHED, VALID, ...
    _ -> OutcomePending

-- | Juspay payment (collection) statuses -> canonical outcome, for INBOUND transfers.
--   PARTIAL_CHARGED is deliberately NEEDS_REVIEW: crediting a partial amount as if
--   full is how wallets go wrong silently.
classifyJuspayTransactionStatus :: Payment.TransactionStatus -> TransferOutcome
classifyJuspayTransactionStatus = \case
  Payment.CHARGED -> OutcomeSucceeded
  Payment.AUTHENTICATION_FAILED -> OutcomeFailed "AUTHENTICATION_FAILED"
  Payment.AUTHORIZATION_FAILED -> OutcomeFailed "AUTHORIZATION_FAILED"
  Payment.JUSPAY_DECLINED -> OutcomeFailed "JUSPAY_DECLINED"
  Payment.CANCELLED -> OutcomeFailed "CANCELLED"
  Payment.AUTO_REFUNDED -> OutcomeFailed "AUTO_REFUNDED"
  Payment.CLIENT_AUTH_TOKEN_EXPIRED -> OutcomeFailed "CLIENT_AUTH_TOKEN_EXPIRED"
  Payment.PARTIAL_CHARGED -> OutcomeNeedsReview "PARTIAL_CHARGED"
  _ -> OutcomePending
