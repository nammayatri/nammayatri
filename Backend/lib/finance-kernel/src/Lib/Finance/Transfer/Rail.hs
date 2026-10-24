{-
  Lib.Finance.Transfer.Rail

  The injection contract between the kernel's transfer orchestration and the outside
  money rails (Juspay payout, Stripe, HDFC bulk, payment gateways). finance-kernel can
  never import lib/payment (the dependency arrow points the other way), so a rail is a
  record of functions the DOMAIN builds — typically closing over lib/payment calls and
  per-call context (person, VPA, payout config) — and hands to the kernel verbs.

  classifyStatus is THE one place a rail's raw status vocabulary is interpreted;
  everywhere else the raw status rides along verbatim in Transfer.railStatus.

  Design: docs/backend/design/finance-kernel-unification-plan.md §5.2.
-}
module Lib.Finance.Transfer.Rail
  ( TransferRail (..),
    RailRegistry,
    lookupRail,
    HoldOrdering (..),
    RailSubmission (..),
    RailSubmitResult (..),
    RailCollectRequest (..),
    RailCollectResult (..),
    TransferOutcome (..),
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Kernel.Types.Id (Id)
import Lib.Finance.Domain.Types.Account (Account, CounterpartyType)
import Lib.Finance.Domain.Types.Transfer (Transfer, TransferMetadata)

-- | When the ledger hold is posted relative to the rail call.
--     HoldThenSubmit — HDFC bulk style: money is parked first, the file goes later;
--       a rejected submit reverses the hold (full compensation).
--     SubmitThenHold — Juspay/Stripe style: the partner is called first; a hold
--       failure AFTER an accepted submit flips the transfer to NEEDS_REVIEW
--       (replacing today's logError-and-forget, which left the money double-spendable).
data HoldOrdering = HoldThenSubmit | SubmitThenHold
  deriving (Eq, Show, Generic)

-- | What the kernel tells a rail when submitting. Deliberately minimal: per-call
--   domain context (VPA, phone, remarks) lives in the submit closure the domain built.
data RailSubmission = RailSubmission
  { transferId :: Id Transfer,
    -- | what the counterparty should receive
    netAmount :: HighPrecMoney,
    grossAmount :: HighPrecMoney,
    fee :: HighPrecMoney,
    currency :: Currency,
    accountId :: Id Account,
    counterpartyType :: CounterpartyType,
    counterpartyId :: Text,
    referenceType :: Text,
    referenceId :: Text,
    metadata :: TransferMetadata,
    merchantId :: Text,
    merchantOperatingCityId :: Text
  }
  deriving (Generic)

-- | The rail's answer to a submit. Rails must do their own confirmed-vs-ambiguous
--   classification (they know which errors are hard rejections); the kernel treats an
--   exception escaping 'submit' as RailAmbiguous — money is never un-held on a guess.
data RailSubmitResult
  = -- | partner acknowledged: order id + its raw status
    RailAccepted {railOrderId :: Text, railStatus :: Text}
  | -- | confirmed rejection: nothing was sent, compensate now
    RailRejected {reason :: Text}
  | -- | unknown outcome (timeout, 5xx): hold is kept, transfer goes to NEEDS_REVIEW
    RailAmbiguous {reason :: Text}
  | -- | accepted locally, sent later (bulk file); batch status check resolves it
    RailDeferred {railStatus :: Text}
  deriving (Eq, Show, Generic)

-- | What the kernel asks an INBOUND rail for: a collectable payment order. When
--   'reuseRailOrderId' is set, the rail must RE-FETCH that order's payload (same open
--   intent, same amount) instead of creating a new one — rails must be idempotent here.
data RailCollectRequest = RailCollectRequest
  { submission :: RailSubmission,
    reuseRailOrderId :: Maybe Text
  }
  deriving (Generic)

-- | The rail's order id (becomes transfer.referenceId + railOrderId, and is what the
--   webhook later correlates on) plus the opaque payload the client app needs to
--   collect the money (SDK payload).
data RailCollectResult = RailCollectResult
  { railOrderId :: Text,
    payload :: Aeson.Value
  }
  deriving (Generic)

-- | A rail-neutral outcome, the only vocabulary Outcome.hs speaks.
data TransferOutcome
  = OutcomeSucceeded
  | OutcomeFailed Text
  | -- | still in flight; bumps railStatus only
    OutcomePending
  | OutcomeNeedsReview Text
  | -- | succeeded earlier, rail reversed it (e.g. Juspay REVERSED)
    OutcomeReversed Text
  deriving (Eq, Show, Generic)

data TransferRail m = TransferRail
  { -- | registry key, stored on the transfer row ("JUSPAY_PAYOUT", "HDFC_BULK", ...)
    railKey :: Text,
    holdOrdering :: HoldOrdering,
    submit :: RailSubmission -> m RailSubmitResult,
    -- | fresh raw rail status for a transfer; Nothing = this rail can't poll
    --   per-transfer (e.g. bulk items resolve via the batch check)
    pollStatus :: Transfer -> m (Maybe Text),
    -- | THE rail→canonical mapping; replaces the per-caller cast tables
    classifyStatus :: Text -> TransferOutcome,
    -- | INBOUND only: create (or, on reuse, re-fetch) the payment order and return its
    --   id + client payload. The kernel drives the whole recharge process through this
    --   closure — domains never create payment orders themselves.
    collectPayload :: Maybe (RailCollectRequest -> m RailCollectResult)
  }

-- | App-side dispatch table, same pattern as the reconciliation RecipeRegistry.
--   Used at webhook/poll time, where only pollStatus/classifyStatus matter.
type RailRegistry m = Map.Map Text (TransferRail m)

lookupRail :: RailRegistry m -> Text -> Maybe (TransferRail m)
lookupRail = flip Map.lookup
