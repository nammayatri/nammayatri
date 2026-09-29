{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Vocabulary shared by every stage of a bulk payout: what a candidate is, what became of one,
-- and the handful of constants and conversions the stages agree on. No queries, no partner calls.
module SharedLogic.Payout.Bulk.Types
  ( BulkCandidate (..),
    PayoutSnapshot (..),
    BulkClaimOutcome (..),
    BulkItemResult (..),
    chunksOf,
    toPayoutRail,
  )
where

import qualified Domain.Types.DriverBankAccount
import qualified Domain.Types.Person as DP
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder

-- | One beneficiary that passed the read-only eligibility pass. The sweep assembles these before
--   any batch exists, so a batch can be opened already knowing who belongs to it, and every row
--   underneath it -- order or excluded request -- can be written carrying its batchId from birth
--   rather than back-tagged afterwards (doc section 3.1).
data BulkCandidate = BulkCandidate
  { person :: DP.Person,
    -- | What this beneficiary would be paid, as of the eligibility pass. Re-read under the
    --   per-person lock before anything is claimed; this copy only sizes the batch.
    amount :: HighPrecMoney,
    -- | Why this beneficiary cannot be paid although they belong to the batch: no bank account on
    --   file, or details the bank cannot use. 'Nothing' means payable. Recorded as an EXCLUDED
    --   payout_request on the batch, with no payout_order and no ledger reservation (doc p.39), and
    --   this text is what the excluded worklist shows -- so it is the operator-facing message.
    exclusionReason :: Maybe Text
  }

-- | Everything one payout decision rests on, read in a single place so the eligibility pass and
--   the claim that follows it cannot drift apart.
data PayoutSnapshot = PayoutSnapshot
  { payoutableBalance :: HighPrecMoney,
    redeemableEntryIds :: [Text],
    merchantTransferAmount :: HighPrecMoney,
    cutoff :: UTCTime,
    mbBankAccount :: Maybe Domain.Types.DriverBankAccount.DriverBankAccount,
    hasPayoutInFlight :: Bool
  }

-- | What became of one beneficiary in a bulk payout cycle. The scheduled sweep ignores this; the
--   adhoc flow reports it back per person.
data BulkClaimOutcome
  = ClaimSubmitted DPayoutOrder.PayoutOrder
  | ClaimExcluded Text
  | ClaimDropped Text

-- | What happened to one item on this status check, for deciding the batch's own state.
data BulkItemResult = ItemOutcomeProcessed | ItemOutcomeRejected | ItemOutcomeDeferred | ItemOutcomePending | ItemOutcomePendingApproval
  deriving (Eq)

-- | Split a batch-submission slice by HDFC's per-call item cap so each chunk becomes its own
--   payout_batch/submitBulkPayout call, instead of one oversized call HDFC would just refuse.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

toPayoutRail :: DPayoutBatch.PayoutBatchRail -> Payout.PayoutRail
toPayoutRail = \case
  DPayoutBatch.NEFT -> Payout.RailNEFT
  DPayoutBatch.RTGS -> Payout.RailRTGS
  DPayoutBatch.IMPS -> Payout.RailIMPS
  DPayoutBatch.A2A -> Payout.RailA2A
