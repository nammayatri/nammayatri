{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Payment.Payout.PayoutItems
  ( PayoutItem (..),
    getPayoutItems,
  )
where

import Kernel.Prelude
import qualified Kernel.Types.Common as Common
import qualified Lib.Payment.Domain.Types.Common as DCommon
import Lib.Payment.Domain.Types.PayoutRequest (PayoutRequest (..), PayoutRequestStatus (..))
import Lib.Payment.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Storage.Queries.PayoutRequestExtra as QPR

-- | A single payout history item, mapped from PayoutRequest.
--   Designed for reuse across domains.
data PayoutItem = PayoutItem
  { amount :: Maybe Common.HighPrecMoney,
    payoutFee :: Common.HighPrecMoney,
    entityName :: Maybe DCommon.EntityName,
    status :: PayoutRequestStatus,
    timestamp :: UTCTime,
    payoutMethod :: Text,
    payoutVpa :: Maybe Text,
    bankName :: Maybe Text,
    bankAccountLast4 :: Maybe Text
  }
  deriving (Generic, Show)

-- | Fetch payout items for a beneficiary with filters.
--   Sorted descending by timestamp (most recent first).
getPayoutItems ::
  BeamFlow m r =>
  Text -> -- beneficiaryId (person ID)
  Maybe UTCTime -> -- from
  Maybe UTCTime -> -- to
  [PayoutRequestStatus] -> -- status filter (empty = all)
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  m [PayoutItem]
getPayoutItems beneficiaryId mbFrom mbTo statuses limit offset = do
  payoutRequests <- QPR.findByBeneficiaryWithFilters beneficiaryId mbFrom mbTo statuses limit offset
  pure $ map toPayoutItem payoutRequests

-- | How the money actually reached the beneficiary. Cash is settled at a booth
--   and never has a destination on the request; a VPA means the Juspay UPI flow;
--   anything else is the Stripe flow, which pays out to a bank account.
payoutMethodOf :: PayoutRequest -> Text
payoutMethodOf pr
  | pr.status `elem` [CASH_PAID, CASH_PENDING] = "CASH"
  | isJust pr.customerVpa = "UPI"
  | otherwise = "BANK"

toPayoutItem :: PayoutRequest -> PayoutItem
toPayoutItem pr =
  PayoutItem
    { amount = pr.amount,
      payoutFee = fromMaybe 0 pr.payoutFee,
      entityName = pr.entityName,
      status = pr.status,
      timestamp = pr.createdAt,
      payoutMethod = payoutMethodOf pr,
      payoutVpa = pr.customerVpa,
      bankName = pr.bankName,
      bankAccountLast4 = pr.bankAccountLast4
    }
