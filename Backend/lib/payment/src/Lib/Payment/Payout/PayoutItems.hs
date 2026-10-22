{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Payment.Payout.PayoutItems
  ( PayoutItem (..),
    getPayoutItems,
    toPayoutItem,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Kernel.Prelude
import qualified Kernel.Types.Common as Common
import Kernel.Types.Id (getId, getShortId)
import qualified Lib.Payment.Domain.Types.Common as DCommon
import Lib.Payment.Domain.Types.PayoutOrder (PayoutOrder)
import Lib.Payment.Domain.Types.PayoutRequest (PayoutRequest (..), PayoutRequestStatus (..), PayoutType)
import Lib.Payment.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Storage.Queries.PayoutOrderExtra as QPayoutOrderExtra
import qualified Lib.Payment.Storage.Queries.PayoutRequestExtra as QPR

-- | A payout history item, from a PayoutRequest and its PayoutOrder if any.
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
    bankAccountLast4 :: Maybe Text,
    payoutType :: Maybe PayoutType,
    transactionId :: Maybe Text,
    bankReference :: Maybe Text,
    bankReferenceType :: Maybe Text,
    statusUpdatedAt :: UTCTime
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
  [PayoutRequestStatus] -> -- statuses to leave out (empty = none)
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  m [PayoutItem]
getPayoutItems beneficiaryId mbFrom mbTo statuses excludedStatuses limit offset =
  QPR.findByBeneficiaryWithFilters beneficiaryId mbFrom mbTo statuses excludedStatuses limit offset >>= toPayoutItems

-- | Map requests to items, loading their orders in one query.
toPayoutItems :: BeamFlow m r => [PayoutRequest] -> m [PayoutItem]
toPayoutItems requests = do
  orders <- QPayoutOrderExtra.findAllByIds (mapMaybe (.payoutTransactionId) requests)
  let orderById = Map.fromList [(getId o.id, o) | o <- orders]
      orderOf pr = pr.payoutTransactionId >>= (`Map.lookup` orderById)
  pure [toPayoutItem pr (orderOf pr) | pr <- requests]

-- | How the money actually reached the beneficiary. Cash is settled at a booth
--   and never has a destination on the request; a VPA means the Juspay UPI flow;
--   anything else (Stripe or HDFC bulk) pays out to a bank account.
payoutMethodOf :: PayoutRequest -> Text
payoutMethodOf pr
  | pr.status `elem` [CASH_PAID, CASH_PENDING] = "CASH"
  | isJust pr.customerVpa = "UPI"
  | otherwise = "BANK"

toPayoutItem :: PayoutRequest -> Maybe PayoutOrder -> PayoutItem
toPayoutItem pr mbOrder =
  PayoutItem
    { amount = pr.amount,
      payoutFee = fromMaybe 0 pr.payoutFee,
      entityName = pr.entityName,
      status = pr.status,
      timestamp = pr.createdAt,
      payoutMethod = payoutMethodOf pr,
      payoutVpa = pr.customerVpa,
      bankName = pr.bankName,
      bankAccountLast4 = pr.bankAccountLast4,
      payoutType = pr.payoutType,
      transactionId = orderTransactionId <$> mbOrder,
      bankReference = mbReference,
      bankReferenceType = mbReferenceType,
      -- The request's last update; for a final payout, when it finished.
      statusUpdatedAt = pr.updatedAt
    }
  where
    -- Short id, else the order id.
    orderTransactionId :: PayoutOrder -> Text
    orderTransactionId order = maybe order.orderId getShortId order.shortId
    -- An empty reference means none.
    mbReference = mfilter (not . T.null) (mbOrder >>= (.settlementRef))
    -- Only set with a reference.
    mbReferenceType = if isJust mbReference then show <$> (mbOrder >>= (.settlementRefType)) else Nothing
