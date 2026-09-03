module SharedLogic.Allocator.Jobs.Settlement.SubscriptionTotals
  ( SubscriptionTotals (..),
    SubscriptionTransactionRow (..),
    PGSettlementTotals (..),
    PGSettlementTransactionRow (..),
    fetchSubscriptionTotals,
    fetchPGSettlementTotals,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport as PgDom
import qualified Lib.Finance.Storage.Queries.PgPaymentSettlementReport as QPgReport
import qualified Storage.Queries.SubscriptionPurchase as QSP

-- ---------------------------------------------------------------------------
-- Subscription purchase totals (aggregated)
-- ---------------------------------------------------------------------------

data SubscriptionTotals = SubscriptionTotals
  { grossAmount :: HighPrecMoney,
    cgst :: HighPrecMoney,
    sgst :: HighPrecMoney,
    igst :: HighPrecMoney,
    netAmount :: HighPrecMoney,
    txnCount :: Int
  }
  deriving (Generic)

data SubscriptionTransactionRow = SubscriptionTransactionRow
  { subscriptionId :: Text,
    debitAmount :: HighPrecMoney,
    creditAmount :: HighPrecMoney,
    status :: Text
  }
  deriving (Generic)

fetchSubscriptionTotals ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m (SubscriptionTotals, [SubscriptionTransactionRow])
fetchSubscriptionTotals merchantOpCityId fromTime toTime = do
  rows <- QSP.findSubscriptionTotalsByDateRange merchantOpCityId fromTime toTime
  let (totals, txnRowsRev) = foldl' go (SubscriptionTotals 0 0 0 0 0 0, []) rows
  pure (totals, reverse txnRowsRev)
  where
    go (acc, rs) (spId, planFee, cgstAmt, sgstAmt, igstAmt, taxableVal, st) =
      ( acc{grossAmount = acc.grossAmount + planFee,
            cgst = acc.cgst + cgstAmt,
            sgst = acc.sgst + sgstAmt,
            igst = acc.igst + igstAmt,
            netAmount = acc.netAmount + taxableVal,
            txnCount = acc.txnCount + 1
           },
        SubscriptionTransactionRow {subscriptionId = spId, debitAmount = planFee, creditAmount = cgstAmt + sgstAmt + igstAmt + taxableVal, status = show st} : rs
      )

-- ---------------------------------------------------------------------------
-- PG settlement totals (aggregated by txnType)
-- ---------------------------------------------------------------------------

data PGSettlementTotals = PGSettlementTotals
  { totalOrderAmount :: HighPrecMoney,
    totalRefundAmount :: HighPrecMoney,
    totalChargebackAmount :: HighPrecMoney,
    orderCount :: Int,
    refundCount :: Int,
    chargebackCount :: Int
  }
  deriving (Generic)

data PGSettlementTransactionRow = PGSettlementTransactionRow
  { amount :: HighPrecMoney,
    txnType :: PgDom.TxnType,
    txnStatus :: Text,
    subscriptionPurchaseId :: Maybe Text
  }
  deriving (Generic)

fetchPGSettlementTotals ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m (PGSettlementTotals, [PGSettlementTransactionRow], [PGSettlementTransactionRow], [PGSettlementTransactionRow])
fetchPGSettlementTotals merchantId merchantOperatingCityId fromTime toTime = do
  reports <- QPgReport.findPGSettlementTotalsByDateRange merchantId merchantOperatingCityId.getId fromTime toTime
  let (totals, ordersRev, refundsRev, chargebacksRev) = foldl' go (PGSettlementTotals 0 0 0 0 0 0, [], [], []) reports
  pure (totals, reverse ordersRev, reverse refundsRev, reverse chargebacksRev)
  where
    go (acc, orders, refunds, chargebacks) r =
      let row = toTransactionRow r
       in case r.txnType of
            PgDom.ORDER ->
              (acc {totalOrderAmount = acc.totalOrderAmount + row.amount, orderCount = acc.orderCount + 1}, row : orders, refunds, chargebacks)
            PgDom.REFUND ->
              (acc {totalRefundAmount = acc.totalRefundAmount + row.amount, refundCount = acc.refundCount + 1}, orders, row : refunds, chargebacks)
            PgDom.CHARGEBACK ->
              (acc {totalChargebackAmount = acc.totalChargebackAmount + row.amount, chargebackCount = acc.chargebackCount + 1}, orders, refunds, row : chargebacks)
            _ ->
              (acc {totalOrderAmount = acc.totalOrderAmount + row.amount, orderCount = acc.orderCount + 1}, row : orders, refunds, chargebacks) -- TODO: Handle other txnTypes REFUND & CHARGEBACK REVERSAL if needed
    toTransactionRow r =
      let amt = case r.txnType of
            PgDom.ORDER -> r.txnAmount
            PgDom.REFUND -> fromMaybe 0 r.refundAmount
            PgDom.CHARGEBACK -> fromMaybe 0 r.chargebackAmount
            _ -> r.txnAmount
       in PGSettlementTransactionRow {amount = amt, txnType = r.txnType, txnStatus = show r.txnStatus, subscriptionPurchaseId = r.subscriptionPurchaseId}
