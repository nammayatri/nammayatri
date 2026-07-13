module SharedLogic.Allocator.Jobs.Settlement.SubscriptionTotals
  ( SubscriptionTotals (..),
    SubscriptionTransactionRow (..),
    PGSettlementTotals (..),
    PGSettlementTransactionRow (..),
    fetchSubscriptionTotals,
    fetchPGSettlementTotals,
  )
where

import qualified Database.Beam as B
import qualified Database.Beam.Query ()
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport as PgDom
import qualified Storage.Beam.Common as BeamCommon
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
  ((mbGross, mbCgst, mbSgst, mbIgst, mbNet, count), rows) <- QSP.findSubscriptionTotalsByDateRange merchantOpCityId fromTime toTime
  let totals =
        SubscriptionTotals
          { grossAmount = fromMaybe 0 mbGross,
            cgst = fromMaybe 0 mbCgst,
            sgst = fromMaybe 0 mbSgst,
            igst = fromMaybe 0 mbIgst,
            netAmount = fromMaybe 0 mbNet,
            txnCount = count
          }
      txnRows = map (\(spId, debit, credit, st) -> SubscriptionTransactionRow spId debit credit (show st)) rows
  pure (totals, txnRows)

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
    txnType :: Text,
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
  ((mbOrder, mbRefund, mbChargeback, mbOrderCnt, mbRefundCnt, mbChargebackCnt), rawRows) <- findPGSettlementTotalsByDateRange merchantId merchantOperatingCityId fromTime toTime
  let totals =
        PGSettlementTotals
          { totalOrderAmount = fromMaybe 0 mbOrder,
            totalRefundAmount = fromMaybe 0 mbRefund,
            totalChargebackAmount = fromMaybe 0 mbChargeback,
            orderCount = fromMaybe 0 mbOrderCnt,
            refundCount = fromMaybe 0 mbRefundCnt,
            chargebackCount = fromMaybe 0 mbChargebackCnt
          }
      txnRows = map toTransactionRow rawRows
      orderRows = filter (\r -> r.txnType == "ORDER") txnRows
      refundRows = filter (\r -> r.txnType == "REFUND") txnRows
      chargebackRows = filter (\r -> r.txnType == "CHARGEBACK") txnRows
  pure (totals, orderRows, refundRows, chargebackRows)
  where
    toTransactionRow (txnAmt, refAmt, cbAmt, tt, ts, spId) =
      let amt = case tt of
            PgDom.ORDER -> txnAmt
            PgDom.REFUND -> fromMaybe 0 refAmt
            PgDom.CHARGEBACK -> fromMaybe 0 cbAmt
       in PGSettlementTransactionRow amt (show tt) (show ts) spId

findPGSettlementTotalsByDateRange ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m ((Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe Int, Maybe Int, Maybe Int), [(HighPrecMoney, Maybe HighPrecMoney, Maybe HighPrecMoney, PgDom.TxnType, PgDom.TxnStatus, Maybe Text)])
findPGSettlementTotalsByDateRange merchantId merchantOperatingCityId startTime endTime = do
  dbConf <- getReplicaBeamConfig
  let filterQuery =
        B.filter_'
          ( \r ->
              r.merchantId B.==?. B.val_ merchantId
                B.&&?. r.merchantOperatingCityId B.==?. B.val_ merchantOperatingCityId.getId
                B.&&?. r.orderType B.==?. B.val_ (Just PgDom.SUBSCRIPTION)
                B.&&?. r.isValidSubscriptionPurchase B.==?. B.val_ (Just True)
                B.&&?. B.sqlBool_ (B.isJust_ r.settlementDate)
                B.&&?. B.sqlBool_ (B.fromMaybe_ (B.val_ startTime) r.settlementDate B.>=. B.val_ startTime)
                B.&&?. B.sqlBool_ (B.fromMaybe_ (B.val_ endTime) r.settlementDate B.<=. B.val_ endTime)
          )
          (B.all_ (BeamCommon.pgPaymentSettlementReport BeamCommon.atlasDB))
  aggRes <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_
            ( \r ->
                ( B.as_ @(Maybe HighPrecMoney) $ B.sum_ (B.ifThenElse_ (r.txnType B.==. B.val_ PgDom.ORDER) r.txnAmount (B.val_ 0)),
                  B.as_ @(Maybe HighPrecMoney) $ B.sum_ (B.ifThenElse_ (r.txnType B.==. B.val_ PgDom.REFUND) (B.coalesce_ [r.refundAmount] (B.val_ 0)) (B.val_ 0)),
                  B.as_ @(Maybe HighPrecMoney) $ B.sum_ (B.ifThenElse_ (r.txnType B.==. B.val_ PgDom.CHARGEBACK) (B.coalesce_ [r.chargebackAmount] (B.val_ 0)) (B.val_ 0)),
                  B.as_ @(Maybe Int) $ B.sum_ (B.ifThenElse_ (r.txnType B.==. B.val_ PgDom.ORDER) (B.val_ (1 :: Int)) (B.val_ 0)),
                  B.as_ @(Maybe Int) $ B.sum_ (B.ifThenElse_ (r.txnType B.==. B.val_ PgDom.REFUND) (B.val_ (1 :: Int)) (B.val_ 0)),
                  B.as_ @(Maybe Int) $ B.sum_ (B.ifThenElse_ (r.txnType B.==. B.val_ PgDom.CHARGEBACK) (B.val_ (1 :: Int)) (B.val_ 0))
                )
            )
            filterQuery
  let totals = case aggRes of
        Right [row] -> row
        _ -> (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
  rowRes <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          fmap
            (\r -> (r.txnAmount, r.refundAmount, r.chargebackAmount, r.txnType, r.txnStatus, r.subscriptionPurchaseId))
            $ B.filter_'
              ( \r ->
                  r.merchantId B.==?. B.val_ merchantId
                    B.&&?. r.merchantOperatingCityId B.==?. B.val_ merchantOperatingCityId.getId
                    B.&&?. r.orderType B.==?. B.val_ (Just PgDom.SUBSCRIPTION)
                    B.&&?. r.isValidSubscriptionPurchase B.==?. B.val_ (Just True)
                    B.&&?. B.sqlBool_ (B.isJust_ r.settlementDate)
                    B.&&?. B.sqlBool_ (B.fromMaybe_ (B.val_ startTime) r.settlementDate B.>=. B.val_ startTime)
                    B.&&?. B.sqlBool_ (B.fromMaybe_ (B.val_ endTime) r.settlementDate B.<=. B.val_ endTime)
              )
              (B.all_ (BeamCommon.pgPaymentSettlementReport BeamCommon.atlasDB))
  rows <- case rowRes of
    Right rs -> pure rs
    Left err -> do
      L.logError ("findPGSettlementTotalsByDateRange:rows" :: Text) $ "failed for merchantId=" <> merchantId <> " error=" <> show err
      pure []
  case aggRes of
    Left err -> do
      L.logError ("findPGSettlementTotalsByDateRange" :: Text) $ "failed for merchantId=" <> merchantId <> " error=" <> show err
      pure (totals, [])
    _ -> pure (totals, rows)
