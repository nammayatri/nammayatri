module SharedLogic.Allocator.Jobs.Settlement.SubscriptionTotals
  ( SubscriptionTotals (..),
    PGSettlementTotals (..),
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

fetchSubscriptionTotals ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m SubscriptionTotals
fetchSubscriptionTotals merchantOpCityId fromTime toTime = do
  (mbGross, mbCgst, mbSgst, mbIgst, mbNet, count) <- QSP.findSubscriptionTotalsByDateRange merchantOpCityId fromTime toTime
  pure
    SubscriptionTotals
      { grossAmount = fromMaybe 0 mbGross,
        cgst = fromMaybe 0 mbCgst,
        sgst = fromMaybe 0 mbSgst,
        igst = fromMaybe 0 mbIgst,
        netAmount = fromMaybe 0 mbNet,
        txnCount = count
      }

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

fetchPGSettlementTotals ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m PGSettlementTotals
fetchPGSettlementTotals merchantId merchantOperatingCityId fromTime toTime = do
  (mbOrder, mbRefund, mbChargeback, mbOrderCnt, mbRefundCnt, mbChargebackCnt) <- findPGSettlementTotalsByDateRange merchantId merchantOperatingCityId fromTime toTime
  pure
    PGSettlementTotals
      { totalOrderAmount = fromMaybe 0 mbOrder,
        totalRefundAmount = fromMaybe 0 mbRefund,
        totalChargebackAmount = fromMaybe 0 mbChargeback,
        orderCount = fromMaybe 0 mbOrderCnt,
        refundCount = fromMaybe 0 mbRefundCnt,
        chargebackCount = fromMaybe 0 mbChargebackCnt
      }

findPGSettlementTotalsByDateRange ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m (Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe Int, Maybe Int, Maybe Int)
findPGSettlementTotalsByDateRange merchantId merchantOperatingCityId startTime endTime = do
  dbConf <- getReplicaBeamConfig
  res <-
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
  case res of
    Right [row] -> pure row
    Right _ -> pure (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
    Left err -> do
      L.logError ("findPGSettlementTotalsByDateRange" :: Text) $ "failed for merchantId=" <> merchantId <> " error=" <> show err
      pure (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
