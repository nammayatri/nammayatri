module SharedLogic.Allocator.Jobs.Settlement.RideRevenueTotals
  ( RideRevenueTotals (..),
    RideFareRevRecTotals (..),
    BuyerAppSettlementTotals (..),
    DriverEarningAccrualTotals (..),
    PayoutTotals (..),
    TdsTotals (..),
    fetchRideRevenueTotals,
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
import qualified Lib.Finance.Domain.Types.Account as AccountDomain
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction as DTTDomain
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as ITTDomain
import qualified Lib.Finance.Domain.Types.LedgerEntry as LedgerDomain
import qualified SharedLogic.Finance.Wallet as Wallet
import qualified Storage.Beam.Common as BeamCommon

-- ---------------------------------------------------------------------------
-- Per-event aggregates (WS3 ride accounting → SAP GL matrix)
-- ---------------------------------------------------------------------------

-- | Shared fare + GST split for online and offline-cash ride revenue recognition.
--   Online ride revenue recognition: Dr BUYER_APP_RECEIVABLE / Cr RIDE_FARE_REVENUE + GST
--   Offline-cash ride: Dr DRIVER_BALANCE / Cr RIDE_FARE_REVENUE + GST
data RideFareRevRecTotals = RideFareRevRecTotals
  { grossAmount :: HighPrecMoney,
    netAmount :: HighPrecMoney,
    cgst :: HighPrecMoney,
    sgst :: HighPrecMoney,
    igst :: HighPrecMoney,
    txnCount :: Int
  }
  deriving (Generic, Show, Eq)

-- | Buyer-app settlement: Dr BANK / BUYER_APP_POOL / Cr BUYER_APP_RECEIVABLE
data BuyerAppSettlementTotals = BuyerAppSettlementTotals
  { settledAmount :: HighPrecMoney,
    txnCount :: Int
  }
  deriving (Generic, Show, Eq)

-- | Driver earning accrual: Dr RIDE_FARE_REVENUE / BUYER_APP_POOL / Cr DRIVER_BALANCE
data DriverEarningAccrualTotals = DriverEarningAccrualTotals
  { accrualAmount :: HighPrecMoney,
    txnCount :: Int
  }
  deriving (Generic, Show, Eq)

-- | Payout request → PG payout: DRIVER_BALANCE → PAYOUT_CLEARING → BANK
--   Amount from SETTLED WalletPayout ledger legs (channel-agnostic; WS4 adds
--   Stripe/bank-file/gating later without changing this aggregate shape).
data PayoutTotals = PayoutTotals
  { payoutAmount :: HighPrecMoney,
    txnCount :: Int
  }
  deriving (Generic, Show, Eq)

-- | TDS threshold / reimbursement (WS3 matrix).
--   deduction* from direct_tax_transaction (tdsTreatment=Deducted);
--   reimbursement* = 0 until WS8 FO TDS-cert reimbursement posts Reimbursed rows.
data TdsTotals = TdsTotals
  { deductionAmount :: HighPrecMoney,
    reimbursementAmount :: HighPrecMoney,
    deductionCount :: Int,
    reimbursementCount :: Int
  }
  deriving (Generic, Show, Eq)

-- ---------------------------------------------------------------------------
-- Combined daily totals for one merchant operating city
-- ---------------------------------------------------------------------------

data RideRevenueTotals = RideRevenueTotals
  { onlineRideRevRec :: RideFareRevRecTotals,
    buyerAppSettlement :: BuyerAppSettlementTotals,
    offlineCashRide :: RideFareRevRecTotals,
    driverEarningAccrual :: DriverEarningAccrualTotals,
    payout :: PayoutTotals,
    tds :: TdsTotals
  }
  deriving (Generic, Show, Eq)

type RideRevenueTotalsFlow m r = (EsqDBFlow m r, MonadFlow m, CacheFlow m r)

fetchRideRevenueTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m RideRevenueTotals
fetchRideRevenueTotals merchantOpCityId fromTime toTime = do
  onlineRideRevRec <-
    fetchRideFareRevRecTotals
      merchantOpCityId
      fromTime
      toTime
      Wallet.walletReferenceGSTOnline
      Wallet.walletReferenceVATOnline
  buyerAppSettlement <- fetchBuyerAppSettlementTotals merchantOpCityId fromTime toTime
  offlineCashRide <-
    fetchRideFareRevRecTotals
      merchantOpCityId
      fromTime
      toTime
      Wallet.walletReferenceGSTCash
      Wallet.walletReferenceVATCash
  driverEarningAccrual <- fetchDriverEarningAccrualTotals merchantOpCityId fromTime toTime
  payout <- fetchPayoutTotals merchantOpCityId fromTime toTime
  tds <- fetchTdsTotals merchantOpCityId fromTime toTime
  pure RideRevenueTotals {..}

-- | RideFare/Output ITT aggregate filtered by SETTLED GST/VAT ledger tax refs
-- (online: GSTOnline/VATOnline; cash: GSTCash/VATCash).
fetchRideFareRevRecTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  Text ->
  Text ->
  m RideFareRevRecTotals
fetchRideFareRevRecTotals merchantOpCityId fromTime toTime taxRefA taxRefB = do
  (mbNet, mbCgst, mbSgst, mbIgst, count) <-
    findRideFareITTTotalsByLedgerTaxRefs merchantOpCityId fromTime toTime taxRefA taxRefB
  let netAmount = fromMaybe 0 mbNet
      cgst = fromMaybe 0 mbCgst
      sgst = fromMaybe 0 mbSgst
      igst = fromMaybe 0 mbIgst
  pure
    RideFareRevRecTotals
      { netAmount,
        cgst,
        sgst,
        igst,
        grossAmount = netAmount + cgst + sgst + igst,
        txnCount = count
      }

-- | SUM RideFare/Output ITT rows whose booking also has a SETTLED tax ledger leg
-- with the given online-or-cash reference types.
findRideFareITTTotalsByLedgerTaxRefs ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  Text ->
  Text ->
  m (Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe HighPrecMoney, Int)
findRideFareITTTotalsByLedgerTaxRefs merchantOpCityId startTime endTime taxRefA taxRefB = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_
            ( \(itt, _le) ->
                ( B.as_ @(Maybe HighPrecMoney) $ B.sum_ itt.taxableValue,
                  B.as_ @(Maybe HighPrecMoney) $ B.sum_ itt.cgstAmount,
                  B.as_ @(Maybe HighPrecMoney) $ B.sum_ itt.sgstAmount,
                  B.as_ @(Maybe HighPrecMoney) $ B.sum_ itt.igstAmount,
                  B.as_ @Int B.countAll_
                )
            )
            $ B.filter_'
              ( \(itt, le) ->
                  itt.merchantOperatingCityId B.==?. B.val_ merchantOpCityId.getId
                    B.&&?. le.merchantOperatingCityId B.==?. B.val_ merchantOpCityId.getId
                    B.&&?. itt.transactionType B.==?. B.val_ ITTDomain.RideFare
                    B.&&?. itt.gstCreditType B.==?. B.val_ ITTDomain.Output
                    B.&&?. B.sqlBool_ (itt.transactionDate B.>=. B.val_ startTime)
                    B.&&?. B.sqlBool_ (itt.transactionDate B.<=. B.val_ endTime)
                    B.&&?. le.status B.==?. B.val_ LedgerDomain.SETTLED
                    B.&&?. B.sqlBool_ (B.isNothing_ le.reversalOf)
              )
              do
                itt <- B.all_ (BeamCommon.indirectTaxTransaction BeamCommon.atlasDB)
                le <-
                  B.join_
                    (BeamCommon.financeLedgerEntry BeamCommon.atlasDB)
                    ( \le ->
                        le.referenceId B.==. itt.referenceId
                          B.&&. (le.referenceType B.==. B.val_ taxRefA B.||. le.referenceType B.==. B.val_ taxRefB)
                    )
                pure (itt, le)
  case res of
    Right [row] -> pure row
    Right _ -> pure (Nothing, Nothing, Nothing, Nothing, 0)
    Left err -> do
      L.logError ("findRideFareITTTotalsByLedgerTaxRefs" :: Text) $
        "failed for mocId=" <> merchantOpCityId.getId <> " taxRefs=" <> taxRefA <> "/" <> taxRefB <> " error=" <> show err
      pure (Nothing, Nothing, Nothing, Nothing, 0)

-- | Buyer-app settlement — blocked on WS2 (BAP settlement feed). Soft-skip with 0.
fetchBuyerAppSettlementTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m BuyerAppSettlementTotals
fetchBuyerAppSettlementTotals merchantOpCityId _fromTime _toTime = do
  logError $
    "fetchBuyerAppSettlementTotals not implemented (depends on WS2); returning zeros for mocId=" <> merchantOpCityId.getId
  pure BuyerAppSettlementTotals {settledAmount = 0, txnCount = 0}

-- | Driver earning accrual: SUM BaseRide legs that credit OwnerLiability
-- (BuyerExternal → OwnerLiability). Excludes cash Control tracking and the
-- intermediate BuyerAsset → BuyerExternal pass-through leg.
fetchDriverEarningAccrualTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m DriverEarningAccrualTotals
fetchDriverEarningAccrualTotals merchantOpCityId fromTime toTime = do
  (mbAmount, count) <- findBaseRideOwnerLiabilityTotals merchantOpCityId fromTime toTime
  pure
    DriverEarningAccrualTotals
      { accrualAmount = fromMaybe 0 mbAmount,
        txnCount = count
      }

findBaseRideOwnerLiabilityTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m (Maybe HighPrecMoney, Int)
findBaseRideOwnerLiabilityTotals merchantOpCityId startTime endTime = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_
            ( \(le, _acc) ->
                ( B.as_ @(Maybe HighPrecMoney) $ B.sum_ le.amount,
                  B.as_ @Int B.countAll_
                )
            )
            $ B.filter_'
              ( \(le, acc) ->
                  le.merchantOperatingCityId B.==?. B.val_ merchantOpCityId.getId
                    B.&&?. le.referenceType B.==?. B.val_ Wallet.walletReferenceBaseRide
                    B.&&?. acc.accountType B.==?. B.val_ AccountDomain.Liability
                    B.&&?. le.status B.==?. B.val_ LedgerDomain.SETTLED
                    B.&&?. B.sqlBool_ (le.timestamp B.>=. B.val_ startTime)
                    B.&&?. B.sqlBool_ (le.timestamp B.<=. B.val_ endTime)
                    B.&&?. B.sqlBool_ (B.isNothing_ le.reversalOf)
              )
              do
                le <- B.all_ (BeamCommon.financeLedgerEntry BeamCommon.atlasDB)
                acc <-
                  B.join_
                    (BeamCommon.financeAccount BeamCommon.atlasDB)
                    (\acc -> acc.id B.==. le.toAccountId)
                pure (le, acc)
  case res of
    Right [row] -> pure row
    Right _ -> pure (Nothing, 0)
    Left err -> do
      L.logError ("findBaseRideOwnerLiabilityTotals" :: Text) $
        "failed for mocId=" <> merchantOpCityId.getId <> " error=" <> show err
      pure (Nothing, 0)

-- | Successful wallet payouts (Juspay today). WS4 (Stripe/bank-file/gating) is an
-- extension of eligibility/channels, not required for this aggregate.
fetchPayoutTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m PayoutTotals
fetchPayoutTotals merchantOpCityId fromTime toTime = do
  (mbAmount, count) <- findWalletPayoutTotals merchantOpCityId fromTime toTime
  pure
    PayoutTotals
      { payoutAmount = fromMaybe 0 mbAmount,
        txnCount = count
      }

-- | SUM SETTLED WalletPayout ledger legs (driver liability debit on success).
-- Single-leg createWalletEntryDelta — no account join needed.
findWalletPayoutTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m (Maybe HighPrecMoney, Int)
findWalletPayoutTotals merchantOpCityId startTime endTime = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_
            ( \le ->
                ( B.as_ @(Maybe HighPrecMoney) $ B.sum_ le.amount,
                  B.as_ @Int B.countAll_
                )
            )
            $ B.filter_'
              ( \le ->
                  le.merchantOperatingCityId B.==?. B.val_ merchantOpCityId.getId
                    B.&&?. le.referenceType B.==?. B.val_ Wallet.walletReferencePayout
                    B.&&?. le.status B.==?. B.val_ LedgerDomain.SETTLED
                    B.&&?. B.sqlBool_ (le.timestamp B.>=. B.val_ startTime)
                    B.&&?. B.sqlBool_ (le.timestamp B.<=. B.val_ endTime)
                    B.&&?. B.sqlBool_ (B.isNothing_ le.reversalOf)
              )
              (B.all_ (BeamCommon.financeLedgerEntry BeamCommon.atlasDB))
  case res of
    Right [row] -> pure row
    Right _ -> pure (Nothing, 0)
    Left err -> do
      L.logError ("findWalletPayoutTotals" :: Text) $
        "failed for mocId=" <> merchantOpCityId.getId <> " error=" <> show err
      pure (Nothing, 0)

-- | TDS deduction: Dr DRIVER_BALANCE / Cr TDS_PAYABLE (from direct_tax_transaction Deducted).
--   Reimbursement (Dr TDS_RECEIVABLE / Cr DRIVER_BALANCE) stays 0 until WS8 FO TDS-cert workflow.
fetchTdsTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m TdsTotals
fetchTdsTotals merchantOpCityId fromTime toTime = do
  (mbDeduction, deductionCount) <- findTdsDeductionTotals merchantOpCityId fromTime toTime
  pure
    TdsTotals
      { deductionAmount = fromMaybe 0 mbDeduction,
        deductionCount,
        -- WS8: FO TDS-certificate reimbursement → tdsTreatment=Reimbursed rows
        reimbursementAmount = 0,
        reimbursementCount = 0
      }

-- | Aggregate Deducted TDS from direct_tax_transaction.
-- No ledger join: unlike RideFare ITT (shared online/cash rows — needs GST/VAT
-- ref-types to split SAP events), DTT is TDS-only, already filtered by
-- tdsTreatment, and created at invoice time from GovtDirect legs that post
-- SETTLED. Online/cash share one SAP TDS JV, so payment-mode classification
-- via ledger is unnecessary.
findTdsDeductionTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m (Maybe HighPrecMoney, Int)
findTdsDeductionTotals merchantOpCityId startTime endTime = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_
            ( \dtt ->
                ( B.as_ @(Maybe HighPrecMoney) $ B.sum_ dtt.tdsAmount,
                  B.as_ @Int B.countAll_
                )
            )
            $ B.filter_'
              ( \dtt ->
                  dtt.merchantOperatingCityId B.==?. B.val_ merchantOpCityId.getId
                    B.&&?. dtt.tdsTreatment B.==?. B.val_ DTTDomain.Deducted
                    B.&&?. B.sqlBool_ (dtt.transactionDate B.>=. B.val_ startTime)
                    B.&&?. B.sqlBool_ (dtt.transactionDate B.<=. B.val_ endTime)
              )
              (B.all_ (BeamCommon.directTaxTransaction BeamCommon.atlasDB))
  case res of
    Right [row] -> pure row
    Right _ -> pure (Nothing, 0)
    Left err -> do
      L.logError ("findTdsDeductionTotals" :: Text) $
        "failed for mocId=" <> merchantOpCityId.getId <> " error=" <> show err
      pure (Nothing, 0)
