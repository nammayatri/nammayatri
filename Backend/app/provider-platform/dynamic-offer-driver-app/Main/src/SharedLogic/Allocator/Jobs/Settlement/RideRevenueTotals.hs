module SharedLogic.Allocator.Jobs.Settlement.RideRevenueTotals
  ( RideRevenueTotals (..),
    RideFareRevRecTotals (..),
    BuyerAppSettlementTotals (..),
    DriverEarningAccrualTotals (..),
    PayoutTotals (..),
    TdsTotals (..),
    SubscriptionRevenueTotals (..),
    RevenueRecognitionTransactionRow (..),
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
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction as DDirectTaxTransaction
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as ITTDomain
import qualified Lib.Finance.Domain.Types.LedgerEntry as LedgerDomain
import qualified Lib.Finance.Storage.Queries.DirectTaxTransaction as QDirectTaxTransaction
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerEntryExtra
import qualified SharedLogic.Finance.Prepaid as Prepaid
import qualified SharedLogic.Finance.Wallet as Wallet
import qualified Storage.Beam.Common as BeamCommon

-- ---------------------------------------------------------------------------
-- Per-transaction row (persisted to journal_entry_transaction after SAP success)
-- ---------------------------------------------------------------------------

-- | One source business transaction that contributed to an aggregated SAP JV.
data RevenueRecognitionTransactionRow = RevenueRecognitionTransactionRow
  { amount :: HighPrecMoney,
    referenceId :: Text, -- bookingId / payoutId / DTT.referenceId
    txnStatus :: Text -- show SETTLED / "Deducted" / ...
  }
  deriving (Generic, Show, Eq)

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

-- | Payout request → PG payout: DRIVER_BALANCE → PAYOUT_CLEARING → BANK.
--   Phase-1: both SAP JVs use this one SETTLED WalletPayout total (trivial same
--   amount). Intended split: JV1 = this ledger debit; JV2 = pg_payout_settlement_report
--   (PG/bank file, WS4 ingest) — same pattern as PGSettlement ← pg_payment_settlement_report.
data PayoutTotals = PayoutTotals
  { payoutAmount :: HighPrecMoney,
    txnCount :: Int
  }
  deriving (Generic, Show, Eq)

-- | TDS threshold / reimbursement (WS3 matrix).
--   deduction* from direct_tax_transaction (tdsTreatment=Deducted);
--   reimbursement* from DTT Reimbursed rows written at WS3 Credit post
--   (FO TDS-cert flow: WS8 submit → maker-checker adjustment).
data TdsTotals = TdsTotals
  { deductionAmount :: HighPrecMoney,
    reimbursementAmount :: HighPrecMoney,
    deductionCount :: Int,
    reimbursementCount :: Int
  }
  deriving (Generic, Show, Eq)

-- | Subscription revenue recognised: Dr DEFERRED_REVENUE / Cr SUBSCRIPTION_REVENUE.
--   Ride vs expiry are separate SAP JVs (different description labels).
data SubscriptionRevenueTotals = SubscriptionRevenueTotals
  { recognizedAmount :: HighPrecMoney,
    txnCount :: Int
  }
  deriving (Generic, Show, Eq)

-- ---------------------------------------------------------------------------
-- Combined daily totals for one merchant operating city
-- ---------------------------------------------------------------------------

-- | Aggregates + per-event source rows.
data RideRevenueTotals = RideRevenueTotals
  { onlineRideRevRec :: (RideFareRevRecTotals, [RevenueRecognitionTransactionRow]),
    buyerAppSettlement :: (BuyerAppSettlementTotals, [RevenueRecognitionTransactionRow]),
    offlineCashRide :: (RideFareRevRecTotals, [RevenueRecognitionTransactionRow]),
    driverEarningAccrual :: (DriverEarningAccrualTotals, [RevenueRecognitionTransactionRow]),
    payout :: (PayoutTotals, [RevenueRecognitionTransactionRow]),
    tds :: (TdsTotals, [RevenueRecognitionTransactionRow], [RevenueRecognitionTransactionRow]),
    subscriptionRideRevenue :: (SubscriptionRevenueTotals, [RevenueRecognitionTransactionRow]),
    subscriptionExpiryRevenue :: (SubscriptionRevenueTotals, [RevenueRecognitionTransactionRow])
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
  subscriptionRideRevenue <- fetchSubscriptionRevenueTotals Prepaid.subscriptionRideReferenceType merchantOpCityId fromTime toTime
  subscriptionExpiryRevenue <- fetchSubscriptionRevenueTotals Prepaid.expiryRevenueRecognitionReferenceType merchantOpCityId fromTime toTime
  pure RideRevenueTotals {..}

-- ---------------------------------------------------------------------------
-- Ride fare rev-rec (online / offline-cash)
-- ---------------------------------------------------------------------------

-- | RideFare/Output ITT + SETTLED GST/VAT ledger tax refs → totals + rows.
-- (online: GSTOnline/VATOnline; cash: GSTCash/VATCash).
fetchRideFareRevRecTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  Text ->
  Text ->
  m (RideFareRevRecTotals, [RevenueRecognitionTransactionRow])
fetchRideFareRevRecTotals merchantOpCityId fromTime toTime taxRefA taxRefB = do
  rawRows <- findRideFareITTRowsByLedgerTaxRefs merchantOpCityId fromTime toTime taxRefA taxRefB
  let (totals, txnRowsRev) = foldl' go (RideFareRevRecTotals 0 0 0 0 0 0, []) rawRows
  pure (totals, reverse txnRowsRev)
  where
    go (acc, rs) (refId, netAmt, cgstAmt, sgstAmt, igstAmt, st) =
      let gross = netAmt + cgstAmt + sgstAmt + igstAmt
       in ( acc{netAmount = acc.netAmount + netAmt,
                cgst = acc.cgst + cgstAmt,
                sgst = acc.sgst + sgstAmt,
                igst = acc.igst + igstAmt,
                grossAmount = acc.grossAmount + gross,
                txnCount = acc.txnCount + 1
               },
            RevenueRecognitionTransactionRow {amount = gross, referenceId = refId, txnStatus = show st} : rs
          )

-- | List RideFare/Output ITT rows whose booking also has a SETTLED tax ledger
-- with the given online-or-cash reference types.
-- EXISTS (not inner join): online GST posts two GSTOnline legs (BuyerAsset →
-- BuyerExternal pass-through + BuyerExternal → GovtIndirect); a join would
-- return the same ITT twice and the fold would double totals.
findRideFareITTRowsByLedgerTaxRefs ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  Text ->
  Text ->
  m [(Text, HighPrecMoney, HighPrecMoney, HighPrecMoney, HighPrecMoney, LedgerDomain.EntryStatus)]
findRideFareITTRowsByLedgerTaxRefs merchantOpCityId startTime endTime taxRefA taxRefB = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          fmap
            ( \itt ->
                ( itt.referenceId,
                  itt.taxableValue,
                  itt.cgstAmount,
                  itt.sgstAmount,
                  itt.igstAmount
                )
            )
            $ B.filter_'
              ( \itt ->
                  itt.merchantOperatingCityId B.==?. B.val_ merchantOpCityId.getId
                    B.&&?. itt.transactionType B.==?. B.val_ ITTDomain.RideFare
                    B.&&?. itt.gstCreditType B.==?. B.val_ ITTDomain.Output
                    B.&&?. B.sqlBool_ (itt.transactionDate B.>=. B.val_ startTime)
                    B.&&?. B.sqlBool_ (itt.transactionDate B.<=. B.val_ endTime)
                    B.&&?. B.sqlBool_
                      ( B.exists_ $
                          B.filter_
                            ( \le ->
                                le.referenceId B.==. itt.referenceId
                                  B.&&. (le.referenceType B.==. B.val_ taxRefA B.||. le.referenceType B.==. B.val_ taxRefB)
                                  B.&&. le.merchantOperatingCityId B.==. B.val_ merchantOpCityId.getId
                                  B.&&. le.status B.==. B.val_ LedgerDomain.SETTLED
                                  B.&&. B.isNothing_ le.reversalOf
                            )
                            (B.all_ (BeamCommon.financeLedgerEntry BeamCommon.atlasDB))
                      )
              )
              (B.all_ (BeamCommon.indirectTaxTransaction BeamCommon.atlasDB))
  case res of
    Right rows ->
      pure $ (\(refId, net, cgst, sgst, igst) -> (refId, net, cgst, sgst, igst, LedgerDomain.SETTLED)) <$> rows
    Left err -> do
      L.logError ("findRideFareITTRowsByLedgerTaxRefs" :: Text) $
        "failed for mocId=" <> merchantOpCityId.getId <> " taxRefs=" <> taxRefA <> "/" <> taxRefB <> " error=" <> show err
      pure []

-- ---------------------------------------------------------------------------
-- Buyer-app settlement (WS2 stub)
-- ---------------------------------------------------------------------------

-- | Buyer-app settlement — blocked on WS2 (BAP settlement feed). Soft-skip with 0.
fetchBuyerAppSettlementTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m (BuyerAppSettlementTotals, [RevenueRecognitionTransactionRow])
fetchBuyerAppSettlementTotals merchantOpCityId _fromTime _toTime = do
  logError $
    "fetchBuyerAppSettlementTotals not implemented (depends on WS2); returning zeros for mocId=" <> merchantOpCityId.getId
  pure (BuyerAppSettlementTotals {settledAmount = 0, txnCount = 0}, [])

-- ---------------------------------------------------------------------------
-- Driver earning accrual
-- ---------------------------------------------------------------------------

-- | Driver earning accrual: SUM BaseRide legs that credit OwnerLiability
-- (BuyerExternal → OwnerLiability). Excludes cash Control tracking and the
-- intermediate BuyerAsset → BuyerExternal pass-through leg.
fetchDriverEarningAccrualTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m (DriverEarningAccrualTotals, [RevenueRecognitionTransactionRow])
fetchDriverEarningAccrualTotals merchantOpCityId fromTime toTime = do
  rawRows <- findBaseRideOwnerLiabilityRows merchantOpCityId fromTime toTime
  let (totals, txnRowsRev) = foldl' go (DriverEarningAccrualTotals 0 0, []) rawRows
  pure (totals, reverse txnRowsRev)
  where
    go (acc, rs) (refId, amt, st) =
      ( acc {accrualAmount = acc.accrualAmount + amt, txnCount = acc.txnCount + 1},
        RevenueRecognitionTransactionRow {amount = amt, referenceId = refId, txnStatus = show st} : rs
      )

-- | List BaseRide legs that credit OwnerLiability.
findBaseRideOwnerLiabilityRows ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m [(Text, HighPrecMoney, LedgerDomain.EntryStatus)]
findBaseRideOwnerLiabilityRows merchantOpCityId startTime endTime = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          fmap
            ( \(le, _acc) ->
                ( le.referenceId,
                  le.amount,
                  le.status
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
    Right rows -> pure rows
    Left err -> do
      L.logError ("findBaseRideOwnerLiabilityRows" :: Text) $
        "failed for mocId=" <> merchantOpCityId.getId <> " error=" <> show err
      pure []

-- ---------------------------------------------------------------------------
-- Payout
-- ---------------------------------------------------------------------------

-- | Successful wallet payouts (Juspay webhook → WalletPayout ledger). Feeds both
-- phase-1 payout JVs. WS4: keep this for PayoutToClearing; PayoutClearingToBank
-- should switch to pg_payout_settlement_report once the payout file is ingested.
fetchPayoutTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m (PayoutTotals, [RevenueRecognitionTransactionRow])
fetchPayoutTotals merchantOpCityId fromTime toTime = do
  -- List SETTLED WalletPayout ledger legs (driver liability debit on success).
  -- Single-leg createWalletEntryDelta — no account join needed.
  rawRows <- QLedgerEntryExtra.findSettledByReferenceTypeAndDateRange Wallet.walletReferencePayout merchantOpCityId.getId fromTime toTime Nothing Nothing
  let (totals, txnRowsRev) = foldl' go (PayoutTotals 0 0, []) rawRows
  pure (totals, reverse txnRowsRev)
  where
    go (acc, rs) le =
      ( acc {payoutAmount = acc.payoutAmount + le.amount, txnCount = acc.txnCount + 1},
        RevenueRecognitionTransactionRow {amount = le.amount, referenceId = le.referenceId, txnStatus = show le.status} : rs
      )

-- ---------------------------------------------------------------------------
-- TDS
-- ---------------------------------------------------------------------------

-- | TDS deduction: Dr DRIVER_BALANCE / Cr TDS_PAYABLE (DTT Deducted).
--   Reimbursement: Dr TDS_RECEIVABLE / Cr DRIVER_BALANCE (DTT Reimbursed at WS3 Credit post).
fetchTdsTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m (TdsTotals, [RevenueRecognitionTransactionRow], [RevenueRecognitionTransactionRow])
fetchTdsTotals merchantOpCityId fromTime toTime = do
  -- List Deducted TDS from direct_tax_transaction.
  -- No ledger join: unlike RideFare ITT (shared online/cash rows — needs GST/VAT
  -- ref-types to split SAP events), DTT is TDS-only, already filtered by
  -- tdsTreatment. Deducted rows are created at invoice time from GovtDirect legs;
  -- Reimbursed rows are written with the FO TDS-cert Credit adjustment (payable =
  -- tdsCreditReceivable). Online/cash share one SAP TDS JV, so payment-mode
  -- classification via ledger is unnecessary.
  let findByTreatment treatment =
        QDirectTaxTransaction.findByTdsTreatmentAndDateRange Nothing Nothing merchantOpCityId.getId treatment fromTime toTime
  deductedRows <- findByTreatment DDirectTaxTransaction.Deducted
  reimbursedRows <- findByTreatment DDirectTaxTransaction.Reimbursed
  let (deductionTotals, deductionRowsRev) = foldl' goDeduction (TdsTotals 0 0 0 0, []) deductedRows
      (totals, reimbursementRowsRev) = foldl' goReimbursement (deductionTotals, []) reimbursedRows
  pure (totals, reverse deductionRowsRev, reverse reimbursementRowsRev)
  where
    goDeduction (acc, rs) dtt =
      ( acc {deductionAmount = acc.deductionAmount + dtt.tdsAmount, deductionCount = acc.deductionCount + 1},
        mkRow dtt : rs
      )
    goReimbursement (acc, rs) dtt =
      ( acc {reimbursementAmount = acc.reimbursementAmount + dtt.tdsAmount, reimbursementCount = acc.reimbursementCount + 1},
        mkRow dtt : rs
      )
    mkRow dtt =
      RevenueRecognitionTransactionRow
        { amount = dtt.tdsAmount,
          referenceId = dtt.referenceId,
          txnStatus = show dtt.tdsTreatment
        }

-- ---------------------------------------------------------------------------
-- Subscription revenue recognised (deferred → revenue after ride / expiry)
-- ---------------------------------------------------------------------------

-- | One SETTLED referenceType → one SAP event (ride vs expiry stay separate JVs).
fetchSubscriptionRevenueTotals ::
  (RideRevenueTotalsFlow m r) =>
  Text ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m (SubscriptionRevenueTotals, [RevenueRecognitionTransactionRow])
fetchSubscriptionRevenueTotals referenceType merchantOpCityId fromTime toTime = do
  rawRows <-
    QLedgerEntryExtra.findSettledByReferenceTypeAndDateRange
      referenceType
      merchantOpCityId.getId
      fromTime
      toTime
      Nothing
      Nothing
  let (totals, txnRowsRev) = foldl' go (SubscriptionRevenueTotals 0 0, []) rawRows
  pure (totals, reverse txnRowsRev)
  where
    go (acc, rs) le =
      ( acc {recognizedAmount = acc.recognizedAmount + le.amount, txnCount = acc.txnCount + 1},
        RevenueRecognitionTransactionRow {amount = le.amount, referenceId = le.referenceId, txnStatus = show le.status} : rs
      )
