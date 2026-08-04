module SharedLogic.Allocator.Jobs.Settlement.RideRevenueTotals
  ( RideRevenueTotals (..),
    RideFareRevRecTotals (..),
    BuyerAppSettlementTotals (..),
    DriverEarningAccrualTotals (..),
    PayoutTotals (..),
    TdsTotals (..),
    RideFareITTRow (..),
    DriverEarningAccrualRow (..),
    WalletPayoutRow (..),
    TdsDeductionRow (..),
    fetchRideRevenueTotals,
    findRideFareITTRowsByLedgerTaxRefs,
    findBaseRideOwnerLiabilityRows,
    findWalletPayoutRows,
    findTdsDeductionRows,
  )
where

import qualified Database.Beam as B
import Database.Beam.Postgres (Postgres)
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
import qualified Lib.Finance.Storage.Beam.Account as BeamAccount
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.IndirectTaxTransaction as BeamITT
import qualified Lib.Finance.Storage.Beam.LedgerEntry as BeamLE
import qualified Lib.Finance.Storage.Queries.DirectTaxTransactionExtra as QDirectTaxExtra
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerEntryExtra
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

type BeamTable2 s table1 table2 = B.Q Postgres BeamCommon.AtlasDB s (table1 (B.QExpr Postgres s), table2 (B.QExpr Postgres s))

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

-- ---------------------------------------------------------------------------
-- OnlineRideRevRec/OfflineCashRide aggregated/per-row queries
-- ---------------------------------------------------------------------------

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

-- | Per-row shape for SAP JV drill-down. Same join/filter as
-- `findRideFareITTTotalsByLedgerTaxRefs` — keep WHERE in sync when changing either.
data RideFareITTRow = RideFareITTRow
  { referenceId :: Text,
    taxableValue :: HighPrecMoney,
    cgstAmount :: HighPrecMoney,
    sgstAmount :: HighPrecMoney,
    igstAmount :: HighPrecMoney,
    transactionDate :: UTCTime
  }
  deriving (Generic, Show, Eq)

-- | SUM RideFare/Output ITT rows whose booking also has a SETTLED tax ledger leg
-- with the given online-or-cash reference types.
-- Keep filter/join identical to `findRideFareITTRowsByLedgerTaxRefs` (dashboard drill-down).
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
            $ rideFareITTSettledLedgerBase merchantOpCityId startTime endTime taxRefA taxRefB
  case res of
    Right [row] -> pure row
    Right _ -> pure (Nothing, Nothing, Nothing, Nothing, 0)
    Left err -> do
      L.logError ("findRideFareITTTotalsByLedgerTaxRefs" :: Text) $
        "failed for mocId=" <> merchantOpCityId.getId <> " taxRefs=" <> taxRefA <> "/" <> taxRefB <> " error=" <> show err
      pure (Nothing, Nothing, Nothing, Nothing, 0)

-- | Row-level twin of `findRideFareITTTotalsByLedgerTaxRefs` for
-- `getFinanceManagementFinanceSapJournalsTransactions` (RevenueRecognition drill-down).
-- Same WHERE/join; no aggregate_ — paginated ITT rows.
findRideFareITTRowsByLedgerTaxRefs ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  Text ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  m [RideFareITTRow]
findRideFareITTRowsByLedgerTaxRefs merchantOpCityId startTime endTime taxRefA taxRefB mbLimit mbOffset = do
  let limitVal = fromIntegral $ min 100 $ fromMaybe 20 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.orderBy_ (\(itt, _le) -> B.desc_ itt.transactionDate) $
                rideFareITTSettledLedgerBase merchantOpCityId startTime endTime taxRefA taxRefB
  case res of
    Right rows ->
      pure $
        map
          ( \(itt, _le) ->
              RideFareITTRow
                { referenceId = BeamITT.referenceId itt,
                  taxableValue = BeamITT.taxableValue itt,
                  cgstAmount = BeamITT.cgstAmount itt,
                  sgstAmount = BeamITT.sgstAmount itt,
                  igstAmount = BeamITT.igstAmount itt,
                  transactionDate = BeamITT.transactionDate itt
                }
          )
          rows
    Left err -> do
      L.logError ("findRideFareITTRowsByLedgerTaxRefs" :: Text) $
        "failed for mocId=" <> merchantOpCityId.getId <> " taxRefs=" <> taxRefA <> "/" <> taxRefB <> " error=" <> show err
      pure []

-- NOTE: Common join/filter logic is shared with `findRideFareITTRowsByLedgerTaxRefs`.
rideFareITTSettledLedgerBase ::
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  Text ->
  Text ->
  BeamTable2 s BeamITT.IndirectTaxTransactionT BeamLE.LedgerEntryT
rideFareITTSettledLedgerBase merchantOpCityId startTime endTime taxRefA taxRefB =
  B.filter_'
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

-- ---------------------------------------------------------------------------
-- BuyerAppSettlement/DriverEarningAccrual aggregated/per-row queries
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- DriverEarningAccrual aggregated/per-row queries
-- ---------------------------------------------------------------------------

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

-- | Per-row shape for SAP JV drill-down. Same join/filter as
-- `findBaseRideOwnerLiabilityTotals` — keep WHERE in sync when changing either.
data DriverEarningAccrualRow = DriverEarningAccrualRow
  { referenceId :: Text,
    amount :: HighPrecMoney,
    timestamp :: UTCTime
  }
  deriving (Generic, Show, Eq)

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
            $ baseRideOwnerLiabilityBase merchantOpCityId startTime endTime
  case res of
    Right [row] -> pure row
    Right _ -> pure (Nothing, 0)
    Left err -> do
      L.logError ("findBaseRideOwnerLiabilityTotals" :: Text) $
        "failed for mocId=" <> merchantOpCityId.getId <> " error=" <> show err
      pure (Nothing, 0)

-- | Row-level twin of `findBaseRideOwnerLiabilityTotals` for
-- `getFinanceManagementFinanceSapJournalsTransactions` (RevenueRecognition drill-down).
findBaseRideOwnerLiabilityRows ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  Maybe Int ->
  Maybe Int ->
  m [DriverEarningAccrualRow]
findBaseRideOwnerLiabilityRows merchantOpCityId startTime endTime mbLimit mbOffset = do
  let limitVal = fromIntegral $ min 100 $ fromMaybe 20 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.orderBy_ (\(le, _acc) -> B.desc_ le.timestamp) $
                baseRideOwnerLiabilityBase merchantOpCityId startTime endTime
  case res of
    Right rows ->
      pure $
        map
          ( \(le, _acc) ->
              DriverEarningAccrualRow
                { referenceId = BeamLE.referenceId le,
                  amount = BeamLE.amount le,
                  timestamp = BeamLE.timestamp le
                }
          )
          rows
    Left err -> do
      L.logError ("findBaseRideOwnerLiabilityRows" :: Text) $
        "failed for mocId=" <> merchantOpCityId.getId <> " error=" <> show err
      pure []

-- NOTE: Common join/filter logic is shared with `findBaseRideOwnerLiabilityRows`.
baseRideOwnerLiabilityBase ::
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  BeamTable2 s BeamLE.LedgerEntryT BeamAccount.AccountT
baseRideOwnerLiabilityBase merchantOpCityId startTime endTime =
  B.filter_'
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

-- ---------------------------------------------------------------------------
-- Payout aggregated/per-row queries
-- ---------------------------------------------------------------------------

--- pg_payout_settlement_report????

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
-- Keep filter identical to `findWalletPayoutRows` (KV drill-down).
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

-- | Per-row shape for SAP JV drill-down (PayoutToClearing / PayoutClearingToBank share this source).
data WalletPayoutRow = WalletPayoutRow
  { referenceId :: Text,
    amount :: HighPrecMoney,
    timestamp :: UTCTime
  }
  deriving (Generic, Show, Eq)

-- | Row-level twin of `findWalletPayoutTotals` via KV
-- (`Lib.Finance.Storage.Queries.LedgerEntryExtra.findSettledByReferenceTypeAndDateRange`).
-- Keep WHERE in sync with the Beam aggregate used by SAP dispatch.
findWalletPayoutRows ::
  (RideRevenueTotalsFlow m r, Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  Maybe Int ->
  Maybe Int ->
  m [WalletPayoutRow]
findWalletPayoutRows merchantOpCityId startTime endTime mbLimit mbOffset = do
  entries <-
    QLedgerEntryExtra.findSettledByReferenceTypeAndDateRange
      Wallet.walletReferencePayout
      merchantOpCityId.getId
      startTime
      endTime
      mbLimit
      mbOffset
  pure $
    map
      ( \e ->
          WalletPayoutRow
            { referenceId = e.referenceId,
              amount = e.amount,
              timestamp = e.timestamp
            }
      )
      entries

-- ---------------------------------------------------------------------------
-- TDS aggregated/per-row queries
-- ---------------------------------------------------------------------------

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
-- Keep filter identical to `findTdsDeductionRows` (KV drill-down).
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

-- | Per-row shape for SAP JV drill-down (TdsDeduction).
data TdsDeductionRow = TdsDeductionRow
  { referenceId :: Text,
    tdsAmount :: HighPrecMoney,
    transactionDate :: UTCTime
  }
  deriving (Generic, Show, Eq)

-- | Row-level twin of `findTdsDeductionTotals` via KV
-- (`Lib.Finance.Storage.Queries.DirectTaxTransactionExtra.findDeductedByDateRange`).
-- Keep WHERE in sync with the Beam aggregate used by SAP dispatch.
findTdsDeductionRows ::
  (RideRevenueTotalsFlow m r, Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  Maybe Int ->
  Maybe Int ->
  m [TdsDeductionRow]
findTdsDeductionRows merchantOpCityId startTime endTime mbLimit mbOffset = do
  entries <-
    QDirectTaxExtra.findDeductedByDateRange
      merchantOpCityId.getId
      startTime
      endTime
      mbLimit
      mbOffset
  pure $
    map
      ( \e ->
          TdsDeductionRow
            { referenceId = e.referenceId,
              tdsAmount = e.tdsAmount,
              transactionDate = e.transactionDate
            }
      )
      entries
