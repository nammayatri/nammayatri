module SharedLogic.Allocator.Jobs.Settlement.RideRevenueTotals
  ( RideRevenueTotals (..),
    RideFareRevRecTotals (..),
    BuyerAppSettlementTotals (..),
    DriverEarningAccrualTotals (..),
    PayoutEventTotals (..),
    LedgerAdjustmentJVConfig (..),
    ledgerAdjustmentJVConfigs,
    TdsTotals (..),
    SubscriptionRevenueTotals (..),
    RideRevenueTransactionRow (..),
    fetchRideRevenueTotals,
  )
where

import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import BecknV2.RSF.Utils (roundToPaise)
import qualified Data.HashSet as HS
import Data.List (partition)
import qualified Data.Map.Strict as M
import qualified Database.Beam as B
import qualified Database.Beam.Query ()
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.LedgerAdjustmentRequest as DLA
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Ride as DRide
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Account as AccountDomain
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction as DDirectTaxTransaction
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as ITTDomain
import qualified Lib.Finance.Domain.Types.JournalEntryTransaction as JET
import qualified Lib.Finance.Domain.Types.LedgerEntry as LedgerDomain
import qualified Lib.Finance.Domain.Types.SapJournalEntry as SJE
import qualified Lib.Finance.Storage.Beam.Account as Fields
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.IndirectTaxTransaction as Fields
import qualified Lib.Finance.Storage.Beam.LedgerEntry as Fields
import qualified Lib.Finance.Storage.Queries.DirectTaxTransaction as QDirectTaxTransaction
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerEntryExtra
import qualified Lib.Payment.Domain.Types.PayoutLifecycleEvent as DPLE
import qualified Lib.Payment.Storage.Queries.PayoutLifecycleEventExtra as QPLEExtra
import SharedLogic.Allocator.Jobs.Settlement.SAPDispatchCommon (driverBalanceAcct, tdsPayableAcct)
import qualified SharedLogic.Finance.Prepaid as Prepaid
import qualified SharedLogic.Finance.Wallet as Wallet
import qualified Storage.Beam.Booking as Fields
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Ride as Fields
import qualified Storage.Beam.RsfOrderState as Fields
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.LedgerAdjustmentRequestExtra as QLedgerAdjustmentRequestExtra

-- ---------------------------------------------------------------------------
-- Per-transaction row (persisted to journal_entry_transaction after SAP success)
-- ---------------------------------------------------------------------------

-- | One source business transaction that contributed to an aggregated SAP JV.
data RideRevenueTransactionRow = RideRevenueTransactionRow
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

-- | Payout event totals — one per lifecycle event (INITIATED, SETTLED_AT_BANK, FAILED, REVERSED).
-- Sourced from payout_lifecycle_event table.
data PayoutEventTotals = PayoutEventTotals
  { totalAmount :: HighPrecMoney,
    txnCount :: Int
  }
  deriving (Generic, Show, Eq)

-- | Config for one SAP JV sourced from ledger_adjustment_request.
-- Add a row to 'ledgerAdjustmentJVConfigs' to post a new adjustment type — no other code change needed.
data LedgerAdjustmentJVConfig = LedgerAdjustmentJVConfig
  { label :: Text,
    category :: DLA.AdjustmentCategory,
    referenceType :: Text,
    transactionType :: SJE.TransactionType,
    referenceJetType :: JET.ReferenceType,
    debitAcct :: Text,
    creditAcct :: Text
  }
  deriving (Generic, Show, Eq)

tdsThresholdDeductionJVConfig :: LedgerAdjustmentJVConfig
tdsThresholdDeductionJVConfig =
  LedgerAdjustmentJVConfig
    { label = "TDS Threshold deduction",
      category = DLA.TdsDeductionDebit,
      referenceType = Wallet.walletReferenceTDSDeductionThreshold,
      transactionType = SJE.TDS,
      referenceJetType = JET.Booking,
      debitAcct = driverBalanceAcct,
      creditAcct = tdsPayableAcct
    }

ledgerAdjustmentJVConfigs :: [LedgerAdjustmentJVConfig]
ledgerAdjustmentJVConfigs =
  [ tdsThresholdDeductionJVConfig
  ]

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
--   onlineRideRevRec and driverEarningAccrual are keyed by bapId so that
--   SAP dispatch can emit a separate JV per buyer app.
data RideRevenueTotals = RideRevenueTotals
  { onlineRideRevRec :: M.Map Text (RideFareRevRecTotals, [RideRevenueTransactionRow]),
    buyerAppSettlement :: M.Map Text (BuyerAppSettlementTotals, [RideRevenueTransactionRow]),
    offlineCashRide :: (RideFareRevRecTotals, [RideRevenueTransactionRow]),
    driverEarningAccrual :: M.Map Text (DriverEarningAccrualTotals, [RideRevenueTransactionRow]),
    payoutInitiated :: (PayoutEventTotals, [RideRevenueTransactionRow]),
    payoutSettled :: (PayoutEventTotals, [RideRevenueTransactionRow]),
    payoutFailed :: (PayoutEventTotals, [RideRevenueTransactionRow]),
    payoutReversed :: (PayoutEventTotals, [RideRevenueTransactionRow]),
    tds :: (TdsTotals, [RideRevenueTransactionRow], [RideRevenueTransactionRow]),
    ledgerAdjustments :: [(LedgerAdjustmentJVConfig, (PayoutEventTotals, [RideRevenueTransactionRow]))],
    subscriptionRideRevenue :: (SubscriptionRevenueTotals, [RideRevenueTransactionRow]),
    subscriptionExpiryRevenue :: (SubscriptionRevenueTotals, [RideRevenueTransactionRow])
  }
  deriving (Generic, Show, Eq)

type RideRevenueTotalsFlow m r = (BeamFlow m r, EsqDBFlow m r, MonadFlow m, CacheFlow m r)

fetchRideRevenueTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m RideRevenueTotals
fetchRideRevenueTotals merchantOpCityId fromTime toTime = do
  (onlineRideRevRec, offlineCashRide) <- fetchRideFareRevRecTotals merchantOpCityId fromTime toTime
  buyerAppSettlement <- fetchBuyerAppSettlementTotalsByBapId merchantOpCityId fromTime toTime
  driverEarningAccrual <- fetchDriverEarningAccrualTotalsByBapId merchantOpCityId fromTime toTime
  payoutInitiated <- fetchPayoutEventTotals merchantOpCityId DPLE.INITIATED fromTime toTime
  payoutSettled <- fetchPayoutEventTotals merchantOpCityId DPLE.SETTLED_AT_BANK fromTime toTime
  payoutFailed <- fetchPayoutEventTotals merchantOpCityId DPLE.FAILED fromTime toTime
  payoutReversed <- fetchPayoutEventTotals merchantOpCityId DPLE.REVERSED fromTime toTime
  tds <- fetchTdsTotals merchantOpCityId fromTime toTime
  ledgerAdjustments <- forM ledgerAdjustmentJVConfigs $ \cfg -> do
    totalsAndRows <- fetchLedgerAdjustmentTotals merchantOpCityId cfg.category cfg.referenceType fromTime toTime
    pure (cfg, totalsAndRows)
  subscriptionRideRevenue <- fetchSubscriptionRevenueTotals Prepaid.subscriptionRideReferenceType merchantOpCityId fromTime toTime
  subscriptionExpiryRevenue <- fetchSubscriptionRevenueTotals Prepaid.expiryRevenueRecognitionReferenceType merchantOpCityId fromTime toTime
  pure RideRevenueTotals {..}

-- ---------------------------------------------------------------------------
-- Ride fare rev-rec (online / offline-cash)
-- ---------------------------------------------------------------------------

-- | Fetch all ride fare ITT rows, partition into online (grouped by bapId) and cash (flat).
-- Booking join removed from the query — bapId is resolved from the booking lookup instead.
fetchRideFareRevRecTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m (M.Map Text (RideFareRevRecTotals, [RideRevenueTransactionRow]), (RideFareRevRecTotals, [RideRevenueTransactionRow]))
fetchRideFareRevRecTotals merchantOpCityId fromTime toTime = do
  rideFareRows <- findRideFareITTRowsByLedgerTaxRefs merchantOpCityId fromTime toTime (Wallet.walletReferenceRideTax Tax.GST) (Wallet.walletReferenceRideTax Tax.VAT)
  bookings <- QBooking.findAllByIds (Id . (\(refId, _, _, _, _, _) -> refId) <$> rideFareRows)
  onlineBookings <- filterM Wallet.resolveIsOnlineFromBooking bookings
  let onlineBookingIds = HS.fromList $ map (.id.getId) onlineBookings
      bapIdByBookingId = M.fromList [(b.id.getId, b.bapId) | b <- onlineBookings]
      (onlineRows, cashRows) = partition (\(refId, _, _, _, _, _) -> refId `HS.member` onlineBookingIds) rideFareRows
  pure (rideFareRevRecTotalsByBapId bapIdByBookingId onlineRows, rideFareRevRecTotalsFlat cashRows)

-- | Flat totals — used for offline-cash ride rev-rec.
rideFareRevRecTotalsFlat :: [(Text, HighPrecMoney, HighPrecMoney, HighPrecMoney, HighPrecMoney, LedgerDomain.EntryStatus)] -> (RideFareRevRecTotals, [RideRevenueTransactionRow])
rideFareRevRecTotalsFlat rawRows =
  let (totals, txnRowsRev) = foldl' go (RideFareRevRecTotals 0 0 0 0 0 0, []) rawRows
   in (totals, reverse txnRowsRev)
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
            RideRevenueTransactionRow {amount = gross, referenceId = refId, txnStatus = show st} : rs
          )

-- | Grouped by bapId — used for online ride rev-rec SAP dispatch.
-- bapId is looked up from the bookings map; rows without a matching booking use "UNKNOWN".
rideFareRevRecTotalsByBapId :: M.Map Text Text -> [(Text, HighPrecMoney, HighPrecMoney, HighPrecMoney, HighPrecMoney, LedgerDomain.EntryStatus)] -> M.Map Text (RideFareRevRecTotals, [RideRevenueTransactionRow])
rideFareRevRecTotalsByBapId bapIdByBookingId rawRows =
  M.map (\(t, rs) -> (t, reverse rs)) $ foldl' go M.empty rawRows
  where
    go acc (refId, netAmt, cgstAmt, sgstAmt, igstAmt, st) =
      let bapId = fromMaybe (error $ "Booking not found for ITT referenceId: " <> refId) $ M.lookup refId bapIdByBookingId
          gross = netAmt + cgstAmt + sgstAmt + igstAmt
          row = RideRevenueTransactionRow {amount = gross, referenceId = refId, txnStatus = show st}
          entry = (RideFareRevRecTotals gross netAmt cgstAmt sgstAmt igstAmt 1, [row])
       in M.insertWith mergeRevRecEntry bapId entry acc
    mergeRevRecEntry (newT, newR) (oldT, oldR) =
      ( RideFareRevRecTotals
          { grossAmount = oldT.grossAmount + newT.grossAmount,
            netAmount = oldT.netAmount + newT.netAmount,
            cgst = oldT.cgst + newT.cgst,
            sgst = oldT.sgst + newT.sgst,
            igst = oldT.igst + newT.igst,
            txnCount = oldT.txnCount + newT.txnCount
          },
        newR ++ oldR
      )

-- | List RideFare/Output ITT rows whose booking also has a SETTLED tax ledger
-- with the given online-or-cash reference types.
-- EXISTS (not inner join): online GST posts two ride-tax legs (BuyerAsset →
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
    Right rows -> pure $ (\(refId, net, cgst, sgst, igst) -> (refId, net, cgst, sgst, igst, LedgerDomain.SETTLED)) <$> rows
    Left err -> do
      L.logError ("findRideFareITTRowsByLedgerTaxRefs" :: Text) $
        "failed for mocId=" <> merchantOpCityId.getId <> " taxRefs=" <> taxRefA <> "/" <> taxRefB <> " error=" <> show err
      pure []

-- ---------------------------------------------------------------------------
-- Buyer-app settlement (from rsf_order_state)
-- ---------------------------------------------------------------------------

-- | Buyer-app settlement grouped by bapId: COALESCE(ride.fareAmount, booking.estimatedFare)
-- from rsf_order_state joined with ride (COMPLETED) and booking, where reportedStatus = "01".
fetchBuyerAppSettlementTotalsByBapId ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m (M.Map Text (BuyerAppSettlementTotals, [RideRevenueTransactionRow]))
fetchBuyerAppSettlementTotalsByBapId merchantOpCityId fromTime toTime = do
  rawRows <- findSettledRsfOrderRows merchantOpCityId fromTime toTime
  let result = foldl' go M.empty rawRows
  pure $ M.map (\(t, rs) -> (t, reverse rs)) result
  where
    go acc (bapId, orderId, amount) =
      let row = RideRevenueTransactionRow {amount = amount, referenceId = orderId, txnStatus = "SETTLED"}
          entry = (BuyerAppSettlementTotals amount 1, [row])
       in M.insertWith mergeSettlementEntry bapId entry acc
    mergeSettlementEntry (newT, newR) (oldT, oldR) =
      ( BuyerAppSettlementTotals
          { settledAmount = oldT.settledAmount + newT.settledAmount,
            txnCount = oldT.txnCount + newT.txnCount
          },
        newR ++ oldR
      )

-- | Fetch settled RSF order rows: reportedStatus = "01", joined with ride and booking.
-- Mirrors RSFLedger.rideFare logic: ride.fareAmount first, then cancellationChargesOnCancel
-- for cancelled rides, then booking.estimatedFare as final fallback.
findSettledRsfOrderRows ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m [(Text, Text, HighPrecMoney)]
findSettledRsfOrderRows merchantOpCityId startTime endTime = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          fmap
            ( \(ros, rd, bk) ->
                ( ros.bapId,
                  ros.orderId,
                  rd.fareAmount,
                  rd.status,
                  rd.cancellationChargesOnCancel,
                  bk.estimatedFare
                )
            )
            $ B.filter_'
              ( \(ros, rd, _bk) ->
                  ros.merchantOperatingCityId B.==?. B.val_ merchantOpCityId.getId
                    B.&&?. ros.reportedStatus B.==?. B.val_ (Just "01")
                    B.&&?. B.sqlBool_ (ros.lastReportedAt B.>=. B.val_ (Just startTime))
                    B.&&?. B.sqlBool_ (ros.lastReportedAt B.<=. B.val_ (Just endTime))
                    B.&&?. B.sqlBool_ (B.not_ (B.isNothing_ ros.bapId))
                    B.&&?. B.sqlBool_ (rd.status B.==. B.val_ DRide.COMPLETED B.||. rd.status B.==. B.val_ DRide.CANCELLED)
              )
              ( do
                  ros <- B.all_ (BeamCommon.rsfOrderState BeamCommon.atlasDB)
                  rd <-
                    B.join_
                      (BeamCommon.ride BeamCommon.atlasDB)
                      (\rd -> rd.bookingId B.==. ros.orderId)
                  bk <-
                    B.join_
                      (BeamCommon.booking BeamCommon.atlasDB)
                      (\bk -> bk.id B.==. ros.orderId)
                  pure (ros, rd, bk)
              )
  case res of
    Right rows ->
      pure $ catMaybes [(\b -> (b, oid, settlementFare mbFare rdStatus mbCancelCharge estFare)) <$> mbBapId | (mbBapId, oid, mbFare, rdStatus, mbCancelCharge, estFare) <- rows]
    Left err -> do
      L.logError ("findSettledRsfOrderRows" :: Text) $
        "failed for mocId=" <> merchantOpCityId.getId <> " error=" <> show err
      pure []
  where
    settlementFare mbFare rdStatus mbCancelCharge estFare
      | Just fare <- mbFare = roundToPaise fare
      | rdStatus == DRide.CANCELLED = roundToPaise (fromMaybe 0 mbCancelCharge)
      | otherwise = roundToPaise estFare

-- ---------------------------------------------------------------------------
-- Driver earning accrual
-- ---------------------------------------------------------------------------

-- | Driver earning accrual grouped by bapId: SUM BaseRide legs that credit
-- OwnerLiability (BuyerExternal → OwnerLiability). Excludes cash Control
-- tracking and the intermediate BuyerAsset → BuyerExternal pass-through leg.
-- bapId is resolved from bookings in Haskell (same pattern as ITT rev-rec).
fetchDriverEarningAccrualTotalsByBapId ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m (M.Map Text (DriverEarningAccrualTotals, [RideRevenueTransactionRow]))
fetchDriverEarningAccrualTotalsByBapId merchantOpCityId fromTime toTime = do
  rawRows <- findBaseRideOwnerLiabilityRows merchantOpCityId fromTime toTime
  bookings <- QBooking.findAllByIds (Id . (\(refId, _, _) -> refId) <$> rawRows)
  let bapIdByBookingId = M.fromList [(b.id.getId, b.bapId) | b <- bookings]
      result = foldl' (go bapIdByBookingId) M.empty rawRows
  pure $ M.map (\(t, rs) -> (t, reverse rs)) result
  where
    go bapIdByBookingId acc (refId, amt, st) =
      let bapId = fromMaybe (error $ "Booking not found for accrual referenceId: " <> refId) $ M.lookup refId bapIdByBookingId
          row = RideRevenueTransactionRow {amount = amt, referenceId = refId, txnStatus = show st}
          entry = (DriverEarningAccrualTotals amt 1, [row])
       in M.insertWith mergeAccrualEntry bapId entry acc
    mergeAccrualEntry (newT, newR) (oldT, oldR) =
      ( DriverEarningAccrualTotals
          { accrualAmount = oldT.accrualAmount + newT.accrualAmount,
            txnCount = oldT.txnCount + newT.txnCount
          },
        newR ++ oldR
      )

-- | List BaseRide legs that credit OwnerLiability.
-- 2-table join (ledger_entry + account); bapId resolved from bookings by caller.
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
-- Payout (from payout_lifecycle_event)
-- ---------------------------------------------------------------------------

-- | Fetch payout event totals from payout_lifecycle_event for a given event type.
fetchPayoutEventTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  DPLE.PayoutLifecycleEventType ->
  UTCTime ->
  UTCTime ->
  m (PayoutEventTotals, [RideRevenueTransactionRow])
fetchPayoutEventTotals merchantOpCityId eventType fromTime toTime = do
  rows <- QPLEExtra.findByMocIdAndEventTypeAndCreatedAtRange merchantOpCityId.getId eventType fromTime toTime
  let (totals, txnRowsRev) = foldl' go (PayoutEventTotals 0 0, []) rows
  pure (totals, reverse txnRowsRev)
  where
    go (acc, rs) entry =
      ( acc {totalAmount = acc.totalAmount + entry.amount, txnCount = acc.txnCount + 1},
        RideRevenueTransactionRow {amount = entry.amount, referenceId = entry.payoutOrderId.getId, txnStatus = show entry.eventType} : rs
      )

-- ---------------------------------------------------------------------------
-- TDS
-- ---------------------------------------------------------------------------

-- | TDS deduction + reimbursement from direct_tax_transaction.
--   TDS deduction query removed — now handled via ledger adjustments (TdsThresholdDeduction config).
fetchTdsTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m (TdsTotals, [RideRevenueTransactionRow], [RideRevenueTransactionRow])
fetchTdsTotals merchantOpCityId fromTime toTime = do
  let findByTreatment treatment =
        QDirectTaxTransaction.findByTdsTreatmentAndDateRange Nothing Nothing merchantOpCityId.getId treatment fromTime toTime
  reimbursedRows <- findByTreatment DDirectTaxTransaction.Reimbursed
  let (totals, reimbursementRowsRev) = foldl' goReimbursement (TdsTotals 0 0 0 0, []) reimbursedRows
  pure (totals, [], reverse reimbursementRowsRev)
  where
    goReimbursement (acc, rs) dtt =
      ( acc {reimbursementAmount = acc.reimbursementAmount + dtt.tdsAmount, reimbursementCount = acc.reimbursementCount + 1},
        mkRow dtt : rs
      )
    mkRow dtt =
      RideRevenueTransactionRow
        { amount = dtt.tdsAmount,
          referenceId = dtt.referenceId,
          txnStatus = show dtt.tdsTreatment
        }

-- ---------------------------------------------------------------------------
-- Ledger adjustments (generic, config-driven)
-- ---------------------------------------------------------------------------

-- | Generic fetch for any POSTED ledger adjustment by category + referenceType.
-- Used by all entries in 'ledgerAdjustmentJVConfigs'.
fetchLedgerAdjustmentTotals ::
  (RideRevenueTotalsFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  DLA.AdjustmentCategory ->
  Text ->
  UTCTime ->
  UTCTime ->
  m (PayoutEventTotals, [RideRevenueTransactionRow])
fetchLedgerAdjustmentTotals merchantOpCityId adjustmentCategory refType fromTime toTime = do
  rows <-
    QLedgerAdjustmentRequestExtra.findPostedByMocIdAndCategoryAndReferenceTypeAndPostedAtRange
      merchantOpCityId
      adjustmentCategory
      refType
      fromTime
      toTime
  let (totals, txnRowsRev) = foldl' go (PayoutEventTotals 0 0, []) rows
  pure (totals, reverse txnRowsRev)
  where
    go (acc, rs) req =
      ( acc {totalAmount = acc.totalAmount + req.amount, txnCount = acc.txnCount + 1},
        RideRevenueTransactionRow {amount = req.amount, referenceId = req.id.getId, txnStatus = show req.status} : rs
      )

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
  m (SubscriptionRevenueTotals, [RideRevenueTransactionRow])
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
        RideRevenueTransactionRow {amount = le.amount, referenceId = le.referenceId, txnStatus = show le.status} : rs
      )
