{-
  Prepaid Subscription :: DSR ↔ Net Earnings Ledger.

  Multi-component recon. For each COMPLETED / CANCELLED booking in the
  chunk we compare a bag of sub-components between DSR-derived expected
  values and the ledger's actual entries + tax transactions.

  Ports doReconciliationDsrVsLedger + runDsrVsLedgerComparison from the
  retired SharedLogic.Finance.Reconciliation module.

  Sub-component comparison logic (processOnlineCompleted / processCashCompleted
  / processCancelled) is preserved verbatim, restated here in local pure
  helpers that consume the JSON blobs packed into srcMeta / tgtMeta by
  the fetchers.

  The entry's aggregate expectedAmount / actualAmount is the "storage"
  value chosen by the same rule as before (Cash-completed → tax
  expected/actual; Cancelled → cust-total / driver-penalty; else → gross
  + toll + parking).
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Reconciliation.Recipes.PrepaidDsrVsLedger
  ( recipe,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Time (nominalDay)
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Extra.MerchantPaymentMethod as MP
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.Ride as DR
import Kernel.Prelude
import qualified Kernel.Types.Common as KTC
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction as DirectTax
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as IndirectTax
import qualified Lib.Finance.Domain.Types.LedgerEntry as LedgerEntry
import Lib.Finance.Reconciliation.Recipe (Recipe (..))
import qualified Lib.Finance.Reconciliation.Types as ReconT
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.DirectTaxTransactionExtra as QDirectTaxExtra
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra as QIndirectTaxExtra
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerExtra
import qualified SharedLogic.Finance.Reconciliation.EntitySync as EntitySync
import SharedLogic.Finance.Wallet
  ( walletReferenceBaseRide,
    walletReferenceCommissionCash,
    walletReferenceCommissionOnline,
    walletReferenceCustomerCancellationCharges,
    walletReferenceCustomerCancellationGST,
    walletReferenceDriverCancellationCharges,
    walletReferenceGSTCash,
    walletReferenceGSTOnline,
    walletReferenceParkingCharges,
    walletReferenceTDSDeductionCancellation,
    walletReferenceTDSDeductionCash,
    walletReferenceTDSDeductionOnline,
    walletReferenceTollCharges,
    walletReferenceVATCash,
    walletReferenceVATOnline,
  )
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.FareParameters as QFareParams
import qualified Storage.Queries.Ride as QRide

-- ─── Sub-component payload ────────────────────────────────────────────────

data BookingMode = ONLINE | CASH
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data BookingStatusTag = COMPLETED | CANCELLED
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Source-side breakdown packed into 'ReconT.srcMeta'.
data ExpectedComponents = ExpectedComponents
  { bookingStatus :: BookingStatusTag,
    mode :: Maybe BookingMode,
    expectedGross :: Maybe KTC.HighPrecMoney,
    expectedTax :: Maybe KTC.HighPrecMoney,
    expectedToll :: Maybe KTC.HighPrecMoney,
    expectedParking :: Maybe KTC.HighPrecMoney,
    expectedTds :: Maybe KTC.HighPrecMoney,
    expectedCustCancelTotal :: Maybe KTC.HighPrecMoney,
    expectedDriverCancelPenalty :: Maybe KTC.HighPrecMoney
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Target-side breakdown packed into 'ReconT.tgtMeta'.
data ActualComponents = ActualComponents
  { baseRide :: Maybe KTC.HighPrecMoney,
    gstOnline :: Maybe KTC.HighPrecMoney,
    gstCash :: Maybe KTC.HighPrecMoney,
    vatOnline :: Maybe KTC.HighPrecMoney,
    vatCash :: Maybe KTC.HighPrecMoney,
    custCancel :: Maybe KTC.HighPrecMoney,
    driverCancel :: Maybe KTC.HighPrecMoney,
    custCancelGst :: Maybe KTC.HighPrecMoney,
    toll :: Maybe KTC.HighPrecMoney,
    parking :: Maybe KTC.HighPrecMoney,
    tdsOnline :: Maybe KTC.HighPrecMoney,
    tdsCash :: Maybe KTC.HighPrecMoney,
    tdsCancel :: Maybe KTC.HighPrecMoney
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- ─── Recipe ────────────────────────────────────────────────────────────────

recipe ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Recipe m
recipe =
  Recipe
    { spec = mySpec,
      chunkPlan = ReconT.ByHour,
      -- Internal-only recon: ledger posts happen inline with the booking.
      settlementBuffer = nominalDay,
      grouping = ReconT.Individual,
      fetchSourceChunk = fetchSources,
      fetchTargetsById = fetchTargets,
      -- Sweep re-fetch not yet implemented: internal recon on completed
      -- bookings, which never revert. Sweep still force-closes on age.
      -- TODO: implement to catch late-arriving ledger writes.
      fetchSourcesByIds = \_ _ -> pure [],
      sweepInterval = 4 * nominalDay,
      maxOpenAge = 30 * nominalDay,
      fetchOrphanTargets = Nothing,
      classify = classifyMultiComponent,
      syncSourceStatus = Just (EntitySync.syncBookingStatus mySpec)
    }
  where
    mySpec = ReconT.ReconciliationSpec ReconT.PREPAID_SUBSCRIPTION ReconT.DSR ReconT.LEDGER

-- ─── Sources ───────────────────────────────────────────────────────────────

fetchSources ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  ReconT.DateRange ->
  m [ReconT.SourceRecord]
fetchSources scope range = do
  bookings <-
    QBooking.findAllByStatusAndDateRange
      (Id scope.merchantOperatingCityId)
      [DB.COMPLETED, DB.CANCELLED]
      range.from
      range.to

  -- Bulk-fetch every intermediate table in one shot; index everything by the
  -- key the per-booking lambda needs so the actual computation stays pure.
  let bookingIds = map (.id) bookings
      bookingIdTexts = map (.getId) bookingIds
  allRides <- QRide.findRidesByBookingId bookingIds
  let ridesByBookingId = HM.fromList [(r.bookingId.getId, r) | r <- allRides]
      fareParamIds = mapMaybe (.fareParametersId) allRides
  allFareParams <- QFareParams.findAllIn fareParamIds
  let fareParamsById = HM.fromList [(fp.id.getId, fp) | fp <- allFareParams]
  allIndirectTax <- QIndirectTaxExtra.findByReferenceIds bookingIdTexts
  let indirectByBooking = HM.fromListWith (<>) [(t.referenceId, [t]) | t <- allIndirectTax]
  allDirectTax <- QDirectTaxExtra.findByReferenceIds bookingIdTexts
  let directByBooking = HM.fromListWith (<>) [(t.referenceId, [t]) | t <- allDirectTax]

  pure $
    flip map bookings $ \booking ->
      let bId = booking.id.getId
          mbRide = HM.lookup bId ridesByBookingId
          mbFareParams = mbRide >>= (.fareParametersId) >>= \fpId -> HM.lookup fpId.getId fareParamsById
          indirectTaxTxns = fromMaybe [] (HM.lookup bId indirectByBooking)
          directTaxTxns = fromMaybe [] (HM.lookup bId directByBooking)
          expected = computeExpected booking mbRide mbFareParams indirectTaxTxns directTaxTxns
          expStored = pickExpectedStored expected
          dcoId = (\r -> r.driverId.getId) <$> mbRide
       in ReconT.SourceRecord
            { srcId = bId,
              srcEntityId = Just bId,
              srcPartyId = dcoId,
              srcAmount = expStored,
              srcMatchKey = Just bId,
              srcComponent = Just (financeComponentLabel booking expected),
              srcMeta = Just (A.toJSON expected),
              srcTimestamp = booking.createdAt,
              srcLifecycle = ReconT.Settled -- COMPLETED / CANCELLED bookings are terminal.
            }

-- ─── Targets ───────────────────────────────────────────────────────────────

fetchTargets ::
  ( BeamFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  HS.HashSet Text ->
  m [ReconT.TargetRecord]
fetchTargets _scope bookingIds = do
  -- Single indexed query for the whole chunk instead of one per booking.
  allEntries <-
    QLedgerExtra.findByReferenceTypesAndReferenceIds allRefTypes (HS.toList bookingIds)
  let byBooking :: HM.HashMap Text [LedgerEntry.LedgerEntry]
      byBooking = HM.fromListWith (<>) [(e.referenceId, [e]) | e <- allEntries]
  pure
    [ let actual = computeActualFromLedger entries
          actStored = pickActualStored actual
       in ReconT.TargetRecord
            { tgtId = bId,
              tgtMatchKey = bId,
              tgtAmount = actStored,
              tgtMeta = Just (A.toJSON actual),
              tgtSettlementId = Nothing,
              tgtSettlementDate = Nothing,
              tgtSettlementMode = Nothing,
              tgtRrn = Nothing,
              tgtTransactionDate = firstEntryTimestamp entries
            }
      | bId <- HS.toList bookingIds,
        let entries = fromMaybe [] (HM.lookup bId byBooking)
    ]
  where
    allRefTypes =
      [ walletReferenceBaseRide,
        walletReferenceGSTOnline,
        walletReferenceGSTCash,
        walletReferenceVATOnline,
        walletReferenceVATCash,
        walletReferenceCustomerCancellationCharges,
        walletReferenceDriverCancellationCharges,
        walletReferenceCustomerCancellationGST,
        walletReferenceTollCharges,
        walletReferenceParkingCharges,
        walletReferenceTDSDeductionOnline,
        walletReferenceTDSDeductionCash,
        walletReferenceTDSDeductionCancellation,
        walletReferenceCommissionOnline,
        walletReferenceCommissionCash
      ]

    firstEntryTimestamp = fmap (.createdAt) . listToMaybe

-- ─── Classifier (multi-component) ─────────────────────────────────────────

-- Only Individual grouping is used for this recipe, so we look at the
-- first source + first target. In-flight sources short-circuit as with
-- 'defaultClassify'.
classifyMultiComponent ::
  [ReconT.SourceRecord] ->
  [ReconT.TargetRecord] ->
  ReconT.ReconResult
classifyMultiComponent srcs _
  | any ((== ReconT.InFlight) . ReconT.srcLifecycle) srcs =
    ReconT.ReconResult ReconT.AWAITING_SETTLEMENT Nothing
classifyMultiComponent [] [] = ReconT.ReconResult ReconT.MATCHED Nothing
classifyMultiComponent [] _ =
  ReconT.ReconResult ReconT.MISSING_IN_SOURCE (Just "Source record missing")
classifyMultiComponent (_ : _) [] =
  ReconT.ReconResult ReconT.MISSING_IN_TARGET (Just "Target record missing")
classifyMultiComponent (s : _) (t : _) =
  let expE = decodeMeta s.srcMeta :: Maybe ExpectedComponents
      actE = decodeMeta t.tgtMeta :: Maybe ActualComponents
   in case (expE, actE) of
        (Just expected, Just actual) -> compareComponents expected actual
        -- Fail closed: a decode failure means the classifier can't do its
        -- job. Reporting MATCHED here would let a change to the meta JSON
        -- shape turn the whole recipe silently green.
        _ -> ReconT.ReconResult ReconT.MISSING_IN_TARGET (Just "Per-component metadata missing or malformed")

decodeMeta :: (FromJSON a) => Maybe A.Value -> Maybe a
decodeMeta = (>>= A.parseMaybe A.parseJSON)

-- | Per-component comparison. Mirrors processOnlineCompleted /
--   processCashCompleted / processCancelled from the retired
--   SharedLogic.Finance.Reconciliation module.
compareComponents :: ExpectedComponents -> ActualComponents -> ReconT.ReconResult
compareComponents expected actual =
  case (expected.bookingStatus, expected.mode) of
    (COMPLETED, Just ONLINE) ->
      firstMismatchOfPairs
        [ ("BaseRide", expected.expectedGross, actual.baseRide),
          ("TaxOnline", expected.expectedTax, actual.gstOnline <|> actual.vatOnline),
          ("TollCharges", expected.expectedToll, actual.toll),
          ("ParkingCharges", expected.expectedParking, actual.parking),
          ("TDSOnline", expected.expectedTds, actual.tdsOnline)
        ]
    (COMPLETED, Just CASH) ->
      firstMismatchOfPairs
        [ ("TaxCash", expected.expectedTax, actual.gstCash <|> actual.vatCash),
          ("TDSCash", expected.expectedTds, actual.tdsCash)
        ]
    (COMPLETED, Nothing) ->
      ReconT.ReconResult ReconT.MISSING_IN_TARGET (Just "Unknown ride mode")
    (CANCELLED, _) -> compareCancellation expected actual
  where
    compareCancellation e a =
      case (a.custCancel, a.driverCancel) of
        (Just custAmt, _) ->
          let actualTotal = custAmt + fromMaybe 0 a.custCancelGst
           in firstMismatchOfPairs
                [ ("CustomerCancellationTotal", e.expectedCustCancelTotal, Just actualTotal),
                  ("CustomerCancellationGST", e.expectedTax, a.custCancelGst),
                  ("TDSCancellation", e.expectedTds, a.tdsCancel)
                ]
        (_, Just driverAmt) ->
          firstMismatchOfPairs
            [ ("DriverCancellationCharges", e.expectedDriverCancelPenalty, Just driverAmt),
              ("TDSCancellation", e.expectedTds, a.tdsCancel)
            ]
        _ ->
          case (e.expectedCustCancelTotal, e.expectedDriverCancelPenalty) of
            (Nothing, Nothing) -> ReconT.ReconResult ReconT.MATCHED Nothing
            _ -> ReconT.ReconResult ReconT.MISSING_IN_TARGET (Just "No cancellation entry found")

firstMismatchOfPairs :: [(Text, Maybe KTC.HighPrecMoney, Maybe KTC.HighPrecMoney)] -> ReconT.ReconResult
firstMismatchOfPairs [] = ReconT.ReconResult ReconT.MATCHED Nothing
firstMismatchOfPairs ((label, expV, actV) : rest) =
  case compareMaybe label expV actV of
    Nothing -> firstMismatchOfPairs rest
    Just r -> r

compareMaybe :: Text -> Maybe KTC.HighPrecMoney -> Maybe KTC.HighPrecMoney -> Maybe ReconT.ReconResult
compareMaybe _label (Just e) (Just a) | e == a = Nothing
compareMaybe label (Just e) (Just a)
  | a > e = Just (ReconT.ReconResult ReconT.HIGHER_IN_TARGET (Just (label <> " mismatch")))
  | otherwise = Just (ReconT.ReconResult ReconT.LOWER_IN_TARGET (Just (label <> " mismatch")))
compareMaybe label _ _ = Just (ReconT.ReconResult ReconT.MISSING_IN_TARGET (Just (label <> " missing")))

-- ─── Expected-side computation ────────────────────────────────────────────

computeExpected ::
  DB.Booking ->
  Maybe DR.Ride ->
  Maybe DFP.FareParameters ->
  [IndirectTax.IndirectTaxTransaction] ->
  [DirectTax.DirectTaxTransaction] ->
  ExpectedComponents
computeExpected booking mbRide mbFareParams indirectTaxTxns directTaxTxns =
  let bookingStatusTag = case booking.status of
        DB.COMPLETED -> COMPLETED
        _ -> CANCELLED
      modeTag = determineMode booking.ledgerWriteMode booking.paymentInstrument
      relevantIndirect = case booking.status of
        DB.COMPLETED -> IndirectTax.RideFare
        _ -> IndirectTax.Cancellation
      mbIndirectTxn = find (\t -> t.transactionType == relevantIndirect) indirectTaxTxns
      expectedTax = do
        txn <- mbIndirectTxn
        txn.totalTaxAmount <|> Just txn.totalGstAmount
      relevantDirect = case booking.status of
        DB.COMPLETED -> DirectTax.RideFare
        _ -> DirectTax.Cancellation
      expectedTds = (.tdsAmount) <$> find (\t -> t.transactionType == relevantDirect) directTaxTxns
      fareParams = fromMaybe booking.fareParams mbFareParams
      baseFare = fromMaybe booking.estimatedFare (mbRide >>= (.fare))
      tollCharges = fromMaybe 0 $ (mbRide >>= (.tollCharges)) <|> booking.tollCharges
      parkingCharges = fromMaybe 0 fareParams.parkingCharge
      govtCharges = fromMaybe 0 fareParams.govtCharges
      expectedGross = Just (baseFare - tollCharges - parkingCharges - govtCharges)
      expectedToll = Just tollCharges
      expectedParking = Just parkingCharges
      expectedCust = mbRide >>= (.cancellationChargesOnCancel)
      expectedDriverPen = mbRide >>= (.driverCancellationPenaltyAmount)
   in ExpectedComponents
        { bookingStatus = bookingStatusTag,
          mode = modeTag,
          expectedGross = expectedGross,
          expectedTax = expectedTax,
          expectedToll = expectedToll,
          expectedParking = expectedParking,
          expectedTds = expectedTds,
          expectedCustCancelTotal = expectedCust,
          expectedDriverCancelPenalty = expectedDriverPen
        }

determineMode :: Maybe Bool -> Maybe MP.PaymentInstrument -> Maybe BookingMode
determineMode (Just True) _ = Just ONLINE
determineMode (Just False) _ = Just CASH
determineMode Nothing (Just MP.Cash) = Just CASH
determineMode Nothing (Just (MP.Card _)) = Just ONLINE
determineMode Nothing (Just (MP.Wallet _)) = Just ONLINE
determineMode Nothing (Just MP.UPI) = Just ONLINE
determineMode Nothing (Just MP.BoothOnline) = Just ONLINE
determineMode Nothing (Just MP.NetBanking) = Just ONLINE
determineMode _ _ = Nothing

pickExpectedStored :: ExpectedComponents -> KTC.HighPrecMoney
pickExpectedStored e = case (e.bookingStatus, e.mode) of
  (COMPLETED, Just CASH) -> fromMaybe 0 e.expectedTax
  (CANCELLED, _) -> fromMaybe 0 (e.expectedCustCancelTotal <|> e.expectedDriverCancelPenalty)
  _ ->
    fromMaybe 0 e.expectedGross
      + fromMaybe 0 e.expectedToll
      + fromMaybe 0 e.expectedParking

pickActualStored :: ActualComponents -> KTC.HighPrecMoney
pickActualStored a =
  fromMaybe 0 a.baseRide
    + fromMaybe 0 a.toll
    + fromMaybe 0 a.parking

financeComponentLabel :: DB.Booking -> ExpectedComponents -> Text
financeComponentLabel booking _e = case booking.status of
  DB.COMPLETED -> "GROSS_RIDE_FARE"
  DB.CANCELLED -> "CANCELLATION"
  _ -> "GROSS_RIDE_FARE"

-- ─── Actual-side computation ──────────────────────────────────────────────

computeActualFromLedger :: [LedgerEntry.LedgerEntry] -> ActualComponents
computeActualFromLedger entries =
  let byType = HM.fromListWith const [(e.referenceType, e.amount) | e <- entries]
      look k = HM.lookup k byType
   in ActualComponents
        { baseRide = look walletReferenceBaseRide,
          gstOnline = look walletReferenceGSTOnline,
          gstCash = look walletReferenceGSTCash,
          vatOnline = look walletReferenceVATOnline,
          vatCash = look walletReferenceVATCash,
          custCancel = look walletReferenceCustomerCancellationCharges,
          driverCancel = look walletReferenceDriverCancellationCharges,
          custCancelGst = look walletReferenceCustomerCancellationGST,
          toll = look walletReferenceTollCharges,
          parking = look walletReferenceParkingCharges,
          tdsOnline = look walletReferenceTDSDeductionOnline,
          tdsCash = look walletReferenceTDSDeductionCash,
          tdsCancel = look walletReferenceTDSDeductionCancellation
        }
