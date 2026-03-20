{-
   Copyright 2022-23, Juspay India Pvt Ltd

   This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
   as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
   is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the
   GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Finance.Reconciliation
  ( -- Domain-level entry point
    reconcileBookingWithLedgerEntries,
    ReconciliationResult (..),
    -- DSR vs Ledger shared computation (used by job runner)
    DsrVsLedgerResult (..),
    runDsrVsLedgerComparison,
    -- Status map types & helpers
    ReconciliationJobType (..),
    ReconciliationStatusMap (..),
    getReconciliationStatus,
    updateReconciliationStatus,
    mkReconciliationStatusValue,
    getReconStatusForJob,
    updateReconStatus,
    -- Shared pure helpers
    determineRideMode,
    mapBookingStatus,
    dsrVsLedgerRefTypes,
    findLedgerEntry,
    -- Entry construction helpers
    computeReconStatus,
    ReconEntryInput (..),
    mkDefaultReconEntryInput,
    mkReconEntry,
    -- Standardised mismatch reason strings
    reasonUnknownRideMode,
    reasonUnsupportedBookingStatus,
    reasonNoCancellationEntry,
    reasonDriverTakeHomeMismatch,
    reasonSubscriptionCreditMismatch,
    reasonNoMatchingSubscription,
    reasonAmountMismatch,
    reasonNoMatchingPayoutRequest,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Extra.MerchantPaymentMethod as MP
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.Ride as DR
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as IndirectTax
import qualified Lib.Finance.Domain.Types.LedgerEntry as LedgerEntry
import qualified Lib.Finance.Domain.Types.ReconciliationEntry as ReconEntry
import qualified Lib.Finance.Domain.Types.ReconciliationSummary as ReconSummary
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra as QIndirectTax
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerExtra
import SharedLogic.Finance.Wallet
  ( walletReferenceBaseRide,
    walletReferenceCustomerCancellationCharges,
    walletReferenceCustomerCancellationGST,
    walletReferenceDriverCancellationCharges,
    walletReferenceGSTCash,
    walletReferenceGSTOnline,
    walletReferenceParkingCharges,
    walletReferenceTollCharges,
  )
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.FareParameters as QFareParams

-- ────────────────────────────────────────────────────────────────────
-- Reconciliation job type (status map key)
-- ────────────────────────────────────────────────────────────────────

data ReconciliationJobType
  = DSRvsLedger
  | SubscriptionRecon
  | PayoutRecon
  | DSSRvsSubscription
  | PgPaymentVsSubscription
  | PgPayoutVsPayoutRequest
  | OtherRecon Text
  deriving (Show, Eq, Generic)

instance ToJSON ReconciliationJobType where
  toJSON = A.String . jobTypeKey

instance FromJSON ReconciliationJobType where
  parseJSON (A.String "DSRvsLedger") = pure DSRvsLedger
  parseJSON (A.String "SubscriptionRecon") = pure SubscriptionRecon
  parseJSON (A.String "DSSRvsSubscription") = pure DSSRvsSubscription
  parseJSON (A.String "PayoutRecon") = pure PayoutRecon
  parseJSON (A.String "PgPaymentVsSubscription") = pure PgPaymentVsSubscription
  parseJSON (A.String "PgPayoutVsPayoutRequest") = pure PgPayoutVsPayoutRequest
  parseJSON (A.String t) = pure $ OtherRecon t
  parseJSON _ = fail "Invalid ReconciliationJobType"

jobTypeKey :: ReconciliationJobType -> Text
jobTypeKey DSRvsLedger = "DSRvsLedger"
jobTypeKey SubscriptionRecon = "SubscriptionRecon"
jobTypeKey DSSRvsSubscription = "DSSRvsSubscription"
jobTypeKey PayoutRecon = "PayoutRecon"
jobTypeKey PgPaymentVsSubscription = "PgPaymentVsSubscription"
jobTypeKey PgPayoutVsPayoutRequest = "PgPayoutVsPayoutRequest"
jobTypeKey (OtherRecon t) = t

-- ────────────────────────────────────────────────────────────────────
-- Status map (stored as JSON in booking.reconciliationStatus)
-- ────────────────────────────────────────────────────────────────────

newtype ReconciliationStatusMap = ReconciliationStatusMap
  { unReconciliationStatusMap :: HM.HashMap Text ReconSummary.ReconciliationStatus
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReconciliationStatusMap where
  toJSON (ReconciliationStatusMap m) = A.Object $ KeyMap.fromHashMapText $ HM.map toJSON m

instance FromJSON ReconciliationStatusMap where
  parseJSON (A.Object o) =
    ReconciliationStatusMap <$> traverse parseJSON (KeyMap.toHashMapText o)
  parseJSON _ = fail "ReconciliationStatusMap must be a JSON object"

getReconciliationStatus :: Maybe A.Value -> ReconciliationStatusMap
getReconciliationStatus Nothing = ReconciliationStatusMap HM.empty
getReconciliationStatus (Just v) =
  case A.fromJSON v of
    A.Success m -> m
    A.Error _ -> ReconciliationStatusMap HM.empty

getReconStatusForJob :: ReconciliationStatusMap -> ReconciliationJobType -> Maybe ReconSummary.ReconciliationStatus
getReconStatusForJob (ReconciliationStatusMap m) jt = HM.lookup (jobTypeKey jt) m

updateReconStatus :: ReconciliationStatusMap -> ReconciliationJobType -> ReconSummary.ReconciliationStatus -> ReconciliationStatusMap
updateReconStatus (ReconciliationStatusMap m) jt s = ReconciliationStatusMap $ HM.insert (jobTypeKey jt) s m

mkReconciliationStatusValue :: ReconciliationStatusMap -> A.Value
mkReconciliationStatusValue = toJSON

-- ────────────────────────────────────────────────────────────────────
-- Domain-level result (simple)
-- ────────────────────────────────────────────────────────────────────

data ReconciliationResult = ReconciliationResult
  { reconStatus :: ReconSummary.ReconciliationStatus,
    mismatchReason :: Maybe Text,
    expectedValue :: HighPrecMoney,
    actualValue :: HighPrecMoney
  }
  deriving (Show)

-- ────────────────────────────────────────────────────────────────────
-- DSR vs Ledger comparison result (richer, used by both modules)
-- ────────────────────────────────────────────────────────────────────

data DsrVsLedgerResult = DsrVsLedgerResult
  { reconStatus :: ReconSummary.ReconciliationStatus,
    mismatchReason :: Maybe Text,
    expectedStored :: HighPrecMoney,
    actualStored :: HighPrecMoney,
    rideMode :: Maybe ReconEntry.RideMode,
    financeComponent :: ReconEntry.FinanceComponent
  }
  deriving (Show)

-- ────────────────────────────────────────────────────────────────────
-- Standardised mismatch reason strings
-- ────────────────────────────────────────────────────────────────────

reasonUnknownRideMode :: Text
reasonUnknownRideMode = "Unknown ride mode"

reasonUnsupportedBookingStatus :: Text
reasonUnsupportedBookingStatus = "Unsupported booking status"

reasonNoCancellationEntry :: Text
reasonNoCancellationEntry = "No cancellation entry found"

reasonDriverTakeHomeMismatch :: Text
reasonDriverTakeHomeMismatch = "Driver take home mismatch"

reasonSubscriptionCreditMismatch :: Text
reasonSubscriptionCreditMismatch = "Subscription credit mismatch"

reasonNoMatchingSubscription :: Text
reasonNoMatchingSubscription = "No matching subscription purchase found"

reasonAmountMismatch :: Text
reasonAmountMismatch = "Amount mismatch"

reasonNoMatchingPayoutRequest :: Text
reasonNoMatchingPayoutRequest = "No matching credited payout request found"

-- ────────────────────────────────────────────────────────────────────
-- Shared pure helpers
-- ────────────────────────────────────────────────────────────────────

-- | Compute reconciliation status from expected and actual values.
computeReconStatus :: HighPrecMoney -> HighPrecMoney -> ReconEntry.ReconciliationStatus
computeReconStatus expected actual
  | expected == actual = ReconEntry.MATCHED
  | actual > expected = ReconEntry.HIGHER_IN_TARGET
  | otherwise = ReconEntry.LOWER_IN_TARGET

-- | All reference types relevant for DSR vs Ledger reconciliation.
dsrVsLedgerRefTypes :: [Text]
dsrVsLedgerRefTypes =
  [ walletReferenceBaseRide,
    walletReferenceGSTOnline,
    walletReferenceGSTCash,
    walletReferenceCustomerCancellationCharges,
    walletReferenceDriverCancellationCharges,
    walletReferenceCustomerCancellationGST,
    walletReferenceTollCharges,
    walletReferenceParkingCharges
  ]

-- | Find a ledger entry by reference type.
findLedgerEntry :: Text -> [LedgerEntry.LedgerEntry] -> Maybe LedgerEntry.LedgerEntry
findLedgerEntry refType = find (\e -> e.referenceType == refType)

-- | Determine ride mode: ledgerWriteMode takes precedence, then paymentInstrument.
determineRideMode :: Maybe Bool -> Maybe MP.PaymentInstrument -> Maybe ReconEntry.RideMode
determineRideMode (Just True) _ = Just ReconEntry.ONLINE
determineRideMode (Just False) _ = Just ReconEntry.CASH
determineRideMode Nothing (Just MP.Cash) = Just ReconEntry.CASH
determineRideMode Nothing (Just (MP.Card _)) = Just ReconEntry.ONLINE
determineRideMode Nothing (Just (MP.Wallet _)) = Just ReconEntry.ONLINE
determineRideMode Nothing (Just MP.UPI) = Just ReconEntry.ONLINE
determineRideMode Nothing (Just MP.BoothOnline) = Just ReconEntry.ONLINE
determineRideMode Nothing (Just MP.NetBanking) = Just ReconEntry.ONLINE
determineRideMode _ _ = Nothing

-- | Map booking status to reconciliation entry status.
mapBookingStatus :: DB.BookingStatus -> ReconEntry.RideStatus
mapBookingStatus DB.COMPLETED = ReconEntry.COMPLETED
mapBookingStatus DB.CANCELLED = ReconEntry.CANCELLED
mapBookingStatus _ = ReconEntry.CANCELLED

-- ────────────────────────────────────────────────────────────────────
-- ReconEntry construction helper (eliminates boilerplate)
-- ────────────────────────────────────────────────────────────────────

data ReconEntryInput = ReconEntryInput
  { reconType :: ReconEntry.ReconciliationType,
    bookingId :: Maybe Text,
    dcoId :: Maybe Text,
    status :: Maybe ReconEntry.RideStatus,
    mode :: Maybe ReconEntry.RideMode,
    expected :: HighPrecMoney,
    actual :: HighPrecMoney,
    reason :: Maybe Text,
    component :: Maybe ReconEntry.FinanceComponent,
    merchantId :: Maybe Text,
    merchantOperatingCityId :: Maybe Text,
    settlementId :: Maybe Text,
    sourceId :: Maybe Text,
    targetId :: Maybe Text,
    settlementDate :: Maybe UTCTime,
    transactionDate :: Maybe UTCTime,
    rrn :: Maybe Text,
    settlementMode :: Maybe Text
  }

mkDefaultReconEntryInput :: ReconEntry.ReconciliationType -> ReconEntryInput
mkDefaultReconEntryInput rt =
  ReconEntryInput
    { reconType = rt,
      bookingId = Nothing,
      dcoId = Nothing,
      status = Nothing,
      mode = Nothing,
      expected = 0,
      actual = 0,
      reason = Nothing,
      component = Nothing,
      merchantId = Nothing,
      merchantOperatingCityId = Nothing,
      settlementId = Nothing,
      sourceId = Nothing,
      targetId = Nothing,
      settlementDate = Nothing,
      transactionDate = Nothing,
      rrn = Nothing,
      settlementMode = Nothing
    }

mkReconEntry :: ReconEntryInput -> UTCTime -> Id ReconEntry.ReconciliationEntry -> ReconEntry.ReconciliationEntry
mkReconEntry inp now entryId =
  let variance = inp.expected - inp.actual
      reconSt = computeReconStatus inp.expected inp.actual
      mismatchRsn = if reconSt /= ReconEntry.MATCHED then inp.reason else Nothing
   in ReconEntry.ReconciliationEntry
        { id = entryId,
          summaryId = Id "",
          reconciliationDate = now,
          reconciliationType = inp.reconType,
          bookingId = inp.bookingId,
          dcoId = inp.dcoId,
          status = inp.status,
          mode = inp.mode,
          expectedDsrValue = inp.expected,
          actualLedgerValue = inp.actual,
          variance = variance,
          reconStatus = reconSt,
          mismatchReason = mismatchRsn,
          timestamp = now,
          financeComponent = inp.component,
          sourceDetails = Nothing,
          targetDetails = Nothing,
          merchantId = inp.merchantId,
          createdAt = now,
          updatedAt = now,
          merchantOperatingCityId = inp.merchantOperatingCityId,
          settlementId = inp.settlementId,
          sourceId = inp.sourceId,
          targetId = inp.targetId,
          settlementDate = inp.settlementDate,
          transactionDate = inp.transactionDate,
          rrn = inp.rrn,
          settlementMode = inp.settlementMode
        }

-- ────────────────────────────────────────────────────────────────────
-- DSR vs Ledger comparison (pure, shared by BOTH modules)
-- ────────────────────────────────────────────────────────────────────

-- Internal: compare expected vs actual, return mismatch info if different.
compareMaybe :: Text -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe (ReconSummary.ReconciliationStatus, Text)
compareMaybe _label (Just expV) (Just act) | expV == act = Nothing
compareMaybe label (Just e) (Just a)
  | a > e = Just (ReconSummary.HIGHER_IN_TARGET, label <> " mismatch")
  | otherwise = Just (ReconSummary.LOWER_IN_TARGET, label <> " mismatch")
compareMaybe label _ _ = Just (ReconSummary.MISSING_IN_TARGET, label <> " missing")

-- | Find first mismatch from a list of comparison results.
firstMismatch :: [Maybe (ReconSummary.ReconciliationStatus, Text)] -> (ReconSummary.ReconciliationStatus, Maybe Text)
firstMismatch [] = (ReconSummary.MATCHED, Nothing)
firstMismatch (Nothing : rest) = firstMismatch rest
firstMismatch (Just (st, reason) : _) = (st, Just reason)

-- | Online completed ride: compare BaseRide, GSTOnline, TollCharges, and ParkingCharges.
processOnlineCompleted ::
  Maybe HighPrecMoney ->
  Maybe LedgerEntry.LedgerEntry ->
  Maybe HighPrecMoney ->
  Maybe LedgerEntry.LedgerEntry ->
  Maybe HighPrecMoney ->
  Maybe LedgerEntry.LedgerEntry ->
  Maybe HighPrecMoney ->
  Maybe LedgerEntry.LedgerEntry ->
  (ReconSummary.ReconciliationStatus, Maybe Text)
processOnlineCompleted expectedGross baseRideEntry expectedGst gstOnlineEntry expectedToll tollEntry expectedParking parkingEntry =
  firstMismatch
    [ compareMaybe "BaseRide" expectedGross (baseRideEntry <&> (.amount)),
      compareMaybe "GSTOnline" expectedGst (gstOnlineEntry <&> (.amount)),
      compareMaybe "TollCharges" expectedToll (tollEntry <&> (.amount)),
      compareMaybe "ParkingCharges" expectedParking (parkingEntry <&> (.amount))
    ]

-- | Cash completed ride: compare GSTCash vs expectedGst only.
processCashCompleted ::
  Maybe HighPrecMoney ->
  Maybe LedgerEntry.LedgerEntry ->
  (ReconSummary.ReconciliationStatus, Maybe Text)
processCashCompleted expectedGst gstCashEntry =
  case (expectedGst, gstCashEntry <&> (.amount)) of
    (Nothing, Nothing) -> (ReconSummary.MATCHED, Nothing)
    _ -> case compareMaybe "GSTCash" expectedGst (gstCashEntry <&> (.amount)) of
      Just (st, reason) -> (st, Just reason)
      Nothing -> (ReconSummary.MATCHED, Nothing)

-- | Cancelled ride: compare cancellation charges.
-- Customer: ride.cancellationChargesOnCancel vs (custCancelEntry + custCancelGstEntry), expectedGst vs custCancelGstEntry
-- Driver: ride.driverCancellationPenaltyAmount vs driverCancelEntry
processCancelled ::
  Maybe HighPrecMoney -> -- expectedCustCancelTotal (ride.cancellationChargesOnCancel)
  Maybe LedgerEntry.LedgerEntry -> -- custCancelEntry
  Maybe LedgerEntry.LedgerEntry -> -- custCancelGstEntry
  Maybe HighPrecMoney -> -- expectedDriverCancelPenalty (ride.driverCancellationPenaltyAmount)
  Maybe LedgerEntry.LedgerEntry -> -- driverCancelEntry
  Maybe HighPrecMoney -> -- expectedGst (for customer cancellation GST check)
  (ReconSummary.ReconciliationStatus, Maybe Text)
processCancelled expectedCustTotal custCancelEntry custCancelGstEntry expectedDriverPenalty driverCancelEntry expectedGst =
  case (custCancelEntry <&> (.amount), driverCancelEntry <&> (.amount)) of
    (Just custAmt, _) ->
      let actualTotal = custAmt + fromMaybe 0 (custCancelGstEntry <&> (.amount))
       in firstMismatch
            [ compareMaybe "CustomerCancellationTotal" expectedCustTotal (Just actualTotal),
              compareMaybe "CustomerCancellationGST" expectedGst (custCancelGstEntry <&> (.amount))
            ]
    (_, Just driverAmt) ->
      firstMismatch
        [ compareMaybe "DriverCancellationCharges" expectedDriverPenalty (Just driverAmt)
        ]
    _ -> case (expectedCustTotal, expectedDriverPenalty) of
      (Nothing, Nothing) -> (ReconSummary.MATCHED, Nothing)
      _ -> (ReconSummary.MISSING_IN_TARGET, Just reasonNoCancellationEntry)

-- ────────────────────────────────────────────────────────────────────
-- Unified DSR vs Ledger comparison (single source of truth)
--   Used by both domain (reconcileBookingWithLedgerEntries) and
--   job runner (processDsrVsLedger).
-- ────────────────────────────────────────────────────────────────────

-- | Core comparison logic for DSR vs Ledger. Accepts booking, optional ride,
--   and pre-fetched ledger entries + indirect tax transactions.
runDsrVsLedgerComparison ::
  DB.Booking ->
  Maybe DR.Ride ->
  Maybe DFP.FareParameters ->
  [LedgerEntry.LedgerEntry] ->
  [IndirectTax.IndirectTaxTransaction] ->
  DsrVsLedgerResult
runDsrVsLedgerComparison booking mbRide mbRideFareParams ledgerEntries indirectTaxTxns =
  let mode = determineRideMode booking.ledgerWriteMode booking.paymentInstrument

      -- Ledger entries by reference type
      baseRideEntry = findLedgerEntry walletReferenceBaseRide ledgerEntries
      gstOnlineEntry = findLedgerEntry walletReferenceGSTOnline ledgerEntries
      gstCashEntry = findLedgerEntry walletReferenceGSTCash ledgerEntries
      custCancelEntry = findLedgerEntry walletReferenceCustomerCancellationCharges ledgerEntries
      driverCancelEntry = findLedgerEntry walletReferenceDriverCancellationCharges ledgerEntries
      custCancelGstEntry = findLedgerEntry walletReferenceCustomerCancellationGST ledgerEntries
      tollEntry = findLedgerEntry walletReferenceTollCharges ledgerEntries
      parkingEntry = findLedgerEntry walletReferenceParkingCharges ledgerEntries

      -- Expected values from DSR
      relevantType = case booking.status of
        DB.COMPLETED -> IndirectTax.RideFare
        _ -> IndirectTax.Cancellation
      expectedGst = (.totalGstAmount) <$> find (\t -> t.transactionType == relevantType) indirectTaxTxns

      fareParams = fromMaybe booking.fareParams mbRideFareParams
      baseFare = fromMaybe booking.estimatedFare (mbRide >>= (.fare))
      tollCharges = fromMaybe 0 $ (mbRide >>= (.tollCharges)) <|> booking.tollCharges
      parkingCharges = fromMaybe 0 fareParams.parkingCharge
      govtCharges = fromMaybe 0 fareParams.govtCharges
      expectedGross = Just $ baseFare - tollCharges - parkingCharges - govtCharges
      expectedToll = Just tollCharges
      expectedParking = Just parkingCharges

      -- Cancellation expected values
      expectedCustTotal = mbRide >>= (.cancellationChargesOnCancel)
      expectedDriverPenalty = mbRide >>= (.driverCancellationPenaltyAmount)

      -- Run comparison
      (st, reason) = case booking.status of
        DB.COMPLETED -> case mode of
          Just ReconEntry.ONLINE -> processOnlineCompleted expectedGross baseRideEntry expectedGst gstOnlineEntry expectedToll tollEntry expectedParking parkingEntry
          Just ReconEntry.CASH -> processCashCompleted expectedGst gstCashEntry
          Nothing -> (ReconSummary.MISSING_IN_TARGET, Just reasonUnknownRideMode)
        DB.CANCELLED -> processCancelled expectedCustTotal custCancelEntry custCancelGstEntry expectedDriverPenalty driverCancelEntry expectedGst
        _ -> (ReconSummary.MISSING_IN_TARGET, Just reasonUnsupportedBookingStatus)

      -- Determine finance component based on booking status and cancellation type
      component = case booking.status of
        DB.COMPLETED -> ReconEntry.GROSS_RIDE_FARE
        DB.CANCELLED
          | isJust (custCancelEntry <&> (.amount)) -> ReconEntry.USER_CANCELLATION
          | isJust (driverCancelEntry <&> (.amount)) -> ReconEntry.DRIVER_CANCELLATION
          | otherwise -> ReconEntry.GROSS_RIDE_FARE
        _ -> ReconEntry.GROSS_RIDE_FARE

      -- Stored values for entry
      (expStored, actStored) = case (booking.status, mode) of
        (DB.COMPLETED, Just ReconEntry.CASH) ->
          (fromMaybe 0 expectedGst, fromMaybe 0 $ gstCashEntry <&> (.amount))
        (DB.CANCELLED, _) ->
          let custTotal = (+ fromMaybe 0 (custCancelGstEntry <&> (.amount))) <$> (custCancelEntry <&> (.amount))
           in ( fromMaybe 0 $ expectedCustTotal <|> expectedDriverPenalty,
                fromMaybe 0 $ custTotal <|> (driverCancelEntry <&> (.amount))
              )
        _ ->
          (fromMaybe 0 expectedGross, fromMaybe 0 $ baseRideEntry <&> (.amount))
   in DsrVsLedgerResult st reason expStored actStored mode component

-- ────────────────────────────────────────────────────────────────────
-- Domain-level entry point
-- ────────────────────────────────────────────────────────────────────

reconcileBookingWithLedgerEntries ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconciliationJobType ->
  DB.Booking ->
  Maybe DR.Ride ->
  m ReconciliationResult
reconcileBookingWithLedgerEntries jobType booking mbRide = do
  let lockKey = "Reconciliation:Booking:" <> booking.id.getId

  Hedis.withLockRedisAndReturnValue lockKey 60 $ do
    let existingStatusMap = getReconciliationStatus booking.reconciliationStatus
    case getReconStatusForJob existingStatusMap jobType of
      Just existingStatus -> do
        logInfo $ "Booking " <> booking.id.getId <> " already reconciled for " <> show jobType <> ": " <> show existingStatus
        pure $
          ReconciliationResult
            { reconStatus = existingStatus,
              mismatchReason = Just "Reconciliation already performed for this job type",
              expectedValue = booking.estimatedFare,
              actualValue = booking.estimatedFare
            }
      Nothing -> do
        ledgerEntries <- QLedgerExtra.findByReferenceIn dsrVsLedgerRefTypes booking.id.getId
        indirectTaxTxns <- QIndirectTax.findByReferenceId booking.id.getId
        rideFareParams <- maybe (pure Nothing) (\ride -> maybe (pure Nothing) (QFareParams.findById) ride.fareParametersId) mbRide
        let dsrResult = runDsrVsLedgerComparison booking mbRide rideFareParams ledgerEntries indirectTaxTxns
            result = ReconciliationResult dsrResult.reconStatus dsrResult.mismatchReason dsrResult.expectedStored dsrResult.actualStored
        let updatedStatusMap = updateReconStatus existingStatusMap jobType result.reconStatus
            newStatusValue = Just $ mkReconciliationStatusValue updatedStatusMap
        QBooking.updateReconciliationStatus booking.id newStatusValue
        logInfo $ "Booking " <> booking.id.getId <> " reconciled for " <> show jobType <> " with status: " <> show result.reconStatus
        pure result

-- ────────────────────────────────────────────────────────────────────
-- Convenience: update booking's reconciliation status
-- ────────────────────────────────────────────────────────────────────

updateReconciliationStatus ::
  ( BeamFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id DB.Booking ->
  ReconciliationJobType ->
  ReconSummary.ReconciliationStatus ->
  m ()
updateReconciliationStatus bookingId jobType status = do
  mbBooking <- QBooking.findById bookingId
  case mbBooking of
    Nothing -> logError $ "Booking not found: " <> bookingId.getId
    Just booking -> do
      let existingMap = getReconciliationStatus booking.reconciliationStatus
          updatedMap = updateReconStatus existingMap jobType status
          newValue = Just $ mkReconciliationStatusValue updatedMap
      QBooking.updateReconciliationStatus bookingId newValue
