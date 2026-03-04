{-
   Copyright 2022-23, Juspay India Pvt Ltd

   This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
   as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
   is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the
   GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Finance.Reconciliation
  ( reconcileBookingWithLedgerEntries,
    ReconciliationResult (..),
    ReconciliationJobType (..),
    ReconciliationStatusMap (..),
    getReconciliationStatus,
    updateReconciliationStatus,
    mkReconciliationStatusValue,
    getReconStatusForJob,
    updateReconStatus,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Extra.MerchantPaymentMethod as MP
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
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedger
import qualified Storage.Queries.Booking as QBooking

-- | Types of reconciliation jobs that can update the status
data ReconciliationJobType
  = DSRvsLedger
  | SubscriptionRecon
  | PayoutRecon
  | DSSRvsSubscription
  | OtherRecon Text
  deriving (Show, Eq, Generic)

instance ToJSON ReconciliationJobType where
  toJSON DSRvsLedger = A.String "DSRvsLedger"
  toJSON SubscriptionRecon = A.String "SubscriptionRecon"
  toJSON DSSRvsSubscription = A.String "DSSRvsSubscription"
  toJSON PayoutRecon = A.String "PayoutRecon"
  toJSON (OtherRecon t) = A.String t

instance FromJSON ReconciliationJobType where
  parseJSON (A.String "DSRvsLedger") = pure DSRvsLedger
  parseJSON (A.String "SubscriptionRecon") = pure SubscriptionRecon
  parseJSON (A.String "DSSRvsSubscription") = pure DSSRvsSubscription
  parseJSON (A.String "PayoutRecon") = pure PayoutRecon
  parseJSON (A.String t) = pure $ OtherRecon t
  parseJSON _ = fail "Invalid ReconciliationJobType"

-- | Map of reconciliation job type to status
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

-- | Result of reconciliation at domain level
data ReconciliationResult = ReconciliationResult
  { reconStatus :: ReconSummary.ReconciliationStatus,
    mismatchReason :: Maybe Text,
    expectedValue :: HighPrecMoney,
    actualValue :: HighPrecMoney
  }
  deriving (Show)

-- | Parse reconciliation status from JSON Value
getReconciliationStatus :: Maybe A.Value -> ReconciliationStatusMap
getReconciliationStatus Nothing = ReconciliationStatusMap HM.empty
getReconciliationStatus (Just v) =
  case A.fromJSON v of
    A.Success m -> m
    A.Error _ -> ReconciliationStatusMap HM.empty

-- | Get status for a specific job type
getReconStatusForJob :: ReconciliationStatusMap -> ReconciliationJobType -> Maybe ReconSummary.ReconciliationStatus
getReconStatusForJob (ReconciliationStatusMap m) jobType =
  HM.lookup (getJobTypeKey jobType) m
  where
    getJobTypeKey DSRvsLedger = "DSRvsLedger"
    getJobTypeKey SubscriptionRecon = "SubscriptionRecon"
    getJobTypeKey DSSRvsSubscription = "DSSRvsSubscription"
    getJobTypeKey PayoutRecon = "PayoutRecon"
    getJobTypeKey (OtherRecon t) = t

-- | Update status for a specific job type
updateReconStatus :: ReconciliationStatusMap -> ReconciliationJobType -> ReconSummary.ReconciliationStatus -> ReconciliationStatusMap
updateReconStatus (ReconciliationStatusMap m) jobType status =
  ReconciliationStatusMap $ HM.insert (getJobTypeKey jobType) status m
  where
    getJobTypeKey DSRvsLedger = "DSRvsLedger"
    getJobTypeKey SubscriptionRecon = "SubscriptionRecon"
    getJobTypeKey DSSRvsSubscription = "DSSRvsSubscription"
    getJobTypeKey PayoutRecon = "PayoutRecon"
    getJobTypeKey (OtherRecon t) = t

-- | Create a JSON Value from status map
mkReconciliationStatusValue :: ReconciliationStatusMap -> A.Value
mkReconciliationStatusValue = toJSON

-- | Main function to reconcile a booking with its ledger entries
-- This should be called after ledger entries are created for a booking
-- Note: Only updates reconciliationStatus if it's not already set for this job type (idempotent)
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

  -- Use Redis lock to prevent concurrent reconciliation
  Hedis.withLockRedisAndReturnValue lockKey 60 $ do
    -- Parse existing reconciliation status
    let existingStatusMap = getReconciliationStatus booking.reconciliationStatus

    -- Check if this job type already has a status (idempotency check)
    case getReconStatusForJob existingStatusMap jobType of
      Just existingStatus -> do
        logInfo $ "Booking " <> booking.id.getId <> " already has reconciliation status for " <> show jobType <> ": " <> show existingStatus <> ". Skipping reconciliation."
        -- Return a result reflecting the existing status
        pure $
          ReconciliationResult
            { reconStatus = existingStatus,
              mismatchReason = Just "Reconciliation already performed for this job type",
              expectedValue = booking.estimatedFare,
              actualValue = booking.estimatedFare
            }
      Nothing -> do
        -- Get all ledger entries for this booking
        ledgerEntries <- getLedgerEntriesForBooking booking.id.getId

        -- Perform reconciliation based on booking status and payment mode
        result <- performReconciliation booking mbRide ledgerEntries

        -- Update the status map with the new status for this job type
        let updatedStatusMap = updateReconStatus existingStatusMap jobType result.reconStatus
            newStatusValue = Just $ mkReconciliationStatusValue updatedStatusMap

        -- Update booking's reconciliation status
        QBooking.updateReconciliationStatus booking.id newStatusValue

        logInfo $ "Booking " <> booking.id.getId <> " reconciled for " <> show jobType <> " with status: " <> show result.reconStatus
        pure result

-- | Fetch all ledger entries for a booking
getLedgerEntriesForBooking ::
  ( BeamFlow m r,
    MonadFlow m
  ) =>
  Text ->
  m [LedgerEntry.LedgerEntry]
getLedgerEntriesForBooking bookingId = do
  refTypes <-
    sequence
      [ QLedger.findByReference "BaseRide" bookingId,
        QLedger.findByReference "GSTOnline" bookingId,
        QLedger.findByReference "GSTCash" bookingId,
        QLedger.findByReference "UserCancellation" bookingId,
        QLedger.findByReference "DriverCancellation" bookingId
      ]
  pure $ concat refTypes

-- | Perform the actual reconciliation logic
performReconciliation ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  DB.Booking ->
  Maybe DR.Ride ->
  [LedgerEntry.LedgerEntry] ->
  m ReconciliationResult
performReconciliation booking _mbRide ledgerEntries = do
  let rideMode = determineRideMode booking.paymentInstrument

  -- Find corresponding ledger entries
  let baseRideEntry = find (\e -> e.referenceType == "BaseRide") ledgerEntries
      gstOnlineEntry = find (\e -> e.referenceType == "GSTOnline") ledgerEntries
      gstCashEntry = find (\e -> e.referenceType == "GSTCash") ledgerEntries
      userCancellationEntry = find (\e -> e.referenceType == "UserCancellation") ledgerEntries
      driverCancellationEntry = find (\e -> e.referenceType == "DriverCancellation") ledgerEntries

  -- Get expected GST from indirect_tax_transaction
  indirectTaxTxns <- QIndirectTax.findByReferenceId booking.id.getId
  let relevantType = case booking.status of
        DB.COMPLETED -> IndirectTax.RideFare
        _ -> IndirectTax.Cancellation
      mbIndirectTxn = find (\t -> t.transactionType == relevantType) indirectTaxTxns
      expectedGstFromTax = (.totalGstAmount) <$> mbIndirectTxn
      expectedGstFallback = (* 0.05) <$> calculateExpectedGross booking
      expectedGst = expectedGstFromTax <|> expectedGstFallback

  -- Get expected gross value
  let expectedGross = calculateExpectedGross booking

  -- Determine status based on booking status and payment mode
  case booking.status of
    DB.COMPLETED ->
      case rideMode of
        Just ReconEntry.ONLINE ->
          reconcileOnlineCompleted expectedGross baseRideEntry expectedGst gstOnlineEntry
        Just ReconEntry.CASH ->
          reconcileCashCompleted expectedGst gstCashEntry
        Nothing ->
          pure $ mkMissingResult "Unknown ride mode"
    DB.CANCELLED ->
      reconcileCancelled expectedGross userCancellationEntry driverCancellationEntry expectedGst gstOnlineEntry
    _ ->
      pure $ mkMissingResult "Unsupported booking status"
  where
    mkMissingResult reason =
      ReconciliationResult
        { reconStatus = ReconSummary.MISSING_IN_TARGET,
          mismatchReason = Just reason,
          expectedValue = 0,
          actualValue = 0
        }

-- | Reconcile online completed ride
reconcileOnlineCompleted ::
  Applicative m =>
  Maybe HighPrecMoney ->
  Maybe LedgerEntry.LedgerEntry ->
  Maybe HighPrecMoney ->
  Maybe LedgerEntry.LedgerEntry ->
  m ReconciliationResult
reconcileOnlineCompleted expectedGross baseRideEntry expectedGst gstOnlineEntry = do
  let actualGross = baseRideEntry <&> (.amount)
      actualGst = gstOnlineEntry <&> (.amount)

      grossMatch = case (expectedGross, actualGross) of
        (Just expV, Just act) -> expV == act
        _ -> False

      gstMatch = case (expectedGst, actualGst) of
        (Just expV, Just act) -> expV == act
        _ -> False

  if grossMatch && gstMatch
    then
      pure $
        ReconciliationResult
          ReconSummary.MATCHED
          Nothing
          (fromMaybe 0 expectedGross)
          (fromMaybe 0 actualGross)
    else
      if not grossMatch
        then
          pure $
            ReconciliationResult
              (mismatchStatus expectedGross actualGross)
              (Just "BaseRide mismatch - expected vs actual gross differs")
              (fromMaybe 0 expectedGross)
              (fromMaybe 0 actualGross)
        else
          pure $
            ReconciliationResult
              (mismatchStatus expectedGst actualGst)
              (Just "GST mismatch - expected vs actual GST differs")
              (fromMaybe 0 expectedGst)
              (fromMaybe 0 actualGst)

-- | Reconcile cash completed ride
reconcileCashCompleted ::
  Applicative m =>
  Maybe HighPrecMoney ->
  Maybe LedgerEntry.LedgerEntry ->
  m ReconciliationResult
reconcileCashCompleted expectedGst gstCashEntry = do
  case (expectedGst, gstCashEntry <&> (.amount)) of
    (Just expV, Just act) ->
      if expV == act
        then pure $ ReconciliationResult ReconSummary.MATCHED Nothing expV act
        else
          pure $
            ReconciliationResult
              (mismatchStatus (Just expV) (Just act))
              (Just "GSTCash mismatch")
              expV
              act
    (Nothing, Just act) ->
      pure $
        ReconciliationResult
          ReconSummary.MISSING_IN_TARGET
          (Just "Expected GST missing for cash ride")
          0
          act
    (Just expV, Nothing) ->
      pure $
        ReconciliationResult
          ReconSummary.MISSING_IN_TARGET
          (Just "GSTCash entry missing in ledger")
          expV
          0
    _ ->
      pure $
        ReconciliationResult
          ReconSummary.MISSING_IN_TARGET
          (Just "Both expected and actual GST missing")
          0
          0

-- | Reconcile cancelled ride
reconcileCancelled ::
  Applicative m =>
  Maybe HighPrecMoney ->
  Maybe LedgerEntry.LedgerEntry ->
  Maybe LedgerEntry.LedgerEntry ->
  Maybe HighPrecMoney ->
  Maybe LedgerEntry.LedgerEntry ->
  m ReconciliationResult
reconcileCancelled expectedGross userCancellationEntry driverCancellationEntry expectedGst gstOnlineEntry =
  case (userCancellationEntry <&> (.amount), driverCancellationEntry <&> (.amount)) of
    (Just userAmt, _) ->
      case expectedGross of
        Just expV ->
          if expV == userAmt
            then pure $ ReconciliationResult ReconSummary.MATCHED Nothing expV userAmt
            else
              pure $
                ReconciliationResult
                  (mismatchStatus (Just expV) (Just userAmt))
                  (Just "User cancellation amount mismatch")
                  expV
                  userAmt
        Nothing ->
          pure $
            ReconciliationResult
              ReconSummary.MISSING_IN_TARGET
              (Just "Expected value missing")
              0
              userAmt
    (_, Just driverAmt) ->
      case (expectedGross, expectedGst, gstOnlineEntry <&> (.amount)) of
        (Just expGross, Just expGst, Just actGst) ->
          if expGross == driverAmt && expGst == actGst
            then pure $ ReconciliationResult ReconSummary.MATCHED Nothing expGross driverAmt
            else
              if expGross /= driverAmt
                then
                  pure $
                    ReconciliationResult
                      (mismatchStatus (Just expGross) (Just driverAmt))
                      (Just "Driver cancellation amount mismatch")
                      expGross
                      driverAmt
                else
                  pure $
                    ReconciliationResult
                      (mismatchStatus (Just expGst) (Just actGst))
                      (Just "GST mismatch for driver cancellation")
                      expGst
                      actGst
        (Nothing, _, _) ->
          pure $
            ReconciliationResult
              ReconSummary.MISSING_IN_TARGET
              (Just "Expected gross missing")
              0
              driverAmt
        (_, Nothing, _) ->
          pure $
            ReconciliationResult
              ReconSummary.MISSING_IN_TARGET
              (Just "Expected GST missing")
              0
              driverAmt
        (_, _, Nothing) ->
          pure $
            ReconciliationResult
              ReconSummary.MISSING_IN_TARGET
              (Just "GSTOnline entry missing")
              (fromMaybe 0 expectedGross)
              driverAmt
    _ ->
      pure $
        ReconciliationResult
          ReconSummary.MISSING_IN_TARGET
          (Just "No cancellation entry found")
          0
          0

-- Helper functions

determineRideMode :: Maybe MP.PaymentInstrument -> Maybe ReconEntry.RideMode
determineRideMode (Just MP.Cash) = Just ReconEntry.CASH
determineRideMode (Just (MP.Card _)) = Just ReconEntry.ONLINE
determineRideMode (Just (MP.Wallet _)) = Just ReconEntry.ONLINE
determineRideMode (Just MP.UPI) = Just ReconEntry.ONLINE
determineRideMode (Just MP.BoothOnline) = Just ReconEntry.ONLINE
determineRideMode (Just MP.NetBanking) = Just ReconEntry.ONLINE
determineRideMode _ = Nothing

calculateExpectedGross :: DB.Booking -> Maybe HighPrecMoney
calculateExpectedGross booking =
  let estimatedFare = booking.estimatedFare
      tollCharges = fromMaybe 0 booking.tollCharges
      parkingCharges = fromMaybe 0 (booking.fareParams.parkingCharge)
   in Just $ estimatedFare - tollCharges - parkingCharges

mismatchStatus :: Maybe HighPrecMoney -> Maybe HighPrecMoney -> ReconSummary.ReconciliationStatus
mismatchStatus expected actual = case (expected, actual) of
  (Just e, Just a) | a > e -> ReconSummary.HIGHER_IN_TARGET
  (Just _e, Just _a) -> ReconSummary.LOWER_IN_TARGET
  _ -> ReconSummary.LOWER_IN_TARGET

-- -- | Convenience function to get reconciliation status for a specific job type from a booking's reconciliation field
-- getReconciliationStatusFromBooking :: DB.Booking -> ReconciliationJobType -> Maybe ReconSummary.ReconciliationStatus
-- getReconciliationStatusFromBooking booking jobType =
--   getReconStatusForJob (getReconciliationStatus booking.reconciliationStatus) jobType

-- | Convenience function to update reconciliation status for a specific job type
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
