module SharedLogic.Finance.LedgerAdjustment
  ( ledgerAdjustmentSubmit,
    ledgerAdjustmentList,
    ledgerAdjustmentApproveAndPost,
    ledgerAdjustmentReject,
  )
where

import qualified API.Types.ProviderPlatform.Management.Endpoints.FinanceManagement as API
import Control.Applicative ((<|>))
import qualified Dashboard.Common
import Data.List (sortOn)
import qualified Data.Ord
import qualified Data.Text as T
import Domain.Action.UI.Ride.EndRide.Internal (makeWalletRunningBalanceLockKey)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Image as DImage
import qualified Domain.Types.LedgerAdjustmentRequest as DLA
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import Environment
import Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance as Finance
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction as DirectTax
import qualified Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest as DTdsReq
import qualified Lib.Finance.Domain.Types.LedgerEntry as DLE
import qualified Lib.Finance.Storage.Queries.FinanceTdsReimbursementInvoiceMapping as QTdsMap
import qualified Lib.Finance.Storage.Queries.FinanceTdsReimbursementRequest as QTdsReq
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedgerEntry
import qualified Lib.Payment.Domain.Types.PayoutRequest as DPayoutRequest
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPayoutRequest
import qualified SharedLogic.FareCalculator as SFC
import qualified SharedLogic.Finance.Prepaid as FinancePrepaid
import qualified SharedLogic.Finance.TdsReimbursement as TdsReimbursement
import qualified SharedLogic.Finance.Wallet as Wallet
import qualified SharedLogic.Merchant as SMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverPanCard as QPanCard
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FareParameters as QFareParams
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Image as QImage
import qualified Storage.Queries.LedgerAdjustmentRequest as QLedgerAdjustmentRequest
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Tools.ActorInfo as ActorInfo
import Tools.Error

mkAdminName :: Text -> Maybe DP.Person -> Text
mkAdminName requestorName = \case
  Nothing -> requestorName
  Just person ->
    person.firstName
      <> maybe "" (" " <>) person.middleName
      <> maybe "" (" " <>) person.lastName

--------------------------------------------------------------------------------
-- Submit (maker)
--------------------------------------------------------------------------------

ledgerAdjustmentSubmit ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  API.SubmitLedgerAdjustmentReq ->
  Flow APISuccess
ledgerAdjustmentSubmit merchantShortId opCity requestorId requestorName req = ActorInfo.withDashboardPersonIdActorInfo (Id @DP.Person requestorId) $ do
  adjustmentRequestId <- generateGUID
  let personId = cast @Dashboard.Common.Person @DP.Person req.personId
      category = castAdjustmentCategory req.category
      direction = castAdjustmentDirection req.direction
      lockKey = ledgerAdjustmentLockKey req.referenceId adjustmentRequestId

  withLedgerAdjustmentLock lockKey $ do
    whenJust req.referenceId $ \referenceId -> do
      mbExisting <-
        QLedgerAdjustmentRequest.findByReferenceIdAndStatuses
          (Just referenceId)
          [DLA.PENDING_APPROVAL, DLA.POSTED, DLA.POST_FAILED] -- except REJECTED
      whenJust mbExisting $ \_ ->
        throwError (LedgerAdjustmentAlreadyExists referenceId)

    merchant <- SMerchant.findMerchantByShortId merchantShortId
    merchantOpCity <-
      CQMOC.findByMerchantIdAndCity merchant.id opCity
        >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)

    person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
    unless (person.merchantOperatingCityId == merchantOpCity.id) $
      throwError (PersonDoesNotExist personId.getId)

    when (req.amount.amount <= 0) $
      throwError (InvalidRequest "Amount should be positive. Use Credit direction to increase wallet balance, Debit to reduce")
    unless (req.amount.currency == merchantOpCity.currency) $
      throwError (InvalidRequest "Invalid currency")

    unless (directionMatchesCategory category direction) $
      throwError (InvalidRequest $ "Category " <> show category <> " does not match direction " <> show direction)

    transporterConfig <-
      getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) Nothing
        >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)

    -- Common wallet gate for all ledger adjustments (category-specific checks below).
    let isPrepaidSubscriptionAndWalletEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled
    unless (isPrepaidSubscriptionAndWalletEnabled || transporterConfig.driverWalletConfig.enableDriverWallet) $
      throwError (InvalidRequest "Wallet is not enabled for this merchant")

    let mbDocumentId = cast @Dashboard.Common.Image @DImage.Image <$> req.documentId
    whenJust mbDocumentId $ \documentId -> do
      image <- QImage.findById documentId >>= fromMaybeM (ImageNotFound documentId.getId)
      unless (image.merchantId == merchant.id) $ throwError (ImageNotFound documentId.getId)
      whenJust image.merchantOperatingCityId \imageMerchantOperatingCityId -> do
        unless (imageMerchantOperatingCityId == merchantOpCity.id) $ throwError (ImageNotFound documentId.getId)

    -- Per-category domain validation (ride/payout/TDS/etc.).
    validateLedgerAdjustmentCategory merchant merchantOpCity transporterConfig personId category direction req

    mbAdminMaker <- QP.findById (Id @DP.Person requestorId)
    let adminMakerName = mkAdminName requestorName mbAdminMaker
    adjustmentRequest <-
      buildLedgerAdjustmentRequest
        adjustmentRequestId
        merchantOpCity
        personId
        category
        direction
        (Id @DP.Person requestorId)
        adminMakerName
        req
    QLedgerAdjustmentRequest.create adjustmentRequest
  pure Success

--------------------------------------------------------------------------------
-- Category validation
--------------------------------------------------------------------------------

validateLedgerAdjustmentCategory ::
  DM.Merchant ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Id DP.Person ->
  DLA.AdjustmentCategory ->
  DLA.AdjustmentDirection ->
  API.SubmitLedgerAdjustmentReq ->
  Flow ()
validateLedgerAdjustmentCategory _merchant merchantOpCity transporterConfig personId category direction req = case category of
  DLA.RideRelatedCredit -> validateRideRelatedAdjustment transporterConfig personId direction req
  DLA.RideRelatedDebit -> validateRideRelatedAdjustment transporterConfig personId direction req
  DLA.PayoutRelatedCredit -> validatePayoutRelatedAdjustment merchantOpCity personId direction req
  DLA.PayoutRelatedDebit -> validatePayoutRelatedAdjustment merchantOpCity personId direction req
  DLA.TdsReimbursementCredit -> validateTdsReimbursementAdjustment merchantOpCity personId direction req
  -- Debit kept in enum for chart symmetry; FO cert reimbursement is Credit-only for now.
  DLA.TdsReimbursementDebit -> throwError $ LedgerAdjustmentCategoryNotSupported (show category)
  DLA.IncentiveCredit -> validateIncentiveAdjustment personId direction req
  DLA.IncentiveDebit -> validateIncentiveAdjustment personId direction req
  DLA.MiscellaneousCredit -> validateMiscellaneousAdjustment personId direction req
  DLA.MiscellaneousDebit -> validateMiscellaneousAdjustment personId direction req
  DLA.TdsDeductionDebit -> validateTdsDeductionAdjustment transporterConfig personId req

validateReferenceType :: MonadFlow m => API.SubmitLedgerAdjustmentReq -> [Text] -> m ()
validateReferenceType req allowedReferenceTypes =
  unless (req.referenceType `elem` allowedReferenceTypes) $
    throwError $
      LedgerAdjustmentReferenceTypeNotSupported (show req.category) req.referenceType allowedReferenceTypes

-- | Ride-related checks (referenceId = bookingId).
validateRideRelatedAdjustment ::
  DTC.TransporterConfig ->
  Id DP.Person ->
  DLA.AdjustmentDirection ->
  API.SubmitLedgerAdjustmentReq ->
  Flow ()
validateRideRelatedAdjustment transporterConfig personId direction req = do
  referenceId <- req.referenceId & fromMaybeM (InvalidRequest "Reference id required for RideRelated adjustments")
  let rideRelatedReferenceTypes =
        [ Wallet.walletReferenceBaseRide,
          Wallet.walletReferenceDriverCancellationCharges,
          Wallet.walletReferenceCustomerCancellationCharges
        ]
  validateReferenceType req rideRelatedReferenceTypes

  booking <- QBooking.findById (Id @DBooking.Booking referenceId) >>= fromMaybeM (BookingDoesNotExist referenceId)
  unless (req.amount.currency == booking.currency) $
    throwError (InvalidRequest "Invalid currency")

  if
      | req.referenceType == Wallet.walletReferenceBaseRide ->
        validateBaseRideAdjustment personId direction booking req
      | req.referenceType == Wallet.walletReferenceDriverCancellationCharges ->
        validateCancellationAdjustment True transporterConfig direction req.amount.amount personId booking
      | req.referenceType == Wallet.walletReferenceCustomerCancellationCharges ->
        validateCancellationAdjustment False transporterConfig direction req.amount.amount personId booking
      | otherwise ->
        throwError (LedgerAdjustmentReferenceTypeNotSupported (show req.category) req.referenceType rideRelatedReferenceTypes)

-- TODO validation on wallet balance for each category for maker/checker?

-- | BaseRide: booking COMPLETED; debit capped by ride base fare (totalFare - gst - toll - parking).
validateBaseRideAdjustment ::
  Id DP.Person ->
  DLA.AdjustmentDirection ->
  DBooking.Booking ->
  API.SubmitLedgerAdjustmentReq ->
  Flow ()
validateBaseRideAdjustment personId direction booking req = do
  unless (booking.status == DBooking.COMPLETED) $
    throwError (BookingInvalidStatus "Booking should be COMPLETED")

  ride <- QRide.findOneByBookingId booking.id >>= fromMaybeM (RideDoesNotExist booking.id.getId)
  unless (fromMaybe ride.driverId ride.fleetOwnerId == personId) $
    throwError (InvalidRequest "Invalid personId")

  when (direction == DLA.Debit) $ do
    totalFare <- ride.fare & fromMaybeM (InternalError "Ride fare is not present.")
    fareParams <- case ride.fareParametersId of
      Just fareParametersId | fareParametersId /= booking.fareParams.id -> do
        B.runInReplica $ QFareParams.findById fareParametersId >>= fromMaybeM (FareParametersNotFound fareParametersId.getId)
      _ -> pure booking.fareParams
    let gstAmount = fromMaybe 0 fareParams.govtCharges
        tollAmount = fromMaybe 0 fareParams.tollCharges
        parkingAmount = fromMaybe 0 fareParams.parkingCharge
        baseFare = totalFare - gstAmount - tollAmount - parkingAmount
    when (req.amount.amount > baseFare) $
      throwError (InvalidRequest $ "Could not debit more than ride base fare: " <> show baseFare)

-- | Cancellation charges: booking CANCELLED.
validateCancellationAdjustment ::
  Bool ->
  DTC.TransporterConfig ->
  DLA.AdjustmentDirection ->
  HighPrecMoney ->
  Id DP.Person ->
  DBooking.Booking ->
  Flow ()
validateCancellationAdjustment isDriverCancellation transporterConfig direction amount personId booking = do
  unless (booking.status == DBooking.CANCELLED) $
    throwError (BookingInvalidStatus "Booking should be CANCELLED")
  ride <- QRide.findOneByBookingId booking.id >>= fromMaybeM (RideDoesNotExist booking.id.getId)
  unless (fromMaybe ride.driverId ride.fleetOwnerId == personId) $
    throwError (InvalidRequest "Invalid personId")
  if isDriverCancellation
    then do
      maxAmount <- ride.driverCancellationPenaltyAmount & fromMaybeM (InternalError "Driver cancellation penalty amount is not present.")
      -- Credit increases driver balance and reduces driver penalty (vice versa for Debit).
      when (direction == DLA.Credit && amount > maxAmount) $
        throwError (InvalidRequest "Could not credit more than cancellation penalty amount")
    else do
      maxAmountWithGst <- ride.cancellationChargesOnCancel & fromMaybeM (InternalError "User cancellation amount is not present.")
      let mbGstRate = SFC.computeTotalGstRate transporterConfig.taxConfig.rideGst
          gstPct :: Double = fromMaybe 0.0 mbGstRate
          maxAmountExcludingGst =
            if gstPct > 0
              then HighPrecMoney $ maxAmountWithGst.getHighPrecMoney / (1 + toRational gstPct)
              else maxAmountWithGst
      when (direction == DLA.Debit && amount > maxAmountExcludingGst) $
        throwError (InvalidRequest $ "Could not debit more than cancellation charges, exluding gst: " <> show maxAmountExcludingGst)

-- | Payout-related checks.
--   referenceId = payoutRequestId; referenceType = "WalletPayout".
validatePayoutRelatedAdjustment ::
  DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  DLA.AdjustmentDirection ->
  API.SubmitLedgerAdjustmentReq ->
  Flow ()
validatePayoutRelatedAdjustment merchantOpCity personId direction req = do
  let payoutRelatedReferenceTypes = [Wallet.walletReferencePayout]
  validateReferenceType req payoutRelatedReferenceTypes
  referenceId <-
    req.referenceId
      & fromMaybeM (InvalidRequest "Reference id required for PayoutRelated adjustments")
  payoutRequest <-
    QPayoutRequest.findById (Id @DPayoutRequest.PayoutRequest referenceId)
      >>= fromMaybeM (InvalidRequest "Payout request does not exist")
  unless
    ( payoutRequest.merchantId == merchantOpCity.merchantId.getId
        && payoutRequest.merchantOperatingCityId == merchantOpCity.id.getId
    )
    $ throwError (InvalidRequest "Payout request does not exist")
  unless (payoutRequest.beneficiaryId == personId.getId) $
    throwError (InvalidRequest "Invalid personId")

  validatePayoutRequestStatus payoutRequest

  payoutAmount <-
    payoutRequest.amount
      & fromMaybeM (InternalError "Payout request amount is not present.")
  when (direction == DLA.Debit && req.amount.amount > payoutAmount) $
    throwError (InvalidRequest $ "Could not adjust more than payout amount: " <> show payoutAmount)

validatePayoutRequestStatus :: DPayoutRequest.PayoutRequest -> Flow ()
validatePayoutRequestStatus payoutRequest = do
  -- Protected statuses are the same ones that the payout system treats as
  -- "not safely mutable" after the payout is effectively credited/cashed.
  let allowedStatuses =
        [ DPayoutRequest.CREDITED,
          DPayoutRequest.CASH_PAID,
          DPayoutRequest.CASH_PENDING
        ]
  unless (payoutRequest.status `elem` allowedStatuses) $
    throwError $
      InvalidRequest $
        "Invalid payout request status for PayoutRelated adjustments. Allowed: "
          <> T.intercalate ", " (map show allowedStatuses)
          <> "; got: "
          <> show payoutRequest.status

-- | Incentive checks.
--   referenceId = rideId; referenceType = "WalletIncentive".
--   Debit additionally caps amount by the latest WalletIncentive ledger entry.
validateIncentiveAdjustment ::
  Id DP.Person ->
  DLA.AdjustmentDirection ->
  API.SubmitLedgerAdjustmentReq ->
  Flow ()
validateIncentiveAdjustment personId direction req = do
  referenceId <- req.referenceId & fromMaybeM (InvalidRequest "Reference id required for Incentive adjustments")
  let incentiveReferenceTypes = [Wallet.walletReferenceWalletIncentive]
  validateReferenceType req incentiveReferenceTypes

  ride <- QRide.findById (Id @DRide.Ride referenceId) >>= fromMaybeM (RideDoesNotExist referenceId)
  unless (req.amount.currency == ride.currency) $
    throwError (InvalidRequest "Invalid currency")
  unless (fromMaybe ride.driverId ride.fleetOwnerId == personId) $
    throwError (InvalidRequest "Invalid personId")
  unless (ride.status == DRide.COMPLETED) $
    throwError (RideInvalidStatus "Ride should be COMPLETED")

  mbLedgerEntry <-
    listToMaybe . sortOn (Data.Ord.Down . (.createdAt))
      <$> QLedgerEntry.findByReference Wallet.walletReferenceWalletIncentive referenceId
  ledgerEntry <- mbLedgerEntry & fromMaybeM (InvalidRequest "Ledger entry does not exist")
  when (direction == DLA.Debit && req.amount.amount > ledgerEntry.amount) $
    throwError (InvalidRequest $ "Could not debit more than incentives amount: " <> show ledgerEntry.amount)

-- | Catch-all adjustments: require description and/or supporting document for audit.
validateMiscellaneousAdjustment ::
  Id DP.Person ->
  DLA.AdjustmentDirection ->
  API.SubmitLedgerAdjustmentReq ->
  Flow ()
validateMiscellaneousAdjustment personId direction req = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  unless (person.role `elem` [DP.DRIVER, DP.FLEET_OWNER, DP.FLEET_BUSINESS]) $
    throwError (InvalidRequest "Miscellaneous adjustments are only supported for drivers and fleet owners")

  -- TODO should we validate possible reference types?
  when (T.null $ T.strip req.referenceType) $
    throwError (InvalidRequest "Reference type required for Miscellaneous adjustments")

  let hasDescription = maybe False (not . T.null . T.strip) req.description
      hasDocument = isJust req.documentId
  unless (hasDescription || hasDocument) $
    throwError (InvalidRequest "Miscellaneous adjustments require a description or a supporting document")

  when (direction == DLA.Debit) $ do
    let counterpartyType = case person.role of
          DP.FLEET_OWNER -> FinancePrepaid.counterpartyFleetOwner
          DP.FLEET_BUSINESS -> FinancePrepaid.counterpartyFleetOwner
          _ -> FinancePrepaid.counterpartyDriver
    walletBalance <-
      Wallet.getWalletBalanceByOwner counterpartyType personId.getId
        >>= maybe (throwError (InvalidRequest "Wallet balance not found")) pure
    when (req.amount.amount > walletBalance) $
      throwError (InvalidRequest $ "Could not debit more than wallet balance: " <> show walletBalance)

-- | TDS reimbursement Credit: referenceType = "TDSReimbursement", referenceId = TDS request id.
--   Payable = frozen Σ tdsCreditReceivable from WS8 submit (not certAmount / Σ tdsAmount).
--   Amount must match payable exactly (±1). TdsReimbursementDebit is unsupported.
validateTdsReimbursementAdjustment ::
  DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  DLA.AdjustmentDirection ->
  API.SubmitLedgerAdjustmentReq ->
  Flow ()
validateTdsReimbursementAdjustment merchantOpCity personId direction req = do
  let tdsReimbursementReferenceTypes = [FinancePrepaid.tdsReimbursementReferenceType]
      roundingTolerance = 1 :: HighPrecMoney
  validateReferenceType req tdsReimbursementReferenceTypes
  unless (direction == DLA.Credit) $
    throwError $ LedgerAdjustmentCategoryNotSupported (show req.category)

  referenceId <-
    req.referenceId
      & fromMaybeM (InvalidRequest "Reference id required for TdsReimbursement adjustments")

  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  unless (person.role == DP.FLEET_BUSINESS) $
    throwError (InvalidRequest "TDS reimbursement adjustments are only supported for business fleet owners")

  tdsRequest <-
    QTdsReq.findByPrimaryKey (Id @DTdsReq.FinanceTdsReimbursementRequest referenceId)
      >>= fromMaybeM (InvalidRequest $ "TDS reimbursement request not found: " <> referenceId)
  unless (tdsRequest.merchantOperatingCityId == merchantOpCity.id.getId) $
    throwError (InvalidRequest $ "TDS reimbursement request not found: " <> referenceId)
  unless (tdsRequest.fleetOwnerId == personId.getId) $
    throwError (InvalidRequest "Invalid personId for TDS reimbursement request")

  whenJust req.documentId $ \docId ->
    unless (docId.getId == tdsRequest.documentId.getId) $
      throwError (InvalidRequest "Document id does not match the TDS reimbursement request document")

  unless (tdsRequest.status == DTdsReq.PENDING) $
    throwError $
      InvalidRequest $
        "TDS reimbursement request must be PENDING for Credit adjustments; got: " <> show tdsRequest.status

  payableAmount <- sumTdsCreditReceivableForRequest tdsRequest.id
  when (abs (req.amount.amount - payableAmount) > roundingTolerance) $
    throwError $
      InvalidRequest $
        "TDS reimbursement Credit amount ("
          <> show req.amount.amount
          <> ") must equal payable amount Σ tdsCreditReceivable ("
          <> show payableAmount
          <> ")"

  postedAdjustmentEntries <- getSettledTdsReimbursementAdjustmentEntries referenceId
  unless (null postedAdjustmentEntries) $
    throwError (InvalidRequest "TDS reimbursement already has a posted ledger adjustment for this request")

  mappingsWithInvoices <- TdsReimbursement.findInvoiceMappings tdsRequest.id
  TdsReimbursement.assertInvoicesNotAlreadyClaimedForTdsReimbursement (Just tdsRequest.id) (snd <$> mappingsWithInvoices)

-- | Frozen payable from WS8 submit: Σ invoice-mapping tdsCreditReceivable.
sumTdsCreditReceivableForRequest ::
  Id DTdsReq.FinanceTdsReimbursementRequest ->
  Flow HighPrecMoney
sumTdsCreditReceivableForRequest requestId = do
  mappings <- QTdsMap.findAllByRequestId requestId
  pure $ sum $ map (.tdsCreditReceivable) mappings

getSettledTdsReimbursementAdjustmentEntries :: Text -> Flow [DLE.LedgerEntry]
getSettledTdsReimbursementAdjustmentEntries referenceId = do
  entries <- QLedgerEntry.findByReference FinancePrepaid.tdsReimbursementReferenceType referenceId
  pure $
    filter
      (\entry -> entry.entryType == DLE.Adjustment && entry.status == DLE.SETTLED)
      entries

-- | Manual TDS withholding (TdsDeductionDebit).
--   Reuses ride-end threshold gating: driverStats.totalEarnings vs tdsConfig.thresholdAmount.
validateTdsDeductionAdjustment ::
  DTC.TransporterConfig ->
  Id DP.Person ->
  API.SubmitLedgerAdjustmentReq ->
  Flow ()
validateTdsDeductionAdjustment transporterConfig personId req = do
  let tdsDeductionReferenceTypes =
        [ Wallet.walletReferenceTDSDeductionOnline,
          Wallet.walletReferenceTDSDeductionCash,
          Wallet.walletReferenceTDSDeductionCancellation
        ]
  validateReferenceType req tdsDeductionReferenceTypes

  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  unless (person.role `elem` [DP.DRIVER, DP.FLEET_OWNER, DP.FLEET_BUSINESS]) $
    throwError (InvalidRequest "TDS deduction is only supported for drivers and fleet owners")

  let counterpartyType = case person.role of
        DP.FLEET_OWNER -> FinancePrepaid.counterpartyFleetOwner
        DP.FLEET_BUSINESS -> FinancePrepaid.counterpartyFleetOwner
        _ -> FinancePrepaid.counterpartyDriver
      panLinkTdsEnabled = Wallet.panAadhaarLinkTdsEnabled transporterConfig.taxConfig
      configTdsRate = (.rate) <$> transporterConfig.taxConfig.defaultTdsRate

  mbMaterializedTdsRate <- case person.role of
    DP.FLEET_OWNER -> lookupFleetOwnerTdsRate personId panLinkTdsEnabled configTdsRate
    DP.FLEET_BUSINESS -> lookupFleetOwnerTdsRate personId panLinkTdsEnabled configTdsRate
    _ -> do
      driverInfo <- QDI.findById personId >>= fromMaybeM DriverInfoNotFound
      pure $ if panLinkTdsEnabled then driverInfo.tdsRate else driverInfo.tdsRate <|> configTdsRate

  mbPanCard <- QPanCard.findByDriverId personId
  let mbEffectiveTdsRate = Wallet.computeEffectiveTdsRate mbPanCard mbMaterializedTdsRate transporterConfig.taxConfig
  unless (maybe False (> 0) mbEffectiveTdsRate) $
    throwError (InvalidRequest "TDS rate is not configured for this person")

  -- Fleet: no cumulative earnings accumulator yet (same as EndRide / CancelRide).
  -- Threshold "crossed?" check and rate×(cumulative−threshold) cap below are skipped —
  -- fleet can debit any amount up to wallet balance as long as a TDS rate is set.
  mbCumulativeEarnings <- case person.role of
    DP.FLEET_OWNER -> pure Nothing
    DP.FLEET_BUSINESS -> pure Nothing
    _ -> do
      mbStats <- B.runInReplica $ QDriverStats.findByPrimaryKey (cast personId)
      pure $ (.totalEarnings) <$> mbStats

  let mbThresholdAmount = Wallet.selectTds mbPanCard transporterConfig.taxConfig >>= (.thresholdAmount)
  case (mbThresholdAmount, mbCumulativeEarnings) of
    (Just thresh, Just cumulative) ->
      when (cumulative <= thresh) $
        throwError $
          InvalidRequest $
            "TDS threshold not crossed yet: cumulative earnings "
              <> show cumulative
              <> " <= "
              <> show thresh
    _ -> pure ()

  walletBalance <-
    Wallet.getWalletBalanceByOwner counterpartyType personId.getId
      >>= maybe (throwError (InvalidRequest "Wallet balance not found")) pure
  when (req.amount.amount > walletBalance) $
    throwError (InvalidRequest $ "Could not debit more than wallet balance: " <> show walletBalance)

  case mbCumulativeEarnings of
    Nothing -> pure ()
    Just cumulative -> do
      let maxTdsAmount = maxThresholdTdsDeductionAmount mbEffectiveTdsRate cumulative mbThresholdAmount
      when (maxTdsAmount <= 0) $
        throwError (InvalidRequest "No TDS deduction is due for this person")
      when (req.amount.amount > maxTdsAmount) $
        throwError (InvalidRequest $ "Could not debit more than TDS deduction amount: " <> show maxTdsAmount)

-- | Upper bound for manual threshold TDS debit (rate × earnings above threshold).
maxThresholdTdsDeductionAmount ::
  Maybe Double ->
  HighPrecMoney ->
  Maybe HighPrecMoney ->
  HighPrecMoney
maxThresholdTdsDeductionAmount mbEffectiveTdsRate cumulative mbThresholdAmount =
  let rate = max 0 $ fromMaybe 0 mbEffectiveTdsRate
      thresh = max 0 $ fromMaybe 0 mbThresholdAmount
      excessBase = max 0 (cumulative - thresh)
   in -- applyThresholdBenefit not need here because we already checked that threshold crossed
      excessBase * realToFrac rate -- tdsRate is already decimal (0.01 = 1%)

lookupFleetOwnerTdsRate ::
  (EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  Bool ->
  Maybe Double ->
  m (Maybe Double)
lookupFleetOwnerTdsRate personId panLinkTdsEnabled configTdsRate = do
  mbFleetInfo <- QFOI.findByPrimaryKey (cast personId)
  let currentRate = mbFleetInfo >>= (.tdsRate)
  pure $ if panLinkTdsEnabled then currentRate else currentRate <|> configTdsRate

--------------------------------------------------------------------------------
-- Builders / helpers
--------------------------------------------------------------------------------

buildLedgerAdjustmentRequest ::
  MonadFlow m =>
  Id DLA.LedgerAdjustmentRequest ->
  DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  DLA.AdjustmentCategory ->
  DLA.AdjustmentDirection ->
  Id DP.Person ->
  Text ->
  API.SubmitLedgerAdjustmentReq ->
  m DLA.LedgerAdjustmentRequest
buildLedgerAdjustmentRequest adjustmentRequestId merchantOpCity personId category direction requestorId adminMakerName req = do
  now <- getCurrentTime
  pure
    DLA.LedgerAdjustmentRequest
      { id = adjustmentRequestId,
        personId,
        category,
        direction,
        amount = req.amount.amount,
        currency = req.amount.currency,
        description = req.description,
        referenceType = req.referenceType,
        referenceId = req.referenceId,
        documentId = cast @Dashboard.Common.Image @DImage.Image <$> req.documentId,
        adminMakerId = requestorId,
        adminMakerName,
        adminCheckerId = Nothing,
        adminCheckerName = Nothing,
        status = DLA.PENDING_APPROVAL,
        errorMessage = Nothing,
        ledgerEntryId = Nothing,
        merchantId = merchantOpCity.merchantId,
        merchantOperatingCityId = merchantOpCity.id,
        postedAt = Nothing,
        createdAt = now,
        updatedAt = now
      }

directionMatchesCategory :: DLA.AdjustmentCategory -> DLA.AdjustmentDirection -> Bool
directionMatchesCategory category direction = expectedDirectionForCategory category == direction

expectedDirectionForCategory :: DLA.AdjustmentCategory -> DLA.AdjustmentDirection
expectedDirectionForCategory = \case
  DLA.RideRelatedCredit -> DLA.Credit
  DLA.RideRelatedDebit -> DLA.Debit
  DLA.PayoutRelatedCredit -> DLA.Credit
  DLA.PayoutRelatedDebit -> DLA.Debit
  DLA.TdsReimbursementCredit -> DLA.Credit
  DLA.TdsReimbursementDebit -> DLA.Debit
  DLA.IncentiveCredit -> DLA.Credit
  DLA.IncentiveDebit -> DLA.Debit
  DLA.MiscellaneousCredit -> DLA.Credit
  DLA.MiscellaneousDebit -> DLA.Debit
  DLA.TdsDeductionDebit -> DLA.Debit

castAdjustmentCategory :: API.AdjustmentCategory -> DLA.AdjustmentCategory
castAdjustmentCategory = \case
  API.RideRelatedCredit -> DLA.RideRelatedCredit
  API.RideRelatedDebit -> DLA.RideRelatedDebit
  API.PayoutRelatedCredit -> DLA.PayoutRelatedCredit
  API.PayoutRelatedDebit -> DLA.PayoutRelatedDebit
  API.TdsReimbursementCredit -> DLA.TdsReimbursementCredit
  API.TdsReimbursementDebit -> DLA.TdsReimbursementDebit
  API.IncentiveCredit -> DLA.IncentiveCredit
  API.IncentiveDebit -> DLA.IncentiveDebit
  API.MiscellaneousCredit -> DLA.MiscellaneousCredit
  API.MiscellaneousDebit -> DLA.MiscellaneousDebit
  API.TdsDeductionDebit -> DLA.TdsDeductionDebit

toApiAdjustmentCategory :: DLA.AdjustmentCategory -> API.AdjustmentCategory
toApiAdjustmentCategory = \case
  DLA.RideRelatedCredit -> API.RideRelatedCredit
  DLA.RideRelatedDebit -> API.RideRelatedDebit
  DLA.PayoutRelatedCredit -> API.PayoutRelatedCredit
  DLA.PayoutRelatedDebit -> API.PayoutRelatedDebit
  DLA.TdsReimbursementCredit -> API.TdsReimbursementCredit
  DLA.TdsReimbursementDebit -> API.TdsReimbursementDebit
  DLA.IncentiveCredit -> API.IncentiveCredit
  DLA.IncentiveDebit -> API.IncentiveDebit
  DLA.MiscellaneousCredit -> API.MiscellaneousCredit
  DLA.MiscellaneousDebit -> API.MiscellaneousDebit
  DLA.TdsDeductionDebit -> API.TdsDeductionDebit

castAdjustmentDirection :: API.AdjustmentDirection -> DLA.AdjustmentDirection
castAdjustmentDirection = \case
  API.Credit -> DLA.Credit
  API.Debit -> DLA.Debit

toApiAdjustmentDirection :: DLA.AdjustmentDirection -> API.AdjustmentDirection
toApiAdjustmentDirection = \case
  DLA.Credit -> API.Credit
  DLA.Debit -> API.Debit

castAdjustmentRequestStatus :: API.AdjustmentRequestStatus -> DLA.AdjustmentRequestStatus
castAdjustmentRequestStatus = \case
  API.PENDING_APPROVAL -> DLA.PENDING_APPROVAL
  API.REJECTED -> DLA.REJECTED
  API.POSTED -> DLA.POSTED
  API.POST_FAILED -> DLA.POST_FAILED

toApiAdjustmentRequestStatus :: DLA.AdjustmentRequestStatus -> API.AdjustmentRequestStatus
toApiAdjustmentRequestStatus = \case
  DLA.PENDING_APPROVAL -> API.PENDING_APPROVAL
  DLA.REJECTED -> API.REJECTED
  DLA.POSTED -> API.POSTED
  DLA.POST_FAILED -> API.POST_FAILED

ledgerAdjustmentLockKey :: Maybe Text -> Id DLA.LedgerAdjustmentRequest -> Text
ledgerAdjustmentLockKey mbReferenceId adjustmentRequestId =
  "ledgerAdjustment:submit:" <> fromMaybe adjustmentRequestId.getId mbReferenceId

withLedgerAdjustmentLock :: Text -> Flow a -> Flow a
withLedgerAdjustmentLock lockKey action =
  Redis.whenWithLockRedisAndReturnValue lockKey 60 action
    >>= either (\() -> throwError LedgerAdjustmentInProgress) pure

--------------------------------------------------------------------------------
-- List
--------------------------------------------------------------------------------

ledgerAdjustmentList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe (Id DLA.LedgerAdjustmentRequest) ->
  Maybe API.AdjustmentRequestStatus ->
  Maybe (Id DP.Person) ->
  Maybe Bool ->
  Maybe API.AdjustmentCategory ->
  Maybe API.AdjustmentDirection ->
  Maybe Text ->
  Maybe Text ->
  Maybe (Id DP.Person) ->
  Maybe (Id DP.Person) ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Text ->
  Flow API.LedgerAdjustmentListRes
ledgerAdjustmentList merchantShortId opCity mbLimit mbOffset mbAdjustmentRequestId mbStatus mbPersonId mbExcludeCurrentAdminMaker mbCategory mbDirection mbReferenceType mbReferenceId mbAdminMakerId mbAdminCheckerId mbFrom mbTo requestorId = ActorInfo.withDashboardPersonIdActorInfo (Id @DP.Person requestorId) $ do
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)

  let excludeAdminMakerId = if mbExcludeCurrentAdminMaker == Just True then Just $ Id @DP.Person requestorId else Nothing
  adjustmentRequests <-
    QLedgerAdjustmentRequest.findAllLedgerAdjustmentItems
      merchantOpCity.id
      mbAdjustmentRequestId
      (castAdjustmentRequestStatus <$> mbStatus)
      mbPersonId
      (castAdjustmentCategory <$> mbCategory)
      (castAdjustmentDirection <$> mbDirection)
      mbReferenceType
      mbReferenceId
      mbAdminMakerId
      mbAdminCheckerId
      excludeAdminMakerId
      mbFrom
      mbTo
      limit
      offset
  let count = length adjustmentRequests
      summary = Dashboard.Common.Summary {totalCount = count, count}
  pure $
    API.LedgerAdjustmentListRes
      { adjustmentRequests = mkLedgerAdjustmentItem <$> adjustmentRequests,
        summary
      }
  where
    maxLimit = 20
    defaultLimit = 10

mkLedgerAdjustmentItem :: DLA.LedgerAdjustmentRequest -> API.LedgerAdjustmentItem
mkLedgerAdjustmentItem DLA.LedgerAdjustmentRequest {..} =
  API.LedgerAdjustmentItem
    { adjustmentRequestId = cast @DLA.LedgerAdjustmentRequest @Dashboard.Common.LedgerAdjustmentRequest id,
      personId = cast @DP.Person @Dashboard.Common.Person personId,
      category = toApiAdjustmentCategory category,
      direction = toApiAdjustmentDirection direction,
      amount = PriceAPIEntity amount currency,
      description,
      referenceType,
      referenceId,
      documentId = cast @DImage.Image @Dashboard.Common.Image <$> documentId,
      adminMakerId = cast @DP.Person @Dashboard.Common.Person adminMakerId,
      adminCheckerId = cast @DP.Person @Dashboard.Common.Person <$> adminCheckerId,
      adminMakerName,
      adminCheckerName,
      status = toApiAdjustmentRequestStatus status,
      errorMessage,
      ledgerEntryId = getId <$> ledgerEntryId,
      createdAt,
      updatedAt,
      postedAt
    }

--------------------------------------------------------------------------------
-- Approve and post (checker)
--------------------------------------------------------------------------------

ledgerAdjustmentApproveAndPost ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DLA.LedgerAdjustmentRequest ->
  Text ->
  Text ->
  Flow APISuccess
ledgerAdjustmentApproveAndPost merchantShortId opCity adjustmentRequestId requestorId requestorName =
  withLogTag ("adjustmentRequestId_" <> adjustmentRequestId.getId) . ActorInfo.withDashboardPersonIdActorInfo (Id @DP.Person requestorId) $ do
    merchant <- SMerchant.findMerchantByShortId merchantShortId
    merchantOpCity <-
      CQMOC.findByMerchantIdAndCity merchant.id opCity
        >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)

    lockKey <- do
      adjustmentRequest <-
        QLedgerAdjustmentRequest.findById adjustmentRequestId
          >>= fromMaybeM (LedgerAdjustmentDoesNotExist adjustmentRequestId.getId)
      pure $ ledgerAdjustmentLockKey adjustmentRequest.referenceId adjustmentRequestId

    withLedgerAdjustmentLock lockKey $ do
      -- Fetch admin request again to avoid race condition
      adjustmentRequest <-
        QLedgerAdjustmentRequest.findById adjustmentRequestId
          >>= fromMaybeM (LedgerAdjustmentDoesNotExist adjustmentRequestId.getId)
      transporterConfig <-
        getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) Nothing
          >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
      when (adjustmentRequest.adminMakerId == Id @DP.Person requestorId) $
        throwError (InvalidRequest "Admin Maker and Admin Checker cannot be same")
      unless (adjustmentRequest.merchantOperatingCityId == merchantOpCity.id) $
        throwError (LedgerAdjustmentDoesNotExist adjustmentRequestId.getId)
      unless (adjustmentRequest.status == DLA.PENDING_APPROVAL) $
        throwError (InvalidRequest $ "Request already " <> show adjustmentRequest.status)
      mbAdminChecker <- QP.findById (Id @DP.Person requestorId)
      let adminCheckerName = mkAdminName requestorName mbAdminChecker
          checkerId = Id @DP.Person requestorId
      res <-
        withTryCatch "ledgerAdjustmentPostAction" $
          ledgerAdjustmentPostAction transporterConfig adjustmentRequest
      case res of
        Right mbLedgerEntryId -> do
          now <- getCurrentTime
          QLedgerAdjustmentRequest.updateStatusCheckerAndPostResult
            DLA.POSTED
            (Just checkerId)
            (Just adminCheckerName)
            Nothing
            mbLedgerEntryId
            (Just now)
            adjustmentRequest.id
        Left (err :: SomeException) -> do
          let errMessage = T.pack (displayException err)
          logError $
            "Ledger adjustment post failed: "
              <> adjustmentRequest.id.getId
              <> "; error message: "
              <> errMessage
          QLedgerAdjustmentRequest.updateStatusAndChecker
            DLA.POST_FAILED
            (Just checkerId)
            (Just adminCheckerName)
            (Just errMessage)
            adjustmentRequest.id
          throwM err
    pure Success

-- | Checker approve: wallet lock + category-specific ledger posts (stubs below).
ledgerAdjustmentPostAction ::
  DTC.TransporterConfig ->
  DLA.LedgerAdjustmentRequest ->
  Flow (Maybe (Id DLE.LedgerEntry))
ledgerAdjustmentPostAction transporterConfig adjustmentRequest =
  Redis.withLockRedisAndReturnValue (makeWalletRunningBalanceLockKey adjustmentRequest.personId.getId) 10 $ do
    logInfo $
      "Ledger adjustment post triggered: "
        <> adjustmentRequest.id.getId
        <> maybe "" (\adminCheckerId -> "; admin checker: " <> adminCheckerId.getId) adjustmentRequest.adminCheckerId
        <> "; category: "
        <> show adjustmentRequest.category
        <> "; direction: "
        <> show adjustmentRequest.direction
        <> maybe "" (("; referenceId: " <>) . show) adjustmentRequest.referenceId
        <> "; amount: "
        <> show adjustmentRequest.amount
    case adjustmentRequest.category of
      DLA.RideRelatedCredit -> postRideRelatedAdjustment transporterConfig adjustmentRequest
      DLA.RideRelatedDebit -> postRideRelatedAdjustment transporterConfig adjustmentRequest
      DLA.PayoutRelatedCredit -> postPayoutRelatedAdjustment transporterConfig adjustmentRequest
      DLA.PayoutRelatedDebit -> postPayoutRelatedAdjustment transporterConfig adjustmentRequest
      DLA.TdsReimbursementCredit -> postTdsReimbursementAdjustment transporterConfig adjustmentRequest
      DLA.TdsReimbursementDebit -> unsupportedLedgerAdjustmentCategory adjustmentRequest
      DLA.IncentiveCredit -> postIncentiveAdjustment transporterConfig adjustmentRequest
      DLA.IncentiveDebit -> postIncentiveAdjustment transporterConfig adjustmentRequest
      DLA.MiscellaneousCredit -> postMiscellaneousAdjustment transporterConfig adjustmentRequest
      DLA.MiscellaneousDebit -> postMiscellaneousAdjustment transporterConfig adjustmentRequest
      DLA.TdsDeductionDebit -> postTdsDeductionAdjustment transporterConfig adjustmentRequest

type PostLedgerAdjustment =
  DTC.TransporterConfig ->
  DLA.LedgerAdjustmentRequest ->
  Flow (Maybe (Id DLE.LedgerEntry))

-- | Common direction mapping for category-specific manual adjustments.
--   Uses the collecting finance helper because the request stores ledgerEntryId.
adjustment ::
  (EsqDBFlow m r, CacheFlow m r, Finance.HasActorInfo m r) =>
  DLA.AdjustmentDirection ->
  Finance.AccountRole ->
  Finance.AccountRole ->
  HighPrecMoney ->
  Text ->
  Finance.FinanceM m (Maybe (Id DLE.LedgerEntry))
adjustment DLA.Credit fromRole toRole amount = Finance.adjustment fromRole toRole amount
adjustment DLA.Debit fromRole toRole amount = Finance.adjustment fromRole toRole (negate amount)

postRideRelatedAdjustment :: PostLedgerAdjustment
postRideRelatedAdjustment transporterConfig adjustmentRequest = do
  referenceId <-
    adjustmentRequest.referenceId
      & fromMaybeM (InvalidRequest "Reference id required for RideRelated adjustments")

  booking <-
    QBooking.findById (Id @DBooking.Booking referenceId)
      >>= fromMaybeM (BookingDoesNotExist referenceId)
  ride <-
    QRide.findOneByBookingId booking.id
      >>= fromMaybeM (RideDoesNotExist booking.id.getId)
  driver <-
    QP.findById ride.driverId
      >>= fromMaybeM (PersonNotFound ride.driverId.getId)

  ctx <-
    Wallet.buildFinanceCtx
      booking
      ride
      (Just driver)
      Nothing
      Nothing
      transporterConfig
      True
  result <-
    Finance.runFinance ctx $
      adjustment
        adjustmentRequest.direction
        Finance.SellerExpense
        Finance.OwnerLiability
        adjustmentRequest.amount
        adjustmentRequest.referenceType
  case result of
    Left err -> throwError $ InternalError ("Failed to create ride ledger adjustment: " <> show err)
    Right (mbLedgerEntryId, _) -> pure mbLedgerEntryId

postPayoutRelatedAdjustment :: PostLedgerAdjustment
postPayoutRelatedAdjustment transporterConfig adjustmentRequest = do
  referenceId <-
    adjustmentRequest.referenceId
      & fromMaybeM (InvalidRequest "Reference id required for PayoutRelated adjustments")

  payoutRequest <-
    QPayoutRequest.findById (Id @DPayoutRequest.PayoutRequest referenceId)
      >>= fromMaybeM (InvalidRequest "Payout request does not exist")

  validatePayoutRequestStatus payoutRequest

  person <-
    QP.findById adjustmentRequest.personId
      >>= fromMaybeM (PersonNotFound adjustmentRequest.personId.getId)

  let ctx = mkFinanceContextWithoutInvoice transporterConfig adjustmentRequest person
  result <-
    Finance.runFinance ctx $
      adjustment
        adjustmentRequest.direction
        Finance.PlatformAsset
        Finance.OwnerLiability
        adjustmentRequest.amount
        adjustmentRequest.referenceType
  case result of
    Left err -> throwError $ InternalError ("Failed to create payout ledger adjustment: " <> show err)
    Right (mbLedgerEntryId, _) -> pure mbLedgerEntryId

-- | Credit-only FO TDS-cert reimbursement post (Debit → unsupportedLedgerAdjustmentCategory).
--   Chart: Dr GovtDirectAsset (TDS Receivable) / Cr OwnerLiability (FO wallet).
--   Also records standalone DirectTaxTransaction rows (tdsTreatment=Reimbursed) per
--   invoice mapping — no new invoice; links to original subscription invoiceNumber.
postTdsReimbursementAdjustment :: PostLedgerAdjustment
postTdsReimbursementAdjustment transporterConfig adjustmentRequest = do
  unless (adjustmentRequest.direction == DLA.Credit) $
    void $ unsupportedLedgerAdjustmentCategory adjustmentRequest

  referenceId <-
    adjustmentRequest.referenceId
      & fromMaybeM (InvalidRequest "Reference id required for TdsReimbursement adjustments")

  tdsRequest <-
    QTdsReq.findByPrimaryKey (Id @DTdsReq.FinanceTdsReimbursementRequest referenceId)
      >>= fromMaybeM (InvalidRequest $ "TDS reimbursement request not found: " <> referenceId)

  person <-
    QP.findById adjustmentRequest.personId
      >>= fromMaybeM (PersonNotFound adjustmentRequest.personId.getId)
  unless (person.role == DP.FLEET_BUSINESS) $
    throwError (InvalidRequest "TDS reimbursement adjustments are only supported for business fleet owners")

  -- TOCTOU: re-check request status and credit idempotency under the wallet lock.
  unless (tdsRequest.status == DTdsReq.PENDING) $
    throwError $
      InvalidRequest $
        "TDS reimbursement request must be PENDING for Credit adjustments; got: " <> show tdsRequest.status
  postedAdjustmentEntries <- getSettledTdsReimbursementAdjustmentEntries referenceId
  unless (null postedAdjustmentEntries) $
    throwError (InvalidRequest "TDS reimbursement already has a posted ledger adjustment for this request")

  mappingsWithInvoices <- TdsReimbursement.findInvoiceMappings tdsRequest.id
  TdsReimbursement.assertInvoicesNotAlreadyClaimedForTdsReimbursement (Just tdsRequest.id) (snd <$> mappingsWithInvoices)

  let directTaxConfigs =
        [ Finance.DirectTaxConfig
            { transactionType = DirectTax.Subscription, -- tds reimbursement request works only for Subscription currently
              referenceId = referenceId,
              grossAmount = mapping.revenueRecognisedSnapshot,
              tdsAmount = mapping.tdsCreditReceivable,
              tdsTreatment = DirectTax.Reimbursed,
              counterpartyId = tdsRequest.fleetOwnerId,
              panOfParty = Nothing,
              panType = Nothing,
              tdsRateReason = Nothing,
              tanOfDeductee = Just tdsRequest.tanNumber,
              tdsSection = tdsRequest.tdsSection,
              invoiceNumber = Just invoice.invoiceNumber
            }
          | (mapping, invoice) <- mappingsWithInvoices,
            mapping.tdsCreditReceivable > 0
        ]

  let ctx = mkFinanceContextWithoutInvoice transporterConfig adjustmentRequest person
  result <-
    Finance.runFinance ctx $ do
      mbLedgerEntryId <-
        adjustment
          DLA.Credit
          Finance.GovtDirectAsset
          Finance.OwnerLiability
          adjustmentRequest.amount
          adjustmentRequest.referenceType
      forM_ directTaxConfigs $ \cfg -> void $ Finance.recordDirectTax cfg
      pure mbLedgerEntryId
  case result of
    Left err -> throwError $ InternalError ("Failed to create TDS reimbursement ledger adjustment: " <> show err)
    Right (mbLedgerEntryId, _) -> do
      QTdsReq.updateStatusAndRejectionReason DTdsReq.APPROVED Nothing tdsRequest.id
      pure mbLedgerEntryId

postIncentiveAdjustment :: PostLedgerAdjustment
postIncentiveAdjustment transporterConfig adjustmentRequest = do
  referenceId <-
    adjustmentRequest.referenceId
      & fromMaybeM (InvalidRequest "Reference id required for Incentive adjustments")

  ride <-
    QRide.findById (Id @DRide.Ride referenceId)
      >>= fromMaybeM (RideDoesNotExist referenceId)
  booking <-
    QBooking.findById ride.bookingId
      >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  driver <-
    QP.findById ride.driverId
      >>= fromMaybeM (PersonNotFound ride.driverId.getId)

  ctx <-
    Wallet.buildFinanceCtx
      booking
      ride
      (Just driver)
      Nothing
      Nothing
      transporterConfig
      True
  result <-
    Finance.runFinance ctx {Finance.referenceId = referenceId} $
      adjustment
        adjustmentRequest.direction
        Finance.OwnerExpense -- SellerAsset ??
        Finance.OwnerLiability
        adjustmentRequest.amount
        adjustmentRequest.referenceType
  case result of
    Left err -> throwError $ InternalError ("Failed to create incentive ledger adjustment: " <> show err)
    Right (mbLedgerEntryId, _) -> pure mbLedgerEntryId

postMiscellaneousAdjustment :: PostLedgerAdjustment
postMiscellaneousAdjustment transporterConfig adjustmentRequest = do
  person <-
    QP.findById adjustmentRequest.personId
      >>= fromMaybeM (PersonNotFound adjustmentRequest.personId.getId)
  unless (person.role `elem` [DP.DRIVER, DP.FLEET_OWNER, DP.FLEET_BUSINESS]) $
    throwError (InvalidRequest "Miscellaneous adjustments are only supported for drivers and fleet owners")

  -- Chart: Misc Control ↔ Driver-FO Balance. SellerExpense stands in for Misc Control
  -- until a dedicated account role / subLedger exists.
  let ctx = mkFinanceContextWithoutInvoice transporterConfig adjustmentRequest person
  result <-
    Finance.runFinance ctx $
      adjustment
        adjustmentRequest.direction
        Finance.SellerExpense
        Finance.OwnerLiability
        adjustmentRequest.amount
        adjustmentRequest.referenceType
  case result of
    Left err -> throwError $ InternalError ("Failed to create miscellaneous ledger adjustment: " <> show err)
    Right (mbLedgerEntryId, _) -> pure mbLedgerEntryId

postTdsDeductionAdjustment :: PostLedgerAdjustment
postTdsDeductionAdjustment transporterConfig adjustmentRequest = do
  person <-
    QP.findById adjustmentRequest.personId
      >>= fromMaybeM (PersonNotFound adjustmentRequest.personId.getId)
  unless (person.role `elem` [DP.DRIVER, DP.FLEET_OWNER, DP.FLEET_BUSINESS]) $
    throwError (InvalidRequest "TDS deduction is only supported for drivers and fleet owners")
  let ctx = mkFinanceContextWithoutInvoice transporterConfig adjustmentRequest person

  -- Credit pair GovtDirect → OwnerLiability; Debit reverses to OwnerLiability → GovtDirect
  -- (Dr driver balance, Cr TDS payable) — same legs as EndRide TDS transfer.
  -- Only Debit category possible currently
  result <-
    Finance.runFinance ctx $
      adjustment
        adjustmentRequest.direction
        Finance.GovtDirect
        Finance.OwnerLiability
        adjustmentRequest.amount
        adjustmentRequest.referenceType
  case result of
    Left err -> throwError $ InternalError ("Failed to create TDS deduction ledger adjustment: " <> show err)
    Right (mbLedgerEntryId, _) -> pure mbLedgerEntryId

mkFinanceContextWithoutInvoice ::
  DTC.TransporterConfig ->
  DLA.LedgerAdjustmentRequest ->
  DP.Person ->
  Finance.FinanceCtx
mkFinanceContextWithoutInvoice transporterConfig adjustmentRequest person =
  let counterpartyType = case person.role of
        DP.FLEET_OWNER -> Finance.FLEET_OWNER
        DP.FLEET_BUSINESS -> Finance.FLEET_OWNER
        _ -> Finance.DRIVER
      referenceId = fromMaybe adjustmentRequest.id.getId adjustmentRequest.referenceId
   in Finance.FinanceCtx
        { merchantId = adjustmentRequest.merchantId.getId,
          merchantOpCityId = adjustmentRequest.merchantOperatingCityId.getId,
          currency = adjustmentRequest.currency,
          isOnline = True,
          counterpartyType,
          counterpartyId = adjustmentRequest.personId.getId,
          concernedIndividualId =
            if counterpartyType == Finance.DRIVER
              then Just adjustmentRequest.personId.getId
              else Nothing,
          referenceId,
          entityReferenceId = Nothing,
          entityReferenceType = Nothing,
          -- Invoice fields (not needed for adjustments)
          merchantName = Nothing,
          merchantShortId = Nothing,
          issuedByAddress = Nothing,
          supplierName = Nothing,
          supplierGSTIN = Nothing,
          supplierVatNumber = Nothing,
          supplierAddress = Nothing,
          merchantGstin = Nothing,
          merchantVatNumber = Nothing,
          supplierId = Nothing,
          panOfParty = Nothing,
          panType = Nothing,
          tdsRateReason = Nothing,
          emitLedgerEntries = maybe True (.emitLedgerEntries) transporterConfig.invoiceConfig,
          fromLocationAddress = Nothing,
          issuedToName = Nothing,
          enableWalletGatedTierCheck = fromMaybe False transporterConfig.driverWalletConfig.enableWalletGatedTierCheck
        }

unsupportedLedgerAdjustmentCategory ::
  DLA.LedgerAdjustmentRequest ->
  Flow (Maybe (Id DLE.LedgerEntry))
unsupportedLedgerAdjustmentCategory adjustmentRequest =
  throwError $ LedgerAdjustmentCategoryNotSupported (show adjustmentRequest.category)

--------------------------------------------------------------------------------
-- Reject (checker)
--------------------------------------------------------------------------------

ledgerAdjustmentReject ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DLA.LedgerAdjustmentRequest ->
  Text ->
  Text ->
  Flow APISuccess
ledgerAdjustmentReject merchantShortId opCity adjustmentRequestId requestorId requestorName =
  withLogTag ("adjustmentRequestId_" <> adjustmentRequestId.getId) . ActorInfo.withDashboardPersonIdActorInfo (Id @DP.Person requestorId) $ do
    merchant <- SMerchant.findMerchantByShortId merchantShortId
    merchantOpCity <-
      CQMOC.findByMerchantIdAndCity merchant.id opCity
        >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)

    lockKey <- do
      adjustmentRequest <-
        QLedgerAdjustmentRequest.findById adjustmentRequestId
          >>= fromMaybeM (LedgerAdjustmentDoesNotExist adjustmentRequestId.getId)
      pure $ ledgerAdjustmentLockKey adjustmentRequest.referenceId adjustmentRequestId

    withLedgerAdjustmentLock lockKey $ do
      -- Fetch admin request again to avoid race condition
      adjustmentRequest <-
        QLedgerAdjustmentRequest.findById adjustmentRequestId
          >>= fromMaybeM (LedgerAdjustmentDoesNotExist adjustmentRequestId.getId)
      when (adjustmentRequest.adminMakerId == Id @DP.Person requestorId) $
        throwError (InvalidRequest "Admin Maker and Admin Checker cannot be same")
      unless (adjustmentRequest.merchantOperatingCityId == merchantOpCity.id) $
        throwError (LedgerAdjustmentDoesNotExist adjustmentRequestId.getId)
      unless (adjustmentRequest.status == DLA.PENDING_APPROVAL) $
        throwError (InvalidRequest $ "Request already " <> show adjustmentRequest.status)
      mbAdminChecker <- QP.findById (Id @DP.Person requestorId)
      let adminCheckerName = mkAdminName requestorName mbAdminChecker
      QLedgerAdjustmentRequest.updateStatusAndChecker
        DLA.REJECTED
        (Just $ Id @DP.Person requestorId)
        (Just adminCheckerName)
        Nothing
        adjustmentRequest.id
      ledgerAdjustmentRejectSideEffect adjustmentRequest
    pure Success

-- | Category-specific side effects after checker reject (mirror of ledgerAdjustmentPostAction).
--   Does not reverse ledger / wallet — only optional domain sync.
type RejectLedgerAdjustmentSideEffect =
  DLA.LedgerAdjustmentRequest -> Flow ()

ledgerAdjustmentRejectSideEffect :: RejectLedgerAdjustmentSideEffect
ledgerAdjustmentRejectSideEffect adjustmentRequest = case adjustmentRequest.category of
  DLA.RideRelatedCredit -> noopRejectSideEffect adjustmentRequest
  DLA.RideRelatedDebit -> noopRejectSideEffect adjustmentRequest
  DLA.PayoutRelatedCredit -> noopRejectSideEffect adjustmentRequest
  DLA.PayoutRelatedDebit -> noopRejectSideEffect adjustmentRequest
  DLA.TdsReimbursementCredit -> rejectTdsReimbursementSideEffect adjustmentRequest
  -- Debit category remains in the enum but is rejected at validate/post; no domain sync.
  DLA.TdsReimbursementDebit -> noopRejectSideEffect adjustmentRequest
  DLA.IncentiveCredit -> noopRejectSideEffect adjustmentRequest
  DLA.IncentiveDebit -> noopRejectSideEffect adjustmentRequest
  DLA.MiscellaneousCredit -> noopRejectSideEffect adjustmentRequest
  DLA.MiscellaneousDebit -> noopRejectSideEffect adjustmentRequest
  DLA.TdsDeductionDebit -> noopRejectSideEffect adjustmentRequest

noopRejectSideEffect :: RejectLedgerAdjustmentSideEffect
noopRejectSideEffect _ = pure ()

-- | Currently we don't have reimbursement request reject api, hence adj reject is currently the only admin path to close a PENDING cert claim,
--   so we mark the TDS request REJECTED (FO can resubmit for the same Q/AY).
rejectTdsReimbursementSideEffect :: RejectLedgerAdjustmentSideEffect
rejectTdsReimbursementSideEffect adjustmentRequest = do
  whenJust adjustmentRequest.referenceId $ \referenceId -> do
    mbTdsRequest <- QTdsReq.findByPrimaryKey (Id @DTdsReq.FinanceTdsReimbursementRequest referenceId)
    whenJust mbTdsRequest $ \tdsRequest ->
      when (tdsRequest.status == DTdsReq.PENDING) $
        QTdsReq.updateStatusAndRejectionReason
          DTdsReq.REJECTED
          (Just "Rejected via ledger adjustment")
          tdsRequest.id
