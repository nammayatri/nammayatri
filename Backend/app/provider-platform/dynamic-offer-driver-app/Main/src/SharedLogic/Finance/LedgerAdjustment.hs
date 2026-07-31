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
import qualified Lib.Finance.Domain.Types.LedgerEntry as DLE
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedgerEntry
import qualified Lib.Payment.Domain.Types.PayoutRequest as DPayoutRequest
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPayoutRequest
import qualified SharedLogic.FareCalculator as SFC
import qualified SharedLogic.Finance.Prepaid as FinancePrepaid
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

-- TODO make req.referenceId/req.referenceType mandatory?
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

  Redis.whenWithLockRedis lockKey 60 $ do
    -- TODO should we check when referenceId is Nothing?
    whenJust req.referenceId $ \referenceId -> do
      mbExisting <-
        QLedgerAdjustmentRequest.findByReferenceIdAndStatuses
          (Just referenceId)
          [DLA.PENDING_APPROVAL, DLA.APPROVED, DLA.POSTED, DLA.POST_FAILED] -- except REJECTED
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
  DLA.TdsReimbursementCredit -> pure () -- TODO: WS8 TDS document validation
  DLA.TdsReimbursementDebit -> pure () -- TODO: WS8 TDS document validation
  DLA.IncentiveCredit -> validateIncentiveAdjustment personId direction req
  DLA.IncentiveDebit -> validateIncentiveAdjustment personId direction req
  DLA.MiscellaneousCredit -> validateMiscellaneousAdjustment personId direction req
  DLA.MiscellaneousDebit -> validateMiscellaneousAdjustment personId direction req
  DLA.TdsDeductionDebit -> validateTdsDeductionAdjustment transporterConfig personId req

rideRelatedReferenceTypes :: [Text]
rideRelatedReferenceTypes =
  [ Wallet.walletReferenceBaseRide,
    Wallet.walletReferenceDriverCancellationCharges,
    Wallet.walletReferenceCustomerCancellationCharges
  ]

tdsDeductionReferenceTypes :: [Text]
tdsDeductionReferenceTypes =
  [ Wallet.walletReferenceTDSDeductionOnline,
    Wallet.walletReferenceTDSDeductionCash,
    Wallet.walletReferenceTDSDeductionCancellation
  ]

requireTdsDeductionReferenceType :: Maybe Text -> Flow Text
requireTdsDeductionReferenceType mbReferenceType = do
  referenceType <-
    mbReferenceType
      & fromMaybeM (InvalidRequest "Reference type required for TdsDeduction adjustments")
  unless (referenceType `elem` tdsDeductionReferenceTypes) $
    throwError (LedgerAdjustmentReferenceTypeNotSupported referenceType)
  pure referenceType

-- | Ride-related checks (referenceId = bookingId).
validateRideRelatedAdjustment ::
  DTC.TransporterConfig ->
  Id DP.Person ->
  DLA.AdjustmentDirection ->
  API.SubmitLedgerAdjustmentReq ->
  Flow ()
validateRideRelatedAdjustment transporterConfig personId direction req = do
  referenceType <- req.referenceType & fromMaybeM (InvalidRequest "Reference type required for RideRelated adjustments")
  referenceId <- req.referenceId & fromMaybeM (InvalidRequest "Reference id required for RideRelated adjustments")
  -- TODO should we use enum for req.referenceType?
  unless (referenceType `elem` rideRelatedReferenceTypes) $
    throwError $
      InvalidRequest $
        "Supported reference types for RideRelated: "
          <> T.intercalate ", " rideRelatedReferenceTypes

  booking <- QBooking.findById (Id @DBooking.Booking referenceId) >>= fromMaybeM (BookingDoesNotExist referenceId)
  unless (req.amount.currency == booking.currency) $
    throwError (InvalidRequest "Invalid currency")

  if
      | referenceType == Wallet.walletReferenceBaseRide ->
        validateBaseRideAdjustment personId direction booking req
      | referenceType == Wallet.walletReferenceDriverCancellationCharges ->
        validateCancellationAdjustment True transporterConfig direction req.amount.amount personId booking
      | referenceType == Wallet.walletReferenceCustomerCancellationCharges ->
        validateCancellationAdjustment False transporterConfig direction req.amount.amount personId booking
      | otherwise ->
        throwError (InvalidRequest "Unsupported reference type for RideRelated adjustments")

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
  referenceType <-
    req.referenceType
      & fromMaybeM (InvalidRequest "Reference type required for PayoutRelated adjustments")
  unless (referenceType == Wallet.walletReferencePayout) $
    throwError (LedgerAdjustmentReferenceTypeNotSupported referenceType)
  referenceId <-
    req.referenceId
      & fromMaybeM (InvalidRequest "Reference id required for PayoutRelated adjustments")
  -- check status?
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

  payoutAmount <-
    payoutRequest.amount
      & fromMaybeM (InternalError "Payout request amount is not present.")
  when (direction == DLA.Debit && req.amount.amount > payoutAmount) $
    throwError (InvalidRequest $ "Could not adjust more than payout amount: " <> show payoutAmount)

-- | Incentive checks.
--   referenceId = rideId; referenceType = "WalletIncentive".
--   Debit additionally caps amount by the latest WalletIncentive ledger entry.
validateIncentiveAdjustment ::
  Id DP.Person ->
  DLA.AdjustmentDirection ->
  API.SubmitLedgerAdjustmentReq ->
  Flow ()
validateIncentiveAdjustment personId direction req = do
  referenceType <- req.referenceType & fromMaybeM (InvalidRequest "Reference type required for Incentive adjustments")
  referenceId <- req.referenceId & fromMaybeM (InvalidRequest "Reference id required for Incentive adjustments")
  unless (referenceType == Wallet.walletReferenceWalletIncentive) $
    throwError (InvalidRequest $ "Supported reference type for Incentive adjustments: " <> Wallet.walletReferenceWalletIncentive)

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
  referenceType <-
    req.referenceType
      & fromMaybeM (InvalidRequest "Reference type required for Miscellaneous adjustments")
  when (T.null $ T.strip referenceType) $
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

-- | Manual TDS withholding (TdsDeductionDebit).
--   Reuses ride-end threshold gating: driverStats.totalEarnings vs tdsConfig.thresholdAmount.
validateTdsDeductionAdjustment ::
  DTC.TransporterConfig ->
  Id DP.Person ->
  API.SubmitLedgerAdjustmentReq ->
  Flow ()
validateTdsDeductionAdjustment transporterConfig personId req = do
  void $ requireTdsDeductionReferenceType req.referenceType

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
        documentId = req.documentId, -- TODO validate documentId
        adminMakerId = requestorId,
        adminMakerName,
        adminCheckerId = Nothing,
        adminCheckerName = Nothing,
        status = DLA.PENDING_APPROVAL,
        errorMessage = Nothing,
        ledgerEntryId = Nothing,
        merchantId = merchantOpCity.merchantId,
        merchantOperatingCityId = merchantOpCity.id,
        approvedAt = Nothing,
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
  API.APPROVED -> DLA.APPROVED
  API.REJECTED -> DLA.REJECTED
  API.POSTED -> DLA.POSTED
  API.POST_FAILED -> DLA.POST_FAILED

toApiAdjustmentRequestStatus :: DLA.AdjustmentRequestStatus -> API.AdjustmentRequestStatus
toApiAdjustmentRequestStatus = \case
  DLA.PENDING_APPROVAL -> API.PENDING_APPROVAL
  DLA.APPROVED -> API.APPROVED
  DLA.REJECTED -> API.REJECTED
  DLA.POSTED -> API.POSTED
  DLA.POST_FAILED -> API.POST_FAILED

ledgerAdjustmentLockKey :: Maybe Text -> Id DLA.LedgerAdjustmentRequest -> Text
ledgerAdjustmentLockKey mbReferenceId adjustmentRequestId =
  "ledgerAdjustment:submit:" <> fromMaybe adjustmentRequestId.getId mbReferenceId

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
      amount = Just $ PriceAPIEntity amount currency,
      description,
      referenceType,
      referenceId,
      documentId,
      adminMakerId = cast @DP.Person @Dashboard.Common.Person adminMakerId,
      adminCheckerId = cast @DP.Person @Dashboard.Common.Person <$> adminCheckerId,
      adminMakerName,
      adminCheckerName,
      status = toApiAdjustmentRequestStatus status,
      errorMessage,
      ledgerEntryId = getId <$> ledgerEntryId,
      createdAt,
      updatedAt,
      approvedAt,
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

    Redis.whenWithLockRedis lockKey 60 $ do
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
            Nothing
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
      DLA.TdsReimbursementDebit -> postTdsReimbursementAdjustment transporterConfig adjustmentRequest
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
  referenceType <-
    adjustmentRequest.referenceType
      & fromMaybeM (InvalidRequest "Reference type required for RideRelated adjustments")
  unless (referenceType `elem` rideRelatedReferenceTypes) $
    throwError (LedgerAdjustmentReferenceTypeNotSupported referenceType)
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
        referenceType
  case result of
    Left err -> throwError $ InternalError ("Failed to create ride ledger adjustment: " <> show err)
    Right (mbLedgerEntryId, _) -> pure mbLedgerEntryId

postPayoutRelatedAdjustment :: PostLedgerAdjustment
postPayoutRelatedAdjustment transporterConfig adjustmentRequest = do
  referenceType <-
    adjustmentRequest.referenceType
      & fromMaybeM (InvalidRequest "Reference type required for PayoutRelated adjustments")
  unless (referenceType == Wallet.walletReferencePayout) $
    throwError (LedgerAdjustmentReferenceTypeNotSupported referenceType)
  referenceId <-
    adjustmentRequest.referenceId
      & fromMaybeM (InvalidRequest "Reference id required for PayoutRelated adjustments")

  -- check status?
  _payoutRequest <-
    QPayoutRequest.findById (Id @DPayoutRequest.PayoutRequest referenceId)
      >>= fromMaybeM (InvalidRequest "Payout request does not exist")
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
        referenceType
  case result of
    Left err -> throwError $ InternalError ("Failed to create payout ledger adjustment: " <> show err)
    Right (mbLedgerEntryId, _) -> pure mbLedgerEntryId

postTdsReimbursementAdjustment :: PostLedgerAdjustment
postTdsReimbursementAdjustment _ = unsupportedLedgerAdjustmentCategory

postIncentiveAdjustment :: PostLedgerAdjustment
postIncentiveAdjustment transporterConfig adjustmentRequest = do
  referenceType <-
    adjustmentRequest.referenceType
      & fromMaybeM (InvalidRequest "Reference type required for Incentive adjustments")
  unless (referenceType == Wallet.walletReferenceWalletIncentive) $
    throwError (LedgerAdjustmentReferenceTypeNotSupported referenceType)
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
        referenceType
  case result of
    Left err -> throwError $ InternalError ("Failed to create incentive ledger adjustment: " <> show err)
    Right (mbLedgerEntryId, _) -> pure mbLedgerEntryId

postMiscellaneousAdjustment :: PostLedgerAdjustment
postMiscellaneousAdjustment transporterConfig adjustmentRequest = do
  referenceType <-
    adjustmentRequest.referenceType
      & fromMaybeM (InvalidRequest "Reference type required for Miscellaneous adjustments")
  when (T.null $ T.strip referenceType) $
    throwError (InvalidRequest "Reference type required for Miscellaneous adjustments")
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
        referenceType
  case result of
    Left err -> throwError $ InternalError ("Failed to create miscellaneous ledger adjustment: " <> show err)
    Right (mbLedgerEntryId, _) -> pure mbLedgerEntryId

postTdsDeductionAdjustment :: PostLedgerAdjustment
postTdsDeductionAdjustment transporterConfig adjustmentRequest = do
  referenceType <- requireTdsDeductionReferenceType adjustmentRequest.referenceType
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
        referenceType
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
          issuedToName = Nothing
        }

-- Will be removed after full implementation
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

    Redis.whenWithLockRedis lockKey 60 $ do
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
        Nothing
        adjustmentRequest.id
    pure Success
