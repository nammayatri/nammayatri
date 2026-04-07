module Domain.Action.Dashboard.AppManagement.AdminRequest
  ( postAdminRequestCreate,
    getAdminRequestList,
    postAdminRequestRespond,
  )
where

import qualified API.Types.Dashboard.AppManagement.AdminRequest as Common
import Control.Applicative ((<|>))
import Dashboard.Common as Common
import Data.List (sortOn)
import qualified Data.Ord
import qualified Data.Text as T
import qualified Domain.Action.Dashboard.Management.Payout as DMPayout
import qualified Domain.Types.AdminRequest as DAdminRequest
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.CommonDriverOnboardingDocuments as DCommonDoc
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverPanCard as DPanCard
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified "this" Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import qualified Environment
import Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance as Finance
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedgerEntry
import qualified Lib.Payment.Domain.Types.PayoutRequest as DPayoutRequest
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPayoutRequest
import qualified SharedLogic.FareCalculator as SFC
import SharedLogic.Finance.Prepaid (counterpartyDriver, counterpartyFleetOwner)
import qualified SharedLogic.Finance.Wallet as SFW
import Storage.Beam.Payment ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.AdminRequest as QAdminRequest
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.CommonDriverOnboardingDocuments as QCommonDriverOnboardingDocuments
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverPanCard as QPanCard
import qualified Storage.Queries.FareParameters as QFareParams
import Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import Tools.Error

postAdminRequestCreate ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Common.CreateAdminRequestReq ->
  Environment.Flow APISuccess
postAdminRequestCreate merchantShortId opCity requestorId requestorName req = do
  Redis.whenWithLockRedis (adminRequestProcessingLockKey req.referenceId) 60 $ do
    mbExistingAdminRequest <- QAdminRequest.findByReferenceIdAndStatuses req.referenceId [DAdminRequest.PENDING, DAdminRequest.APPROVED]
    whenJust mbExistingAdminRequest $ \_ -> throwError (AdminRequestAlreadyExists req.referenceId)

    merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
    merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
    person <- B.runInReplica $ QP.findById req.personId >>= fromMaybeM (PersonDoesNotExist req.personId.getId)
    unless (person.merchantOperatingCityId == merchantOpCity.id) $ throwError (PersonDoesNotExist req.personId.getId)

    whenJust req.amount $ \reqAmount ->
      when (reqAmount.amount <= 0) $ do
        throwError (InvalidRequest "Amount should be positive. Use Credit adjustment type for increase wallet balance, Debit for reduce")
    transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
    unless (req.actionType `elem` fromMaybe [] transporterConfig.enableAdminMakerChecker) $ do
      throwError (InvalidRequest $ "Admin Maker & Checker is not enabled for this action type: " <> show req.actionType)
    -- admin request data validation
    (mbAmount, mbCurrency) <- case req.actionType of
      DAdminRequest.LedgerAdjustment -> do
        let isPrepaidSubscriptionAndWalletEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled
        unless (isPrepaidSubscriptionAndWalletEnabled && transporterConfig.driverWalletConfig.enableDriverWallet) $
          throwError (InvalidRequest "Wallet is not enabled for this merchant")
        reqReferenceType <- req.referenceType & fromMaybeM (InvalidRequest "Reference type required for this action type")
        reqAdjustmentType <- req.adjustmentType & fromMaybeM (InvalidRequest "Adjustment type required for this action type")
        _reqAdjustmentSource <- req.source & fromMaybeM (InvalidRequest "Adjustment source required for this action type")
        reqAmount <- req.amount & fromMaybeM (InvalidRequest "Amount required for this action type")
        adjustmentReferenceType <-
          castAdjustmentReferenceType reqReferenceType
            & fromMaybeM (InvalidRequest $ "Supported reference types for this action type: " <> T.intercalate ", " adjustmentReferenceTypes)
        case adjustmentReferenceType of
          RideReference WalletIncentive -> do
            unless (req.referenceTable == DAdminRequest.RIDE) $ throwError (InvalidRequest "Invalid reference table for this action type and reference type")
            ride <- QRide.findById (Id @DRide.Ride req.referenceId) >>= fromMaybeM (RideDoesNotExist req.referenceId)
            unless (reqAmount.currency == ride.currency) $ throwError (InvalidRequest "Invalid currency")
            unless (fromMaybe ride.driverId ride.fleetOwnerId == req.personId) $ throwError (InvalidRequest "Invalid personId")
            unless (ride.status == DRide.COMPLETED) $ do
              throwError (RideInvalidStatus "Ride should be COMPLETED")
            mbLedgerEntry <- listToMaybe . sortOn (Data.Ord.Down . (.createdAt)) <$> QLedgerEntry.findByReference SFW.walletReferenceWalletIncentive req.referenceId
            ledgerEntry <- mbLedgerEntry & fromMaybeM (InvalidRequest "Ledger entry does not exist")
            when (reqAdjustmentType == DAdminRequest.Debit && reqAmount.amount > ledgerEntry.amount) $ do
              throwError (InvalidRequest $ "Could not debit more than incentives amount: " <> show ledgerEntry.amount)
          BookingReference bookingReferenceType -> do
            unless (req.referenceTable == DAdminRequest.BOOKING) $ throwError (InvalidRequest "Invalid reference table for this action type and reference type")
            booking <- QBooking.findById (Id @DBooking.Booking req.referenceId) >>= fromMaybeM (BookingDoesNotExist req.referenceId)
            unless (reqAmount.currency == booking.currency) $ throwError (InvalidRequest "Invalid currency")
            case bookingReferenceType of
              BaseRide -> do
                unless (booking.status == DBooking.COMPLETED) $ do
                  throwError (BookingInvalidStatus "Booking should be COMPLETED")
                ride <- QRide.findOneByBookingId booking.id >>= fromMaybeM (RideDoesNotExist booking.id.getId)
                unless (fromMaybe ride.driverId ride.fleetOwnerId == req.personId) $ throwError (InvalidRequest "Invalid personId")
                when (reqAdjustmentType == DAdminRequest.Debit) $ do
                  totalFare <- ride.fare & fromMaybeM (InternalError "Ride fare is not present.")
                  fareParams <- case ride.fareParametersId of
                    Just fareParametersId | fareParametersId /= booking.fareParams.id -> do
                      runInReplica $ QFareParams.findById fareParametersId >>= fromMaybeM (FareParametersNotFound fareParametersId.getId)
                    _ -> pure booking.fareParams
                  let gstAmount = fromMaybe 0 fareParams.govtCharges
                      tollAmount = fromMaybe 0 fareParams.tollCharges
                      parkingAmount = fromMaybe 0 fareParams.parkingCharge
                      baseFare = totalFare - gstAmount - tollAmount - parkingAmount
                  when (reqAmount.amount > baseFare) $ do
                    throwError (InvalidRequest $ "Could not debit more than ride base fare: " <> show baseFare)
              DriverCancellationCharges -> validateCancellationAdjustment True transporterConfig reqAdjustmentType reqAmount.amount req.personId booking
              CustomerCancellationCharges -> validateCancellationAdjustment False transporterConfig reqAdjustmentType reqAmount.amount req.personId booking
        pure (Just reqAmount.amount, Just reqAmount.currency)
      DAdminRequest.FailedPayoutReTrigger -> do
        whenJust req.referenceType $ \_ -> throwError (InvalidRequest "Reference type not required for this action type")
        whenJust req.adjustmentType $ \_ -> throwError (InvalidRequest "Adjustment type not required for this action type")
        whenJust req.source $ \_ -> throwError (InvalidRequest "Adjustment source not required for this action type")
        case req.referenceTable of
          DAdminRequest.PAYOUT_REQUEST -> do
            payoutRequest <- QPayoutRequest.findById (Id @DPayoutRequest.PayoutRequest req.referenceId) >>= fromMaybeM (InvalidRequest "Payout request does not exist")
            unless (payoutRequest.status `elem` PayoutRequest.retryablePayoutStatuses) $
              throwError $ InvalidRequest "Payout is not eligible for retry"
            unless (payoutRequest.beneficiaryId == req.personId.getId) $
              throwError (InvalidRequest "Invalid personId")
            whenJust req.amount $ \_ -> throwError (InvalidRequest "Amount not required for this action type")
            pure (Nothing, Nothing)
          _ -> throwError (InvalidRequest "Invalid reference table for this action type")
      DAdminRequest.TDSReimbursement -> do
        case req.referenceTable of
          DAdminRequest.COMMON_DRIVER_ONBOARDING_DOCUMENTS -> do
            whenJust req.referenceType $ \_ -> throwError (InvalidRequest "Reference type not required for this action type")
            whenJust req.adjustmentType $ \_ -> throwError (InvalidRequest "Adjustment type not required for this action type")
            whenJust req.source $ \_ -> throwError (InvalidRequest "Adjustment source not required for this action type")
            whenJust req.amount $ \_ -> throwError (InvalidRequest "Amount not required for this action type")
            mbDocument <- QCommonDriverOnboardingDocuments.findById (Id @DCommonDoc.CommonDriverOnboardingDocuments req.referenceId)
            document <- mbDocument & fromMaybeM (DocumentNotFound req.referenceId)
            unless (document.driverId == Just req.personId) $
              throwError (InvalidRequest "Invalid personId")
            unless (document.documentType == DVC.TDSCertificate) $
              throwError (InvalidRequest $ show DVC.TDSCertificate <> " document type supported for this action type")
            unless (document.verificationStatus == Documents.MANUAL_VERIFICATION_REQUIRED) $
              throwError (InvalidRequest $ show Documents.MANUAL_VERIFICATION_REQUIRED <> " verification status supported for this action type")
            pure (Nothing, Nothing)
          _ -> throwError (InvalidRequest "Invalid reference table for this action type")

    mbAdminMaker <- QP.findById (Id @DP.Person requestorId)
    let adminMakerName = case mbAdminMaker of
          Nothing -> requestorName
          Just adminMaker -> adminMaker.firstName <> maybe "" (" " <>) adminMaker.middleName <> maybe "" (" " <>) adminMaker.lastName
    adminRequest <- buildAdminRequest merchantOpCity requestorId adminMakerName mbAmount mbCurrency req
    QAdminRequest.create adminRequest
  pure Success

-- helper types only for clear adjustment logic, not used in api and db table
data AdjustmentReferenceType = BookingReference BookingReferenceType | RideReference RideReferenceType

data BookingReferenceType = BaseRide | DriverCancellationCharges | CustomerCancellationCharges

data RideReferenceType = WalletIncentive

castAdjustmentReferenceType :: Text -> Maybe AdjustmentReferenceType
castAdjustmentReferenceType referenceType = case referenceType of
  _ | referenceType == SFW.walletReferenceBaseRide -> Just $ BookingReference BaseRide
  _ | referenceType == SFW.walletReferenceCustomerCancellationCharges -> Just $ BookingReference CustomerCancellationCharges
  _ | referenceType == SFW.walletReferenceDriverCancellationCharges -> Just $ BookingReference DriverCancellationCharges
  _ | referenceType == SFW.walletReferenceWalletIncentive -> Just $ RideReference WalletIncentive
  _ -> Nothing

adjustmentReferenceTypes :: [Text]
adjustmentReferenceTypes = [SFW.walletReferenceBaseRide, SFW.walletReferenceDriverCancellationCharges, SFW.walletReferenceCustomerCancellationCharges, SFW.walletReferenceWalletIncentive]

buildAdminRequest ::
  MonadFlow m =>
  DMOC.MerchantOperatingCity ->
  Text ->
  Text ->
  Maybe HighPrecMoney ->
  Maybe Currency ->
  Common.CreateAdminRequestReq ->
  m DAdminRequest.AdminRequest
buildAdminRequest merchantOpertingCity requestorId adminMakerName amount currency Common.CreateAdminRequestReq {amount = _amount, ..} = do
  id <- generateGUID
  now <- getCurrentTime
  pure
    DAdminRequest.AdminRequest
      { status = DAdminRequest.PENDING,
        adminMakerId = Id @DP.Person requestorId,
        adminCheckerId = Nothing,
        merchantId = merchantOpertingCity.merchantId,
        merchantOperatingCityId = merchantOpertingCity.id,
        errorMessage = Nothing,
        createdAt = now,
        updatedAt = now,
        adminMakerName,
        adminCheckerName = Nothing,
        ..
      }

validateCancellationAdjustment ::
  Bool ->
  DTC.TransporterConfig ->
  DAdminRequest.AdjustmentType ->
  HighPrecMoney ->
  Id DP.Person ->
  DBooking.Booking ->
  Environment.Flow ()
validateCancellationAdjustment isDriverCancellation transporterConfig adjustmentType amount personId booking = do
  unless (booking.status == DBooking.CANCELLED) $
    throwError (BookingInvalidStatus "Booking should be CANCELLED")
  ride <- QRide.findOneByBookingId booking.id >>= fromMaybeM (RideDoesNotExist booking.id.getId)
  unless (fromMaybe ride.driverId ride.fleetOwnerId == personId) $
    throwError (InvalidRequest "Invalid personId")
  if isDriverCancellation
    then do
      maxAmount <- ride.driverCancellationPenaltyAmount & fromMaybeM (InternalError "Driver cancellation penalty amount is not present.")
      -- Note: Here Credit means increase driver balance, and hence reduce driver penalty (vice versa for Debit)
      when (adjustmentType == DAdminRequest.Credit && amount > maxAmount) $
        throwError (InvalidRequest "Could not credit more than cancellation penalty amount")
    else do
      maxAmountWithGst <- ride.cancellationChargesOnCancel & fromMaybeM (InternalError "User cancellation amount is not present.")
      let mbGstRate = SFC.computeTotalGstRate transporterConfig.taxConfig.rideGst
          gstPct :: Double = fromMaybe 0.0 mbGstRate
      let maxAmountExcludingGst =
            if gstPct > 0
              then HighPrecMoney $ maxAmountWithGst.getHighPrecMoney / (1 + toRational gstPct)
              else maxAmountWithGst
      when (adjustmentType == DAdminRequest.Debit && amount > maxAmountExcludingGst) $ do
        throwError (InvalidRequest $ "Could not debit more than cancellation charges, exluding gst: " <> show maxAmountExcludingGst)

getAdminRequestList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe (Id DAdminRequest.AdminRequest) ->
  Maybe DAdminRequest.AdminRequestStatus ->
  Maybe (Id DP.Person) ->
  Maybe Bool ->
  Maybe DAdminRequest.ActionType ->
  Maybe DAdminRequest.AdjustmentType ->
  Maybe Text ->
  Maybe Text ->
  Maybe DAdminRequest.ReferenceTable ->
  Maybe DAdminRequest.AdjustmentSource ->
  Maybe (Id DP.Person) ->
  Maybe (Id DP.Person) ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Text ->
  Environment.Flow Common.AdminRequestResp
getAdminRequestList merchantShortId opCity mbLimit mbOffset adminRequestId status personId excludeCurrentAdminMaker actionType adjustmentType referenceType referenceId referenceTable source adminMakerId adminCheckerId from to requestorId = do
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)

  let excludeAdminMakerId = if excludeCurrentAdminMaker == Just True then Just $ Id @DP.Person requestorId else Nothing
  adminRequests <- QAdminRequest.findAllAdminRequestItems merchantOpCity.id adminRequestId status personId actionType adjustmentType referenceType referenceId referenceTable source adminMakerId adminCheckerId excludeAdminMakerId from to limit offset
  let count = length adminRequests
  let summary = Common.Summary {totalCount = count, count}
  pure $ Common.AdminRequestResp {adminRequests = mkAdminRequestItem <$> adminRequests, summary = summary}
  where
    maxLimit = 20
    defaultLimit = 10

mkAdminRequestItem ::
  DAdminRequest.AdminRequest ->
  Common.AdminRequestItem
mkAdminRequestItem DAdminRequest.AdminRequest {..} =
  Common.AdminRequestItem
    { adminRequestId = id,
      amount = PriceAPIEntity <$> amount <*> currency,
      ..
    }

postAdminRequestRespond ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DAdminRequest.AdminRequest ->
  Text ->
  Text ->
  Common.RespondAdminRequestReq ->
  Environment.Flow APISuccess
postAdminRequestRespond merchantShortId opCity adminRequestId requestorId requestorName req = withLogTag ("adminRequestId_" <> adminRequestId.getId) $ do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  referenceId <- (.referenceId) <$> (QAdminRequest.findByPrimaryKey adminRequestId >>= fromMaybeM (AdminRequestDoesNotExist adminRequestId.getId))
  Redis.whenWithLockRedis (adminRequestProcessingLockKey referenceId) 60 $ do
    -- Fetch admin request again to avoid race condition
    adminRequest <- QAdminRequest.findByPrimaryKey adminRequestId >>= fromMaybeM (AdminRequestDoesNotExist adminRequestId.getId)
    transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
    unless (adminRequest.actionType `elem` fromMaybe [] transporterConfig.enableAdminMakerChecker) $ do
      throwError (InvalidRequest $ "Admin Maker & Checker is not enabled for this action type: " <> show adminRequest.actionType)
    when (adminRequest.adminMakerId == Id @DP.Person requestorId) $
      throwError (InvalidRequest "Admin Maker and Admin Checker cannot be same")
    unless (adminRequest.merchantOperatingCityId == merchantOpCity.id) $ throwError (AdminRequestDoesNotExist adminRequestId.getId)
    unless (adminRequest.status == DAdminRequest.PENDING) $ do
      throwError (InvalidRequest $ "Request already " <> show adminRequest.status)
    mbAdminChecker <- QP.findById (Id @DP.Person requestorId)
    let adminCheckerName = case mbAdminChecker of
          Nothing -> requestorName
          Just adminChecker -> adminChecker.firstName <> maybe "" (" " <>) adminChecker.middleName <> maybe "" (" " <>) adminChecker.lastName
    if req.approve
      then do
        let updAdminRequest = adminRequest{status = DAdminRequest.APPROVED, adminCheckerId = Just $ Id @DP.Person requestorId}
        res <- case updAdminRequest.actionType of
          DAdminRequest.LedgerAdjustment -> withTryCatch "ledgerAdjustmentAction" $ ledgerAdjustmentAction transporterConfig updAdminRequest
          DAdminRequest.FailedPayoutReTrigger -> withTryCatch "failedPayoutReTriggerAction" $ failedPayoutReTriggerAction merchantShortId opCity updAdminRequest
          DAdminRequest.TDSReimbursement -> withTryCatch "tdsReimbursementAction" $ tdsReimbursementAction updAdminRequest
        case res of
          Right () -> QAdminRequest.updateStatusErrorMessageAndAdminChecker DAdminRequest.APPROVED Nothing (Just $ Id @DP.Person requestorId) (Just adminCheckerName) adminRequest.id
          Left (err :: SomeException) -> do
            let errMessage = T.pack (displayException err)
            logError $ "Admin request action failed: " <> updAdminRequest.id.getId <> "; error message: " <> errMessage
            QAdminRequest.updateStatusErrorMessageAndAdminChecker DAdminRequest.FAILED (Just errMessage) (Just $ Id @DP.Person requestorId) (Just adminCheckerName) adminRequest.id
            throwM err
      else QAdminRequest.updateStatusErrorMessageAndAdminChecker DAdminRequest.REJECTED Nothing (Just $ Id @DP.Person requestorId) (Just adminCheckerName) adminRequest.id
  pure Success

adminRequestProcessingLockKey :: Text -> Text
adminRequestProcessingLockKey referenceId = "adminRequest:processing:" <> referenceId

ledgerAdjustmentAction :: DTC.TransporterConfig -> DAdminRequest.AdminRequest -> Environment.Flow ()
ledgerAdjustmentAction transporterConfig adminRequest = do
  -- FIXME check other places should be: makeWalletRunningBalanceLockKey (fromMaybe ride.driverId.getId ride.fleetOwnerId.getId)
  Redis.withWaitOnLockRedisWithExpiry (SFW.makeWalletRunningBalanceLockKey adminRequest.personId.getId) 10 10 $ do
    logInfo $ "Ledger adjustment action triggered: " <> adminRequest.id.getId <> maybe "" (\adminCheckerId -> "; admin checker: " <> adminCheckerId.getId) adminRequest.adminCheckerId
    referenceType <- adminRequest.referenceType & fromMaybeM (InvalidRequest "Reference type required for this action type")
    adjustmentReferenceType <-
      castAdjustmentReferenceType referenceType
        & fromMaybeM (InvalidRequest $ "Supported reference types for this action type: " <> T.intercalate ", " adjustmentReferenceTypes)
    adjustmentType <- adminRequest.adjustmentType & fromMaybeM (InvalidRequest "Adjustment type required for this action type")
    adjustmentSource <- adminRequest.source & fromMaybeM (InvalidRequest "Adjustment source required for this action type")
    additionalAdjustments <- case adjustmentReferenceType of
      BookingReference bookingReferenceType -> do
        booking <- QBooking.findById (Id @DBooking.Booking adminRequest.referenceId) >>= fromMaybeM (BookingDoesNotExist adminRequest.referenceId)
        ride <- QRide.findOneByBookingId booking.id >>= fromMaybeM (RideDoesNotExist booking.id.getId)
        driver <- QP.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
        driverInfo <- QDI.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
        let driverOrFleetPersonId = fromMaybe ride.driverId ride.fleetOwnerId
        mbPanCard <- QPanCard.findByDriverId driverOrFleetPersonId
        adminRequestAmount <- adminRequest.amount & fromMaybeM (InternalError $ "Amount required for this action type: " <> show adminRequest.actionType)
        let ledgerAdjustmentParams = LedgerAdjustmentParams {..}

        case bookingReferenceType of
          BaseRide -> rideRelatedAdjustmnent ledgerAdjustmentParams
          CustomerCancellationCharges -> userCancellationRelatedAdjustmnent ledgerAdjustmentParams
          DriverCancellationCharges -> driverCancellationRelatedAdjustmnent ledgerAdjustmentParams
      RideReference WalletIncentive -> incentiveRelatedAdjustment transporterConfig adminRequest
    logInfo $
      "Ledger adjustment entries created successfully: "
        <> referenceType
        <> "; adjustmentType: "
        <> show adjustmentType
        <> "; adjustmentSource: "
        <> show adjustmentSource
        <> ", referenceId: "
        <> adminRequest.referenceId
        <> maybe "" (("; adjustment base amount: " <>) . show) adminRequest.amount
        <> "; additional adjustments: "
        <> T.intercalate ", " (mapMaybe (\(adjDesc, mbAdjAmount) -> mbAdjAmount <&> (\adjAmount -> adjDesc <> ": " <> show adjAmount)) additionalAdjustments)

data LedgerAdjustmentParams = LedgerAdjustmentParams
  { adminRequest :: DAdminRequest.AdminRequest,
    adjustmentType :: DAdminRequest.AdjustmentType,
    adjustmentSource :: DAdminRequest.AdjustmentSource,
    adminRequestAmount :: HighPrecMoney,
    booking :: DBooking.Booking,
    ride :: DRide.Ride,
    driver :: DP.Person,
    mbPanCard :: Maybe DPanCard.DriverPanCard,
    driverInfo :: DI.DriverInformation,
    transporterConfig :: DTC.TransporterConfig
  }

type FinanceFlow m r = (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r)

rideRelatedAdjustmnent :: FinanceFlow m r => LedgerAdjustmentParams -> m [(Text, Maybe HighPrecMoney)]
rideRelatedAdjustmnent LedgerAdjustmentParams {..} = do
  let adjustmentBaseAmount = adminRequestAmount
  (mbGstAmount, mbTdsAmount) <- case adjustmentSource of
    DAdminRequest.BuyerApp -> do
      let mbGstRate = SFC.computeTotalGstRate transporterConfig.taxConfig.rideGst
      let gstRate :: Double = fromMaybe 0.0 mbGstRate
      let gstAmount = HighPrecMoney $ toRational gstRate * adjustmentBaseAmount.getHighPrecMoney
      mbTdsAmount <- calculateAdjustmentTdsAmount transporterConfig mbPanCard driverInfo ride adjustmentBaseAmount
      pure (Just gstAmount, mbTdsAmount)
    DAdminRequest.Internal -> pure (Nothing, Nothing)

  let mbTollAmount = Nothing -- TODO add later
  let mbParkingAmount = Nothing -- TODO add later
  ctx <- SFW.buildFinanceCtx booking ride (Just driver) mbPanCard (Just driverInfo) transporterConfig
  result <- Finance.runFinance ctx $ do
    -- Online: Asset(BUYER) -> External(BUYER) -> destination for each component
    let onlineComponents =
          [(adjustmentBaseAmount, SFW.walletReferenceBaseRide, Finance.OwnerLiability)]
            <> maybe [] (\gstAmount -> [(gstAmount, SFW.walletReferenceGSTOnline, Finance.GovtIndirect)]) mbGstAmount
    -- <> maybe [] (\tollAmount -> [(tollAmount, walletReferenceTollCharges, Finance.OwnerLiability)]) mbTollAmount, -- TODO add later
    -- <> maybe [] (\parkingAmount -> [(parkingAmount, walletReferenceParkingCharges, Finance.OwnerLiability)]) mbParkingAmount -- TODO add later
    forM_ onlineComponents $ \(amt, ref, dest) -> do
      let adjustments = case adjustmentSource of
            DAdminRequest.BuyerApp ->
              [ adjustment adjustmentType Finance.BuyerAsset Finance.BuyerExternal amt ref,
                adjustment adjustmentType Finance.BuyerExternal dest amt ref
              ]
            DAdminRequest.Internal ->
              [ adjustment adjustmentType Finance.SellerExpense dest amt ref
              ]
      sequenceA_ $ case adjustmentType of
        DAdminRequest.Credit -> adjustments
        DAdminRequest.Debit -> reverse adjustments -- reverse transfers order

    -- TDS: Liability(DRIVER/FLEET_OWNER) -> Liability(GOVERNMENT_DIRECT)
    whenJust mbTdsAmount $ \tdsAmount ->
      adjustment adjustmentType Finance.OwnerLiability Finance.GovtDirect tdsAmount SFW.walletReferenceTDSDeductionOnline
  case result of
    Left err -> fromEitherM (\e -> InternalError ("Failed to create ride ledger adjustments: " <> show e)) (Left err)
    Right (_mbInvoiceId, _entryIds) -> do
      pure [("gstAmount", mbGstAmount), ("tdsAmount", mbTdsAmount), ("tollAmount", mbTollAmount), ("parkingAmount", mbParkingAmount)]

calculateAdjustmentTdsAmount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  DTC.TransporterConfig ->
  Maybe DPanCard.DriverPanCard ->
  DI.DriverInformation ->
  DRide.Ride ->
  HighPrecMoney ->
  m (Maybe HighPrecMoney)
calculateAdjustmentTdsAmount transporterConfig mbPanCard driverInfo ride adjustmentBaseAmount = do
  let configTdsRate = transporterConfig.taxConfig.defaultTdsRate
  mbTdsRate <- case ride.fleetOwnerId of
    Just fleetOwnerId -> do
      mbFleetInfo <- QFOI.findByPrimaryKey (cast fleetOwnerId)
      let currentRate = mbFleetInfo >>= (.tdsRate)
      pure (currentRate <|> configTdsRate)
    Nothing -> do
      let currentRate = driverInfo.tdsRate
      pure (currentRate <|> configTdsRate)

  let effectiveTdsRate = SFW.computeEffectiveTdsRate mbPanCard mbTdsRate (transporterConfig.taxConfig.defaultTdsRate) (transporterConfig.taxConfig.invalidPanTdsRate)
      baseFareForTds = max 0 adjustmentBaseAmount
      mbTdsAmount = do
        rate <- effectiveTdsRate
        let amount = baseFareForTds * realToFrac rate -- tdsRate is already decimal (0.01 = 1%)
        if amount > 0 then Just amount else Nothing
  pure mbTdsAmount

userCancellationRelatedAdjustmnent :: FinanceFlow m r => LedgerAdjustmentParams -> m [(Text, Maybe HighPrecMoney)]
userCancellationRelatedAdjustmnent LedgerAdjustmentParams {..} = do
  let baseCancellation = adminRequestAmount
  (mbGstOnCancellation, mbTdsAmount) <- case adjustmentSource of
    DAdminRequest.BuyerApp -> do
      let mbGstRate = SFC.computeTotalGstRate transporterConfig.taxConfig.rideGst
      let gstPct :: Double = fromMaybe 0.0 mbGstRate
          mbGstOnCancellation' = if gstPct > 0 then Just $ HighPrecMoney $ baseCancellation.getHighPrecMoney * toRational gstPct else Nothing
          -- TDS on cancellation charges (same rate as ride)
          mbTdsRate = transporterConfig.taxConfig.defaultTdsRate
          mbTdsAmount' = do
            rate <- mbTdsRate
            let amount = baseCancellation * realToFrac rate
            if amount > 0 then Just amount else Nothing
      pure (mbGstOnCancellation', mbTdsAmount')
    DAdminRequest.Internal -> pure (Nothing, Nothing)

  let cancellationComponents =
        [(baseCancellation, SFW.walletReferenceCustomerCancellationCharges, Finance.OwnerLiability)]
          <> maybe [] (\gstOnCancellation -> [(gstOnCancellation, SFW.walletReferenceCustomerCancellationGST, Finance.GovtIndirect)]) mbGstOnCancellation

  ctx <- SFW.buildFinanceCtx booking ride (Just driver) mbPanCard (Just driverInfo) transporterConfig
  result <- Finance.runFinance ctx $ do
    mapM_
      ( \(amt, ref, dest) -> do
          let adjustments = case adjustmentSource of
                DAdminRequest.BuyerApp ->
                  [ adjustment adjustmentType Finance.BuyerAsset Finance.BuyerExternal amt ref,
                    adjustment adjustmentType Finance.BuyerExternal dest amt ref
                  ]
                DAdminRequest.Internal ->
                  [ adjustment adjustmentType Finance.SellerExpense dest amt ref
                  ]

          sequenceA_ $ case adjustmentType of
            DAdminRequest.Credit -> adjustments
            DAdminRequest.Debit -> reverse adjustments -- reverse transfers order
      )
      cancellationComponents
    -- TDS: Liability(DRIVER/FLEET_OWNER) -> Liability(GOVERNMENT_DIRECT)
    whenJust mbTdsAmount $ \tdsAmount ->
      adjustment adjustmentType Finance.OwnerLiability Finance.GovtDirect tdsAmount SFW.walletReferenceTDSDeductionCancellation
  case result of
    Left err -> fromEitherM (\e -> InternalError ("Failed to create user cancellation ledger adjustments: " <> show e)) (Left err)
    Right _ -> do
      pure [("gstAmount", mbGstOnCancellation), ("tdsAmount", mbTdsAmount)]

driverCancellationRelatedAdjustmnent :: FinanceFlow m r => LedgerAdjustmentParams -> m [(Text, Maybe HighPrecMoney)]
driverCancellationRelatedAdjustmnent LedgerAdjustmentParams {..} = do
  let penaltyAdjusment = adminRequestAmount
  ctx <- SFW.buildFinanceCtx booking ride (Just driver) mbPanCard (Just driverInfo) transporterConfig
  result <- Finance.runFinance ctx $ do
    -- Note: Here Credit means increase driver balance, and hence reduce driver penalty (vice versa for Debit)
    adjustment adjustmentType Finance.OwnerRevenue Finance.OwnerLiability penaltyAdjusment SFW.walletReferenceDriverCancellationCharges
  case result of
    Left err -> fromEitherM (\e -> InternalError ("Failed to create driver cancellation ledger adjustments: " <> show e)) (Left err)
    Right _ -> pure []

incentiveRelatedAdjustment ::
  FinanceFlow m r =>
  DTC.TransporterConfig ->
  DAdminRequest.AdminRequest ->
  m [(Text, Maybe HighPrecMoney)]
incentiveRelatedAdjustment transporterConfig adminRequest = do
  adminRequestAmount <- adminRequest.amount & fromMaybeM (InternalError $ "Amount required for this action type: " <> show adminRequest.actionType)
  adjustmentType <- adminRequest.adjustmentType & fromMaybeM (InvalidRequest "Adjustment type required for this action type")
  let adjustmentBaseAmount = adminRequestAmount
      adjustmentDelta = case adjustmentType of
        DAdminRequest.Credit -> adjustmentBaseAmount
        DAdminRequest.Debit -> negate adjustmentBaseAmount
  let referenceId = adminRequest.referenceId
  ride <- QRide.findById (Id @DRide.Ride adminRequest.referenceId) >>= fromMaybeM (RideDoesNotExist adminRequest.referenceId)
  let counterparty = case ride.fleetOwnerId of
        Just _fleetOwnerId -> counterpartyFleetOwner
        Nothing -> counterpartyDriver
  res <-
    SFW.createAdjustmentWalletEntryDelta
      counterparty
      adminRequest.personId.getId
      adjustmentDelta
      transporterConfig.currency
      transporterConfig.merchantId.getId
      transporterConfig.merchantOperatingCityId.getId
      SFW.walletReferenceWalletIncentive
      referenceId
      Nothing
  case res of
    Left err -> fromEitherM (\e -> InternalError ("Failed to create incentive ledger adjustments: " <> show e)) (Left err)
    Right _ -> pure []

-- | Adjustment helper, which does NOT collect the entry ID (no invoices for adjustments created for now).
--   Credit: direct transfer_ fromRole -> toRole
--   Debit: reversal transfer_ toRole -> fromRole
adjustment ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  DAdminRequest.AdjustmentType ->
  Finance.AccountRole -> -- fromRole
  Finance.AccountRole -> -- toRole
  HighPrecMoney ->
  Text -> -- Reference type
  Finance.FinanceM m ()
adjustment DAdminRequest.Credit fromRole toRole amount = Finance.adjustment_ fromRole toRole amount
adjustment DAdminRequest.Debit fromRole toRole amount = Finance.adjustment_ fromRole toRole (negate amount)

failedPayoutReTriggerAction ::
  ShortId DM.Merchant ->
  Context.City ->
  DAdminRequest.AdminRequest ->
  Environment.Flow ()
failedPayoutReTriggerAction merchantShortId opCity adminRequest = do
  logInfo $ "Failed payout re-trigger action: " <> adminRequest.id.getId <> maybe "" (\adminCheckerId -> "; admin checker: " <> adminCheckerId.getId) adminRequest.adminCheckerId
  let payoutRequestId = Id @DPayoutRequest.PayoutRequest adminRequest.referenceId
  void $ DMPayout.postPayoutPayoutRetry merchantShortId opCity payoutRequestId

tdsReimbursementAction :: DAdminRequest.AdminRequest -> Environment.Flow ()
tdsReimbursementAction adminRequest = do
  logInfo $ "TDS reimbursement action triggered: " <> adminRequest.id.getId <> maybe "" (\adminCheckerId -> "; admin checker: " <> adminCheckerId.getId) adminRequest.adminCheckerId
  mbDocument <- QCommonDriverOnboardingDocuments.findById (Id @DCommonDoc.CommonDriverOnboardingDocuments adminRequest.referenceId)
  document <- mbDocument & fromMaybeM (DocumentNotFound adminRequest.referenceId)
  QCommonDriverOnboardingDocuments.updateVerificationStatus Documents.VALID document.id
