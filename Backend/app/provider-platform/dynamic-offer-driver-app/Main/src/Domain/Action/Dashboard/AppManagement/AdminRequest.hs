module Domain.Action.Dashboard.AppManagement.AdminRequest
  ( postAdminRequestCreate,
    getAdminRequestList,
    postAdminRequestRespond,
  )
where

import qualified API.Types.Dashboard.AppManagement.AdminRequest as Common
import Dashboard.Common as Common
import qualified Data.Text as T
import qualified Domain.Action.Dashboard.Management.Payout as DMPayout
import qualified Domain.Types.AdminRequest as DAdminRequest
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified "this" Domain.Types.Person as DP
import qualified Environment
import Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PayoutRequest as DPayoutRequest
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPayoutRequest
import qualified SharedLogic.Finance.Wallet as SFW
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.AdminRequest as QAdminRequest
import qualified Storage.Queries.Booking as QBooking
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

    -- admin request data validation
    (amount', currency') <- case req.actionType of
      DAdminRequest.LedgerAdjustment -> do
        reqReferenceType <- req.referenceType & fromMaybeM (InvalidRequest "Reference type required for this action type")
        reqAdjustmentType <- req.adjustmentType & fromMaybeM (InvalidRequest "Adjustment type required for this action type")
        _reqAdjustmentSource <- req.source & fromMaybeM (InvalidRequest "Adjustment source required for this action type")
        reqAmount <- req.amount & fromMaybeM (InvalidRequest "Amount required for this action type")
        case reqReferenceType of
          _ | reqReferenceType == SFW.walletReferenceWalletIncentive -> do
            case req.referenceTable of
              DAdminRequest.RIDE -> do
                pure () -- TODO referenceId, amount and currency validation
              _ -> throwError (InvalidRequest "Invalid reference table for this action type")
          _ | reqReferenceType `elem` [SFW.walletReferenceBaseRide, SFW.walletReferenceDriverCancellationCharges, SFW.walletReferenceCustomerCancellationCharges] -> do
            case req.referenceTable of
              DAdminRequest.BOOKING -> do
                booking <- QBooking.findById (Id @DBooking.Booking req.referenceId) >>= fromMaybeM (BookingDoesNotExist req.referenceId)
                unless (reqAmount.currency == booking.currency) $ throwError (InvalidRequest "Invalid currency")
                when (reqReferenceType == SFW.walletReferenceBaseRide) do
                  unless (booking.status == DBooking.COMPLETED) $ do
                    throwError (BookingInvalidStatus $ show booking.status)
                  ride <- QRide.findOneByBookingId booking.id >>= fromMaybeM (RideDoesNotExist booking.id.getId)
                  unless (fromMaybe ride.driverId ride.fleetOwnerId == req.personId) $ throwError (InvalidRequest "Invalid personId")
                  rideFare <- ride.fare & fromMaybeM (InternalError "Ride fare is not present.")
                  when (reqAdjustmentType == DAdminRequest.Debit && reqAmount.amount > rideFare) $ do
                    throwError (InvalidRequest "Could not debit more than ride fare")
                pure () -- TODO referenceId, amount and currency validation for other referenceType
              _ -> throwError (InvalidRequest "Invalid reference table for this action type and reference type")
          _ -> throwError (InvalidRequest $ "Supported reference types for this action type: " <> T.intercalate ", " [SFW.walletReferenceBaseRide, SFW.walletReferenceDriverCancellationCharges, SFW.walletReferenceCustomerCancellationCharges, SFW.walletReferenceWalletIncentive])
        pure (reqAmount.amount, reqAmount.currency)
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
            payoutAmount <- payoutRequest.amount & fromMaybeM (InternalError "Payout request amount not found")
            case req.amount of
              Nothing -> pure (payoutAmount, merchantOpCity.currency)
              Just reqAmount -> do
                unless (reqAmount.amount == payoutAmount) $
                  throwError (InvalidRequest "Amount should match payout request amount")
                unless (reqAmount.currency == merchantOpCity.currency) $
                  throwError (InvalidRequest "Currency should match merchant operating city currency")
                pure (reqAmount.amount, reqAmount.currency)
          _ -> throwError (InvalidRequest "Invalid reference table for this action type")
      DAdminRequest.TDSReimbursement -> do
        whenJust req.referenceType $ \_ -> throwError (InvalidRequest "Reference type not required for this action type")
        whenJust req.adjustmentType $ \_ -> throwError (InvalidRequest "Adjustment type not required for this action type")
        whenJust req.source $ \_ -> throwError (InvalidRequest "Adjustment source not required for this action type")
        -- TODO referenceId, amount and currency validation
        let amount = maybe 0.0 (.amount) req.amount
            currency = maybe INR (.currency) req.amount
        pure (amount, currency)

    mbAdminMaker <- QP.findById (Id @DP.Person requestorId)
    let adminMakerName = case mbAdminMaker of
          Nothing -> requestorName
          Just adminMaker -> adminMaker.firstName <> maybe "" (" " <>) adminMaker.middleName <> maybe "" (" " <>) adminMaker.lastName
    adminRequest <- buildAdminRequest merchantOpCity requestorId adminMakerName amount' currency' req
    QAdminRequest.create adminRequest
  pure Success

buildAdminRequest ::
  MonadFlow m =>
  DMOC.MerchantOperatingCity ->
  Text ->
  Text ->
  HighPrecMoney ->
  Currency ->
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
      amount = PriceAPIEntity amount currency,
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
postAdminRequestRespond merchantShortId opCity adminRequestId requestorId requestorName req = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  referenceId <- (.referenceId) <$> (QAdminRequest.findByPrimaryKey adminRequestId >>= fromMaybeM (AdminRequestDoesNotExist adminRequestId.getId))
  Redis.whenWithLockRedis (adminRequestProcessingLockKey referenceId) 60 $ do
    -- Fetch admin request again to avoid race condition
    adminRequest <- QAdminRequest.findByPrimaryKey adminRequestId >>= fromMaybeM (AdminRequestDoesNotExist adminRequestId.getId)
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
          DAdminRequest.LedgerAdjustment -> withTryCatch "ledgerAdjustmentAction" $ ledgerAdjustmentAction updAdminRequest
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

ledgerAdjustmentAction ::
  DAdminRequest.AdminRequest ->
  Environment.Flow ()
ledgerAdjustmentAction adminRequest = do
  logInfo $ "Ledger adjustment action triggered: " <> adminRequest.id.getId <> maybe "" (\adminCheckerId -> "; admin checker: " <> adminCheckerId.getId) adminRequest.adminCheckerId
  throwError $ InvalidRequest "Ledger adjustment action is not implemented"

failedPayoutReTriggerAction ::
  ShortId DM.Merchant ->
  Context.City ->
  DAdminRequest.AdminRequest ->
  Environment.Flow ()
failedPayoutReTriggerAction merchantShortId opCity adminRequest = do
  logInfo $ "Failed payout re-trigger action: " <> adminRequest.id.getId <> maybe "" (\adminCheckerId -> "; admin checker: " <> adminCheckerId.getId) adminRequest.adminCheckerId
  let payoutRequestId = Id @DPayoutRequest.PayoutRequest adminRequest.referenceId
  void $ DMPayout.postPayoutPayoutRetry merchantShortId opCity payoutRequestId

tdsReimbursementAction ::
  DAdminRequest.AdminRequest ->
  Environment.Flow ()
tdsReimbursementAction adminRequest = do
  logInfo $ "TDS reimbursement action triggered: " <> adminRequest.id.getId <> maybe "" (\adminCheckerId -> "; admin checker: " <> adminCheckerId.getId) adminRequest.adminCheckerId
  throwError $ InvalidRequest "TDS reimbursement action is not implemented"
