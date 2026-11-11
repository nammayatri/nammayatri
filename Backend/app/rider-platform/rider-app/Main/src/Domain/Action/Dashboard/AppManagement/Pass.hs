module Domain.Action.Dashboard.AppManagement.Pass
  ( getPassCustomerAvailablePasses,
    getPassCustomerPurchasedPasses,
    getPassCustomerTransactions,
    postPassCustomerActivateToday,
    postPassCustomerPassSelect,
    getPassCustomerPaymentStatus,
    postPassCustomerPassResetDeviceSwitchCount,
    postPassCustomerPassUpdateProfilePicture,
    getPassCustomerPassPhoto,
    postPassCustomerPassRestore,
    listPassCatalog,
    createPass,
    updatePass,
    deletePass,
    getPassCatalog,
    listPassType,
    createPassType,
    updatePassType,
    listPassCategory,
    createPassCategory,
    updatePassCategory,
    getPurchasedPassPaymentTrips,
    adjustPurchasedPassPaymentTrips,
    terminatePurchasedPassPayment,
  )
where

import qualified API.Types.Dashboard.AppManagement.Pass
import qualified "this" API.Types.UI.Pass
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Time
import qualified "this" Domain.Action.UI.Pass as DPass
import qualified Domain.Action.UI.Payment as UIPayment
import qualified "this" Domain.Types.EntityValueLog as DEntityValueLog
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified "this" Domain.Types.Pass
import qualified "this" Domain.Types.PassCategory
import qualified Domain.Types.PassOverrideConfig as DPassOverrideConfig
import qualified "this" Domain.Types.PassType
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.PurchasedPass
import qualified Domain.Types.PurchasedPassPayment
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.Common.UI.Issue as IssueCommon
import qualified IssueManagement.Domain.Action.UI.Issue as IssueAction
import qualified IssueManagement.Domain.Types.MediaFile
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Types
import qualified Kernel.External.Types as Lang
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Lib.Payment.Storage.HistoryQueries.Refunds as HQRefunds
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified SharedLogic.FRFSPassOverride as FRFSPassOverride
import qualified SharedLogic.PassRestore as PassRestore
import qualified SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.PassCategory as CQPassCategory
import qualified Storage.CachedQueries.PassType as CQPassType
import qualified Storage.Queries.EntityValueLog as QEntityValueLog
import qualified Storage.Queries.Pass as QPass
import qualified Storage.Queries.PassCategory as QPassCategory
import qualified Storage.Queries.PassType as QPassType
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PurchasedPass as QPurchasedPass
import qualified Storage.Queries.PurchasedPassPayment as QPurchasedPassPayment
import Tools.Error

getPassCustomerAvailablePasses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Lang.Language -> Environment.Flow [API.Types.UI.Pass.PassInfoAPIEntity])
getPassCustomerAvailablePasses merchantShortId _opCity personId language = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DPass.getMultimodalPassAvailablePasses (Just personId, merchant.id) language

getPassCustomerPurchasedPasses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Kernel.Prelude.Maybe Domain.Types.PurchasedPass.StatusType -> Environment.Flow [API.Types.UI.Pass.PurchasedPassAPIEntity])
getPassCustomerPurchasedPasses merchantShortId _opCity personId language status = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DPass.getMultimodalPassListUtil True (Just personId, merchant.id) Nothing Nothing language Nothing Nothing status

getPassCustomerTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow [API.Types.UI.Pass.PurchasedPassTransactionAPIEntity])
getPassCustomerTransactions merchantShortId _opCity personId limit offset status = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DPass.getMultimodalPassTransactions (Just personId, merchant.id) limit offset status

postPassCustomerActivateToday :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment) -> Kernel.Prelude.Maybe Data.Time.Day -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPassCustomerActivateToday merchantShortId _opCity personId passNumber mbPurchasedPassPaymentId startDay = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DPass.postMultimodalPassActivateTodayUtil True (Just personId, merchant.id) passNumber startDay mbPurchasedPassPaymentId

postPassCustomerPassSelect :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.Dashboard.AppManagement.Pass.PurchasedPassSelectReq -> Environment.Flow API.Types.UI.Pass.PassSelectionAPIEntity)
postPassCustomerPassSelect merchantShortId _opCity personId passId _mbRequestorId req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DPass.postMultimodalPassSelectUtil True (Just personId, merchant.id) passId Nothing Nothing req.profilePicture Nothing req.startDay False Nothing

getPassCustomerPaymentStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow Lib.Payment.Domain.Action.PaymentStatusResp)
getPassCustomerPaymentStatus merchantShortId _opCity personId orderId _mbRequestorId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  UIPayment.getStatus (personId, merchant.id) orderId

postPassCustomerPassResetDeviceSwitchCount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPassCustomerPassResetDeviceSwitchCount merchantShortId _opCity personId purchasedPassId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DPass.postMultimodalPassResetDeviceSwitchCount (Just personId, merchant.id) purchasedPassId

postPassCustomerPassUpdateProfilePicture :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> IssueCommon.IssueMediaUploadReq -> Environment.Flow IssueCommon.IssueMediaUploadRes)
postPassCustomerPassUpdateProfilePicture merchantShortId _opCity personId purchasedPassId req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  uploadRes <- IssueAction.mediaUploadToS3 merchant.mediaFileSizeUpperLimit merchant.mediaFileUrlPattern req "pass-photo" personId.getId
  _ <- DPass.postMultimodalPassUpdateProfilePictureUtil personId merchant.id purchasedPassId uploadRes.fileId
  pure uploadRes

getPassCustomerPassPhoto :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile -> Environment.Flow Kernel.Prelude.Text)
getPassCustomerPassPhoto merchantShortId _opCity personId mediaId = do
  _ <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DPass.fetchPassPhotoFromS3 personId mediaId

postPassCustomerPassRestore :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPassCustomerPassRestore merchantShortId _opCity personId = do
  _ <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbMobileNumber <- mapM decrypt person.mobileNumber
  case mbMobileNumber of
    Just mobileNumber -> do
      PassRestore.restorePurchasedPassesIfNeeded person mobileNumber
      pure Kernel.Types.APISuccess.Success
    Nothing -> throwError $ InvalidRequest "Person has no mobile number, cannot restore passes"

listPassCatalog :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassType.PassType) -> Environment.Flow [API.Types.Dashboard.AppManagement.Pass.PassCatalogItem])
listPassCatalog merchantShortId opCity mbEnable mbPassTypeId = DPass.listPassCatalog merchantShortId opCity mbEnable mbPassTypeId

createPass :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Pass.PassCreateReq -> Environment.Flow API.Types.Dashboard.AppManagement.Pass.PassCreateResp)
createPass merchantShortId opCity req = DPass.createPassCatalog merchantShortId opCity req

updatePass :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> API.Types.Dashboard.AppManagement.Pass.PassUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
updatePass merchantShortId opCity passId req = DPass.updatePassCatalog merchantShortId opCity passId req

deletePass :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deletePass merchantShortId opCity passId = DPass.deletePassCatalog merchantShortId opCity passId

getPassCatalog :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> Environment.Flow API.Types.Dashboard.AppManagement.Pass.PassCatalogItem)
getPassCatalog = DPass.getPassCatalog

listPassType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory) -> Environment.Flow [API.Types.Dashboard.AppManagement.Pass.PassTypeItem])
listPassType merchantShortId opCity mbEnable mbPassCategoryId = do
  merchantOperatingCity <- DPass.findMerchantOperatingCity merchantShortId opCity
  categories <- QPassCategory.findAllByMerchantOperatingCityId merchantOperatingCity.id
  let targetCategories = maybe categories (\passCategoryId -> filter ((== passCategoryId) . (.id)) categories) mbPassCategoryId
  fmap concat . forM targetCategories $ \category -> do
    passTypes <- QPassType.findAllByPassCategoryId category.id
    pure . map (mkPassTypeItem category) $ filter (\passType -> maybe True (== DPass.isEnabledPassType passType) mbEnable) passTypes

createPassType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Pass.PassTypeCreateReq -> Environment.Flow API.Types.Dashboard.AppManagement.Pass.PassTypeCreateResp)
createPassType merchantShortId opCity req = do
  rejectNegative "maxPhotoChangeLimit" req.maxPhotoChangeLimit
  rejectNegative "photoReUploadTimeLimit" req.photoReUploadTimeLimit
  merchantOperatingCity <- DPass.findMerchantOperatingCity merchantShortId opCity
  category <- findPassCategoryInCity req.passCategoryId merchantOperatingCity.id
  passTypeId <- generateGUID
  now <- getCurrentTime
  QPassType.create
    Domain.Types.PassType.PassType
      { id = passTypeId,
        passCategoryId = category.id,
        name = req.name,
        catchline = req.catchline,
        title = req.title,
        description = req.description,
        order = req.order,
        merchantId = merchantOperatingCity.merchantId,
        merchantOperatingCityId = merchantOperatingCity.id,
        passEnum = req.passEnum,
        maxPhotoChangeLimit = req.maxPhotoChangeLimit,
        photoReUploadTimeLimit = req.photoReUploadTimeLimit,
        enable = req.enable,
        createdAt = now,
        updatedAt = now
      }
  CQPassType.clearCache passTypeId [category.id]
  pure $ API.Types.Dashboard.AppManagement.Pass.PassTypeCreateResp {passTypeId = passTypeId}

updatePassType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.PassType.PassType -> API.Types.Dashboard.AppManagement.Pass.PassTypeUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
updatePassType merchantShortId opCity passTypeId req = do
  rejectNegative "maxPhotoChangeLimit" req.maxPhotoChangeLimit
  rejectNegative "photoReUploadTimeLimit" req.photoReUploadTimeLimit
  merchantOperatingCity <- DPass.findMerchantOperatingCity merchantShortId opCity
  passType <- QPassType.findById passTypeId >>= fromMaybeM (PassTypeNotFound passTypeId.getId)
  unless (passType.merchantOperatingCityId == merchantOperatingCity.id) $
    throwError (InvalidRequest $ "Pass type " <> passTypeId.getId <> " does not belong to city " <> show opCity)
  whenJust req.passCategoryId $ \passCategoryId -> void $ findPassCategoryInCity passCategoryId merchantOperatingCity.id
  now <- getCurrentTime
  let updatedPassType =
        passType
          { Domain.Types.PassType.passCategoryId = fromMaybe passType.passCategoryId req.passCategoryId,
            Domain.Types.PassType.name = req.name <|> passType.name,
            Domain.Types.PassType.catchline = req.catchline <|> passType.catchline,
            Domain.Types.PassType.title = fromMaybe passType.title req.title,
            Domain.Types.PassType.description = req.description <|> passType.description,
            Domain.Types.PassType.order = fromMaybe passType.order req.order,
            Domain.Types.PassType.passEnum = req.passEnum <|> passType.passEnum,
            Domain.Types.PassType.maxPhotoChangeLimit = req.maxPhotoChangeLimit <|> passType.maxPhotoChangeLimit,
            Domain.Types.PassType.photoReUploadTimeLimit = req.photoReUploadTimeLimit <|> passType.photoReUploadTimeLimit,
            Domain.Types.PassType.enable = req.enable <|> passType.enable,
            Domain.Types.PassType.updatedAt = now
          }
  QPassType.updateByPrimaryKey updatedPassType
  CQPassType.clearCache passTypeId (nub [passType.passCategoryId, updatedPassType.passCategoryId])
  pure Kernel.Types.APISuccess.Success

listPassCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.Flow [API.Types.Dashboard.AppManagement.Pass.PassCategoryItem])
listPassCategory merchantShortId opCity mbEnable = do
  merchantOperatingCity <- DPass.findMerchantOperatingCity merchantShortId opCity
  categories <- QPassCategory.findAllByMerchantOperatingCityId merchantOperatingCity.id
  pure . map mkPassCategoryItem $ filter (\category -> maybe True (== DPass.isEnabledCategory category) mbEnable) categories

createPassCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Pass.PassCategoryCreateReq -> Environment.Flow API.Types.Dashboard.AppManagement.Pass.PassCategoryCreateResp)
createPassCategory merchantShortId opCity req = do
  merchantOperatingCity <- DPass.findMerchantOperatingCity merchantShortId opCity
  passCategoryId <- generateGUID
  now <- getCurrentTime
  QPassCategory.create
    Domain.Types.PassCategory.PassCategory
      { id = passCategoryId,
        name = req.name,
        description = req.description,
        order = req.order,
        merchantId = merchantOperatingCity.merchantId,
        merchantOperatingCityId = merchantOperatingCity.id,
        enable = req.enable,
        createdAt = now,
        updatedAt = now
      }
  CQPassCategory.clearCache passCategoryId merchantOperatingCity.id
  pure $ API.Types.Dashboard.AppManagement.Pass.PassCategoryCreateResp {passCategoryId = passCategoryId}

updatePassCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory -> API.Types.Dashboard.AppManagement.Pass.PassCategoryUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
updatePassCategory merchantShortId opCity passCategoryId req = do
  merchantOperatingCity <- DPass.findMerchantOperatingCity merchantShortId opCity
  category <- findPassCategoryInCity passCategoryId merchantOperatingCity.id
  now <- getCurrentTime
  QPassCategory.updateByPrimaryKey
    category
      { Domain.Types.PassCategory.name = fromMaybe category.name req.name,
        Domain.Types.PassCategory.description = fromMaybe category.description req.description,
        Domain.Types.PassCategory.order = req.order <|> category.order,
        Domain.Types.PassCategory.enable = req.enable <|> category.enable,
        Domain.Types.PassCategory.updatedAt = now
      }
  CQPassCategory.clearCache passCategoryId merchantOperatingCity.id
  pure Kernel.Types.APISuccess.Success

getPurchasedPassPaymentTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> Environment.Flow API.Types.Dashboard.AppManagement.Pass.PurchasedPassPaymentTripsResp)
getPurchasedPassPaymentTrips merchantShortId opCity purchasedPassPaymentId = do
  merchantOperatingCity <- DPass.findMerchantOperatingCity merchantShortId opCity
  payment <- findPaymentInCity purchasedPassPaymentId merchantOperatingCity.id
  mbEntry <- paymentEntry payment
  buildTripsResp payment mbEntry

adjustPurchasedPassPaymentTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> API.Types.Dashboard.AppManagement.Pass.PurchasedPassPaymentTripsAdjustReq -> Environment.Flow API.Types.Dashboard.AppManagement.Pass.PurchasedPassPaymentTripsResp)
adjustPurchasedPassPaymentTrips merchantShortId opCity purchasedPassPaymentId req = do
  when (req.delta == 0) $ throwError (InvalidRequest "delta must not be 0")
  when (abs req.delta > 10000) $ throwError (InvalidRequest "delta must be between -10000 and 10000")
  when (T.null (T.strip req.reason)) $ throwError (InvalidRequest "reason is required")
  let operationId = T.strip req.operationId
  when (T.null operationId) $ throwError (InvalidRequest "operationId is required")
  merchantOperatingCity <- DPass.findMerchantOperatingCity merchantShortId opCity
  void $ findPaymentInCity purchasedPassPaymentId merchantOperatingCity.id
  let adjustLocked = do
        attempts <-
          filter (\logEntry -> logEntry.operationType == DEntityValueLog.TripCountAdjust && logEntry.status /= DEntityValueLog.FAILED)
            <$> QEntityValueLog.findAllByEntityTypeAndEntityIdAndOperationId FRFSPassOverride.passPaymentEntityType purchasedPassPaymentId.getId operationId
        case attempts of
          existing : _
            | existing.status == DEntityValueLog.PENDING ->
              pure . Left $ "A previous adjustment with operationId " <> operationId <> " did not finish, check the trip count and retry with a new operationId"
            | existing.delta == Just req.delta -> do
              logInfo $ "adjustPurchasedPassPaymentTrips duplicate operationId=" <> operationId <> " paymentId=" <> purchasedPassPaymentId.getId <> ", not applied again"
              pure (Right ())
            | otherwise -> pure . Left $ "operationId " <> operationId <> " was already used for a different adjustment"
          [] -> do
            payment <- findPaymentInCity purchasedPassPaymentId merchantOperatingCity.id
            unless (payment.status `elem` [Domain.Types.PurchasedPass.Active, Domain.Types.PurchasedPass.PreBooked]) $
              throwError (InvalidRequest $ "Trips can only be adjusted on an active or pre-booked pass, this one is " <> show payment.status)
            entry <- paymentEntry payment >>= fromMaybeM (InvalidRequest "This pass has no trip-based benefit")
            FRFSPassOverride.remainingTrips payment (DPassOverrideConfig.entryBenefit entry) >>= \case
              Nothing -> pure (Left "This pass has unlimited trips")
              Just before
                | before + req.delta < 0 -> pure . Left $ "Only " <> show before <> " trips left, cannot remove " <> show (negate req.delta)
                | otherwise -> do
                  logEntry <- logTripAdjustment payment operationId req before
                  let markFailed = void . withTryCatch "adjustPurchasedPassPaymentTrips:markFailed" $ QEntityValueLog.updateByPrimaryKey logEntry {DEntityValueLog.status = DEntityValueLog.FAILED, DEntityValueLog.valueAfter = Just before}
                  withTryCatch "adjustPurchasedPassPaymentTrips:adjust" (FRFSPassOverride.adjustTrips payment entry req.delta) >>= \case
                    Left err -> markFailed >> throwM err
                    Right (Left reason) -> do
                      markFailed
                      pure (Left reason)
                    Right (Right remaining) ->
                      withTryCatch "adjustPurchasedPassPaymentTrips:markApplied" (QEntityValueLog.updateByPrimaryKey logEntry {DEntityValueLog.status = DEntityValueLog.APPLIED, DEntityValueLog.valueBefore = Just (remaining - req.delta), DEntityValueLog.valueAfter = Just remaining}) >>= \case
                        Left err -> do
                          FRFSPassOverride.revertTripAdjustment payment req.delta
                          markFailed
                          throwM err
                        Right () -> do
                          logInfo $ "adjustPurchasedPassPaymentTrips paymentId=" <> purchasedPassPaymentId.getId <> " operationId=" <> operationId <> " delta=" <> show req.delta <> " remaining=" <> show remaining <> " reason=" <> req.reason
                          pure (Right ())
  FRFSPassOverride.tryWithTripCountLock purchasedPassPaymentId adjustLocked >>= \case
    Nothing -> throwError (InvalidRequest "This pass's trip count is being updated, please retry")
    Just (Left reason) -> throwError (InvalidRequest reason)
    Just (Right ()) -> do
      updatedPayment <- findPaymentInCity purchasedPassPaymentId merchantOperatingCity.id
      paymentEntry updatedPayment >>= buildTripsResp updatedPayment

logTripAdjustment :: Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> Kernel.Prelude.Text -> API.Types.Dashboard.AppManagement.Pass.PurchasedPassPaymentTripsAdjustReq -> Kernel.Prelude.Int -> Environment.Flow DEntityValueLog.EntityValueLog
logTripAdjustment payment operationId req before = do
  logId <- generateGUID
  now <- getCurrentTime
  actorInfo <- asks (.actorInfo)
  let logEntry =
        DEntityValueLog.EntityValueLog
          { id = logId,
            entityType = FRFSPassOverride.passPaymentEntityType,
            entityId = payment.id.getId,
            operationId = operationId,
            operationType = DEntityValueLog.TripCountAdjust,
            status = DEntityValueLog.PENDING,
            delta = Just req.delta,
            valueBefore = Just before,
            valueAfter = Just (before + req.delta),
            reason = Just (T.strip req.reason),
            actorType = actorInfo.actorType,
            actorId = actorInfo.actorId,
            merchantId = payment.merchantId,
            merchantOperatingCityId = payment.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }
  QEntityValueLog.create logEntry
  pure logEntry

terminatePurchasedPassPayment :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> API.Types.Dashboard.AppManagement.Pass.PurchasedPassPaymentTerminateReq -> Environment.Flow API.Types.Dashboard.AppManagement.Pass.PurchasedPassPaymentTripsResp)
terminatePurchasedPassPayment merchantShortId opCity purchasedPassPaymentId req = do
  let reason = T.strip req.reason
  when (T.null reason) $ throwError (InvalidRequest "reason is required")
  let refundRequested = fromMaybe False req.refund
  merchantOperatingCity <- DPass.findMerchantOperatingCity merchantShortId opCity
  initialPayment <- findPaymentInCity purchasedPassPaymentId merchantOperatingCity.id
  alreadyRefunded <- if refundRequested then terminatedWithRefund initialPayment else pure False
  when (refundRequested && not alreadyRefunded) $ ensureRefundable initialPayment
  let terminateLocked = do
        payment <- findPaymentInCity purchasedPassPaymentId merchantOperatingCity.id
        terminationLogs <- passTerminationLogs payment
        let hasAppliedTermination = any (\entry -> entry.status == DEntityValueLog.APPLIED) terminationLogs
            mbPendingTermination = find (\entry -> entry.status == DEntityValueLog.PENDING) terminationLogs
        if refundRequested && hasAppliedTermination && payment.status `elem` terminatedRefundStatuses
          then pure (Right ())
          else
            if payment.status `elem` [Domain.Types.PurchasedPass.Active, Domain.Types.PurchasedPass.PreBooked]
              then Right <$> (maybe (logPassTermination payment reason) pure mbPendingTermination >>= applyTermination payment)
              else case mbPendingTermination of
                Just pendingLog | payment.status == Domain.Types.PurchasedPass.Expired -> Right <$> applyTermination payment pendingLog
                _ -> pure . Left $ "Only an active or pre-booked pass can be terminated, this one is " <> show payment.status
  FRFSPassOverride.tryWithTripCountLock purchasedPassPaymentId terminateLocked >>= \case
    Nothing -> throwError (InvalidRequest "This pass's trip count is being updated, please retry")
    Just (Left err) -> throwError (InvalidRequest err)
    Just (Right ()) -> do
      when alreadyRefunded $
        logInfo $ "terminatePurchasedPassPayment repeat request paymentId=" <> purchasedPassPaymentId.getId <> ", already terminated with a refund"
      when (refundRequested && not alreadyRefunded) $ refundTerminatedPayment initialPayment
      updatedPayment <- findPaymentInCity purchasedPassPaymentId merchantOperatingCity.id
      paymentEntry updatedPayment >>= buildTripsResp updatedPayment

terminatedWithRefund :: Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> Environment.Flow Bool
terminatedWithRefund payment = do
  terminationLogs <- passTerminationLogs payment
  let terminated =
        any (\entry -> entry.status == DEntityValueLog.APPLIED) terminationLogs
          && payment.status `elem` terminatedRefundStatuses
  if terminated
    then do
      order <- QPaymentOrder.findById payment.orderId >>= fromMaybeM (PaymentOrderNotFound payment.orderId.getId)
      isJust <$> HQRefunds.findLatestByOrderId order.shortId
    else pure False

terminatedRefundStatuses :: [Domain.Types.PurchasedPass.StatusType]
terminatedRefundStatuses =
  [ Domain.Types.PurchasedPass.Expired,
    Domain.Types.PurchasedPass.RefundPending,
    Domain.Types.PurchasedPass.RefundInitiated,
    Domain.Types.PurchasedPass.Refunded
  ]

ensureRefundable :: Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> Environment.Flow ()
ensureRefundable payment = do
  when (payment.amount <= 0) $ throwError (InvalidRequest "This pass was not paid for, there is nothing to refund")
  order <- QPaymentOrder.findById payment.orderId >>= fromMaybeM (PaymentOrderNotFound payment.orderId.getId)
  unless (order.status == Payment.CHARGED) $
    throwError (InvalidRequest $ "Only a charged payment can be refunded, this one is " <> show order.status)
  existingRefund <- HQRefunds.findLatestByOrderId order.shortId
  whenJust existingRefund $ \_ -> throwError (InvalidRequest "A refund already exists for this pass payment")

refundTerminatedPayment :: Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> Environment.Flow ()
refundTerminatedPayment payment =
  withTryCatch "terminatePurchasedPassPayment:refund" (SPayment.markRefundPendingAndSyncOrderStatus payment.merchantId payment.personId payment.orderId) >>= \case
    Right _ -> logInfo $ "terminatePurchasedPassPayment refund initiated paymentId=" <> payment.id.getId <> " orderId=" <> payment.orderId.getId <> " amount=" <> show payment.amount
    Left err -> do
      logError $ "terminatePurchasedPassPayment refund failed paymentId=" <> payment.id.getId <> " err=" <> show err
      throwError (InternalError "The pass was terminated but the refund could not be started, retry terminate with refund to try again")

applyTermination :: Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> DEntityValueLog.EntityValueLog -> Environment.Flow ()
applyTermination payment logEntry = do
  now <- getCurrentTime
  today <- FRFSPassOverride.localTripDayForCity payment.merchantOperatingCityId now
  let endDate = Data.Time.addDays (-1) today
      startDate = min payment.startDate endDate
  mbBenefit <- fmap DPassOverrideConfig.entryBenefit <$> paymentEntry payment
  zeroedFrom <- FRFSPassOverride.zeroTrips payment mbBenefit
  let before = logEntry.valueBefore <|> zeroedFrom
      recordedLog = logEntry {DEntityValueLog.valueBefore = before}
  when (isNothing logEntry.valueBefore && isJust before) $
    QEntityValueLog.updateByPrimaryKey recordedLog
  QPurchasedPassPayment.updateStatusAndDatesById startDate endDate Domain.Types.PurchasedPass.Expired payment.id
  expireParentPass payment startDate endDate
  QEntityValueLog.updateByPrimaryKey
    recordedLog
      { DEntityValueLog.status = DEntityValueLog.APPLIED,
        DEntityValueLog.delta = negate <$> before,
        DEntityValueLog.valueBefore = before,
        DEntityValueLog.valueAfter = 0 <$ before
      }
  logInfo $ "terminatePurchasedPassPayment paymentId=" <> payment.id.getId <> " endDate=" <> show endDate <> " tripsBefore=" <> show before <> " logId=" <> logEntry.id.getId

passTerminationLogs :: Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> Environment.Flow [DEntityValueLog.EntityValueLog]
passTerminationLogs payment =
  filter (\entry -> entry.operationType == DEntityValueLog.PassTerminate)
    <$> QEntityValueLog.findAllByEntityTypeAndEntityId FRFSPassOverride.passPaymentEntityType payment.id.getId

expireParentPass :: Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> Data.Time.Day -> Data.Time.Day -> Environment.Flow ()
expireParentPass payment startDate endDate =
  QPurchasedPass.findByPrimaryKey payment.purchasedPassId >>= \case
    Nothing -> logWarning $ "terminatePurchasedPassPayment: purchased pass not found for paymentId=" <> payment.id.getId
    Just purchasedPass -> do
      let isCurrentPayment = case purchasedPass.currentPaymentOrderId of
            Just orderId -> orderId == payment.orderId
            Nothing -> purchasedPass.startDate == payment.startDate
      when isCurrentPayment $
        QPurchasedPass.updateByPrimaryKey
          purchasedPass
            { Domain.Types.PurchasedPass.status = Domain.Types.PurchasedPass.Expired,
              Domain.Types.PurchasedPass.startDate = startDate,
              Domain.Types.PurchasedPass.endDate = endDate
            }

logPassTermination :: Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> Kernel.Prelude.Text -> Environment.Flow DEntityValueLog.EntityValueLog
logPassTermination payment reason = do
  logId <- generateGUID
  now <- getCurrentTime
  actorInfo <- asks (.actorInfo)
  let logEntry =
        DEntityValueLog.EntityValueLog
          { id = logId,
            entityType = FRFSPassOverride.passPaymentEntityType,
            entityId = payment.id.getId,
            operationId = logId.getId,
            operationType = DEntityValueLog.PassTerminate,
            status = DEntityValueLog.PENDING,
            delta = Nothing,
            valueBefore = Nothing,
            valueAfter = Nothing,
            reason = Just reason,
            actorType = actorInfo.actorType,
            actorId = actorInfo.actorId,
            merchantId = payment.merchantId,
            merchantOperatingCityId = payment.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }
  QEntityValueLog.create logEntry
  pure logEntry

rejectNegative :: (Ord a, Num a) => Kernel.Prelude.Text -> Kernel.Prelude.Maybe a -> Environment.Flow ()
rejectNegative fieldName value =
  when (maybe False (< 0) value) $
    throwError (InvalidRequest $ fieldName <> " must not be negative")

findPassCategoryInCity :: Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Environment.Flow Domain.Types.PassCategory.PassCategory
findPassCategoryInCity passCategoryId merchantOperatingCityId = do
  category <- QPassCategory.findById passCategoryId >>= fromMaybeM (PassCategoryNotFound passCategoryId.getId)
  unless (category.merchantOperatingCityId == merchantOperatingCityId) $
    throwError (InvalidRequest $ "Pass category " <> passCategoryId.getId <> " does not belong to this city")
  pure category

findPaymentInCity :: Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Environment.Flow Domain.Types.PurchasedPassPayment.PurchasedPassPayment
findPaymentInCity purchasedPassPaymentId merchantOperatingCityId = do
  payment <- QPurchasedPassPayment.findByPrimaryKey purchasedPassPaymentId >>= fromMaybeM (PurchasedPassPaymentNotFound purchasedPassPaymentId.getId)
  unless (payment.merchantOperatingCityId == merchantOperatingCityId) $
    throwError (PurchasedPassPaymentNotFound purchasedPassPaymentId.getId)
  pure payment

paymentEntry :: Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> Environment.Flow (Kernel.Prelude.Maybe DPassOverrideConfig.OverrideBenefitEntry)
paymentEntry payment = do
  mbPass <- maybe (pure Nothing) QPass.findById payment.passId
  maybe (pure Nothing) (FRFSPassOverride.entryForPayment payment) mbPass

buildTripsResp :: Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> Kernel.Prelude.Maybe DPassOverrideConfig.OverrideBenefitEntry -> Environment.Flow API.Types.Dashboard.AppManagement.Pass.PurchasedPassPaymentTripsResp
buildTripsResp payment mbEntry = do
  let mbBenefit = DPassOverrideConfig.entryBenefit <$> mbEntry
  remainingTripCount <- maybe (pure Nothing) (FRFSPassOverride.remainingTrips payment) mbBenefit
  effectiveMaximumTripCount <- forM (mbBenefit >>= (.maximumTripCount)) $ \soldCapacity ->
    max 0 . (soldCapacity +) <$> FRFSPassOverride.netTripAdjustment payment
  pure
    API.Types.Dashboard.AppManagement.Pass.PurchasedPassPaymentTripsResp
      { purchasedPassPaymentId = payment.id,
        purchasedPassId = payment.purchasedPassId,
        passCode = payment.passCode,
        status = payment.status,
        startDate = payment.startDate,
        endDate = payment.endDate,
        sourceStopCode = payment.sourceStopCode,
        destinationStopCode = payment.destinationStopCode,
        unlimitedTripCount = maybe False FRFSPassOverride.isUnlimitedBenefit mbBenefit,
        maximumTripCount = effectiveMaximumTripCount,
        remainingTripCount = remainingTripCount,
        storedTripCount = payment.availableTripCount
      }

mkPassTypeItem :: Domain.Types.PassCategory.PassCategory -> Domain.Types.PassType.PassType -> API.Types.Dashboard.AppManagement.Pass.PassTypeItem
mkPassTypeItem category passType =
  API.Types.Dashboard.AppManagement.Pass.PassTypeItem
    { id = passType.id,
      passCategoryId = passType.passCategoryId,
      passCategoryName = category.name,
      name = passType.name,
      catchline = passType.catchline,
      title = passType.title,
      description = passType.description,
      order = passType.order,
      passEnum = passType.passEnum,
      maxPhotoChangeLimit = passType.maxPhotoChangeLimit,
      photoReUploadTimeLimit = passType.photoReUploadTimeLimit,
      enable = DPass.isEnabledPassType passType
    }

mkPassCategoryItem :: Domain.Types.PassCategory.PassCategory -> API.Types.Dashboard.AppManagement.Pass.PassCategoryItem
mkPassCategoryItem category =
  API.Types.Dashboard.AppManagement.Pass.PassCategoryItem
    { id = category.id,
      name = category.name,
      description = category.description,
      order = category.order,
      enable = DPass.isEnabledCategory category
    }
