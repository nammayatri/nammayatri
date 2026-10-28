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
  )
where

import qualified API.Types.Dashboard.AppManagement.Pass
import qualified "this" API.Types.UI.Pass
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Time
import qualified "this" Domain.Action.UI.Pass as DPass
import qualified Domain.Action.UI.Payment as UIPayment
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
import qualified Kernel.External.Types
import qualified Kernel.External.Types as Lang
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified SharedLogic.FRFSPassOverride as FRFSPassOverride
import qualified SharedLogic.PassRestore as PassRestore
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.PassCategory as CQPassCategory
import qualified Storage.CachedQueries.PassType as CQPassType
import qualified Storage.Queries.Pass as QPass
import qualified Storage.Queries.PassCategory as QPassCategory
import qualified Storage.Queries.PassType as QPassType
import qualified Storage.Queries.Person as QP
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
getPassCatalog merchantShortId opCity passId = DPass.getPassCatalog merchantShortId opCity passId

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
  when (T.null (T.strip req.reason)) $ throwError (InvalidRequest "reason is required")
  merchantOperatingCity <- DPass.findMerchantOperatingCity merchantShortId opCity
  payment <- findPaymentInCity purchasedPassPaymentId merchantOperatingCity.id
  unless (payment.status `elem` [Domain.Types.PurchasedPass.Active, Domain.Types.PurchasedPass.PreBooked]) $
    throwError (InvalidRequest $ "Trips can only be adjusted on an active or pre-booked pass, this one is " <> show payment.status)
  entry <- paymentEntry payment >>= fromMaybeM (InvalidRequest "This pass has no trip-based benefit")
  Hedis.whenWithLockRedisAndReturnValue ("PassTripAdjust:Lock:" <> purchasedPassPaymentId.getId) 30 (FRFSPassOverride.adjustTrips payment entry req.delta) >>= \case
    Left _ -> throwError (InvalidRequest "Another trip adjustment is in progress for this pass, please retry")
    Right (Left reason) -> throwError (InvalidRequest reason)
    Right (Right remaining) -> do
      logInfo $ "adjustPurchasedPassPaymentTrips paymentId=" <> purchasedPassPaymentId.getId <> " delta=" <> show req.delta <> " remaining=" <> show remaining <> " reason=" <> req.reason
      updatedPayment <- findPaymentInCity purchasedPassPaymentId merchantOperatingCity.id
      paymentEntry updatedPayment >>= buildTripsResp updatedPayment

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
        maximumTripCount = mbBenefit >>= (.maximumTripCount),
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
