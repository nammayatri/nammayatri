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
  )
where

import qualified API.Types.Dashboard.AppManagement.Pass
import qualified "this" API.Types.UI.Pass
import qualified Data.Time
import qualified "this" Domain.Action.UI.Pass as DPass
import qualified Domain.Action.UI.Payment as UIPayment
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Pass
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
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified SharedLogic.PassRestore as PassRestore
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Pass as CQPass
import qualified Storage.CachedQueries.PassCategory as CQPassCategory
import qualified Storage.CachedQueries.PassType as CQPassType
import qualified Storage.Queries.Pass as QPass
import qualified Storage.Queries.PassExtra as QPassExtra
import qualified Storage.Queries.PassTypeExtra as QPassType
import qualified Storage.Queries.Person as QP
import qualified Tools.ActorInfo as ActorInfo
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
postPassCustomerPassSelect merchantShortId _opCity personId passId mbRequestorId req = ActorInfo.withDashboardMbPersonIdActorInfo ((Kernel.Types.Id.Id @Domain.Types.Person.Person) <$> mbRequestorId) $ do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DPass.postMultimodalPassSelectUtil True (Just personId, merchant.id) passId Nothing Nothing req.profilePicture Nothing req.startDay

getPassCustomerPaymentStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow Lib.Payment.Domain.Action.PaymentStatusResp)
getPassCustomerPaymentStatus merchantShortId _opCity personId orderId mbRequestorId = ActorInfo.withDashboardMbPersonIdActorInfo ((Kernel.Types.Id.Id @Domain.Types.Person.Person) <$> mbRequestorId) $ do
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
listPassCatalog merchantShortId opCity mbEnable mbPassTypeId = do
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  categories <- CQPassCategory.findAllByMerchantOperatingCityId merchantOperatingCity.id
  passTypes <- case mbPassTypeId of
    Just passTypeId ->
      CQPassType.findById passTypeId
        <&> maybeToList . mfilter ((== merchantOperatingCity.id) . (.merchantOperatingCityId))
    Nothing -> concat <$> mapM (CQPassType.findAllByPassCategoryId . (.id)) categories
  let categoryNameById = map (\c -> (c.id, c.name)) categories
      enableFilters = maybe [True, False] (: []) mbEnable
  fmap concat $
    forM passTypes $ \passType -> do
      passes <- concat <$> mapM (CQPass.findAllByPassTypeIdAndEnabled passType.id) enableFilters
      pure $ map (mkPassCatalogItem passType (Kernel.Prelude.lookup passType.passCategoryId categoryNameById)) passes

mkPassCatalogItem :: Domain.Types.PassType.PassType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.Pass.Pass -> API.Types.Dashboard.AppManagement.Pass.PassCatalogItem
mkPassCatalogItem passType mbCategoryName passRow =
  API.Types.Dashboard.AppManagement.Pass.PassCatalogItem
    { id = passRow.id,
      passTypeId = passRow.passTypeId,
      passTypeTitle = passType.title,
      passCategoryName = fromMaybe "" mbCategoryName,
      code = passRow.code,
      name = passRow.name,
      description = passRow.description,
      amount = passRow.amount,
      benefitDescription = passRow.benefitDescription,
      benefit = passRow.benefit,
      applicableVehicleServiceTiers = passRow.applicableVehicleServiceTiers,
      documentsRequired = passRow.documentsRequired,
      pricingTiers = passRow.pricingTiers,
      maxValidTrips = passRow.maxValidTrips,
      maxValidDays = passRow.maxValidDays,
      verificationValidity = passRow.verificationValidity,
      order = passRow.order,
      enable = passRow.enable,
      autoApply = passRow.autoApply,
      maxSwitchCount = (.maxSwitchCount) <$> passRow.passConfig,
      minFare = passRow.minFare,
      maxFare = passRow.maxFare,
      formVerificationConfig = passRow.formVerificationConfig
    }

createPass :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Pass.PassCreateReq -> Environment.Flow API.Types.Dashboard.AppManagement.Pass.PassCreateResp)
createPass merchantShortId opCity req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  passType <- QPassType.findById req.passTypeId >>= fromMaybeM (PassTypeNotFound req.passTypeId.getId)
  -- A pass under a pass type belonging to another city would be invisible to the
  -- city-scoped catalog walk, so reject rather than create an orphan row.
  unless (passType.merchantOperatingCityId == merchantOperatingCity.id) $
    throwError (InvalidRequest $ "Pass type " <> req.passTypeId.getId <> " does not belong to city " <> show opCity)
  whenJustM (QPassExtra.findByCodeAndMerchantOperatingCityId req.code merchantOperatingCity.id) $ \_ ->
    throwError (InvalidRequest $ "Pass with code " <> req.code <> " already exists in this city")
  passId <- generateGUID
  now <- getCurrentTime
  let passRow =
        Domain.Types.Pass.Pass
          { id = passId,
            passTypeId = req.passTypeId,
            code = req.code,
            name = req.name,
            description = req.description,
            amount = req.amount,
            benefitDescription = req.benefitDescription,
            benefit = req.benefit,
            applicableVehicleServiceTiers = req.applicableVehicleServiceTiers,
            documentsRequired = req.documentsRequired,
            pricingTiers = req.pricingTiers,
            maxValidTrips = req.maxValidTrips,
            maxValidDays = req.maxValidDays,
            verificationValidity = fromMaybe (Seconds 9000) req.verificationValidity,
            order = req.order,
            enable = req.enable,
            autoApply = req.autoApply,
            passConfig = Domain.Types.Pass.PassConfig <$> req.maxSwitchCount,
            minFare = req.minFare,
            maxFare = req.maxFare,
            formVerificationConfig = req.formVerificationConfig,
            purchaseEligibilityJsonLogic = [],
            redeemEligibilityJsonLogic = [],
            merchantId = merchant.id,
            merchantOperatingCityId = merchantOperatingCity.id,
            createdAt = now,
            updatedAt = now
          }
  QPass.create passRow
  CQPass.clearCacheByPassTypeIdAndEnabled passRow.passTypeId passRow.enable
  pure $ API.Types.Dashboard.AppManagement.Pass.PassCreateResp {passId = passId}

updatePass :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> API.Types.Dashboard.AppManagement.Pass.PassUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
updatePass merchantShortId opCity passId req = do
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  passRow <- QPass.findByPrimaryKey passId >>= fromMaybeM (PassNotFound passId.getId)
  unless (passRow.merchantOperatingCityId == merchantOperatingCity.id) $
    throwError (InvalidRequest $ "Pass " <> passId.getId <> " does not belong to city " <> show opCity)
  whenJust req.passTypeId $ \newPassTypeId -> do
    passType <- QPassType.findById newPassTypeId >>= fromMaybeM (PassTypeNotFound newPassTypeId.getId)
    unless (passType.merchantOperatingCityId == merchantOperatingCity.id) $
      throwError (InvalidRequest $ "Pass type " <> newPassTypeId.getId <> " does not belong to city " <> show opCity)
  whenJust (mfilter (/= passRow.code) req.code) $ \newCode ->
    whenJustM (QPassExtra.findByCodeAndMerchantOperatingCityId newCode merchantOperatingCity.id) $ \_ ->
      throwError (InvalidRequest $ "Pass with code " <> newCode <> " already exists in this city")
  now <- getCurrentTime
  let updatedPass =
        passRow
          { Domain.Types.Pass.passTypeId = fromMaybe passRow.passTypeId req.passTypeId,
            Domain.Types.Pass.code = fromMaybe passRow.code req.code,
            Domain.Types.Pass.name = req.name <|> passRow.name,
            Domain.Types.Pass.description = req.description <|> passRow.description,
            Domain.Types.Pass.amount = fromMaybe passRow.amount req.amount,
            Domain.Types.Pass.benefitDescription = fromMaybe passRow.benefitDescription req.benefitDescription,
            Domain.Types.Pass.benefit = req.benefit <|> passRow.benefit,
            Domain.Types.Pass.applicableVehicleServiceTiers = fromMaybe passRow.applicableVehicleServiceTiers req.applicableVehicleServiceTiers,
            Domain.Types.Pass.documentsRequired = fromMaybe passRow.documentsRequired req.documentsRequired,
            Domain.Types.Pass.pricingTiers = req.pricingTiers <|> passRow.pricingTiers,
            Domain.Types.Pass.maxValidTrips = req.maxValidTrips <|> passRow.maxValidTrips,
            Domain.Types.Pass.maxValidDays = req.maxValidDays <|> passRow.maxValidDays,
            Domain.Types.Pass.verificationValidity = fromMaybe passRow.verificationValidity req.verificationValidity,
            Domain.Types.Pass.order = fromMaybe passRow.order req.order,
            Domain.Types.Pass.enable = fromMaybe passRow.enable req.enable,
            Domain.Types.Pass.autoApply = fromMaybe passRow.autoApply req.autoApply,
            Domain.Types.Pass.passConfig = (Domain.Types.Pass.PassConfig <$> req.maxSwitchCount) <|> passRow.passConfig,
            Domain.Types.Pass.minFare = req.minFare <|> passRow.minFare,
            Domain.Types.Pass.maxFare = req.maxFare <|> passRow.maxFare,
            Domain.Types.Pass.formVerificationConfig = req.formVerificationConfig <|> passRow.formVerificationConfig,
            Domain.Types.Pass.updatedAt = now
          }
  QPass.updateByPrimaryKey updatedPass
  CQPass.clearCacheByPassTypeIdAndEnabled passRow.passTypeId passRow.enable
  CQPass.clearCacheByPassTypeIdAndEnabled updatedPass.passTypeId updatedPass.enable
  pure Kernel.Types.APISuccess.Success

-- | Soft delete (enable = false) so purchased_pass rows keep their pass reference.
deletePass :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deletePass merchantShortId opCity passId = do
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  passRow <- QPass.findByPrimaryKey passId >>= fromMaybeM (PassNotFound passId.getId)
  unless (passRow.merchantOperatingCityId == merchantOperatingCity.id) $
    throwError (InvalidRequest $ "Pass " <> passId.getId <> " does not belong to city " <> show opCity)
  now <- getCurrentTime
  QPass.updateByPrimaryKey passRow {Domain.Types.Pass.enable = False, Domain.Types.Pass.updatedAt = now}
  CQPass.clearCacheByPassTypeIdAndEnabled passRow.passTypeId True
  CQPass.clearCacheByPassTypeIdAndEnabled passRow.passTypeId False
  pure Kernel.Types.APISuccess.Success
