module Domain.Action.Dashboard.Offer
  ( postOfferCreate,
    postOfferUpdate,
    getOfferList,
    postOfferToggle,
    postOfferValidateEligibility,
    getOfferEligibilitySchema,
  )
where

import qualified API.Types.RiderPlatform.Management.Offer as Common
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.Offer as DOffer
import qualified Lib.Payment.Storage.Queries.Offer as QOffer
import qualified Lib.Yudhishthira.Tools.Utils as LYUtils
import qualified Lib.Yudhishthira.TypesTH as YTH
import SharedLogic.Offer (OfferEligibilityInput)
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

postOfferCreate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.CreateOfferReq ->
  Flow APISuccess
postOfferCreate merchantShortId opCity req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  validateMinimumAmount req.minimumAmount
  validateMaxApplyCount req.maxApplyCount
  now <- getCurrentTime
  offerId <- generateGUID
  let offer =
        DOffer.Offer
          { id = offerId,
            offerCode = req.offerCode,
            offerType = req.offerType,
            discountType = req.discountType,
            discountValue = req.discountValue,
            maxDiscount = req.maxDiscount,
            title = req.title,
            description = req.description,
            sponsoredBy = req.sponsoredBy,
            tnc = req.tnc,
            offerEligibilityJsonLogic = req.offerEligibilityJsonLogic,
            validTill = req.validTill,
            currency = req.currency,
            isActive = True,
            minimumAmount = req.minimumAmount,
            autoApply = req.autoApply,
            isHidden = req.isHidden,
            frequencyType = req.frequencyType,
            maxApplyCount = req.maxApplyCount,
            merchantId = merchant.id.getId,
            merchantOperatingCityId = merchantOpCity.id.getId,
            createdAt = now,
            updatedAt = now
          }
  QOffer.create offer
  pure Success

postOfferUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DOffer.Offer ->
  Common.UpdateOfferReq ->
  Flow APISuccess
postOfferUpdate merchantShortId opCity offerId req = do
  _merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  _merchantOpCity <- CQMOC.findByMerchantIdAndCity _merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> _merchant.id.getId <> "-city-" <> show opCity)
  offer <- QOffer.findById offerId >>= fromMaybeM (InvalidRequest $ "Offer not found: " <> offerId.getId)
  validateMinimumAmount req.minimumAmount
  validateMaxApplyCount req.maxApplyCount
  now <- getCurrentTime
  let updatedOffer =
        offer
          { DOffer.discountValue = fromMaybe offer.discountValue req.discountValue,
            DOffer.maxDiscount = req.maxDiscount <|> offer.maxDiscount,
            DOffer.title = req.title <|> offer.title,
            DOffer.description = req.description <|> offer.description,
            DOffer.sponsoredBy = req.sponsoredBy <|> offer.sponsoredBy,
            DOffer.tnc = req.tnc <|> offer.tnc,
            DOffer.offerEligibilityJsonLogic = req.offerEligibilityJsonLogic <|> offer.offerEligibilityJsonLogic,
            DOffer.validTill = req.validTill <|> offer.validTill,
            DOffer.isActive = fromMaybe offer.isActive req.isActive,
            DOffer.minimumAmount = req.minimumAmount <|> offer.minimumAmount,
            DOffer.autoApply = req.autoApply <|> offer.autoApply,
            DOffer.isHidden = req.isHidden <|> offer.isHidden,
            DOffer.frequencyType = req.frequencyType <|> offer.frequencyType,
            DOffer.maxApplyCount = req.maxApplyCount <|> offer.maxApplyCount,
            DOffer.updatedAt = now
          }
  QOffer.updateByPrimaryKey updatedOffer
  pure Success

validateMinimumAmount :: Maybe HighPrecMoney -> Flow ()
validateMinimumAmount mbMinimumAmount =
  whenJust mbMinimumAmount $ \minimumAmount ->
    when (minimumAmount < 0) $ throwError (InvalidRequest "minimumAmount cannot be negative")

validateMaxApplyCount :: Maybe Int -> Flow ()
validateMaxApplyCount mbMaxApplyCount =
  whenJust mbMaxApplyCount $ \maxApplyCount ->
    when (maxApplyCount < 1) $ throwError (InvalidRequest "maxApplyCount must be at least 1")

getOfferList ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow [Common.OfferResp]
getOfferList merchantShortId opCity = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  activeOffers <- QOffer.findAllActiveByMerchant merchant.id.getId merchantOpCity.id.getId True
  inactiveOffers <- QOffer.findAllActiveByMerchant merchant.id.getId merchantOpCity.id.getId False
  pure $ map mkOfferResp (activeOffers <> inactiveOffers)

mkOfferResp :: DOffer.Offer -> Common.OfferResp
mkOfferResp offer =
  Common.OfferResp
    { id = offer.id,
      offerCode = offer.offerCode,
      offerType = offer.offerType,
      discountType = offer.discountType,
      discountValue = offer.discountValue,
      maxDiscount = offer.maxDiscount,
      title = offer.title,
      description = offer.description,
      sponsoredBy = offer.sponsoredBy,
      tnc = offer.tnc,
      offerEligibilityJsonLogic = offer.offerEligibilityJsonLogic,
      validTill = offer.validTill,
      currency = offer.currency,
      isActive = offer.isActive,
      minimumAmount = offer.minimumAmount,
      autoApply = offer.autoApply,
      isHidden = offer.isHidden,
      frequencyType = offer.frequencyType,
      maxApplyCount = offer.maxApplyCount,
      createdAt = offer.createdAt,
      updatedAt = offer.updatedAt
    }

postOfferToggle ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DOffer.Offer ->
  Flow APISuccess
postOfferToggle merchantShortId opCity offerId = do
  _merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  _merchantOpCity <- CQMOC.findByMerchantIdAndCity _merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> _merchant.id.getId <> "-city-" <> show opCity)
  offer <- QOffer.findById offerId >>= fromMaybeM (InvalidRequest $ "Offer not found: " <> offerId.getId)
  now <- getCurrentTime
  QOffer.updateByPrimaryKey offer {DOffer.isActive = not offer.isActive, DOffer.updatedAt = now}
  pure Success

postOfferValidateEligibility ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.ValidateOfferEligibilityReq ->
  Flow Common.ValidateOfferEligibilityResp
postOfferValidateEligibility merchantShortId opCity req = do
  _merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  _merchantOpCity <- CQMOC.findByMerchantIdAndCity _merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> _merchant.id.getId <> "-city-" <> show opCity)
  logicResp <- LYUtils.runLogics [req.jsonLogic] req.inputData
  case logicResp.result of
    A.Bool result ->
      pure
        Common.ValidateOfferEligibilityResp
          { eligible = result,
            errors = map toText logicResp.errors
          }
    _ ->
      pure
        Common.ValidateOfferEligibilityResp
          { eligible = False,
            errors = ["Logic did not return a boolean result"] <> map toText logicResp.errors
          }

getOfferEligibilitySchema ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow Common.OfferEligibilitySchemaResp
getOfferEligibilitySchema _ _ = do
  let defaultVal = maybe (A.object []) A.toJSON (listToMaybe $ YTH.genDef (Proxy @OfferEligibilityInput))
  pure Common.OfferEligibilitySchemaResp {defaultValue = defaultVal}
