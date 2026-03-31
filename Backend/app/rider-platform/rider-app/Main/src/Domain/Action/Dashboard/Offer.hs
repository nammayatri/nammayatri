module Domain.Action.Dashboard.Offer
  ( createOffer,
    updateOffer,
    listOffers,
    toggleOfferActive,
    CreateOfferReq (..),
    UpdateOfferReq (..),
  )
where

import Control.Applicative ((<|>))
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
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

data CreateOfferReq = CreateOfferReq
  { offerCode :: Text,
    offerType :: DOffer.OfferType,
    discountType :: DOffer.DiscountType,
    discountValue :: HighPrecMoney,
    maxDiscount :: Maybe HighPrecMoney,
    title :: Maybe Text,
    description :: Maybe Text,
    sponsoredBy :: Maybe Text,
    tnc :: Maybe Text,
    offerEligibilityJsonLogic :: Maybe Value,
    currency :: Currency
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data UpdateOfferReq = UpdateOfferReq
  { discountValue :: Maybe HighPrecMoney,
    maxDiscount :: Maybe HighPrecMoney,
    title :: Maybe Text,
    description :: Maybe Text,
    sponsoredBy :: Maybe Text,
    tnc :: Maybe Text,
    offerEligibilityJsonLogic :: Maybe Value,
    isActive :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

createOffer ::
  ShortId DM.Merchant ->
  Context.City ->
  CreateOfferReq ->
  Flow APISuccess
createOffer merchantShortId opCity req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
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
            currency = req.currency,
            isActive = True,
            merchantId = merchant.id.getId,
            merchantOperatingCityId = merchantOpCity.id.getId,
            createdAt = now,
            updatedAt = now
          }
  QOffer.create offer
  pure Success

updateOffer ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DOffer.Offer ->
  UpdateOfferReq ->
  Flow APISuccess
updateOffer merchantShortId opCity offerId req = do
  _merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  _merchantOpCity <- CQMOC.findByMerchantIdAndCity _merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> _merchant.id.getId <> "-city-" <> show opCity)
  offer <- QOffer.findById offerId >>= fromMaybeM (InvalidRequest $ "Offer not found: " <> offerId.getId)
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
            DOffer.isActive = fromMaybe offer.isActive req.isActive,
            DOffer.updatedAt = now
          }
  QOffer.updateByPrimaryKey updatedOffer
  pure Success

listOffers ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow [DOffer.Offer]
listOffers merchantShortId opCity = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  QOffer.findAllActiveByMerchant merchant.id.getId merchantOpCity.id.getId True

toggleOfferActive ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DOffer.Offer ->
  Flow APISuccess
toggleOfferActive merchantShortId opCity offerId = do
  _merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  _merchantOpCity <- CQMOC.findByMerchantIdAndCity _merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> _merchant.id.getId <> "-city-" <> show opCity)
  offer <- QOffer.findById offerId >>= fromMaybeM (InvalidRequest $ "Offer not found: " <> offerId.getId)
  now <- getCurrentTime
  QOffer.updateByPrimaryKey offer {DOffer.isActive = not offer.isActive, DOffer.updatedAt = now}
  pure Success
