{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.Discount (getDiscountList) where

import qualified API.Types.UI.Discount
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Plan
import qualified Domain.Types.VehicleCategory
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Logging
import qualified Storage.CachedQueries.DiscountTierTranslation as QDiscountTierTranslation
import qualified Storage.CachedQueries.DiscountTranslation as QDiscountTranslation
import qualified Storage.Queries.Discount as QDiscountDB
import qualified Storage.Queries.DiscountTier as QDiscountTier

getDiscountList ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Kernel.External.Types.Language) ->
    Kernel.Prelude.Maybe Text ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Plan.Plan) ->
    Kernel.Prelude.Maybe (Domain.Types.VehicleCategory.VehicleCategory) ->
    Kernel.Prelude.Maybe (Domain.Types.Plan.PaymentMode) ->
    Environment.Flow API.Types.UI.Discount.GetDiscountsRes
  )
getDiscountList (_, _, merchantOpCityId) mbLanguage mbDiscountType mbPlanId mbVehicleCategory mbPaymentMode = do
  logInfo $ "getDiscountList called with language: " <> show mbLanguage <> ", discountType: " <> show mbDiscountType <> ", planId: " <> show mbPlanId <> ", vehicleCategory: " <> show mbVehicleCategory <> ", paymentMode: " <> show mbPaymentMode
  let lang = fromMaybe Kernel.External.Types.ENGLISH mbLanguage
  discounts <- QDiscountDB.findByFilters merchantOpCityId True mbDiscountType mbPlanId mbVehicleCategory mbPaymentMode
  discountInfos <- forM discounts $ \discount -> do
    mbTranslation <- QDiscountTranslation.findByDiscountIdAndLanguage discount.id lang
    translation <- case mbTranslation of
      Just t -> pure (Just t)
      Nothing -> QDiscountTranslation.findByDiscountIdAndLanguage discount.id Kernel.External.Types.ENGLISH
    tiers <- QDiscountTier.findAllByDiscountIdOrderByTier Nothing Nothing discount.id
    tierInfos <- forM tiers $ \tier -> do
      mbTierTrans <- QDiscountTierTranslation.findByTierIdAndLanguage tier.id lang
      tierTrans <- case mbTierTrans of
        Just t -> pure (Just t)
        Nothing -> QDiscountTierTranslation.findByTierIdAndLanguage tier.id Kernel.External.Types.ENGLISH
      pure $
        API.Types.UI.Discount.DiscountTierInfo
          { tierOrder = tier.tierOrder,
            thresholdValue = tier.thresholdValue,
            discountValue = tier.discountValue,
            discountValueType = tier.discountValueType,
            name = maybe "" (.name) tierTrans,
            description = maybe "" (.description) tierTrans
          }
    pure $
      API.Types.UI.Discount.DiscountInfo
        { discountId = discount.id,
          discountType = discount.discountType,
          name = maybe "" (.name) translation,
          description = maybe "" (.description) translation,
          planId = discount.planId,
          vehicleCategory = discount.vehicleCategory,
          paymentMode = discount.paymentMode,
          validFrom = discount.validFrom,
          validTo = discount.validTo,
          tiers = tierInfos
        }
  pure $ API.Types.UI.Discount.GetDiscountsRes {discounts = discountInfos}
