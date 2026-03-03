module Domain.Action.Dashboard.Management.DomainDiscountConfig
  ( postDomainDiscountConfigCreate,
    getDomainDiscountConfigList,
    deleteDomainDiscountConfigDelete,
  )
where

import qualified API.Types.ProviderPlatform.Management.DomainDiscountConfig as Common
import qualified Domain.Types.DomainDiscountConfig as DDDC
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions (deleteWithKV)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.Type as SLT
import qualified Storage.Beam.DomainDiscountConfig as Beam
import qualified Storage.CachedQueries.DomainDiscountConfig as CQDomainDiscount
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DomainDiscountConfig as QDomainDiscount

postDomainDiscountConfigCreate ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateDomainDiscountConfigReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postDomainDiscountConfigCreate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  now <- getCurrentTime
  let billingCat = castBillingCategory req.billingCategory
  let domainDiscountConfig =
        DDDC.DomainDiscountConfig
          { billingCategory = billingCat,
            createdAt = now,
            discountPercentage = req.discountPercentage,
            domain = req.domain,
            enabled = req.enabled,
            merchantOperatingCityId = merchantOpCityId,
            updatedAt = now,
            vehicleServiceTier = req.vehicleServiceTier,
            merchantId = Just (Id.cast merchant.id)
          }
  QDomainDiscount.create domainDiscountConfig
  CQDomainDiscount.clearCache merchantOpCityId req.domain billingCat req.vehicleServiceTier
  pure Kernel.Types.APISuccess.Success

getDomainDiscountConfigList ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.BillingCategory ->
  Environment.Flow [Common.DomainDiscountConfigRes]
getDomainDiscountConfigList merchantShortId opCity billingCategory = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let billingCat = castBillingCategory billingCategory
  configs <- QDomainDiscount.findAllByMerchantOpCityIdAndBillingCategory merchantOpCityId billingCat
  pure $ map mkDomainDiscountConfigRes configs

deleteDomainDiscountConfigDelete ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.DeleteDomainDiscountConfigReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteDomainDiscountConfigDelete merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let billingCat = castBillingCategory req.billingCategory
  deleteWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Id.getId merchantOpCityId),
          Se.Is Beam.domain $ Se.Eq req.domain,
          Se.Is Beam.billingCategory $ Se.Eq billingCat,
          Se.Is Beam.vehicleServiceTier $ Se.Eq req.vehicleServiceTier
        ]
    ]
  CQDomainDiscount.clearCache merchantOpCityId req.domain billingCat req.vehicleServiceTier
  pure Kernel.Types.APISuccess.Success

-- Helper: convert dashboard BillingCategory to BPP BillingCategory
castBillingCategory :: Common.BillingCategory -> SLT.BillingCategory
castBillingCategory Common.PERSONAL = SLT.PERSONAL
castBillingCategory Common.BUSINESS = SLT.BUSINESS

-- Helper: convert BPP BillingCategory to dashboard BillingCategory
castBillingCategoryToCommon :: SLT.BillingCategory -> Common.BillingCategory
castBillingCategoryToCommon SLT.PERSONAL = Common.PERSONAL
castBillingCategoryToCommon SLT.BUSINESS = Common.BUSINESS

mkDomainDiscountConfigRes :: DDDC.DomainDiscountConfig -> Common.DomainDiscountConfigRes
mkDomainDiscountConfigRes config =
  Common.DomainDiscountConfigRes
    { domain = config.domain,
      billingCategory = castBillingCategoryToCommon config.billingCategory,
      vehicleServiceTier = config.vehicleServiceTier,
      discountPercentage = config.discountPercentage,
      enabled = config.enabled,
      createdAt = config.createdAt,
      updatedAt = config.updatedAt
    }
