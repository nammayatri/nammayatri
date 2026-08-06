module Domain.Action.Dashboard.Management.VasBanner
  ( getVasBannerList,
    postVasBannerCreate,
    postVasBannerUpdate,
    postVasBannerDelete,
  )
where

import qualified API.Types.ProviderPlatform.Management.VasBanner as API
import qualified Dashboard.Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.VasBannerConfig as DVC
import qualified Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime)
import qualified SharedLogic.Merchant as SM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.VasBannerConfig as QVBC
import Tools.Error (GenericError (InvalidRequest))

getVasBannerList :: ShortId DM.Merchant -> Context.City -> Maybe (Bool) -> Environment.Flow API.VasBannerListRes
getVasBannerList merchantShortId opCity enabled = do
  merchant <- SM.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  banners <-
    case enabled of
      Just isEnabled -> QVBC.findAllEnabledByCity Nothing Nothing merchantOpCityId isEnabled
      Nothing -> QVBC.findAllByMerchantOperatingCityId Nothing Nothing merchantOpCityId
  pure $ API.VasBannerListRes {banners = map toVasBannerRes banners}

postVasBannerCreate :: ShortId DM.Merchant -> Context.City -> API.VasBannerCreateReq -> Environment.Flow API.VasBannerRes
postVasBannerCreate merchantShortId opCity req = do
  merchant <- SM.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  id <- generateGUID
  now <- getCurrentTime
  let banner =
        DVC.VasBannerConfig
          { id = id,
            merchantId = cast merchant.id,
            merchantOperatingCityId = merchantOpCityId,
            title = req.title,
            subtitle = req.subtitle,
            imageUrl = req.imageUrl,
            deepLink = req.deepLink,
            whatsappTemplateId = req.whatsappTemplateId,
            linkType = toDomainLinkType req.linkType,
            priority = req.priority,
            enabled = req.enabled,
            validFrom = req.validFrom,
            validTo = req.validTo,
            createdAt = now,
            updatedAt = now
          }
  QVBC.create banner
  pure $ toVasBannerRes banner

postVasBannerUpdate :: ShortId DM.Merchant -> Context.City -> Id Dashboard.Common.VasBannerConfig -> API.VasBannerUpdateReq -> Environment.Flow API.VasBannerRes
postVasBannerUpdate merchantShortId opCity bannerId req = do
  merchant <- SM.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let bannerDomainId = cast bannerId
  existing <- QVBC.findById bannerDomainId >>= fromMaybeM (InvalidRequest "Vas banner does not exist")
  let updated =
        existing
          { DVC.merchantId = cast merchant.id,
            DVC.merchantOperatingCityId = merchantOpCityId,
            DVC.title = req.title,
            DVC.subtitle = req.subtitle,
            DVC.imageUrl = req.imageUrl,
            DVC.deepLink = req.deepLink,
            DVC.whatsappTemplateId = req.whatsappTemplateId,
            DVC.linkType = toDomainLinkType req.linkType,
            DVC.priority = req.priority,
            DVC.enabled = req.enabled,
            DVC.validFrom = req.validFrom,
            DVC.validTo = req.validTo
          }
  QVBC.updateByPrimaryKey updated
  pure $ toVasBannerRes updated

postVasBannerDelete :: ShortId DM.Merchant -> Context.City -> Id Dashboard.Common.VasBannerConfig -> Environment.Flow APISuccess
postVasBannerDelete _merchantShortId _opCity bannerId = do
  let bannerDomainId = cast bannerId
  void $ QVBC.findById bannerDomainId >>= fromMaybeM (InvalidRequest "Vas banner does not exist")
  QVBC.deleteById bannerDomainId
  pure Kernel.Types.APISuccess.Success

toVasBannerRes :: DVC.VasBannerConfig -> API.VasBannerRes
toVasBannerRes DVC.VasBannerConfig {..} =
  API.VasBannerRes
    { id = cast @DVC.VasBannerConfig @Dashboard.Common.VasBannerConfig id,
      linkType = toApiLinkType linkType,
      ..
    }

toDomainLinkType :: API.VasBannerLinkType -> DVC.VasBannerLinkType
toDomainLinkType = \case
  API.Service -> DVC.Service
  API.SmartFinance -> DVC.SmartFinance
  API.WhatsApp -> DVC.WhatsApp
  API.ExternalUrl -> DVC.ExternalUrl
  API.Other -> DVC.Other

toApiLinkType :: DVC.VasBannerLinkType -> API.VasBannerLinkType
toApiLinkType = \case
  DVC.Service -> API.Service
  DVC.SmartFinance -> API.SmartFinance
  DVC.WhatsApp -> API.WhatsApp
  DVC.ExternalUrl -> API.ExternalUrl
  DVC.Other -> API.Other
