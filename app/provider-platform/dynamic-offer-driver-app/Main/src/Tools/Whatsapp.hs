module Tools.Whatsapp
  ( module Reexport,
    whatsAppOptAPI,
  )
where

import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Kernel.External.Whatsapp.Interface as Reexport hiding
  ( whatsAppOptApi,
  )
import qualified Kernel.External.Whatsapp.Interface as GupShup
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import Tools.Metrics (CoreMetrics)

whatsAppOptAPI :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id Merchant -> GupShup.OptApiReq -> m APISuccess
whatsAppOptAPI merchantId req = do
  void $ GupShup.whatsAppOptApi handler req
  return Success
  where
    handler = GupShup.WhatsappHandler {..}

    getProvidersPriorityList = do
      merchantConfig <- QMSUC.findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
      let whatsappServiceProviders = merchantConfig.whatsappProvidersPriorityList
      when (null whatsappServiceProviders) $ throwError $ InternalError ("No whatsapp service provider configured for the merchant, merchantId:" <> merchantId.getId)
      pure whatsappServiceProviders

    getProviderConfig provider = do
      merchantWhatsappServiceConfig <-
        QMSC.findByMerchantIdAndService merchantId (DMSC.WhatsappService provider)
          >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
      case merchantWhatsappServiceConfig.serviceConfig of
        DMSC.WhatsappServiceConfig msc -> pure msc
        _ -> throwError $ InternalError "Unknown Service Config"
