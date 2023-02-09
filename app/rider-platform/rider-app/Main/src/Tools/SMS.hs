module Tools.SMS
  ( module Reexport,
    sendSMS,
  )
where

import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Kernel.External.SMS as Reexport hiding
  ( sendSMS,
  )
import qualified Kernel.External.SMS as Sms
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
  ( EncFlow,
    EsqDBFlow,
    fromMaybeM,
    throwError,
  )
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import Tools.Error
import Tools.Metrics

sendSMS :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id Merchant -> SendSMSReq -> m SendSMSRes
sendSMS merchantId = Sms.sendSMS handler
  where
    handler = Sms.SmsHandler {..}

    getProvidersPriorityList = do
      merchantConfig <- QMSUC.findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
      let smsServiceProviders = merchantConfig.smsProvidersPriorityList
      when (null smsServiceProviders) $ throwError $ InternalError ("No sms service provider configured for the merchant, merchantId:" <> merchantId.getId)
      pure smsServiceProviders

    getProviderConfig provider = do
      merchantSmsServiceConfig <-
        QMSC.findByMerchantIdAndService merchantId (DMSC.SmsService provider)
          >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
      case merchantSmsServiceConfig.serviceConfig of
        DMSC.SmsServiceConfig msc -> pure msc
        _ -> throwError $ InternalError "Unknown Service Config"
