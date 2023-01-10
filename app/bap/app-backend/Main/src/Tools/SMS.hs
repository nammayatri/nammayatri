module Tools.SMS
  ( module Reexport,
    sendSMS,
  )
where

import Beckn.External.SMS as Reexport hiding
  ( sendSMS,
  )
import qualified Beckn.External.SMS as Sms
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
  ( EncFlow,
    EsqDBFlow,
    fromMaybeM,
    throwError,
  )
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Domain.Types.Merchant.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import Tools.Error
import Tools.Metrics

sendSMS :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id Merchant -> SendSMSReq -> m SendSMSRes
sendSMS = runWithServiceConfig Sms.sendSMS (.sendSMS)

runWithServiceConfig ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) =>
  (SmsServiceConfig -> req -> m resp) ->
  (MerchantServiceUsageConfig -> SmsService) ->
  Id Merchant ->
  req ->
  m resp
runWithServiceConfig func getCfg merchantId req = do
  merchantConfig <- QMSUC.findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  merchantSmsServiceConfig <-
    QMSC.findByMerchantIdAndService merchantId (DMSC.SmsService $ getCfg merchantConfig)
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  case merchantSmsServiceConfig.serviceConfig of
    DMSC.SmsServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"
