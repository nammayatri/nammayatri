{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.SMS
  ( module Reexport,
    sendSMS,
  )
where

import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
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

sendSMS :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id DMOC.MerchantOperatingCity -> SendSMSReq -> m SendSMSRes
sendSMS merchantOperatingCityId = Sms.sendSMS handler
  where
    handler = Sms.SmsHandler {..}

    getProvidersPriorityList = do
      merchantConfig <- QMSUC.findByMerchantId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
      let smsServiceProviders = merchantConfig.smsProvidersPriorityList
      when (null smsServiceProviders) $ throwError $ InternalError ("No sms service provider configured for the merchant, merchantId:" <> merchantOperatingCityId.getId)
      pure smsServiceProviders

    getProviderConfig provider = do
      merchantSmsServiceConfig <-
        QMSC.findByMerchantIdAndService merchantOperatingCityId (DMSC.SmsService provider)
          >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
      case merchantSmsServiceConfig.serviceConfig of
        DMSC.SmsServiceConfig msc -> pure msc
        _ -> throwError $ InternalError "Unknown Service Config"
