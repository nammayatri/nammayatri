{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.MultiModal where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Kernel.External.MultiModal.Interface.Types as MultiModal
import Kernel.External.MultiModal.Types as MultiModal
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import Tools.Error

getMultiModalConfig :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m MultiModal.MultiModalServiceConfig
getMultiModalConfig merchantId merchantOperatingCityId = do
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOperatingCityId merchantOperatingCityId
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
  merchantServiceConfig <-
    CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (DMSC.MultiModalService merchantServiceUsageConfig.getMultiModalService)
      >>= fromMaybeM (InternalError $ "No MultiModal service provider configured for the merchant, merchantId:" <> merchantId.getId)

  case merchantServiceConfig.serviceConfig of
    DMSC.MultiModalServiceConfig multiModalServiceConfig -> return multiModalServiceConfig
    cfg -> throwError $ InternalError $ "Unknown Service Config in multimodalConfig" <> show cfg

getTransitServiceReq :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m MultiModal.MultiModalServiceConfig
getTransitServiceReq = getMultiModalConfig

getOTPRestServiceReq :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m BaseUrl
getOTPRestServiceReq merchantId merchantOperatingCityId = do
  transitServiceReq <- CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (DMSC.MultiModalStaticDataService MultiModal.OTPTransit) >>= fromMaybeM (InternalError "No OTP Transit Service Config Found")
  transitServiceReq' <- case transitServiceReq.serviceConfig of
    DMSC.MultiModalStaticDataServiceConfig multiModalServiceConfig -> return multiModalServiceConfig
    cfg -> throwError $ InternalError $ "Unknown Service Config in otprestservicereq" <> show cfg
  case transitServiceReq' of
    OTPTransitConfig otpTransitConfig -> return otpTransitConfig.baseUrl
    config -> throwError $ InternalError $ "Unknown Service Config" <> show config
