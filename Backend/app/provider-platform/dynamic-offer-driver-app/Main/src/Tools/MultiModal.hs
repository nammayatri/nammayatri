{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE.See the GNU Affero General Public License for more details.You should have received a copy of

 the GNU Affero General Public License along with this program.If not, see <https://www.gnu.org/licenses/>.
 -}

module Tools.MultiModal where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Kernel.External.MultiModal.Types as MultiModal
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Tools.Error

getOTPRestServiceReq :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m BaseUrl
getOTPRestServiceReq _merchantId merchantOperatingCityId = do
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity (DMSC.MultiModalStaticDataService MultiModal.OTPTransit) merchantOperatingCityId
      >>= fromMaybeM (InternalError "No OTP Transit Service Config Found")
  case merchantServiceConfig.serviceConfig of
    DMSC.MultiModalStaticDataServiceConfig multiModalServiceConfig -> case multiModalServiceConfig of
      MultiModal.GoogleTransitConfig url _ -> return url
      MultiModal.OTPTransitConfig url -> return url
    cfg -> throwError $ InternalError $ "Unknown Service Config" <> show cfg
