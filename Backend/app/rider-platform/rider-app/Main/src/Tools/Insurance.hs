{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Insurance
  ( createInsurance,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import EulerHS.Prelude
import qualified Kernel.External.Insurance.Interface as Insurance
import qualified Kernel.External.Insurance.Interface.Types as Insurance
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC

createInsurance :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Insurance.InsuranceRequest -> m Insurance.InsuranceResponse
createInsurance = runWithServiceConfig Insurance.createInsurance

runWithServiceConfig ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  (Insurance.InsuranceConfig -> req -> m resp) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func merchantId merchantOperatingCityId req = do
  merchantConfig <-
    QMSUC.findByMerchantOperatingCityId merchantOperatingCityId
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
  merchantInsuranceServiceConfig <-
    QMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (DMSC.InsuranceService merchantConfig.insuranceService)
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  case merchantInsuranceServiceConfig.serviceConfig of
    DMSC.InsuranceServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"
