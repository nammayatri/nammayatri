{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Tools.Call
  ( module Reexport,
    initiateCall,
  )
where

import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Domain.Types.Merchant.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
import Kernel.External.Call as Reexport hiding
  ( initiateCall,
  )
import qualified Kernel.External.Call as Call
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import Tools.Error
import Tools.Metrics

initiateCall ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    CoreMetrics m,
    ToJSON a
  ) =>
  Id Merchant ->
  InitiateCallReq a ->
  m InitiateCallResp
initiateCall = runWithServiceConfig Call.initiateCall (.initiateCall)

runWithServiceConfig ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) =>
  (CallServiceConfig -> req -> m resp) ->
  (MerchantServiceUsageConfig -> CallService) ->
  Id Merchant ->
  req ->
  m resp
runWithServiceConfig func getCfg merchantId req = do
  merchantConfig <- QMSUC.findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  merchantCallServiceConfig <-
    QMSC.findByMerchantIdAndService merchantId (DMSC.CallService $ getCfg merchantConfig)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "call" (show $ getCfg merchantConfig))
  case merchantCallServiceConfig.serviceConfig of
    DMSC.CallServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown ServiceConfig"
