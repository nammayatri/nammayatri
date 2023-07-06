{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Payment
  ( module Reexport,
    createOrder,
    orderStatus,
  )
where

import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Kernel.External.Payment.Interface as Reexport hiding
  ( createOrder,
    orderStatus,
  )
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Tools.Metrics (CoreMetrics)

createOrder :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id DMOC.MerchantOperatingCity -> Payment.CreateOrderReq -> m Payment.CreateOrderResp
createOrder = runWithServiceConfig Payment.createOrder

orderStatus :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id DMOC.MerchantOperatingCity -> Payment.OrderStatusReq -> m Payment.OrderStatusResp
orderStatus = runWithServiceConfig Payment.orderStatus

runWithServiceConfig ::
  (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) =>
  (Payment.PaymentServiceConfig -> req -> m resp) ->
  Id DMOC.MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func merchantOperatingCityId req = do
  merchantServiceConfig <-
    CQMSC.findByMerchantIdAndService merchantOperatingCityId (DMSC.PaymentService Payment.Juspay)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOperatingCityId.getId "Payment" (show Payment.Juspay))
  case merchantServiceConfig.serviceConfig of
    DMSC.PaymentServiceConfig vsc -> func vsc req
    _ -> throwError $ InternalError "Unknown Service Config"
