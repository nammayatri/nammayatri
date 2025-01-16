{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Payout where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC

createPayoutOrder :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payout.CreatePayoutOrderReq -> m Payout.CreatePayoutOrderResp
createPayoutOrder = runWithServiceConfigAndName Payout.createPayoutOrder

payoutOrderStatus :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payout.PayoutOrderStatusReq -> m Payout.PayoutOrderStatusResp
payoutOrderStatus = runWithServiceConfigAndName Payout.payoutOrderStatus

runWithServiceConfigAndName ::
  ServiceFlow m r =>
  (Payout.PayoutServiceConfig -> req -> m resp) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DMSC.ServiceName ->
  req ->
  m resp
runWithServiceConfigAndName func merchantId merchantOperatingCity serviceName req = do
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity serviceName merchantOperatingCity
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payout" (show Payout.Juspay))
  case merchantServiceConfig.serviceConfig of
    DMSC.PayoutServiceConfig vsc -> func vsc req
    DMSC.RentalPayoutServiceConfig vsc -> func vsc req
    _ -> throwError $ InternalError "Unknown Service Config"
