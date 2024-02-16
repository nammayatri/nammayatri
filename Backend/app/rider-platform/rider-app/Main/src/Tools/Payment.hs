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

import Control.Applicative ((<|>))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Domain.Types.TicketPlace
import Kernel.External.Payment.Interface as Reexport hiding
  ( createOrder,
    orderStatus,
  )
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.PlaceBasedServiceConfig as CQPBSC

createOrder :: ServiceFlow m r => Id DM.Merchant -> Maybe (Id TicketPlace) -> Payment.CreateOrderReq -> m Payment.CreateOrderResp
createOrder = runWithServiceConfig Payment.createOrder

orderStatus :: ServiceFlow m r => Id DM.Merchant -> Maybe (Id TicketPlace) -> Payment.OrderStatusReq -> m Payment.OrderStatusResp
orderStatus = runWithServiceConfig Payment.orderStatus

runWithServiceConfig ::
  ServiceFlow m r =>
  (Payment.PaymentServiceConfig -> req -> m resp) ->
  Id DM.Merchant ->
  Maybe (Id TicketPlace) ->
  req ->
  m resp
runWithServiceConfig func merchantId mbPlaceId req = do
  placeBasedConfig <- case mbPlaceId of
    Just id -> CQPBSC.findByPlaceIdAndServiceName id (DMSC.PaymentService Payment.Juspay)
    Nothing -> return Nothing
  merchantServiceConfig <-
    CQMSC.findByMerchantIdAndService merchantId (DMSC.PaymentService Payment.Juspay)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show Payment.Juspay))
  case (placeBasedConfig <&> (.serviceConfig)) <|> Just merchantServiceConfig.serviceConfig of
    Just (DMSC.PaymentServiceConfig vsc) -> func vsc req
    _ -> throwError $ InternalError "Unknown Service Config"
