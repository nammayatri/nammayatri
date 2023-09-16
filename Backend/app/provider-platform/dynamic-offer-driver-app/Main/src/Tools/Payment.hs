{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Payment where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC

createOrder :: ServiceFlow m r => Id DM.Merchant -> Payment.CreateOrderReq -> m Payment.CreateOrderResp
createOrder = runWithServiceConfig Payment.createOrder

orderStatus :: ServiceFlow m r => Id DM.Merchant -> Payment.OrderStatusReq -> m Payment.OrderStatusResp
orderStatus = runWithServiceConfig Payment.orderStatus

offerList :: ServiceFlow m r => Id DM.Merchant -> Payment.OfferListReq -> m Payment.OfferListResp
offerList = runWithServiceConfig Payment.offerList

offerApply :: ServiceFlow m r => Id DM.Merchant -> Payment.OfferApplyReq -> m Payment.OfferApplyResp
offerApply = runWithServiceConfig Payment.offerApply

mandateRevoke :: ServiceFlow m r => Id DM.Merchant -> Payment.MandateRevokeReq -> m Payment.MandateRevokeRes
mandateRevoke = runWithServiceConfig Payment.mandateRevoke

mandateNotification :: (ServiceFlow m r) => Id DM.Merchant -> Payment.MandateNotificationReq -> m Payment.MandateNotificationRes
mandateNotification = runWithServiceConfig Payment.mandateNotification

mandateExecution :: ServiceFlow m r => Id DM.Merchant -> Payment.MandateExecutionReq -> m Payment.MandateExecutionRes
mandateExecution = runWithServiceConfig Payment.mandateExecution

runWithServiceConfig ::
  ServiceFlow m r =>
  (Payment.PaymentServiceConfig -> req -> m resp) ->
  Id DM.Merchant ->
  req ->
  m resp
runWithServiceConfig func merchantId req = do
  merchantServiceConfig <-
    CQMSC.findByMerchantIdAndService merchantId (DMSC.PaymentService Payment.Juspay)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show Payment.Juspay))
  case merchantServiceConfig.serviceConfig of
    DMSC.PaymentServiceConfig vsc -> func vsc req
    _ -> throwError $ InternalError "Unknown Service Config"
