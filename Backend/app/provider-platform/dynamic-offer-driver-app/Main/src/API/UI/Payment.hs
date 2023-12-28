{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Payment
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.Payment as DPayment
import Domain.Types.Invoice (Invoice)
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Notification (Notification)
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.API as Payment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  TokenAuth
    :> Payment.API "invoiceId" "notificationId" Invoice Notification Payment.NotificationStatusResp

handler :: FlowServer API
handler authInfo =
  createOrder authInfo
    :<|> getStatus authInfo
    :<|> getOrder authInfo
    :<|> getNotificationStatus authInfo

createOrder :: (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Id Invoice -> FlowHandler Payment.CreateOrderResp
createOrder tokenDetails invoiceId = withFlowHandlerAPI $ DPayment.createOrder tokenDetails invoiceId

-- This APIs are decoupled from Driver Fee Table.
getStatus :: (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Id DOrder.PaymentOrder -> FlowHandler DPayment.PaymentStatusResp
getStatus tokenDetails orderId = withFlowHandlerAPI $ DPayment.getStatus tokenDetails orderId

getOrder :: (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Id DOrder.PaymentOrder -> FlowHandler DOrder.PaymentOrderAPIEntity
getOrder tokenDetails orderId = withFlowHandlerAPI $ DPayment.getOrder tokenDetails orderId

getNotificationStatus :: (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Id Notification -> FlowHandler Payment.NotificationStatusResp
getNotificationStatus notificationId = withFlowHandlerAPI . DPayment.pdnNotificationStatus notificationId
