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
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Payment.PaymentOrder as DOrder
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "payment"
    :> ( Capture "riderId" (Id DRide.Ride)
           :> "createOrder"
           :> TokenAuth
           --  :> ReqBody '[JSON] DPayment.CreateOrderRequest
           :> Post '[JSON] Payment.CreateOrderResp
           :<|> Capture "orderId" (Id DOrder.PaymentOrder)
             :> "status"
             :> TokenAuth
             :> Get '[JSON] DPayment.PaymentStatusResp
       )

handler :: FlowServer API
handler =
  createOrder
    :<|> getStatus

createOrder :: Id DRide.Ride -> (Id DP.Person, Id Merchant.Merchant) -> FlowHandler Payment.CreateOrderResp
createOrder rideId tokenDetails = withFlowHandlerAPI $ DPayment.createOrder tokenDetails rideId

getStatus :: Id DOrder.PaymentOrder -> (Id DP.Person, Id Merchant.Merchant) -> FlowHandler DPayment.PaymentStatusResp
getStatus orderId tokenDetails = withFlowHandlerAPI $ DPayment.getStatus tokenDetails orderId
