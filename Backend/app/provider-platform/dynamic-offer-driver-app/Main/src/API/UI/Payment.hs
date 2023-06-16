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
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.API as Payment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Servant
import Tools.Auth

type API =
  TokenAuth
    :> Payment.API DOrder.PaymentOrder

handler :: FlowServer API
handler authInfo =
  createOrder authInfo
    :<|> getStatus authInfo

createOrder :: (Id DP.Person, Id Merchant.Merchant) -> Id DOrder.PaymentOrder -> FlowHandler Payment.CreateOrderResp
createOrder tokenDetails rideId = withFlowHandlerAPI $ DPayment.createOrder tokenDetails rideId

getStatus :: (Id DP.Person, Id Merchant.Merchant) -> Id DOrder.PaymentOrder -> FlowHandler DPayment.PaymentStatusResp
getStatus tokenDetails orderId = withFlowHandlerAPI $ DPayment.getStatus tokenDetails orderId
