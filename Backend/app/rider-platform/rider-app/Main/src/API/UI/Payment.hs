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
    S2SAPI,
    handlerS2S,
  )
where

import qualified API.Types.UI.Payment as PaymentAPI
import qualified Data.Text
import qualified Domain.Action.UI.Payment as DPayment
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Wallet as Wallet
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.API as Payment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.Person as QP
import Tools.Auth
import Tools.FlowHandling (withFlowHandlerAPIPersonId)

type API =
  TokenAuth
    :> Payment.API "rideId" "rideId" DRide.Ride DRide.Ride Payment.CreateOrderResp PaymentAPI.WalletRechargeReq

type S2SAPI =
  "s2s" :> "payment" :> Capture "orderId" (Id DOrder.PaymentOrder) :> Capture "customerId" (Id DP.Person) :> "status" :> Header "api-key" Data.Text.Text :> Get '[JSON] DPayment.PaymentStatusResp

handler :: FlowServer API
handler authInfo =
  createOrder authInfo
    :<|> getStatus authInfo
    :<|> getOrder authInfo
    :<|> createOrder authInfo -- Fix properly
    :<|> postWalletRecharge authInfo
    :<|> getWalletBalance authInfo

handlerS2S :: FlowServer S2SAPI
handlerS2S = getStatusS2S

createOrder :: (Id DP.Person, Id Merchant.Merchant) -> Id DRide.Ride -> FlowHandler Payment.CreateOrderResp
createOrder (personId, merchantId) rideId = withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $ DPayment.createOrder (personId, merchantId) rideId

getStatus :: (Id DP.Person, Id Merchant.Merchant) -> Id DOrder.PaymentOrder -> FlowHandler DPayment.PaymentStatusResp
getStatus (personId, merchantId) orderId = withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $ DPayment.getStatus (personId, merchantId) orderId

getOrder :: (Id DP.Person, Id Merchant.Merchant) -> Id DOrder.PaymentOrder -> FlowHandler DOrder.PaymentOrderAPIEntity
getOrder (personId, merchantId) orderId = withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $ DPayment.getOrder (personId, merchantId) orderId

getStatusS2S :: Id DOrder.PaymentOrder -> Id DP.Person -> Maybe Data.Text.Text -> FlowHandler DPayment.PaymentStatusResp
getStatusS2S orderId personId mbApiKey = withFlowHandlerAPI $ do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound "Person not found")
  DPayment.getStatusS2S orderId personId person.merchantId mbApiKey

postWalletRecharge :: (Id DP.Person, Id Merchant.Merchant) -> PaymentAPI.WalletRechargeReq -> FlowHandler APISuccess
postWalletRecharge (personId, merchantId) req = withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $ DPayment.postWalletRecharge (personId, merchantId) req

getWalletBalance :: (Id DP.Person, Id Merchant.Merchant) -> FlowHandler Wallet.WalletBalanceData
getWalletBalance (personId, merchantId) = withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $ DPayment.getWalletBalance (personId, merchantId)
