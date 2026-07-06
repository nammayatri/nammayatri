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
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Notification (Notification)
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Wallet as Wallet
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.API as PaymentAPI
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo as ActorInfo
import Tools.Auth

type API =
  TokenAuth
    :> ( "payment"
           :> "status"
           :> "v2"
           :> Capture "orderId" Text
           :> Get '[JSON] DPayment.PaymentStatusResp
           :<|> PaymentAPI.API "invoiceId" "notificationId" Invoice Notification Payment.NotificationStatusResp ()
       )

handler :: FlowServer API
handler authInfo =
  getStatusV2 authInfo
    :<|> ( createOrder authInfo
             :<|> getStatus authInfo
             :<|> getOrder authInfo
             :<|> getNotificationStatus authInfo
             :<|> postWalletRecharge authInfo
             :<|> getWalletBalance authInfo
         )

createOrder :: (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Id Invoice -> FlowHandler Payment.CreateOrderResp
createOrder tokenDetails@(personId, _, _) invoiceId = withFlowHandlerAPI . ActorInfo.withPersonIdActorInfo personId $ DPayment.createOrder tokenDetails invoiceId

-- This APIs are decoupled from Driver Fee Table.
getStatus :: (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Id DOrder.PaymentOrder -> FlowHandler DPayment.PaymentStatusResp
getStatus tokenDetails@(personId, _, _) orderId = withFlowHandlerAPI . ActorInfo.withPersonIdActorInfo personId $ DPayment.getStatus tokenDetails orderId

getOrder :: (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Id DOrder.PaymentOrder -> FlowHandler DOrder.PaymentOrderAPIEntity
getOrder tokenDetails@(personId, _, _) orderId = withFlowHandlerAPI . ActorInfo.withPersonIdActorInfo personId $ DPayment.getOrder tokenDetails orderId

getNotificationStatus :: (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Id Notification -> FlowHandler Payment.NotificationStatusResp
getNotificationStatus tokenDetails@(personId, _, _) = withFlowHandlerAPI . ActorInfo.withPersonIdActorInfo personId . DPayment.pdnNotificationStatus tokenDetails

postWalletRecharge :: (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> () -> FlowHandler Payment.CreateOrderResp
postWalletRecharge tokenDetails@(personId, _, _) _ = withFlowHandlerAPI . ActorInfo.withPersonIdActorInfo personId $ DPayment.postWalletRecharge tokenDetails ()

getWalletBalance :: (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler Wallet.WalletBalanceData
getWalletBalance tokenDetails@(personId, _, _) = withFlowHandlerAPI . ActorInfo.withPersonIdActorInfo personId $ DPayment.getWalletBalance tokenDetails

getStatusV2 :: (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Text -> FlowHandler DPayment.PaymentStatusResp
getStatusV2 tokenDetails@(personId, _, _) orderIdText = withFlowHandlerAPI . ActorInfo.withPersonIdActorInfo personId $ DPayment.getStatusV2 tokenDetails orderIdText
