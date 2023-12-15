{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.CustomerSupport
  ( API,
    handler,
    DCS.OrderResp (..),
    DCS.OrderDetails (..),
    DCS.OrderInfo (..),
    DCS.LoginReq (..),
    DCS.LoginRes (..),
    DCS.LogoutRes (..),
  )
where

import qualified Domain.Action.UI.CustomerSupport as DCS
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

-- Customer Support Flow --

type API =
  "customerSupport"
    :> ( "login"
           :> ReqBody '[JSON] DCS.LoginReq
           :> Post '[JSON] DCS.LoginRes
           :<|> "logout"
             :> TokenAuth
             :> Post '[JSON] DCS.LogoutRes
           :<|> "orders"
             :> TokenAuth
             :> QueryParam "id" Text
             :> QueryParam "phone" Text
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> Get '[JSON] [DCS.OrderResp]
       )

handler :: FlowServer API
handler =
  login
    :<|> logout
    :<|> listOrder

login :: DCS.LoginReq -> FlowHandler DCS.LoginRes
login = withFlowHandlerAPI . DCS.login

logout :: (Id SP.Person, Id Merchant.Merchant) -> FlowHandler DCS.LogoutRes
logout = withFlowHandlerAPI . DCS.logout

listOrder :: (Id SP.Person, Id Merchant.Merchant) -> Maybe Text -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler [DCS.OrderResp]
listOrder (personId, _) mRequestId mMobile mlimit = withFlowHandlerAPI . DCS.listOrder personId mRequestId mMobile mlimit
