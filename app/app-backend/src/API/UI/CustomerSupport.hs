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

import App.Types
import Beckn.Types.Id
import qualified Domain.Action.UI.CustomerSupport as DCS
import Domain.Types.Person as SP
import EulerHS.Prelude hiding (id)
import Servant
import Utils.Auth
import Utils.Common

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

logout :: Id SP.Person -> FlowHandler DCS.LogoutRes
logout = withFlowHandlerAPI . DCS.logout

listOrder :: Id SP.Person -> Maybe Text -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler [DCS.OrderResp]
listOrder personId mRequestId mMobile mlimit = withFlowHandlerAPI . DCS.listOrder personId mRequestId mMobile mlimit
