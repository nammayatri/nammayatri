module API.UI.TranspAdmin.Handler (API, handler) where

import API.UI.TranspAdmin.Types
import App.Types
import Beckn.Prelude
import qualified Domain.Action.UI.TranspAdmin as DTranspAdmin
import qualified Domain.Types.Person as SP
import Servant
import Utils.Auth (AdminTokenAuth)
import Utils.Common (withFlowHandlerAPI)

type API =
  "orgAdmin" :> "profile"
    :> ( AdminTokenAuth
           :> Get '[JSON] TranspAdminProfileRes
           :<|> AdminTokenAuth
             :> ReqBody '[JSON] UpdateTranspAdminProfileReq
             :> Post '[JSON] UpdateTranspAdminProfileRes
       )

handler :: FlowServer API
handler =
  getProfile
    :<|> updateProfile

getProfile :: SP.Person -> FlowHandler TranspAdminProfileRes
getProfile = withFlowHandlerAPI . DTranspAdmin.getProfile

updateProfile :: SP.Person -> UpdateTranspAdminProfileReq -> FlowHandler UpdateTranspAdminProfileRes
updateProfile admin = withFlowHandlerAPI . DTranspAdmin.updateProfile admin
