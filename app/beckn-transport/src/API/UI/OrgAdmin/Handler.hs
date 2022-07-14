module API.UI.OrgAdmin.Handler where

import API.UI.OrgAdmin.Types
import App.Types
import Beckn.Prelude
import qualified Domain.Action.UI.OrgAdmin as DOrgAdmin
import qualified Domain.Types.Person as SP
import Servant
import Utils.Auth (AdminTokenAuth)
import Utils.Common (withFlowHandlerAPI)

type API =
  "orgAdmin" :> "profile"
    :> ( AdminTokenAuth
           :> Get '[JSON] OrgAdminProfileRes
           :<|> AdminTokenAuth
             :> ReqBody '[JSON] UpdateOrgAdminProfileReq
             :> Post '[JSON] UpdateOrgAdminProfileRes
       )

handler :: FlowServer API
handler =
  getProfile
    :<|> updateProfile

getProfile :: SP.Person -> FlowHandler OrgAdminProfileRes
getProfile = withFlowHandlerAPI . DOrgAdmin.getProfile

updateProfile :: SP.Person -> UpdateOrgAdminProfileReq -> FlowHandler UpdateOrgAdminProfileRes
updateProfile admin = withFlowHandlerAPI . DOrgAdmin.updateProfile admin
