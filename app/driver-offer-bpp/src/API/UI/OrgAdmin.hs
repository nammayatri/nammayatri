module API.UI.OrgAdmin
  ( API,
    handler,
    DOrgAdmin.OrgAdminProfileRes (..),
    DOrgAdmin.UpdateOrgAdminProfileReq (..),
    DOrgAdmin.UpdateOrgAdminProfileRes,
  )
where

import Beckn.Utils.Common
import qualified Domain.Action.UI.OrgAdmin as DOrgAdmin
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth

type API =
  "orgAdmin" :> "profile"
    :> AdminTokenAuth
    :> Get '[JSON] DOrgAdmin.OrgAdminProfileRes
    :<|> AdminTokenAuth
      :> ReqBody '[JSON] DOrgAdmin.UpdateOrgAdminProfileReq
      :> Post '[JSON] DOrgAdmin.UpdateOrgAdminProfileRes

handler :: FlowServer API
handler =
  getProfile
    :<|> updateProfile

getProfile :: SP.Person -> FlowHandler DOrgAdmin.OrgAdminProfileRes
getProfile = withFlowHandlerAPI . DOrgAdmin.getProfile

updateProfile :: SP.Person -> DOrgAdmin.UpdateOrgAdminProfileReq -> FlowHandler DOrgAdmin.UpdateOrgAdminProfileRes
updateProfile admin = withFlowHandlerAPI . DOrgAdmin.updateProfile admin
