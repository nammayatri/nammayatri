{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.OrgAdmin
  ( API,
    handler,
    DOrgAdmin.OrgAdminProfileRes (..),
    DOrgAdmin.UpdateOrgAdminProfileReq (..),
    DOrgAdmin.UpdateOrgAdminProfileRes,
  )
where

import qualified Domain.Action.UI.OrgAdmin as DOrgAdmin
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
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
