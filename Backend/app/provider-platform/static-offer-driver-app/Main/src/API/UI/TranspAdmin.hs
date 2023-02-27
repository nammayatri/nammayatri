{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.TranspAdmin (module Reexport, API, handler) where

import Domain.Action.UI.TranspAdmin as Reexport
  ( TranspAdminProfileRes (..),
    UpdateTranspAdminProfileReq (..),
    UpdateTranspAdminProfileRes,
  )
import qualified Domain.Action.UI.TranspAdmin as DTranspAdmin
import qualified Domain.Types.Person as SP
import Environment
import Kernel.Prelude
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant
import Tools.Auth (AdminTokenAuth)

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
