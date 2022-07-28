module API.UI.TranspAdmin (module Reexport, API, handler) where

import Beckn.Prelude
import Beckn.Utils.Common (withFlowHandlerAPI)
import Domain.Action.UI.TranspAdmin as Reexport
  ( TranspAdminProfileRes (..),
    UpdateTranspAdminProfileReq (..),
    UpdateTranspAdminProfileRes,
  )
import qualified Domain.Action.UI.TranspAdmin as DTranspAdmin
import qualified Domain.Types.Person as SP
import Environment
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
