module API.UI.Whatsapp where

import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Kernel.Utils.Logging
import qualified Domain.Action.UI.Whatsapp as Whatsapp
import qualified Domain.Types.Person as Person
import Environment (FlowHandler, FlowServer)
import EulerHS.Prelude
import Servant
import Tools.Auth

type API =
  "whatsapp"
    :> ( "opt"
           :> TokenAuth
           :> ReqBody '[JSON] Whatsapp.OptAPIRequest
           :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler = whatsAppOptAPI

whatsAppOptAPI :: Id Person.Person -> Whatsapp.OptAPIRequest -> FlowHandler APISuccess
whatsAppOptAPI personId = withFlowHandlerAPI . withPersonIdLogTag personId . Whatsapp.whatsAppOptAPI personId
