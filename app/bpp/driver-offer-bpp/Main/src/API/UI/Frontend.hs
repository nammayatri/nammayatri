module API.UI.Frontend
  ( DFrontend.GetDriverFlowStatusRes,
    API,
    handler,
  )
where

import qualified Domain.Action.UI.Frontend as DFrontend
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "frontend"
    :> "flowStatus"
    :> TokenAuth
    :> Get '[JSON] DFrontend.GetDriverFlowStatusRes

handler :: FlowServer API
handler =
  getDriverFlowStatus

getDriverFlowStatus :: Id Person.Person -> FlowHandler DFrontend.GetDriverFlowStatusRes
getDriverFlowStatus = withFlowHandlerAPI . DFrontend.getDriverFlowStatus
