module API.UI.Performance where

import qualified Domain.Action.UI.Performance as Domain
import qualified Domain.Types.Person as SP
import Environment (FlowHandler, FlowServer)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant
import Tools.Auth

type API =
  "driver" :> "performance"
    :> TokenAuth
    :> Get '[JSON] Domain.PerformanceRes

handler :: FlowServer API
handler =
  getDriverPerformance

getDriverPerformance :: Id SP.Person -> FlowHandler Domain.PerformanceRes
getDriverPerformance = withFlowHandlerAPI . Domain.getDriverPerformance
