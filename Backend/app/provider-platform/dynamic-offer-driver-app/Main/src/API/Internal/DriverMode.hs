module API.Internal.DriverMode
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.DriverMode as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  ( "driverMode"
      :> Header "api-key" Text
      :> ReqBody '[JSON] Domain.DriverModeReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  setDriverMode

setDriverMode :: Maybe Text -> Domain.DriverModeReq -> FlowHandler APISuccess
setDriverMode apiKey = withFlowHandlerAPI . Domain.setDriverMode apiKey
