module API.Internal.DriverInactiveFCM
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.DriverInactiveFCM as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  ( "driverInactiveFCM"
      :> ReqBody '[JSON] Domain.DriverInactiveFCMReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  driverInactiveFCM

driverInactiveFCM :: Domain.DriverInactiveFCMReq -> FlowHandler APISuccess
driverInactiveFCM = withFlowHandlerAPI . Domain.driverInactiveFCM
