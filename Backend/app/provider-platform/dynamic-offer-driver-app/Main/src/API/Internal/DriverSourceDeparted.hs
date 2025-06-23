module API.Internal.DriverSourceDeparted
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.DriverSourceDeparted as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  ( "sourceDeparted"
      :> ReqBody '[JSON] Domain.DriverSourceDepartedReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  driverSourceDeparted

driverSourceDeparted :: Domain.DriverSourceDepartedReq -> FlowHandler APISuccess
driverSourceDeparted = withFlowHandlerAPI . Domain.driverSourceDeparted
