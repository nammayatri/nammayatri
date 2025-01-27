module API.Internal.DriverReachedDestination
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.DriverReachedDestination as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  ( "destinationReached"
      :> ReqBody '[JSON] Domain.DriverReachedDestinationReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  driverReachedDestination

driverReachedDestination :: Domain.DriverReachedDestinationReq -> FlowHandler APISuccess
driverReachedDestination = withFlowHandlerAPI . Domain.driverReachedDestination
