module API.Internal.DriverArrivalNotf
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.DriverArrivalNotf as Domain
import Environment
import GHC.Base
import Kernel.Types.APISuccess
import Kernel.Utils.Error
import Servant

type API =
  ( "driverArrivalNotification"
      :> ReqBody '[JSON] Domain.DANTypeValidationReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  driverArrivalNotfHandler

driverArrivalNotfHandler :: Domain.DANTypeValidationReq -> FlowHandler APISuccess
driverArrivalNotfHandler = withFlowHandlerAPI . Domain.driverArrivalNotfHandler
