module API.Internal.StopDetection
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.StopDetection as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  ( "stopDetection"
      :> ReqBody '[JSON] Domain.StopDetectionReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  stopDetection

stopDetection :: Domain.StopDetectionReq -> FlowHandler APISuccess
stopDetection = withFlowHandlerAPI . Domain.stopDetection
