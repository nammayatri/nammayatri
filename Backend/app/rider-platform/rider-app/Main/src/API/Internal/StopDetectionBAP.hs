module API.Internal.StopDetectionBAP 
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.StopDetectionBAP as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  ( "stopDetectionBAP"
      :> ReqBody '[JSON] Domain.StopDetectionBAPReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  stopDetectionBAP

stopDetectionBAP :: Domain.StopDetectionBAPReq -> FlowHandler APISuccess
stopDetectionBAP = withFlowHandlerAPI . Domain.stopDetectionBAP













