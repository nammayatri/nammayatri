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
  ( "violationDetection"
      :> ReqBody '[JSON] Domain.ViolationDetectionReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  stopDetectionBAP

violationDetection :: Domain.ViolationDetectionReq -> FlowHandler APISuccess
violationDetection = withFlowHandlerAPI . Domain.violationDetection
