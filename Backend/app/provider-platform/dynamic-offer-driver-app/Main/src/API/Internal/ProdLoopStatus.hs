module API.Internal.ProdLoopStatus where

import qualified Domain.Action.Internal.ProdLoopStatus as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  ( "prodLoopStatus"
      :> ReqBody '[JSON] Domain.ProdLoopStatusReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler = prodLoopStatus

prodLoopStatus :: Domain.ProdLoopStatusReq -> FlowHandler APISuccess
prodLoopStatus = withFlowHandlerAPI . Domain.prodLoopStatus
