module API.Internal.LiveActivityUpdate
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.LiveActivityUpdate as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  ( "liveActivityUpdate"
      :> ReqBody '[JSON] Domain.LiveActivityUpdateReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  liveActivityUpdate

liveActivityUpdate :: Domain.LiveActivityUpdateReq -> FlowHandler APISuccess
liveActivityUpdate = withFlowHandlerAPI . Domain.liveActivityUpdate
