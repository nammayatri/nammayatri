module API.Internal.BetterDriverSearch
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.BetterDriverSearch as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  ( "betterDriverSearch"
      :> ReqBody '[JSON] Domain.BetterDriverSearchReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  betterDriverSearch

betterDriverSearch :: Domain.BetterDriverSearchReq -> FlowHandler APISuccess
betterDriverSearch = withFlowHandlerAPI . Domain.betterDriverSearch
