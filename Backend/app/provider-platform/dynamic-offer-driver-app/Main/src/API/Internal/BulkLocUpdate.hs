module API.Internal.BulkLocUpdate
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.BulkLocUpdate as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  ( "bulkLocUpdate"
      :> ReqBody '[JSON] Domain.BulkLocUpdateReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  bulkLocUpdate

bulkLocUpdate :: Domain.BulkLocUpdateReq -> FlowHandler APISuccess
bulkLocUpdate = withFlowHandlerAPI . Domain.bulkLocUpdate
