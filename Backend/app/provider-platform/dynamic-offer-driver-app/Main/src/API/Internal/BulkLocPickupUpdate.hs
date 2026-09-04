module API.Internal.BulkLocPickupUpdate
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.BulkLocPickupUpdate as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  ( "bulkLocPickupUpdate"
      :> ReqBody '[JSON] Domain.BulkLocPickupUpdateReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  bulkLocPickupUpdate

bulkLocPickupUpdate :: Domain.BulkLocPickupUpdateReq -> FlowHandler APISuccess
bulkLocPickupUpdate = withFlowHandlerAPI . Domain.bulkLocPickupUpdate
