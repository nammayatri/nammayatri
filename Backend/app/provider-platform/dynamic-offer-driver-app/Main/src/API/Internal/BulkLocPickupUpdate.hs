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
import SharedLogic.InternalAuth
import Storage.Beam.SystemConfigs ()

type API =
  ( "bulkLocPickupUpdate"
      :> Header "api-key" Text
      :> ReqBody '[JSON] Domain.BulkLocPickupUpdateReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  bulkLocPickupUpdate

bulkLocPickupUpdate :: Maybe Text -> Domain.BulkLocPickupUpdateReq -> FlowHandler APISuccess
bulkLocPickupUpdate apiKey req = withFlowHandlerAPI $ do
  checkLtsKey "bulkLocPickupUpdate" apiKey
  Domain.bulkLocPickupUpdate req
