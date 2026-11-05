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
import SharedLogic.InternalAuth
import Storage.Beam.SystemConfigs ()

type API =
  ( "bulkLocUpdate"
      :> Header "api-key" Text
      :> ReqBody '[JSON] Domain.BulkLocUpdateReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  bulkLocUpdate

bulkLocUpdate :: Maybe Text -> Domain.BulkLocUpdateReq -> FlowHandler APISuccess
bulkLocUpdate apiKey req = withFlowHandlerAPI $ do
  checkLtsKey "bulkLocUpdate" apiKey
  Domain.bulkLocUpdate req
