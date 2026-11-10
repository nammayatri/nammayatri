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
import SharedLogic.InternalAuth
import Storage.Beam.SystemConfigs ()

type API =
  ( "stopDetection"
      :> Header "api-key" Text
      :> ReqBody '[JSON] Domain.StopDetectionReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  stopDetection

stopDetection :: Maybe Text -> Domain.StopDetectionReq -> FlowHandler APISuccess
stopDetection apiKey req = withFlowHandlerAPI $ do
  checkLtsKey "stopDetection" apiKey
  Domain.stopDetection req
