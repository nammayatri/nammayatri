module API.Internal.ViolationDetection
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.ViolationDetection as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import SharedLogic.InternalAuth
import Storage.Beam.SystemConfigs ()

type API =
  ( "violationDetection"
      :> Header "api-key" Text
      :> ReqBody '[JSON] Domain.ViolationDetectionReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  violationDetection

violationDetection :: Maybe Text -> Domain.ViolationDetectionReq -> FlowHandler APISuccess
violationDetection apiKey req = withFlowHandlerAPI $ do
  checkLtsKey "violationDetection" apiKey
  Domain.violationDetection req
