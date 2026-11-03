module API.Internal.TollChargeApproval where

import qualified Domain.Action.Internal.TollChargeApproval as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  "tollChargeApproval"
    :> "request"
    :> Header "token" Text
    :> Capture "bppBookingId" Text
    :> ReqBody '[JSON] Domain.RequestTollChargeApprovalReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = requestTollChargeApproval

requestTollChargeApproval :: Maybe Text -> Text -> Domain.RequestTollChargeApprovalReq -> FlowHandler APISuccess
requestTollChargeApproval token bppBookingId req = withFlowHandlerAPI $ Domain.requestTollChargeApproval token bppBookingId req
