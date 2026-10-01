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
    :> ( "mode"
           :> Header "token" Text
           :> Capture "bppBookingId" Text
           :> Get '[JSON] Domain.TollChargeApprovalModeRes
           :<|> "request"
             :> Header "token" Text
             :> Capture "bppBookingId" Text
             :> ReqBody '[JSON] Domain.RequestTollChargeApprovalReq
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  getTollChargeApprovalMode
    :<|> requestTollChargeApproval

getTollChargeApprovalMode :: Maybe Text -> Text -> FlowHandler Domain.TollChargeApprovalModeRes
getTollChargeApprovalMode token bppBookingId = withFlowHandlerAPI $ Domain.getTollChargeApprovalMode token bppBookingId

requestTollChargeApproval :: Maybe Text -> Text -> Domain.RequestTollChargeApprovalReq -> FlowHandler APISuccess
requestTollChargeApproval token bppBookingId req = withFlowHandlerAPI $ Domain.requestTollChargeApproval token bppBookingId req
