module API.Internal.TollChargeApproval
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.Ride.EndRideRequirements as Domain
import Domain.Types.Ride
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  Capture "rideId" (Id Ride)
    :> "tollChargeApproval"
    :> Header "token" Text
    :> ReqBody '[JSON] Domain.TollChargeApprovalDecisionReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler =
  tollChargeApproval

tollChargeApproval :: Id Ride -> Maybe Text -> Domain.TollChargeApprovalDecisionReq -> FlowHandler APISuccess
tollChargeApproval rideId apiKey req = withFlowHandlerAPI $ Domain.applyTollChargeApprovalDecision rideId req apiKey
