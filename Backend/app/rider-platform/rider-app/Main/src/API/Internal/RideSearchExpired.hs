module API.Internal.RideSearchExpired where

import qualified Domain.Action.Internal.RideSearchExpired as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  "rideSearchExpired"
    :> Header "token" Text
    :> ReqBody '[JSON] Domain.RideSearchExpiredReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler =
  rideSearchExpired

rideSearchExpired :: Maybe Text -> Domain.RideSearchExpiredReq -> FlowHandler APISuccess
rideSearchExpired token req = withFlowHandlerAPI $ Domain.rideSearchExpired token req
