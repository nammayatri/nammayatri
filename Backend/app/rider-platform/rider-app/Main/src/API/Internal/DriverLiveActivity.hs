module API.Internal.DriverLiveActivity
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.DriverLiveActivity as Domain
import Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant

type API =
  ( "driverLiveActivity"
      :> Header "token" Text
      :> ReqBody '[JSON] Domain.DriverLiveActivityReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler = driverLiveActivityHandler

driverLiveActivityHandler :: Maybe Text -> Domain.DriverLiveActivityReq -> FlowHandler APISuccess
driverLiveActivityHandler _token = withFlowHandlerAPI . Domain.driverLiveActivityHandler
