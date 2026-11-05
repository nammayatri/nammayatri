module API.Internal.DriverReachedDestination
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.DriverReachedDestination as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import SharedLogic.InternalAuth
import Storage.Beam.SystemConfigs ()

type API =
  ( "destinationReached"
      :> Header "api-key" Text
      :> ReqBody '[JSON] Domain.DriverReachedDestinationReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  driverReachedDestination

driverReachedDestination :: Maybe Text -> Domain.DriverReachedDestinationReq -> FlowHandler APISuccess
driverReachedDestination apiKey req = withFlowHandlerAPI $ do
  checkLtsKey "destinationReached" apiKey
  Domain.driverReachedDestination req
