module API.Internal.KnowYourDriver
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.KnowYourDriver as Domain
import qualified Domain.Types.Person as DP
import Domain.Types.Ride
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  Capture "rideId" (Id Ride)
    :> "knowYourDriver"
    :> Header "token" Text
    :> Get '[JSON] Domain.DriverProfileRes
    :<|> Capture "driverId" (Id DP.Person)
      :> "knowYourFavDriver"
      :> Header "token" Text
      :> Get '[JSON] Domain.DriverProfileRes

handler :: FlowServer API
handler =
  knowYourDriver
    :<|> knowYourFavDriver

knowYourDriver :: Id Ride -> Maybe Text -> FlowHandler Domain.DriverProfileRes
knowYourDriver rideId = withFlowHandlerAPI . Domain.knowYourDriver rideId

knowYourFavDriver :: Id DP.Person -> Maybe Text -> FlowHandler Domain.DriverProfileRes
knowYourFavDriver driverId = withFlowHandlerAPI . Domain.knowYourFavDriver driverId
