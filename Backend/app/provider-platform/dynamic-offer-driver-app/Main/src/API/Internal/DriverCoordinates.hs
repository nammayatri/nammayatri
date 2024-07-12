module API.Internal.DriverCoordinates
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.DriverCoordinates as Domain
import Domain.Types.Ride
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  Capture "rideId" (Id Ride)
    :> "getDriverCoordinates"
    :> Header "token" Text
    :> Get '[JSON] (Maybe Maps.LatLong)

handler :: FlowServer API
handler =
  getDriverCoordinates

getDriverCoordinates :: Id Ride -> Maybe Text -> FlowHandler (Maybe Maps.LatLong)
getDriverCoordinates rideId = withFlowHandlerAPI . Domain.getDriverCoordinates rideId
