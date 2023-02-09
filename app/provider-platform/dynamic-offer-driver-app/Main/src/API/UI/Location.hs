module API.UI.Location
  ( API,
    handler,
    DLocation.GetLocationRes,
    DLocation.Waypoint (..),
    DLocation.UpdateLocationReq,
  )
where

import qualified Domain.Action.UI.Location as DLocation
import qualified Domain.Action.UI.Location.UpdateLocation as DLocation
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import Servant
import Tools.Auth

-- Location update and get for tracking is as follows
type API =
  "driver" :> "location"
    :> ( Capture "rideId" (Id SRide.Ride) -- TODO: add auth
           :> Get '[JSON] DLocation.GetLocationRes
           :<|> TokenAuth
             :> ReqBody '[JSON] DLocation.UpdateLocationReq
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  getLocation
    :<|> updateLocation

updateLocation :: Id Person.Person -> DLocation.UpdateLocationReq -> FlowHandler APISuccess
updateLocation personId waypoints = withFlowHandlerAPI $ do
  hdl <- DLocation.buildUpdateLocationHandle personId
  DLocation.updateLocationHandler hdl waypoints

getLocation :: Id SRide.Ride -> FlowHandler DLocation.GetLocationRes
getLocation = withFlowHandlerAPI . DLocation.getLocation
