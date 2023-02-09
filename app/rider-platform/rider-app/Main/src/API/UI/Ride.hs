module API.UI.Ride
  ( API,
    handler,
    DRide.GetDriverLocRes,
  )
where

import Data.Aeson.Types ()
import qualified Domain.Action.UI.Ride as DRide
import qualified Domain.Types.Person as SPerson
import qualified Domain.Types.Ride as SRide
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "ride"
    :> Capture "rideId" (Id SRide.Ride)
    :> "driver"
    :> "location"
    :> TokenAuth
    :> Post '[JSON] DRide.GetDriverLocRes

handler :: FlowServer API
handler = getDriverLoc

getDriverLoc :: Id SRide.Ride -> Id SPerson.Person -> FlowHandler DRide.GetDriverLocRes
getDriverLoc rideId personId = withFlowHandlerAPI . withPersonIdLogTag personId $ DRide.getDriverLoc rideId personId
