module API.UI.Location
  ( API,
    handler,
    DLocation.GetLocationRes,
    DLocation.Waypoint (..),
    DLocation.UpdateLocationReq,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Id
import Beckn.Utils.Common hiding (id)
import qualified Domain.Action.UI.Location as DLocation
import qualified Domain.Action.UI.Location.UpdateLocation as DLocation
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import Environment
import qualified Lib.LocationUpdates as LocUpd
import Servant
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
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
  hdl <- constructHandler
  DLocation.updateLocation hdl personId waypoints
  where
    constructHandler = do
      pure $
        DLocation.UpdateLocationHandler
          { findPersonById = Person.findById,
            findDriverLocationById = DrLoc.findById,
            upsertDriverLocation = \driverId point timestamp ->
              Esq.runTransaction $ DrLoc.upsertGpsCoord driverId point timestamp,
            getInProgressByDriverId = QRide.getInProgressByDriverId,
            addIntermediateRoutePoints = LocUpd.addIntermediateRoutePoints LocUpd.defaultRideInterpolationHandler
          }

getLocation :: Id SRide.Ride -> FlowHandler DLocation.GetLocationRes
getLocation = withFlowHandlerAPI . DLocation.getLocation
