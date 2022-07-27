module API.UI.Location (module Reexport, API, handler) where

import App.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Id
import Domain.Action.UI.Location as Reexport
  ( GetLocationRes (..),
    Status (..),
  )
import qualified Domain.Action.UI.Location as DLocation
import Domain.Action.UI.Location.UpdateLocation as Reexport
  ( UpdateLocationReq,
    UpdateLocationRes,
    Waypoint (..),
  )
import qualified Domain.Action.UI.Location.UpdateLocation as DLocation
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import GHC.Records.Extra
import Servant
import SharedLogic.LocationUpdates
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import Utils.Auth (TokenAuth)
import Utils.Common hiding (id)

-- Location update and get for tracking is as follows
type API =
  "driver" :> "location"
    :> ( Capture "rideId" (Id SRide.Ride) -- TODO: add auth
           :> Get '[JSON] GetLocationRes
           :<|> TokenAuth
           :> ReqBody '[JSON] UpdateLocationReq
           :> Post '[JSON] UpdateLocationRes
       )

handler :: FlowServer API
handler =
  getLocation
    :<|> updateLocation

updateLocation :: Id Person.Person -> UpdateLocationReq -> FlowHandler APISuccess
updateLocation personId waypoints = withFlowHandlerAPI $ do
  hdlr <- constructHandler
  DLocation.updateLocationHandler hdlr personId waypoints
  where
    constructHandler = do
      refreshPeriod <- fromIntegral <$> asks (.updateLocationRefreshPeriod)
      allowedDelay <- fromIntegral <$> asks (.updateLocationAllowedDelay)
      pure $
        DLocation.Handler
          { refreshPeriod,
            allowedDelay,
            findPersonById = Person.findById,
            findDriverLocationById = DrLoc.findById,
            upsertDriverLocation = \driverId point timestamp ->
              Esq.runTransaction $ DrLoc.upsertGpsCoord driverId point timestamp,
            getInProgressByDriverId = QRide.getInProgressByDriverId,
            interpolationHandler = defaultRideInterpolationHandler
          }

getLocation :: Id SRide.Ride -> FlowHandler GetLocationRes
getLocation = withFlowHandlerAPI . DLocation.getLocation
