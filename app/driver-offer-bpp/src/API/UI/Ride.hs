module API.UI.Ride
  ( RideStart.StartRideReq (..),
    RideEnd.EndRideReq (..),
    RideCancel.CancelRideReq (..),
    DRide.DriverRideListRes (..),
    DRide.DriverRideRes (..),
    API,
    handler,
  )
where

import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.UI.Ride as DRide
import qualified Domain.Action.UI.Ride.CancelRide as RideCancel
import qualified Domain.Action.UI.Ride.CancelRide.Internal as RideCancel
import qualified Domain.Action.UI.Ride.EndRide as RideEnd
import qualified Domain.Action.UI.Ride.StartRide as RideStart
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as Ride
import Environment
import EulerHS.Prelude hiding (id)
import Servant
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Auth

type API =
  "driver" :> "ride"
    :> ( "list"
           :> TokenAuth
           :> QueryParam "limit" Integer
           :> QueryParam "offset" Integer
           :> QueryParam "onlyActive" Bool
           :> Get '[JSON] DRide.DriverRideListRes
           :<|> TokenAuth
           :> Capture "rideId" (Id Ride.Ride)
           :> "start"
           :> ReqBody '[JSON] RideStart.StartRideReq
           :> Post '[JSON] APISuccess
           :<|> TokenAuth
           :> Capture "rideId" (Id Ride.Ride)
           :> "end"
           :> ReqBody '[JSON] RideEnd.EndRideReq
           :> Post '[JSON] APISuccess
           :<|> TokenAuth
           :> Capture "rideId" (Id Ride.Ride)
           :> "cancel"
           :> ReqBody '[JSON] RideCancel.CancelRideReq
           :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  listDriverRides
    :<|> startRide
    :<|> endRide
    :<|> cancelRide

startRide :: Id SP.Person -> Id Ride.Ride -> RideStart.StartRideReq -> FlowHandler APISuccess
startRide personId rideId req = withFlowHandlerAPI $ do
  handle <- RideStart.buildStartRideHandle personId rideId
  RideStart.startRideHandler handle (cast rideId) req

endRide :: Id SP.Person -> Id Ride.Ride -> RideEnd.EndRideReq -> FlowHandler APISuccess
endRide personId rideId req = withFlowHandlerAPI $ do
  handle <- RideEnd.buildEndRideHandle personId rideId
  RideEnd.endRideHandler handle rideId req

cancelRide :: Id SP.Person -> Id Ride.Ride -> RideCancel.CancelRideReq -> FlowHandler APISuccess
cancelRide personId rideId req =
  withFlowHandlerAPI $
    RideCancel.cancelRideHandler handle personId rideId req
  where
    handle =
      RideCancel.ServiceHandle
        { findRideById = QRide.findById,
          findById = QPerson.findById,
          cancelRide = RideCancel.cancelRideImpl
        }

listDriverRides ::
  Id SP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  FlowHandler DRide.DriverRideListRes
listDriverRides driverId mbLimit mbOffset = withFlowHandlerAPI . DRide.listDriverRides driverId mbLimit mbOffset
