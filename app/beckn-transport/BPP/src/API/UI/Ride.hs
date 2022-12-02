module API.UI.Ride
  ( module Reexport,
    API,
    handler,
  )
where

import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Action.UI.Ride as Reexport
  ( DriverRideListRes (..),
    DriverRideRes (..),
  )
import qualified Domain.Action.UI.Ride as DRide
import Domain.Action.UI.Ride.CancelRide as Reexport (CancelRideReq (..))
import qualified Domain.Action.UI.Ride.CancelRide as CHandler
import qualified Domain.Action.UI.Ride.CancelRide.Internal as CInternal
import Domain.Action.UI.Ride.EndRide as Reexport (EndRideReq (..))
import qualified Domain.Action.UI.Ride.EndRide as EHandler
import Domain.Action.UI.Ride.StartRide as Reexport (StartRideReq (..))
import qualified Domain.Action.UI.Ride.StartRide as SHandler
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import Environment
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
           :> Get '[JSON] DriverRideListRes
           :<|> TokenAuth
             :> Capture "rideId" (Id SRide.Ride)
             :> "start"
             :> ReqBody '[JSON] SHandler.StartRideReq
             :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> Capture "rideId" (Id SRide.Ride)
             :> "end"
             :> ReqBody '[JSON] EHandler.EndRideReq
             :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> Capture "rideId" (Id SRide.Ride)
             :> "cancel"
             :> ReqBody '[JSON] CancelRideReq
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  listDriverRides
    :<|> startRide
    :<|> endRide
    :<|> cancelRide

startRide :: Id SP.Person -> Id SRide.Ride -> SHandler.StartRideReq -> FlowHandler APISuccess.APISuccess
startRide personId rideId req = withFlowHandlerAPI $ do
  shandle <- SHandler.buildStartRideHandle personId rideId
  SHandler.startRide shandle (cast rideId) req

endRide :: Id SP.Person -> Id SRide.Ride -> EHandler.EndRideReq -> FlowHandler APISuccess.APISuccess
endRide personId rideId req = withFlowHandlerAPI $ do
  shandle <- EHandler.buildEndRideHandle personId rideId
  EHandler.endRide shandle personId req

cancelRide :: Id SP.Person -> Id SRide.Ride -> CancelRideReq -> FlowHandler APISuccess.APISuccess
cancelRide personId rideId req = withFlowHandlerAPI $ do
  CHandler.cancelRideHandler shandle personId rideId req
  where
    shandle =
      CHandler.ServiceHandle
        { findRideById = QRide.findById,
          findById = QPerson.findById,
          cancelRide = CInternal.cancelRide
        }

listDriverRides ::
  Id SP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  FlowHandler DriverRideListRes
listDriverRides driverId mbLimit mbOffset =
  withFlowHandlerAPI . DRide.listDriverRides driverId mbLimit mbOffset
