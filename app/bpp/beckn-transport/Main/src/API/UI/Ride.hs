module API.UI.Ride
  ( module Reexport,
    API,
    handler,
    StartRideReq (..),
    EndRideReq (..),
    CancelRideReq (..),
  )
where

import Beckn.External.Maps.Types
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
import qualified Domain.Action.UI.Ride.CancelRide as CHandler
import qualified Domain.Action.UI.Ride.EndRide as EHandler
import qualified Domain.Action.UI.Ride.StartRide as SHandler
import Domain.Types.CancellationReason (CancellationReasonCode (..))
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import Environment
import Servant
import SharedLogic.Person (findPerson)
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
             :> "arrived"
             :> "pickup"
             :> ReqBody '[JSON] LatLong
             :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> Capture "rideId" (Id SRide.Ride)
             :> "start"
             :> ReqBody '[JSON] StartRideReq
             :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> Capture "rideId" (Id SRide.Ride)
             :> "end"
             :> ReqBody '[JSON] EndRideReq
             :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> Capture "rideId" (Id SRide.Ride)
             :> "cancel"
             :> ReqBody '[JSON] CancelRideReq
             :> Post '[JSON] APISuccess
       )

data StartRideReq = StartRideReq
  { rideOtp :: Text,
    point :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype EndRideReq = EndRideReq
  { point :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CancelRideReq = CancelRideReq
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

handler :: FlowServer API
handler =
  listDriverRides
    :<|> arrivedAtPickup
    :<|> startRide
    :<|> endRide
    :<|> cancelRide

startRide :: Id SP.Person -> Id SRide.Ride -> StartRideReq -> FlowHandler APISuccess.APISuccess
startRide requestorId rideId StartRideReq {rideOtp, point} = withFlowHandlerAPI $ do
  requestor <- findPerson requestorId
  let driverReq = SHandler.DriverStartRideReq {rideOtp, point, requestor}
  shandle <- SHandler.buildStartRideHandle requestor.merchantId
  SHandler.driverStartRide shandle rideId driverReq

endRide :: Id SP.Person -> Id SRide.Ride -> EndRideReq -> FlowHandler APISuccess.APISuccess
endRide requestorId rideId EndRideReq {point} = withFlowHandlerAPI $ do
  requestor <- findPerson requestorId
  let driverReq = EHandler.DriverEndRideReq {point, requestor}
  shandle <- EHandler.buildEndRideHandle requestor.merchantId rideId
  EHandler.driverEndRide shandle rideId driverReq

cancelRide :: Id SP.Person -> Id SRide.Ride -> CancelRideReq -> FlowHandler APISuccess.APISuccess
cancelRide personId rideId CancelRideReq {reasonCode, additionalInfo} = withFlowHandlerAPI $ do
  let driverReq = CHandler.CancelRideReq {reasonCode, additionalInfo}
  CHandler.driverCancelRideHandler CHandler.cancelRideHandle personId rideId driverReq

listDriverRides ::
  Id SP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  FlowHandler DriverRideListRes
listDriverRides driverId mbLimit mbOffset =
  withFlowHandlerAPI . DRide.listDriverRides driverId mbLimit mbOffset

arrivedAtPickup :: Id SP.Person -> Id SRide.Ride -> LatLong -> FlowHandler APISuccess
arrivedAtPickup _ rideId req = withFlowHandlerAPI $ DRide.arrivedAtPickup rideId req
