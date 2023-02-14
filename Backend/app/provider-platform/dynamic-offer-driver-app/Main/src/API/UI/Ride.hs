 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Ride
  ( StartRideReq (..),
    EndRideReq (..),
    CancelRideReq (..),
    DRide.DriverRideListRes (..),
    DRide.DriverRideRes (..),
    API,
    handler,
  )
where

import qualified Domain.Action.UI.Ride as DRide
import qualified Domain.Action.UI.Ride.CancelRide as RideCancel
import qualified Domain.Action.UI.Ride.EndRide as RideEnd
import qualified Domain.Action.UI.Ride.StartRide as RideStart
import Domain.Types.CancellationReason (CancellationReasonCode (..))
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as Ride
import Environment
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
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
           :> QueryParam "status" Ride.RideStatus
           :> Get '[JSON] DRide.DriverRideListRes
           :<|> TokenAuth
           :> Capture "rideId" (Id Ride.Ride)
           :> "arrived"
           :> "pickup"
           :> ReqBody '[JSON] LatLong
           :> Post '[JSON] APISuccess
           :<|> TokenAuth
           :> Capture "rideId" (Id Ride.Ride)
           :> "start"
           :> ReqBody '[JSON] StartRideReq
           :> Post '[JSON] APISuccess
           :<|> TokenAuth
           :> Capture "rideId" (Id Ride.Ride)
           :> "end"
           :> ReqBody '[JSON] EndRideReq
           :> Post '[JSON] APISuccess
           :<|> TokenAuth
           :> Capture "rideId" (Id Ride.Ride)
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

startRide :: Id SP.Person -> Id Ride.Ride -> StartRideReq -> FlowHandler APISuccess
startRide requestorId rideId StartRideReq {rideOtp, point} = withFlowHandlerAPI $ do
  requestor <- findPerson requestorId
  let driverReq = RideStart.DriverStartRideReq {rideOtp, point, requestor}
  shandle <- RideStart.buildStartRideHandle requestor.merchantId
  RideStart.driverStartRide shandle rideId driverReq

endRide :: Id SP.Person -> Id Ride.Ride -> EndRideReq -> FlowHandler APISuccess
endRide requestorId rideId EndRideReq {point} = withFlowHandlerAPI $ do
  requestor <- findPerson requestorId
  let driverReq = RideEnd.DriverEndRideReq {point, requestor}
  shandle <- RideEnd.buildEndRideHandle requestor.merchantId
  RideEnd.driverEndRide shandle rideId driverReq

cancelRide :: Id SP.Person -> Id Ride.Ride -> CancelRideReq -> FlowHandler APISuccess
cancelRide personId rideId CancelRideReq {reasonCode, additionalInfo} = withFlowHandlerAPI $ do
  let driverReq = RideCancel.CancelRideReq {reasonCode, additionalInfo}
  RideCancel.driverCancelRideHandler RideCancel.cancelRideHandle personId rideId driverReq

listDriverRides ::
  Id SP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  Maybe Ride.RideStatus ->
  FlowHandler DRide.DriverRideListRes
listDriverRides driverId mbLimit mbOffset mbRideStatus = withFlowHandlerAPI . DRide.listDriverRides driverId mbLimit mbOffset mbRideStatus

arrivedAtPickup :: Id SP.Person -> Id Ride.Ride -> LatLong -> FlowHandler APISuccess
arrivedAtPickup _ rideId req = withFlowHandlerAPI $ DRide.arrivedAtPickup rideId req
