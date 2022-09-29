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
import Beckn.Utils.SlidingWindowLimiter (checkSlidingWindowLimit)
import qualified Domain.Action.UI.Ride as DRide
import qualified Domain.Action.UI.Ride.CancelRide as RideCancel
import qualified Domain.Action.UI.Ride.CancelRide.Internal as RideCancel
import qualified Domain.Action.UI.Ride.EndRide as RideEnd
import qualified Domain.Action.UI.Ride.EndRide.Internal as RideEnd
import qualified Domain.Action.UI.Ride.StartRide as RideStart
import qualified Domain.Action.UI.Ride.StartRide.Internal as RideStart
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as Ride
import Environment
import EulerHS.Prelude hiding (id)
import qualified Lib.LocationUpdates as LocUpd
import Servant
import SharedLogic.CallBAP
import qualified SharedLogic.CallBAP as CallBAP
import qualified SharedLogic.FareCalculator as Fare
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.TransporterConfig as QTConf
import Tools.Auth
import Tools.Metrics

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
startRide personId rideId req =
  withFlowHandlerAPI $
    RideStart.startRideHandler handle personId (cast rideId) req
  where
    handle =
      RideStart.ServiceHandle
        { findById = QPerson.findById,
          findBookingById = QRB.findById,
          findRideById = QRide.findById,
          startRideAndUpdateLocation = RideStart.startRideTransaction,
          notifyBAPRideStarted = sendRideStartedUpdateToBAP,
          rateLimitStartRide = \personId' rideId' -> checkSlidingWindowLimit (getId personId' <> "_" <> getId rideId'),
          initializeDistanceCalculation = LocUpd.initializeDistanceCalculation LocUpd.defaultRideInterpolationHandler rideId
        }

endRide :: Id SP.Person -> Id Ride.Ride -> RideEnd.EndRideReq -> FlowHandler APISuccess
endRide personId rideId req =
  withFlowHandlerAPI $
    RideEnd.endRideHandler handle personId rideId req
  where
    handle =
      RideEnd.ServiceHandle
        { findById = QPerson.findById,
          findBookingById = QRB.findById,
          findRideById = QRide.findById,
          notifyCompleteToBAP = CallBAP.sendRideCompletedUpdateToBAP,
          endRide = RideEnd.endRideTransaction,
          calculateFare = Fare.calculateFare,
          putDiffMetric = putFareAndDistanceDeviations,
          findDriverLocById = DrLoc.findById,
          isDistanceCalculationFailed = LocUpd.isDistanceCalculationFailed LocUpd.defaultRideInterpolationHandler,
          finalDistanceCalculation = LocUpd.finalDistanceCalculation LocUpd.defaultRideInterpolationHandler rideId,
          getDefaultPickupLocThreshold = asks (.defaultPickupLocThreshold),
          getDefaultDropLocThreshold = asks (.defaultDropLocThreshold),
          findConfigByOrgId = QTConf.findValueByOrgId
        }

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
