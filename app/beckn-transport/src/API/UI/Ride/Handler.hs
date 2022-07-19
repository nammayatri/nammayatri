module API.UI.Ride.Handler (API, handler) where

import API.UI.Ride.Types
import App.Types
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import Beckn.Utils.SlidingWindowLimiter (checkSlidingWindowLimit)
import qualified Domain.Action.UI.Ride as DRide
import qualified Domain.Action.UI.Ride.CancelRide as CHandler
import qualified Domain.Action.UI.Ride.CancelRide.Internal as CInternal
import qualified Domain.Action.UI.Ride.EndRide as EHandler
import qualified Domain.Action.UI.Ride.EndRide.Internal as EInternal
import qualified Domain.Action.UI.Ride.StartRide as SHandler
import qualified Domain.Action.UI.Ride.StartRide.Internal as SInternal
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import Product.BecknProvider.BP
import qualified Product.FareCalculator as Fare
import qualified Product.RentalFareCalculator as RentalFare
import Servant
import SharedLogic.LocationUpdates
import qualified SharedLogic.MissingLocationUpdatesMarker as MLUMarker
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import Tools.Metrics (putFareAndDistanceDeviations)
import Utils.Auth
import Utils.Common

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
             :> ReqBody '[JSON] StartRideReq
             :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> Capture "rideId" (Id SRide.Ride)
             :> "end"
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

startRide :: Id SP.Person -> Id SRide.Ride -> StartRideReq -> FlowHandler APISuccess.APISuccess
startRide personId rideId req = withFlowHandlerAPI $ do
  SHandler.startRideHandler handle personId (cast rideId) (req.rideOtp)
  where
    handle =
      SHandler.ServiceHandle
        { findById = QPerson.findById,
          findRideBookingById = QRB.findById,
          findRideById = QRide.findById,
          startRide = SInternal.startRideTransaction,
          notifyBAPRideStarted = sendRideStartedUpdateToBAP,
          rateLimitStartRide = \personId' rideId' -> checkSlidingWindowLimit (getId personId' <> "_" <> getId rideId')
        }

endRide :: Id SP.Person -> Id SRide.Ride -> FlowHandler APISuccess.APISuccess
endRide personId rideId = withFlowHandlerAPI $ do
  EHandler.endRideHandler handle personId rideId
  where
    handle =
      EHandler.ServiceHandle
        { findById = QPerson.findById,
          findRideBookingById = QRB.findById,
          findRideById = QRide.findById,
          notifyCompleteToBAP = sendRideCompletedUpdateToBAP,
          endRideTransaction = EInternal.endRideTransaction,
          calculateFare = Fare.calculateFare,
          calculateRentalFare = RentalFare.calculateRentalFare,
          buildRentalFareBreakups = RentalFare.buildRentalFareBreakups,
          buildFareBreakups = Fare.buildFareBreakups,
          recalculateFareEnabled = asks (.recalculateFareEnabled),
          putDiffMetric = putFareAndDistanceDeviations,
          findDriverLocById = DrLoc.findById,
          isMarketAsMissingLocationUpdates = MLUMarker.isMarketAsMissingLocationUpdates,
          updateLocationAllowedDelay = asks (.updateLocationAllowedDelay) <&> fromIntegral,
          recalcDistanceEnding = recalcDistanceBatches defaultRideInterpolationHandler True
        }

cancelRide :: Id SP.Person -> Id SRide.Ride -> CancelRideReq -> FlowHandler APISuccess.APISuccess
cancelRide personId rideId req = withFlowHandlerAPI $ do
  CHandler.cancelRideHandler handle personId rideId req
  where
    handle =
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
