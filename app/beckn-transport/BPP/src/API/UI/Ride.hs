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
import Beckn.Utils.SlidingWindowLimiter (checkSlidingWindowLimit)
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
import qualified Domain.Action.UI.Ride.EndRide.Internal as EInternal
import Domain.Action.UI.Ride.StartRide as Reexport (StartRideReq (..))
import qualified Domain.Action.UI.Ride.StartRide as SHandler
import qualified Domain.Action.UI.Ride.StartRide.Internal as SInternal
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import Environment
import Servant
import SharedLogic.CallBAP
import qualified SharedLogic.FareCalculator.OneWayFareCalculator as Fare
import qualified SharedLogic.FareCalculator.RentalFareCalculator as RentalFare
import SharedLogic.LocationUpdates
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.FarePolicy.RentalFarePolicy as QRentalFP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Auth
import Tools.Error (RentalFarePolicyError (NoRentalFarePolicy))
import Tools.Metrics (putFareAndDistanceDeviations)

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
  SHandler.startRideHandler shandle personId (cast rideId) req
  where
    shandle =
      SHandler.ServiceHandle
        { findById = QPerson.findById,
          findBookingById = QRB.findById,
          findRideById = QRide.findById,
          startRideAndUpdateLocation = SInternal.startRideTransaction,
          notifyBAPRideStarted = sendRideStartedUpdateToBAP,
          rateLimitStartRide = \personId' rideId' -> checkSlidingWindowLimit (getId personId' <> "_" <> getId rideId'),
          addFirstWaypoint = \driverId pt -> do
            clearLocationUpdatesOnRideEnd defaultRideInterpolationHandler driverId
            addPoints defaultRideInterpolationHandler driverId $ pt :| []
        }

endRide :: Id SP.Person -> Id SRide.Ride -> EHandler.EndRideReq -> FlowHandler APISuccess.APISuccess
endRide personId rideId req = withFlowHandlerAPI $ do
  EHandler.endRideHandler shandle personId rideId req
  where
    shandle =
      EHandler.ServiceHandle
        { findById = QPerson.findById,
          findBookingById = QRB.findById,
          findRideById = QRide.findById,
          notifyCompleteToBAP = sendRideCompletedUpdateToBAP,
          endRideTransaction = EInternal.endRideTransaction,
          calculateFare = Fare.calculateFare,
          calculateRentalFare = RentalFare.calculateRentalFare,
          getRentalFarePolicy = QRentalFP.findById >=> fromMaybeM NoRentalFarePolicy,
          buildRentalFareBreakups = RentalFare.buildRentalFareBreakups,
          buildOneWayFareBreakups = Fare.buildOneWayFareBreakups,
          recalculateFareEnabled = asks (.recalculateFareEnabled),
          putDiffMetric = putFareAndDistanceDeviations,
          findDriverLocById = DrLoc.findById,
          addLastWaypointAndRecalcDistanceOnEnd = \driverId pt ->
            processWaypoints defaultRideInterpolationHandler driverId True $ pt :| [],
          thereWasFailedDistanceRecalculation = isDistanceCalculationFailed
        }

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
