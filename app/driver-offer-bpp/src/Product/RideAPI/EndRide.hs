module Product.RideAPI.EndRide where

import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as Ride
import Environment (FlowHandler)
import EulerHS.Prelude hiding (id)
import Product.BecknProvider.BP
import qualified Product.FareCalculator.Flow as Fare
import qualified Product.RideAPI.Handlers.EndRide as Handler
import SharedLogic.LocationUpdates
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.DriverStats as DriverStats
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import Tools.Metrics (putFareAndDistanceDeviations)
import Types.API.Ride
import Types.App (Driver)
import Utils.Common (withFlowHandlerAPI)

endRide :: Id SP.Person -> Id Ride.Ride -> EndRideReq -> FlowHandler APISuccess.APISuccess
endRide personId rideId req = withFlowHandlerAPI $ do
  Handler.endRideHandler handle personId rideId req
  where
    handle =
      Handler.ServiceHandle
        { findById = Person.findById,
          findBookingById = QRB.findById,
          findRideById = QRide.findById,
          notifyCompleteToBAP = sendRideCompletedUpdateToBAP,
          endRideTransaction,
          calculateFare = Fare.calculateFare,
          putDiffMetric = putFareAndDistanceDeviations,
          findDriverLocById = DrLoc.findById,
          addLastWaypointAndRecalcDistanceOnEnd = \driverId pt -> do
            addPoints defaultRideInterpolationHandler driverId $ pt :| []
            recalcDistanceBatches defaultRideInterpolationHandler True driverId
        }

endRideTransaction :: EsqDBFlow m r => Id SRB.Booking -> Ride.Ride -> Id Driver -> m ()
endRideTransaction bookingId ride driverId = Esq.runTransaction $ do
  QRide.updateAll ride.id ride
  QRide.updateStatus ride.id Ride.COMPLETED
  QRB.updateStatus bookingId SRB.COMPLETED
  DriverInformation.updateOnRide driverId False
  DriverStats.updateIdleTime driverId
