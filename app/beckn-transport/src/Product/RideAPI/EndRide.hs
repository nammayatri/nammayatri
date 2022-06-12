module Product.RideAPI.EndRide where

import App.Types (FlowHandler)
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Redis.Queries as Redis
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.FareBreakup as DFareBreakup
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RideBooking as SRB
import EulerHS.Prelude hiding (id)
import Product.BecknProvider.BP
import qualified Product.FareCalculator as Fare
import qualified Product.RentalFareCalculator as RentalFare
import qualified Product.RideAPI.Handlers.EndRide as Handler
import SharedLogic.LocationUpdates
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.DriverStats as DriverStats
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import Tools.Metrics (putFareAndDistanceDeviations)
import Types.App (Driver)
import Utils.Common (withFlowHandlerAPI)

endRide :: Id SP.Person -> Id Ride.Ride -> FlowHandler APISuccess.APISuccess
endRide personId rideId = withFlowHandlerAPI $ do
  Handler.endRideHandler handle personId rideId
  where
    handle =
      Handler.ServiceHandle
        { findById = Person.findById,
          findRideBookingById = QRB.findById,
          findRideById = QRide.findById,
          notifyCompleteToBAP = sendRideCompletedUpdateToBAP,
          endRideTransaction,
          calculateFare = Fare.calculateFare,
          calculateRentalFare = RentalFare.calculateRentalFare,
          buildRentalFareBreakups = RentalFare.buildRentalFareBreakups,
          buildFareBreakups = Fare.buildFareBreakups,
          recalculateFareEnabled = asks (.recalculateFareEnabled),
          putDiffMetric = putFareAndDistanceDeviations,
          findDriverLocById = DrLoc.findById,
          getKeyRedis = Redis.getKeyRedis,
          updateLocationAllowedDelay = asks (.updateLocationAllowedDelay) <&> fromIntegral,
          recalcDistanceEnding = recalcDistanceBatches defaultRideInterpolationHandler True
        }

endRideTransaction :: EsqDBFlow m r => Id SRB.RideBooking -> Ride.Ride -> Id Driver -> [DFareBreakup.FareBreakup] -> m ()
endRideTransaction rideBookingId ride driverId fareBreakups = Esq.runTransaction $ do
  QRide.updateAll ride.id ride
  QRide.updateStatus ride.id Ride.COMPLETED
  QRB.updateStatus rideBookingId SRB.COMPLETED
  DriverInformation.updateOnRide driverId False
  DriverStats.updateIdleTime driverId
  traverse_ QFareBreakup.create fareBreakups
