module Product.RideAPI.EndRide where

import App.Types (FlowHandler)
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import EulerHS.Prelude hiding (id)
import Product.BecknProvider.BP
import qualified Product.FareCalculator.Interpreter as Fare
import qualified Product.RideAPI.Handlers.EndRide as Handler
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as DriverStats
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Quote as PI
import qualified Storage.Queries.Ride as QRide
import Types.App (Driver)
import qualified Types.Storage.Person as SP
import qualified Types.Storage.OldRide as Ride
import Utils.Metrics (putFareAndDistanceDeviations)
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Beckn.Utils.Common

endRide :: Id SP.Person -> Id Ride.Ride -> FlowHandler APISuccess.APISuccess
endRide personId rideId = withFlowHandlerAPI $ do
  Handler.endRideHandler handle personId rideId
  where
    handle =
      Handler.ServiceHandle
        { findPersonById = Person.findPersonById,
          findPIById = PI.findById',
          findRideById = QRide.findById,
          findSearchRequestById= QSearchRequest.findById,
          notifyUpdateToBAP = notifyUpdateToBAP,
          endRideTransaction,
          calculateFare = \orgId vehicleVariant distance -> Fare.calculateFare orgId vehicleVariant (Right distance),
          recalculateFareEnabled = asks (.recalculateFareEnabled),
          putDiffMetric = putFareAndDistanceDeviations
        }

endRideTransaction :: DBFlow m r => Id Ride.Ride -> Id Driver -> Amount -> m ()
endRideTransaction rideId driverId actualPrice = DB.runSqlDBTransaction $ do
  QRide.updateActualPrice actualPrice rideId
  QRide.updateStatus rideId Ride.COMPLETED
  DriverInformation.updateOnRide driverId False
  DriverStats.updateIdleTime driverId
