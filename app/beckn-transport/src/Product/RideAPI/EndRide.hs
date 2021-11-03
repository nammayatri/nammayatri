module Product.RideAPI.EndRide where

import App.Types (FlowHandler)
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.Mobility.Order as Mobility
import EulerHS.Prelude hiding (id)
import Product.BecknProvider.BP
import qualified Product.FareCalculator.Interpreter as Fare
import qualified Product.RideAPI.Handlers.EndRide as Handler
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as DriverStats
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Types.App (Driver)
import qualified Types.Storage.Person as SP
import qualified Types.Storage.Ride as Ride
import qualified Types.Storage.RideBooking as SRB
import Utils.Common (withFlowHandlerAPI)
import Utils.Metrics (putFareAndDistanceDeviations)

endRide :: Id SP.Person -> Id Ride.Ride -> FlowHandler APISuccess.APISuccess
endRide personId rideId = withFlowHandlerAPI $ do
  Handler.endRideHandler handle personId rideId
  where
    handle =
      Handler.ServiceHandle
        { findPersonById = Person.findPersonById,
          findRideBookingById = QRB.findById,
          findRideById = QRide.findById,
          findSearchRequestById = QSearchRequest.findById,
          findQuoteById = QQuote.findById,
          notifyCompleteToBAP = \quote rideBooking ride -> notifyUpdateToBAP quote rideBooking ride Mobility.COMPLETED,
          endRideTransaction,
          calculateFare = Fare.calculateFare,
          recalculateFareEnabled = asks (.recalculateFareEnabled),
          putDiffMetric = putFareAndDistanceDeviations
        }

endRideTransaction :: DBFlow m r => Id SRB.RideBooking -> Id Ride.Ride -> Id Driver -> Amount -> Double -> m ()
endRideTransaction rideBookingId rideId driverId actualPrice chargableDistance = DB.runSqlDBTransaction $ do
  QRide.updateActualPrice actualPrice rideId
  QRide.updateChargableDistance chargableDistance rideId
  QRide.updateStatus rideId Ride.COMPLETED
  QRB.updateStatus rideBookingId SRB.COMPLETED
  DriverInformation.updateOnRide driverId False
  DriverStats.updateIdleTime driverId
