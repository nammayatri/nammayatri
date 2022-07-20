module Domain.Action.UI.Ride.EndRide.Internal where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as DriverStats
import qualified Storage.Queries.FarePolicy.FareBreakup as QFareBreakup
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import Types.App (Driver)

endRideTransaction :: EsqDBFlow m r => Id SRB.RideBooking -> SRide.Ride -> Id Driver -> [DFareBreakup.FareBreakup] -> m ()
endRideTransaction rideBookingId ride driverId fareBreakups = Esq.runTransaction $ do
  QRide.updateAll ride.id ride
  QRide.updateStatus ride.id SRide.COMPLETED
  QRB.updateStatus rideBookingId SRB.COMPLETED
  DriverInformation.updateOnRide driverId False
  DriverStats.updateIdleTime driverId
  traverse_ QFareBreakup.create fareBreakups
