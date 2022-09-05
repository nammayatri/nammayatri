module Domain.Action.UI.Ride.EndRide.Internal (endRideTransaction) where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as Ride
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as DriverStats
import qualified Storage.Queries.Ride as QRide

endRideTransaction :: EsqDBFlow m r => Id SRB.Booking -> Ride.Ride -> Id DP.Driver -> m ()
endRideTransaction bookingId ride driverId = Esq.runTransaction $ do
  QRide.updateAll ride.id ride
  QRide.updateStatus ride.id Ride.COMPLETED
  QRB.updateStatus bookingId SRB.COMPLETED
  DriverInformation.updateOnRide driverId False
  DriverStats.updateIdleTime driverId
