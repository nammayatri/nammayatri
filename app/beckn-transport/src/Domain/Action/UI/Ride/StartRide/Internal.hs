module Domain.Action.UI.Ride.StartRide.Internal where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.Ride as QRide

startRideTransaction :: EsqDBFlow m r => Id SRide.Ride -> Id SRB.RideBooking -> Id SP.Person -> m ()
startRideTransaction rId rideBookingId driverId = Esq.runTransaction $ do
  QRide.updateStatus rId SRide.INPROGRESS
  QRide.updateStartTime rId
  QBE.logRideCommencedEvent (cast driverId) rideBookingId rId