module Domain.Action.UI.Ride.StartRide.Internal (startRideTransaction) where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.Ride as QRide

startRideTransaction :: EsqDBFlow m r => Id SRide.Ride -> Id SRB.Booking -> Id SP.Person -> LatLong -> m ()
startRideTransaction rideId bookingId driverId firstPoint = Esq.runTransaction $ do
  QRide.updateStatus rideId SRide.INPROGRESS
  QRide.updateStartTime rideId
  QBE.logRideCommencedEvent (cast driverId) bookingId rideId
  now <- getCurrentTime
  DrLoc.upsertGpsCoord driverId firstPoint now
