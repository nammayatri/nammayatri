module Domain.Action.UI.Ride.StartRide.Internal where

import Beckn.External.Maps.Types (LatLong)
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.Booking.Type as SRB
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.Ride as QRide

startRideTransaction :: EsqDBFlow m r => Id SRide.Ride -> Id SRB.Booking -> Id SP.Person -> LatLong -> m ()
startRideTransaction rId bookingId driverId firstPoint = Esq.runTransaction $ do
  QRide.updateStatus rId SRide.INPROGRESS
  QRide.updateStartTimeAndLoc rId firstPoint
  QBE.logRideCommencedEvent (cast driverId) bookingId rId
  now <- getCurrentTime
  DrLoc.upsertGpsCoord driverId firstPoint now
