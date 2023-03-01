{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.StartRide.Internal (startRideTransaction) where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import qualified SharedLogic.Ride as CQRide
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.Ride as QRide

startRideTransaction :: forall m r. (CacheFlow m r, EsqDBFlow m r) => Id SP.Person -> Id SRide.Ride -> Id SRB.Booking -> LatLong -> m ()
startRideTransaction driverId rideId bookingId firstPoint = do
  Esq.runTransaction $ do
    QRide.updateStatus @m rideId SRide.INPROGRESS
    QRide.updateStartTimeAndLoc rideId firstPoint
    QBE.logRideCommencedEvent (cast driverId) bookingId rideId
    QDFS.updateStatus driverId DDFS.ON_RIDE {rideId}
    now <- getCurrentTime
    void $ DrLoc.upsertGpsCoord driverId firstPoint now
  CQRide.clearCache driverId
