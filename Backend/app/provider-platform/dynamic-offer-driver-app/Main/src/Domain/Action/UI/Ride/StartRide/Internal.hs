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
import qualified Domain.Types.Merchant as Dmerch
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Lib.SessionizerMetrics.Types.Event
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.Ride as QRide
import Tools.Event

startRideTransaction :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EventStreamFlow m r) => Id SP.Person -> SRide.Ride -> Id SRB.Booking -> LatLong -> Id Dmerch.Merchant -> m ()
startRideTransaction driverId ride bookingId firstPoint merchantId = do
  triggerRideStartEvent RideEventData {ride = ride{status = SRide.INPROGRESS}, personId = driverId, merchantId = merchantId}
  QRide.updateStatus ride.id SRide.INPROGRESS
  QRide.updateStartTimeAndLoc ride.id firstPoint
  QBE.logRideCommencedEvent (cast driverId) bookingId ride.id
  QDFS.updateStatus driverId DDFS.ON_RIDE {rideId = ride.id}
  now <- getCurrentTime
  void $ DrLoc.upsertGpsCoord driverId firstPoint now merchantId
