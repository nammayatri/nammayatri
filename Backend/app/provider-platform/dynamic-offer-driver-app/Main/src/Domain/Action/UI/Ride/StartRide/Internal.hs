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
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, throwError)
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Event

startRideTransaction :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EventStreamFlow m r, LT.HasLocationService m r) => Id SP.Person -> SRide.Ride -> SRB.Booking -> LatLong -> Maybe Centesimal -> m ()
startRideTransaction driverId ride booking firstPoint odometerStartReading = do
  when (booking.bookingType == SRB.RentalBooking) $ do
    unless (isJust odometerStartReading) $ throwError $ InternalError "No odometer start reading found"
    void $ QRide.updateOdometerStartReading ride.id odometerStartReading
  triggerRideStartEvent RideEventData {ride = ride{status = SRide.INPROGRESS}, personId = driverId, merchantId = booking.providerId}
  void $ LF.rideStart ride.id firstPoint.lat firstPoint.lon booking.providerId driverId
  QRide.updateStatus ride.id SRide.INPROGRESS
  QRide.updateStartTimeAndLoc ride.id firstPoint
  QBE.logRideCommencedEvent (cast driverId) booking.id ride.id
  QDFS.updateStatus driverId DDFS.ON_RIDE {rideId = ride.id}
