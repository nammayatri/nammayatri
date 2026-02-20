{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.DriverCoordinates where

import Control.Lens ((^?), _head)
import Domain.Types.Ride
import Environment
import Kernel.Beam.Functions
import qualified Kernel.External.Maps.HasCoordinates as HC
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Prelude
import Kernel.Types.Common ()
import Kernel.Types.Id
import Kernel.Utils.Error
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import Tools.Error

getDriverCoordinates :: Id Ride -> Maybe Text -> Flow (Maybe Maps.LatLong)
getDriverCoordinates rideId apiKey = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  merchantId <- maybe ((runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)) <&> (.providerId)) return ride.merchantId
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"

  driverLocations <- LF.driversLocation [ride.driverId]
  pure $ fmap HC.getCoordinates (driverLocations ^? _head)
