{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.BulkLocUpdate where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Common as DC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude
import Kernel.External.Maps.Types
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.LocationUpdates
import qualified Lib.LocationUpdates as LocUpd
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Maps as TM

data BulkLocUpdateReq = BulkLocUpdateReq
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    loc :: NonEmpty LatLong
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

bulkLocUpdate :: BulkLocUpdateReq -> Flow APISuccess
bulkLocUpdate req = do
  let driverId = req.driverId
      rideId = req.rideId
      loc = req.loc
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchantId <- fromMaybeM (InternalError "Ride does not have a merchantId") $ ride.merchantId
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler merchantId ride.merchantOperatingCityId False
  rectificationServiceConfig <-
    if DC.shouldRectifyDistantPointsSnapToRoadFailure booking.tripCategory
      then Just <$> TM.getServiceConfigForRectifyingSnapToRoadDistantPointsFailure booking.providerId booking.merchantOperatingCityId
      else pure Nothing
  let isTollApplicableForServiceTier = DC.isTollApplicable booking.vehicleServiceTier
  _ <- addIntermediateRoutePoints defaultRideInterpolationHandler rectificationServiceConfig isTollApplicableForServiceTier rideId driverId loc
  pure Success
