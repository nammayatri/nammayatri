{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Route where

import qualified Dashboard.Common as Common
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.RideStatus as Ride
import Environment
import Kernel.Beam.Functions
import Kernel.External.Maps (LatLong (..))
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Interface.Types
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Maps (getTripRoutes)

mkGetLocation :: ShortId DM.Merchant -> Id Common.Ride -> Double -> Double -> Bool -> Flow GetRoutesResp
mkGetLocation _ rideId pickupLocationLat pickupLocationLon isPickUpRoute = do
  ride <- runInReplica $ QRide.findById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.status == Ride.NEW || ride.status == Ride.INPROGRESS) $ throwError (RideInvalidStatus $ show ride.status)
  booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let mbToLocation = case booking.bookingDetails of
        DRB.RentalDetails _ -> Nothing
        DRB.OneWayDetails details -> Just details.toLocation
        DRB.DriverOfferDetails details -> Just details.toLocation
        DRB.OneWaySpecialZoneDetails details -> Just details.toLocation
        DRB.InterCityDetails details -> Just details.toLocation
        DRB.AmbulanceDetails details -> Just details.toLocation
        DRB.DeliveryDetails details -> Just details.toLocation
        DRB.MeterRideDetails details -> details.toLocation
      merchantOperatingCityId = booking.merchantOperatingCityId
      fromLocation = LatLong pickupLocationLat pickupLocationLon
  targetLocation <-
    bool
      (mbToLocation & fromMaybeM (InvalidRequest "Drop location does not exist for this ride"))
      (pure booking.fromLocation)
      isPickUpRoute
  let toLocation = LatLong targetLocation.lat targetLocation.lon
      listOfLatLong = [fromLocation, toLocation]
      waypointsList = NE.fromList listOfLatLong
      mkGetRoutesResp =
        Maps.GetRoutesReq
          { waypoints = waypointsList,
            mode = Just CAR,
            calcPoints = True
          }
  getTripRoutes booking.riderId booking.merchantId (Just merchantOperatingCityId) (Just rideId.getId) mkGetRoutesResp
