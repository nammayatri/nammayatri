{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Route where

import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride as Common
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Booking.Type as DRB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as Ride
import Environment
import Kernel.External.Maps (LatLong (..))
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Interface.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runInReplica)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Maps (getTripRoutes)

mkGetLocation :: ShortId DM.Merchant -> Id Common.Ride -> Double -> Double -> Flow GetRoutesResp
mkGetLocation shortMerchantId rideId pickupLocationLat pickupLocationLon = do
  merchant <- QMerchant.findByShortId shortMerchantId >>= fromMaybeM (MerchantDoesNotExist shortMerchantId.getShortId)
  ride <- runInReplica $ QRide.findById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.status == Ride.NEW || ride.status == Ride.INPROGRESS) $ throwError (RideInvalidStatus $ show ride.status)
  booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let bookingLocation = case booking.bookingDetails of
        DRB.RentalDetails _ -> []
        DRB.OneWayDetails details -> details.toLocation
        DRB.DriverOfferDetails details -> details.toLocation
        DRB.OneWaySpecialZoneDetails details -> details.toLocation
  destination <- case lastMaybe bookingLocation of
    Just toLoc -> return toLoc
    Nothing -> throwError $ InternalError "To location not found."
  let fromLocation = LatLong pickupLocationLat pickupLocationLon
  let toLocation = LatLong destination.lat destination.lon
  let listOfLatLong = [fromLocation, toLocation]
  let waypointsList = NE.fromList listOfLatLong
  let mkGetRoutesResp =
        Maps.GetRoutesReq
          { waypoints = waypointsList,
            mode = Just CAR,
            calcPoints = True
          }
  getTripRoutes merchant.id mkGetRoutesResp
