{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Route where

import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Booking.Type as DRB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
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

newtype TripRouteReq = TripRouteReq
  { pickupLocation :: Maps.LatLong
  }
  deriving (Show, ToJSON, FromJSON, Generic, ToSchema)

mkGetLocation :: ShortId DM.Merchant -> Id DRide.Ride -> TripRouteReq -> Flow GetRoutesResp
mkGetLocation shortMerchantId rideId req = do
  merchant <-
    QMerchant.findByShortId shortMerchantId
      >>= fromMaybeM (MerchantDoesNotExist shortMerchantId.getShortId)
  ride <- runInReplica $ QRide.findById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let mbToLocation = case booking.bookingDetails of
        DRB.OneWayDetails details -> Just details.toLocation
        DRB.RentalDetails _ -> Nothing
        _ -> Nothing
  let bookingLocation = fromJust mbToLocation
  let fromLocation = LatLong bookingLocation.lat bookingLocation.lon
  let toLocation = LatLong req.pickupLocation.lat req.pickupLocation.lon
  let listofLatLong = [fromLocation, toLocation]
  let ne = NE.fromList listofLatLong

  let mkGetRoutesResp =
        Maps.GetRoutesReq
          { waypoints = ne,
            mode = Just CAR,
            calcPoints = True
          }
  getTripRoutes merchant.id mkGetRoutesResp
