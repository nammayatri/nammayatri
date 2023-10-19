{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnStatus (buildOnStatusReq) where

import qualified Beckn.Types.Core.Taxi.API.OnStatus as OnStatus
import qualified Beckn.Types.Core.Taxi.OnStatus as OnStatus
import qualified Domain.Action.Beckn.OnStatus as DOnStatus
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id (Id (Id))
import Kernel.Utils.Common

buildOnStatusReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text]
  ) =>
  OnStatus.OnStatusReq ->
  m (Maybe DOnStatus.OnStatusReq)
buildOnStatusReq req = do
  validateContext Context.ON_STATUS req.context
  handleError req.contents $ \message ->
    return $
      DOnStatus.OnStatusReq
        { bppBookingId = Id message.order.id,
          bookingStatus = mapToDomainBookingStatus message.order.status,
          mbRideInfo = mkRideInfo <$> message.order.fulfillment
        }

mkRideInfo :: OnStatus.FulfillmentInfo -> DOnStatus.RideInfo
mkRideInfo fulfillment =
  DOnStatus.RideInfo
    { bppRideId = Id fulfillment.id,
      rideStatus = mapToDomainRideStatus fulfillment.status
    }

handleError ::
  (MonadFlow m) =>
  Either Error OnStatus.OnStatusMessage ->
  (OnStatus.OnStatusMessage -> m DOnStatus.OnStatusReq) ->
  m (Maybe DOnStatus.OnStatusReq)
handleError etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_status req" $ "on_status error: " <> show err
      pure Nothing

mapToDomainBookingStatus :: OnStatus.BookingStatus -> DBooking.BookingStatus
mapToDomainBookingStatus OnStatus.NEW_BOOKING = DBooking.NEW
mapToDomainBookingStatus OnStatus.BOOKING_CONFIRMED = DBooking.CONFIRMED
mapToDomainBookingStatus OnStatus.TRIP_ASSIGNED = DBooking.TRIP_ASSIGNED
mapToDomainBookingStatus OnStatus.BOOKING_COMPLETED = DBooking.COMPLETED
mapToDomainBookingStatus OnStatus.BOOKING_CANCELLED = DBooking.CANCELLED

mapToDomainRideStatus :: OnStatus.RideStatus -> DRide.RideStatus
mapToDomainRideStatus OnStatus.NEW = DRide.NEW
mapToDomainRideStatus OnStatus.INPROGRESS = DRide.INPROGRESS
mapToDomainRideStatus OnStatus.COMPLETED = DRide.COMPLETED
mapToDomainRideStatus OnStatus.CANCELLED = DRide.CANCELLED
