{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnCancel (buildOnCancelReq) where

import qualified Beckn.Types.Core.Taxi.API.OnCancel as OnCancel
import qualified Beckn.Types.Core.Taxi.OnCancel as OnCancel
import qualified Domain.Action.Beckn.OnCancel as DOnCancel
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as DCancellationReason
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id (Id (Id))
import Kernel.Utils.Common

buildOnCancelReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text]
  ) =>
  OnCancel.OnCancelReq ->
  m (Maybe DOnCancel.OnCancelReq)
buildOnCancelReq req = do
  validateContext Context.ON_CANCEL req.context
  handleError req.contents $ \message ->
    return $
      DOnCancel.OnCancelReq
        { bppBookingId = Id message.order.id,
          bookingStatus = mapToDomainBookingStatus message.order.status,
          cancellationSource = maptoDomaincancellationSource message.order.cancellationSource,
          mbRideInfo = mkRideInfo <$> message.order.fulfillment
        }

mkRideInfo :: OnCancel.FulfillmentInfo -> DOnCancel.RideInfo
mkRideInfo fulfillment =
  DOnCancel.RideInfo
    { bppRideId = Id fulfillment.id,
      rideStatus = mapToDomainRideStatus fulfillment.status
    }

handleError ::
  (MonadFlow m) =>
  Either Error OnCancel.OnCancelMessage ->
  (OnCancel.OnCancelMessage -> m DOnCancel.OnCancelReq) ->
  m (Maybe DOnCancel.OnCancelReq)
handleError etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_cancel req" $ "on_cancel error: " <> show err
      pure Nothing

mapToDomainBookingStatus :: OnCancel.BookingStatus -> DBooking.BookingStatus
mapToDomainBookingStatus OnCancel.NEW_BOOKING = DBooking.NEW
mapToDomainBookingStatus OnCancel.TRIP_ASSIGNED = DBooking.TRIP_ASSIGNED
mapToDomainBookingStatus OnCancel.BOOKING_COMPLETED = DBooking.COMPLETED
mapToDomainBookingStatus OnCancel.BOOKING_CANCELLED = DBooking.CANCELLED

mapToDomainRideStatus :: OnCancel.RideStatus -> DRide.RideStatus
mapToDomainRideStatus OnCancel.NEW = DRide.NEW
mapToDomainRideStatus OnCancel.INPROGRESS = DRide.INPROGRESS
mapToDomainRideStatus OnCancel.COMPLETED = DRide.COMPLETED
mapToDomainRideStatus OnCancel.CANCELLED = DRide.CANCELLED

maptoDomaincancellationSource :: OnCancel.CancellationSource -> DCancellationReason.CancellationSource
maptoDomaincancellationSource OnCancel.CANCELLED_BY_USER = DCancellationReason.ByUser
maptoDomaincancellationSource OnCancel.CANCELLED_BY_DRIVER = DCancellationReason.ByDriver
maptoDomaincancellationSource OnCancel.CANCELLED_BY_MERCHANT = DCancellationReason.ByMerchant
maptoDomaincancellationSource OnCancel.CANCELLED_BY_ALLOCATOR = DCancellationReason.ByAllocator
maptoDomaincancellationSource OnCancel.CANCELLED_BY_APPLICATION = DCancellationReason.ByApplication
