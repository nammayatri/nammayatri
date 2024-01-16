{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnStatus (buildOnStatusReq, buildOnStatusReqV2) where

import qualified Beckn.Types.Core.Taxi.API.OnStatus as OnStatus
import qualified Beckn.Types.Core.Taxi.OnStatus as OnStatus
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
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
mapToDomainBookingStatus OnStatus.TRIP_ASSIGNED = DBooking.TRIP_ASSIGNED
mapToDomainBookingStatus OnStatus.BOOKING_COMPLETED = DBooking.COMPLETED
mapToDomainBookingStatus OnStatus.BOOKING_CANCELLED = DBooking.CANCELLED

mapToDomainRideStatus :: OnStatus.RideStatus -> DRide.RideStatus
mapToDomainRideStatus OnStatus.NEW = DRide.NEW
mapToDomainRideStatus OnStatus.INPROGRESS = DRide.INPROGRESS
mapToDomainRideStatus OnStatus.COMPLETED = DRide.COMPLETED
mapToDomainRideStatus OnStatus.CANCELLED = DRide.CANCELLED

buildOnStatusReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text]
  ) =>
  Spec.OnStatusReq ->
  m (Maybe DOnStatus.OnStatusReq)
buildOnStatusReqV2 req = do
  ContextV2.validateContext Context.ON_STATUS req.onStatusReqContext
  handleErrorV2 req $ \message ->
    case parseData message of
      Left err -> do
        logTagError "on_status req" $ "on_status error: " <> show err
        return Nothing
      Right (bppBookingId, bookingStatus, mbRideInfo) -> do
        return $
          Just $
            DOnStatus.OnStatusReq
              { bppBookingId,
                bookingStatus,
                mbRideInfo
              }
  where
    parseData :: Spec.ConfirmReqMessage -> Either Text (Id DBooking.BPPBooking, DBooking.BookingStatus, Maybe DOnStatus.RideInfo)
    parseData message = do
      let order = message.confirmReqMessageOrder

      bppBookingIdText <-
        order.orderId
          & maybe (Left "Invalid OrderId") Right
      let bppBookingId = Id bppBookingIdText

      bookingStatusText <-
        order.orderStatus
          & maybe (Left "Invalid OrderStatus") Right
      let bookingStatus = mapToDomainBookingStatusV2 bookingStatusText

      rideIdText <-
        order.orderFulfillments
          >>= listToMaybe
          >>= (.fulfillmentId)
          & maybe (Left "Invalid Fulfillment Id") Right

      let rideId = Id rideIdText

      rideStatusText <-
        order.orderFulfillments
          >>= listToMaybe
          >>= (.fulfillmentType)
          & maybe (Left "Invalid Fulfillment Type") Right

      let rideStatus = mapToDomainRideStatusV2 rideStatusText

      let mbRideInfo = Just $ DOnStatus.RideInfo rideId rideStatus

      Right (bppBookingId, bookingStatus, mbRideInfo)

handleErrorV2 ::
  (MonadFlow m) =>
  Spec.OnStatusReq ->
  (Spec.ConfirmReqMessage -> m (Maybe DOnStatus.OnStatusReq)) ->
  m (Maybe DOnStatus.OnStatusReq)
handleErrorV2 req action =
  case req.onStatusReqError of
    Nothing -> req.onStatusReqMessage & maybe (pure Nothing) action
    Just err -> do
      logTagError "on_status req" $ "on_status error: " <> show err
      pure Nothing

mapToDomainBookingStatusV2 :: Text -> DBooking.BookingStatus
mapToDomainBookingStatusV2 "NEW_BOOKING" = DBooking.NEW
mapToDomainBookingStatusV2 "TRIP_ASSIGNED" = DBooking.TRIP_ASSIGNED
mapToDomainBookingStatusV2 "BOOKING_COMPLETED" = DBooking.COMPLETED
mapToDomainBookingStatusV2 "BOOKING_CANCELLED" = DBooking.CANCELLED
mapToDomainBookingStatusV2 _ = DBooking.CANCELLED

mapToDomainRideStatusV2 :: Text -> DRide.RideStatus
mapToDomainRideStatusV2 "NEW" = DRide.NEW
mapToDomainRideStatusV2 "INPROGRESS" = DRide.INPROGRESS
mapToDomainRideStatusV2 "COMPLETED" = DRide.COMPLETED
mapToDomainRideStatusV2 "CANCELLED" = DRide.CANCELLED
mapToDomainRideStatusV2 _ = DRide.CANCELLED
