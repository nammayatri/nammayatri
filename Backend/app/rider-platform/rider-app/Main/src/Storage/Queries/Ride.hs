{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Ride where

import Domain.Types.Booking.Type (Booking)
import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.Ride as Ride
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.Booking as Booking
import Storage.Tabular.Ride as Ride

create :: Ride -> SqlDB m ()
create = Esq.create

updateStatus ::
  Id Ride ->
  RideStatus ->
  SqlDB m ()
updateStatus rideId status_ = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideUpdatedAt =. val now,
        RideStatus =. val status_
      ]
    where_ $ tbl ^. RideId ==. val (getId rideId)

updateTrackingUrl ::
  Id Ride ->
  BaseUrl ->
  SqlDB m ()
updateTrackingUrl rideId url = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideUpdatedAt =. val now,
        RideTrackingUrl =. val (Just $ showBaseUrl url)
      ]
    where_ $ tbl ^. RideId ==. val (getId rideId)

updateRideRating ::
  Id Ride ->
  Int ->
  SqlDB m ()
updateRideRating rideId rideRating = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideUpdatedAt =. val now,
        RideRideRating =. val (Just rideRating)
      ]
    where_ $ tbl ^. RideId ==. val (getId rideId)

findById :: forall m ma. Transactionable ma m => Id Ride -> Proxy ma -> m (Maybe Ride)
findById rideId _ = Esq.findById @m @ma rideId

findByBPPRideId :: forall m ma. Transactionable ma m => Id BPPRide -> Proxy ma -> m (Maybe Ride)
findByBPPRideId bppRideId_ _ =
  findOne @m @ma $ do
    ride <- from $ table @RideT
    where_ $ ride ^. RideBppRideId ==. val (getId bppRideId_)
    return ride

updateMultiple :: Id Ride -> Ride -> SqlDB m ()
updateMultiple rideId ride = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideUpdatedAt =. val now,
        RideStatus =. val ride.status,
        RideFare =. val (realToFrac <$> ride.fare),
        RideTotalFare =. val (realToFrac <$> ride.totalFare),
        RideChargeableDistance =. val ride.chargeableDistance,
        RideRideStartTime =. val ride.rideStartTime,
        RideRideEndTime =. val ride.rideEndTime
      ]
    where_ $ tbl ^. RideId ==. val (getId rideId)

findActiveByRBId :: forall m ma. Transactionable ma m => Id Booking -> Proxy ma -> m (Maybe Ride)
findActiveByRBId rbId _ =
  findOne @m @ma $ do
    ride <- from $ table @RideT
    where_ $
      ride ^. RideBookingId ==. val (toKey rbId)
        &&. ride ^. RideStatus !=. val CANCELLED
    return ride

findAllByRBId :: forall m ma. Transactionable ma m => Id Booking -> Proxy ma -> m [Ride]
findAllByRBId bookingId _ =
  findAll @m @ma $ do
    ride <- from $ table @RideT
    where_ $ ride ^. RideBookingId ==. val (toKey bookingId)
    orderBy [desc $ ride ^. RideCreatedAt]
    return ride

updateDriverArrival :: Id Ride -> SqlDB m ()
updateDriverArrival rideId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideDriverArrivalTime =. val (Just now),
        RideUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideTId ==. val (toKey rideId)

upcoming6HrsCond :: SqlExpr (Entity RideT) -> UTCTime -> SqlExpr (Esq.Value Bool)
upcoming6HrsCond ride now = ride ^. RideCreatedAt +. Esq.interval [Esq.HOUR 6] <=. val now

data StuckRideItem = StuckRideItem
  { rideId :: Id Ride,
    bookingId :: Id Booking,
    riderId :: Id Person
  }

findStuckRideItems :: forall m ma. Transactionable ma m => Id Merchant -> [Id Booking] -> UTCTime -> Proxy ma -> m [StuckRideItem]
findStuckRideItems merchantId bookingIds now _ = do
  res <- Esq.findAll @m @ma $ do
    ride :& booking <-
      from $
        table @RideT
          `innerJoin` table @BookingT
            `Esq.on` ( \(ride :& booking) ->
                         ride ^. Ride.RideBookingId ==. booking ^. Booking.BookingTId
                     )
    where_ $
      booking ^. BookingMerchantId ==. val (toKey merchantId)
        &&. booking ^. BookingTId `in_` valList (toKey <$> bookingIds)
        &&. (ride ^. RideStatus ==. val Ride.NEW &&. upcoming6HrsCond ride now)
    pure (ride ^. RideTId, booking ^. BookingTId, booking ^. BookingRiderId)
  pure $ mkStuckRideItem <$> res
  where
    mkStuckRideItem (rideId, bookingId, riderId) = StuckRideItem {..}

cancelRides :: [Id Ride] -> UTCTime -> SqlDB m ()
cancelRides rideIds now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideStatus =. val CANCELLED,
        RideUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideTId `in_` valList (toKey <$> rideIds)
