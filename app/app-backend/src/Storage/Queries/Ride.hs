module Storage.Queries.Ride where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Booking (Booking)
import Domain.Types.Ride
import Storage.Tabular.Ride

create :: Ride -> SqlDB ()
create = Esq.create

updateStatus ::
  Id Ride ->
  RideStatus ->
  SqlDB ()
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
  SqlDB ()
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
  SqlDB ()
updateRideRating rideId rideRating = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideUpdatedAt =. val now,
        RideRideRating =. val (Just rideRating)
      ]
    where_ $ tbl ^. RideId ==. val (getId rideId)

findById :: Transactionable m => Id Ride -> m (Maybe Ride)
findById = Esq.findById

findByBPPRideId :: Transactionable m => Id BPPRide -> m (Maybe Ride)
findByBPPRideId bppRideId_ =
  findOne $ do
    ride <- from $ table @RideT
    where_ $ ride ^. RideBppRideId ==. val (getId bppRideId_)
    return ride

updateMultiple :: Id Ride -> Ride -> SqlDB ()
updateMultiple rideId ride = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideUpdatedAt =. val now,
        RideStatus =. val ride.status,
        RideFare =. val ride.fare,
        RideTotalFare =. val ride.totalFare,
        RideChargeableDistance =. val ride.chargeableDistance,
        RideRideStartTime =. val ride.rideStartTime,
        RideRideEndTime =. val ride.rideEndTime
      ]
    where_ $ tbl ^. RideId ==. val (getId rideId)

findActiveByRBId :: Transactionable m => Id Booking -> m (Maybe Ride)
findActiveByRBId rbId =
  findOne $ do
    ride <- from $ table @RideT
    where_ $
      ride ^. RideBookingId ==. val (toKey rbId)
        &&. ride ^. RideStatus !=. val CANCELLED
    return ride

findAllByRBId :: EsqDBFlow m r => Id Booking -> m [Ride]
findAllByRBId bookingId =
  findAll $ do
    ride <- from $ table @RideT
    where_ $ ride ^. RideBookingId ==. val (toKey bookingId)
    orderBy [desc $ ride ^. RideCreatedAt]
    return ride
