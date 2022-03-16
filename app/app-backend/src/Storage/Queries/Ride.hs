module Storage.Queries.Ride where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Ride
import Domain.Types.RideBooking (RideBooking)
import Storage.Tabular.Ride

create :: Ride -> SqlDB ()
create = create'

updateStatus ::
  Id Ride ->
  RideStatus ->
  SqlDB ()
updateStatus rideId status_ = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ RideUpdatedAt =. val now,
        RideStatus =. val status_
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
  update' $ \tbl -> do
    set
      tbl
      [ RideUpdatedAt =. val now,
        RideStatus =. val ride.status,
        RideFare =. val ride.fare,
        RideTotalFare =. val ride.totalFare,
        RideChargeableDistance =. val ride.chargeableDistance
      ]
    where_ $ tbl ^. RideId ==. val (getId rideId)

findActiveByRBId :: Transactionable m => Id RideBooking -> m (Maybe Ride)
findActiveByRBId rbId =
  findOne $ do
    ride <- from $ table @RideT
    where_ $
      ride ^. RideBookingId ==. val (toKey rbId)
        &&. ride ^. RideStatus !=. val CANCELLED
    return ride

findAllByRBId :: EsqDBFlow m r => Id RideBooking -> m [Ride]
findAllByRBId rideBookingId =
  findAll $ do
    ride <- from $ table @RideT
    where_ $ ride ^. RideBookingId ==. val (toKey rideBookingId)
    orderBy [desc $ ride ^. RideCreatedAt]
    return ride
