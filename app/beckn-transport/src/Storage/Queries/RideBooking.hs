module Storage.Queries.RideBooking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Organization
import Domain.Types.Person
import Domain.Types.RideBooking as Booking
import Domain.Types.RideBooking.RentalRideBooking as Booking
import Domain.Types.RiderDetails (RiderDetails)
import Storage.Tabular.Ride as Ride
import Storage.Tabular.RideBooking as Booking
import Utils.Common

create :: RideBooking -> SqlDB ()
create rideBooking = do
  create' rideBooking
  case rideBooking.rideBookingDetails of
    Booking.OneWayDetails _ -> pure ()
    Booking.RentalDetails rentalDetails -> create' (mkRentalRideBooking rideBooking.id rentalDetails)

updateStatus :: Id RideBooking -> RideBookingStatus -> SqlDB ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  Esq.update' $ \tbl -> do
    set
      tbl
      [ RideBookingStatus =. val rbStatus,
        RideBookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideBookingTId ==. val (toKey rbId)

updateRiderId :: Id RideBooking -> Id RiderDetails -> SqlDB ()
updateRiderId rbId riderId = do
  now <- getCurrentTime
  Esq.update' $ \tbl -> do
    set
      tbl
      [ RideBookingRiderId =. val (Just $ toKey riderId),
        RideBookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideBookingTId ==. val (toKey rbId)

findById :: Transactionable m => Id RideBooking -> m (Maybe RideBooking)
findById = Esq.findById

findAllByOrg :: Transactionable m => Id Organization -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [RideBooking]
findAllByOrg orgId mbLimit mbOffset mbIsOnlyActive = do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbIsOnlyActive
  findAll $ do
    rideBooking <- from $ table @RideBookingT
    where_ $
      rideBooking ^. RideBookingProviderId ==. val (toKey orgId)
        &&. not_ (rideBooking ^. RideBookingStatus `in_` valList [Booking.CONFIRMED, Booking.AWAITING_REASSIGNMENT])
        &&. whenTrue_ isOnlyActive (not_ $ rideBooking ^. RideBookingStatus `in_` valList [Booking.COMPLETED, Booking.CANCELLED])
    orderBy [desc $ rideBooking ^. RideBookingCreatedAt]
    limit limitVal
    offset offsetVal
    return rideBooking

findAllByDriver :: Transactionable m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [RideBooking]
findAllByDriver driverId mbLimit mbOffset mbIsOnlyActive = do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbIsOnlyActive
  findAll $ do
    (rideBooking :& ride) <-
      from $
        table @RideBookingT
          `innerJoin` table @RideT
            `Esq.on` ( \(rideBooking :& ride) ->
                         ride ^. Ride.RideBookingId ==. rideBooking ^. Booking.RideBookingTId
                     )
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
        &&. whenTrue_ isOnlyActive (not_ $ rideBooking ^. RideBookingStatus `in_` valList [Booking.COMPLETED, Booking.CANCELLED])
    orderBy [desc $ rideBooking ^. RideBookingCreatedAt]
    limit limitVal
    offset offsetVal
    return rideBooking

increaseReallocationsCounter :: Id RideBooking -> SqlDB ()
increaseReallocationsCounter rbId = do
  now <- getCurrentTime
  Esq.update' $ \tbl -> do
    set
      tbl
      [ RideBookingReallocationsCount +=. val 1,
        RideBookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideBookingTId ==. val (toKey rbId)
