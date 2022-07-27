module Storage.Queries.Booking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Booking as Booking
import Domain.Types.Organization
import Domain.Types.Person
import Domain.Types.RiderDetails (RiderDetails)
import Storage.Queries.FullEntityBuilders
import Storage.Tabular.Booking as Booking
import Storage.Tabular.Ride as Ride
import Utils.Common

create :: Booking -> SqlDB ()
create booking = do
  Esq.withFullEntity booking $ \(bookingT, bookingDetailsT) -> do
    Esq.create' bookingT
    case bookingDetailsT of
      Booking.OneWayDetailsT -> pure ()
      Booking.RentalDetailsT rentalDetails -> Esq.create' rentalDetails

updateStatus :: Id Booking -> BookingStatus -> SqlDB ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingStatus =. val rbStatus,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey rbId)

updateRiderId :: Id Booking -> Id RiderDetails -> SqlDB ()
updateRiderId rbId riderId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingRiderId =. val (Just $ toKey riderId),
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey rbId)

findById :: Transactionable m => Id Booking -> m (Maybe Booking)
findById bookingId = Esq.buildDType $ do
  booking <- Esq.findById' bookingId
  join <$> mapM buildFullBooking booking

findAllByOrg :: Transactionable m => Id Organization -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Booking]
findAllByOrg orgId mbLimit mbOffset mbIsOnlyActive = Esq.buildDType $ do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbIsOnlyActive
  bookingT <- Esq.findAll' $ do
    booking <- from $ table @BookingT
    where_ $
      booking ^. BookingProviderId ==. val (toKey orgId)
        &&. not_ (booking ^. BookingStatus `in_` valList [Booking.CONFIRMED, Booking.AWAITING_REASSIGNMENT])
        &&. whenTrue_ isOnlyActive (not_ $ booking ^. BookingStatus `in_` valList [Booking.COMPLETED, Booking.CANCELLED])
    orderBy [desc $ booking ^. BookingCreatedAt]
    limit limitVal
    offset offsetVal
    return booking
  catMaybes <$> mapM buildFullBooking bookingT

findAllByDriver :: Transactionable m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Booking]
findAllByDriver driverId mbLimit mbOffset mbIsOnlyActive = Esq.buildDType $ do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbIsOnlyActive
  bookingT <- Esq.findAll' $ do
    (booking :& ride) <-
      from $
        table @BookingT
          `innerJoin` table @RideT
            `Esq.on` ( \(booking :& ride) ->
                         ride ^. Ride.RideBookingId ==. booking ^. Booking.BookingTId
                     )
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
        &&. whenTrue_ isOnlyActive (not_ $ booking ^. BookingStatus `in_` valList [Booking.COMPLETED, Booking.CANCELLED])
    orderBy [desc $ booking ^. BookingCreatedAt]
    limit limitVal
    offset offsetVal
    return booking
  catMaybes <$> mapM buildFullBooking bookingT

increaseReallocationsCounter :: Id Booking -> SqlDB ()
increaseReallocationsCounter rbId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingReallocationsCount +=. val 1,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey rbId)
