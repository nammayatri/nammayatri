{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Ride where

import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride as Common
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import SharedLogic.Types.Booking.Type (Booking)
import SharedLogic.Types.Merchant
import SharedLogic.Types.Person
import SharedLogic.Types.Ride as Ride
import Storage.Tabular.Booking as Booking
import Storage.Tabular.Person as Person
import Storage.Tabular.Ride as Ride

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
        RideFare =. val (realToFrac <$> ride.fare),
        RideTotalFare =. val (realToFrac <$> ride.totalFare),
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

findAllByRBId :: Transactionable m => Id Booking -> m [Ride]
findAllByRBId bookingId =
  findAll $ do
    ride <- from $ table @RideT
    where_ $ ride ^. RideBookingId ==. val (toKey bookingId)
    orderBy [desc $ ride ^. RideCreatedAt]
    return ride

updateDriverArrival :: Id Ride -> SqlDB ()
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

findStuckRideItems :: Transactionable m => Id Merchant -> [Id Booking] -> UTCTime -> m [StuckRideItem]
findStuckRideItems merchantId bookingIds now = do
  res <- Esq.findAll $ do
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

cancelRides :: [Id Ride] -> UTCTime -> SqlDB ()
cancelRides rideIds now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideStatus =. val CANCELLED,
        RideUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideTId `in_` valList (toKey <$> rideIds)

data RideItem = RideItem
  { person :: Person,
    ride :: Ride,
    bookingStatus :: Common.BookingStatus
  }

findAllRideItems ::
  Transactionable m =>
  Id Merchant ->
  Int ->
  Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Ride) ->
  Maybe DbHash ->
  Maybe Text ->
  UTCTime ->
  m [RideItem]
findAllRideItems merchantId limitVal offsetVal mbBookingStatus mbRideShortId mbCustomerPhoneDBHash mbDriverPhone now = do
  res <- Esq.findAll $ do
    booking :& ride :& person <-
      from $
        table @BookingT
          `innerJoin` table @RideT
            `Esq.on` ( \(booking :& ride) ->
                         ride ^. Ride.RideBookingId ==. booking ^. Booking.BookingTId
                     )
          `innerJoin` table @PersonT
            `Esq.on` ( \(booking :& _ :& person) ->
                         booking ^. Booking.BookingRiderId ==. person ^. Person.PersonTId
                     )
    let bookingStatusVal = mkBookingStatusVal ride
    where_ $
      booking ^. BookingMerchantId ==. val (toKey merchantId)
        &&. whenJust_ mbBookingStatus (\bookingStatus -> bookingStatusVal ==. val bookingStatus)
        &&. whenJust_ mbRideShortId (\rideShortId -> ride ^. Ride.RideShortId ==. val rideShortId.getShortId)
        &&. whenJust_ mbCustomerPhoneDBHash (\hash -> person ^. Person.PersonMobileNumberHash ==. val (Just hash))
        &&. whenJust_ mbDriverPhone (\driverMobileNumber -> ride ^. Ride.RideDriverMobileNumber ==. val driverMobileNumber)
    limit $ fromIntegral limitVal
    offset $ fromIntegral offsetVal
    return
      ( person,
        ride,
        bookingStatusVal
      )
  pure $ mkRideItem <$> res
  where
    mkBookingStatusVal ride = do
      -- ride considered as ONGOING_6HRS if ride.status = INPROGRESS, but somehow ride.rideStartTime = Nothing
      let ongoing6HrsCond =
            ride ^. Ride.RideRideStartTime +. just (Esq.interval [Esq.HOUR 6]) <=. val (Just now)
      case_
        [ when_ (ride ^. Ride.RideStatus ==. val Ride.NEW &&. not_ (upcoming6HrsCond ride now)) then_ $ val Common.UPCOMING,
          when_ (ride ^. Ride.RideStatus ==. val Ride.NEW &&. upcoming6HrsCond ride now) then_ $ val Common.UPCOMING_6HRS,
          when_ (ride ^. Ride.RideStatus ==. val Ride.INPROGRESS &&. not_ ongoing6HrsCond) then_ $ val Common.ONGOING,
          when_ (ride ^. Ride.RideStatus ==. val Ride.COMPLETED) then_ $ val Common.RCOMPLETED,
          when_ (ride ^. Ride.RideStatus ==. val Ride.CANCELLED) then_ $ val Common.RCANCELLED
        ]
        (else_ $ val Common.ONGOING_6HRS)
    mkRideItem (person, ride, bookingStatus) = do
      RideItem {..}

-- countRides :: Transactionable m => Id Merchant -> m Int
-- countRides merchantId =
--   mkCount <$> do
--     Esq.findAll $ do
--       (_ride :& booking) <-
--         from $
--           table @RideT
--             `innerJoin` table @BookingT
--               `Esq.on` ( \(ride :& booking) ->
--                            ride ^. Ride.RideBookingId ==. booking ^. Booking.BookingTId
--                        )
--       where_ $ booking ^. BookingMerchantId ==. val (toKey merchantId)
--       return (countRows :: SqlExpr (Esq.Value Int))
--   where
--     mkCount [counter] = counter
--     mkCount _ = 0
