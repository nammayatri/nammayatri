{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Ride where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Ride as Common
import Domain.Types.Booking.Type as Booking
import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.Ride as Ride
import Domain.Types.RideDetails as RideDetails
import Domain.Types.RiderDetails as RiderDetails
import Kernel.External.Encryption
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Queries.Booking
import Storage.Queries.FullEntityBuilders
import Storage.Tabular.Booking as Booking
import Storage.Tabular.Rating as Rating
import Storage.Tabular.Ride as Ride
import Storage.Tabular.RideDetails as RideDetails
import Storage.Tabular.RiderDetails as RiderDetails

create :: forall m. Monad m => Ride -> SqlDB m ()
create dRide = Esq.runTransaction $
  withFullEntity dRide $ \(sRide, _mbSRating) -> do
    Esq.create' @RideT @m sRide

fullRideTable ::
  From
    ( Table RideT
        :& MbTable RatingT
    )
fullRideTable =
  table @RideT
    `leftJoin` table @RatingT
      `Esq.on` ( \(ride :& mbRating) ->
                   just (ride ^. Ride.RideTId) ==. mbRating ?. Rating.RatingRideId
               )

findById :: forall m ma. Transactionable ma m => Id Ride -> Proxy ma -> m (Maybe Ride)
findById rideId _ = Esq.buildDType $ do
  mbFullRideT <- Esq.findOne' @m @ma $ do
    (ride :& mbRating) <- from fullRideTable
    where_ $ ride ^. RideTId ==. val (toKey rideId)
    pure (ride, mbRating)
  pure $ extractSolidType @Ride <$> mbFullRideT

findActiveByRBId :: forall m ma. Transactionable ma m => Id Booking -> Proxy ma -> m (Maybe Ride)
findActiveByRBId rbId _ = Esq.buildDType $ do
  mbFullRideT <- Esq.findOne' @m @ma $ do
    (ride :& mbRating) <- from fullRideTable
    where_ $
      ride ^. Ride.RideBookingId ==. val (toKey rbId)
        &&. ride ^. RideStatus !=. val Ride.CANCELLED
    pure (ride, mbRating)
  pure $ extractSolidType @Ride <$> mbFullRideT

findAllCancelledByRBId :: forall m ma. Transactionable ma m => Id Booking -> Proxy ma -> m [Ride]
findAllCancelledByRBId bookingId _ = Esq.buildDType $ do
  fullRidesT <- Esq.findAll' @m @ma $ do
    (ride :& mbRating) <- from fullRideTable
    where_ $
      ride ^. Ride.RideBookingId ==. val (toKey bookingId)
        &&. ride ^. RideStatus ==. val Ride.CANCELLED
    pure (ride, mbRating)
  pure $ extractSolidType @Ride <$> fullRidesT

findAllByDriverId ::
  forall m ma.
  Transactionable ma m =>
  Id Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  Proxy ma ->
  m [(Ride, Booking)]
findAllByDriverId driverId mbLimit mbOffset mbOnlyActive proxy = Esq.buildDType $ do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbOnlyActive
  res <- Esq.findAll' @m @ma $ do
    ((ride :& mbRating) :& (booking :& fromLoc :& mbOneWayBooking :& mbToLoc :& mbRentalBooking)) <-
      from $
        fullRideTable
          `innerJoin` fullBookingTable
            `Esq.on` ( \((ride :& _) :& (booking :& _ :& _ :& _ :& _)) ->
                         ride ^. Ride.RideBookingId ==. booking ^. Booking.BookingTId
                     )
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
        &&. whenTrue_ isOnlyActive (not_ $ ride ^. RideStatus `in_` valList [Ride.COMPLETED, Ride.CANCELLED])
    orderBy [desc $ ride ^. RideCreatedAt]
    limit limitVal
    offset offsetVal
    return ((ride, mbRating), (booking, fromLoc, mbOneWayBooking, mbToLoc, mbRentalBooking))
  fmap catMaybes $
    for res $ \(fullRideT, fullBookingT) -> do
      mbFullBooking <- buildFullBooking fullBookingT proxy
      let ride = extractSolidType @Ride fullRideT
      return $ mbFullBooking <&> (ride,)

findOneByDriverId :: forall m ma. Transactionable ma m => Id Person -> Proxy ma -> m (Maybe Ride)
findOneByDriverId driverId _ = Esq.buildDType $ do
  mbFullRideT <- Esq.findOne' @m @ma $ do
    (ride :& mbRating) <- from fullRideTable
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
    limit 1
    pure (ride, mbRating)
  pure $ extractSolidType @Ride <$> mbFullRideT

findAllRideAPIEntityDataByRBId :: forall m ma. Transactionable ma m => Id Booking -> Proxy ma -> m [(Ride, RideDetails)]
findAllRideAPIEntityDataByRBId rbId _ = Esq.buildDType $ do
  res <- Esq.findAll' @m @ma $ do
    ((ride :& mbRating) :& rideDetails) <-
      from $
        fullRideTable
          `innerJoin` table @RideDetailsT
            `Esq.on` ( \((ride :& _) :& rideDetails) ->
                         (ride ^. Ride.RideTId) ==. rideDetails ^. RideDetails.RideDetailsId
                     )
    where_ $
      ride ^. Ride.RideBookingId ==. val (toKey rbId)
    orderBy [desc $ ride ^. RideCreatedAt]
    return ((ride, mbRating), rideDetails)
  return $
    res <&> \(fullRideT, rideDetails :: RideDetailsT) ->
      (extractSolidType @Ride fullRideT, extractSolidType @RideDetails rideDetails)

getInProgressByDriverId :: forall m ma. Transactionable ma m => Id Person -> Proxy ma -> m (Maybe Ride)
getInProgressByDriverId driverId _ = Esq.buildDType $ do
  mbFullRideT <- Esq.findOne' @m @ma $ do
    (ride :& mbRating) <- from fullRideTable
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
        &&. ride ^. RideStatus ==. val Ride.INPROGRESS
    pure (ride, mbRating)
  pure $ extractSolidType @Ride <$> mbFullRideT

getInProgressRideIdByDriverId :: forall m ma. Transactionable ma m => Id Person -> Proxy ma -> m (Maybe (Id Ride))
getInProgressRideIdByDriverId driverId _ = do
  id <- Esq.findOne @m @ma $ do
    ride <- from $ table @RideT
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
        &&. ride ^. RideStatus ==. val Ride.INPROGRESS
    pure $ ride ^. RideId
  pure $ Id <$> id

getActiveByDriverId :: forall m ma. Transactionable ma m => Id Person -> Proxy ma -> m (Maybe Ride)
getActiveByDriverId driverId _ = Esq.buildDType $ do
  mbFullRideT <- Esq.findOne' @m @ma $ do
    (ride :& mbRating) <- from fullRideTable
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
        &&. ( ride ^. RideStatus ==. val Ride.INPROGRESS
                ||. ride ^. RideStatus ==. val Ride.NEW
            )
    pure (ride, mbRating)
  pure $ extractSolidType @Ride <$> mbFullRideT

updateStatus ::
  Id Ride ->
  RideStatus ->
  SqlDB m ()
updateStatus rideId status = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideStatus =. val status,
        RideUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideTId ==. val (toKey rideId)

updateStartTimeAndLoc ::
  Id Ride ->
  LatLong ->
  SqlDB m ()
updateStartTimeAndLoc rideId point = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideTripStartTime =. val (Just now),
        RideTripStartLat =. val (Just point.lat),
        RideTripStartLon =. val (Just point.lon),
        RideUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideTId ==. val (toKey rideId)

updateStatusByIds ::
  [Id Ride] ->
  RideStatus ->
  SqlDB m ()
updateStatusByIds ids status = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideStatus =. val status,
        RideUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideTId `in_` valList (toKey <$> ids)

updateDistance ::
  Id Person ->
  HighPrecMeters ->
  SqlDB m ()
updateDistance driverId distance = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideTraveledDistance +=. val distance,
        RideUpdatedAt =. val now
      ]
    where_ $
      tbl ^. RideDriverId ==. val (toKey driverId)
        &&. tbl ^. RideStatus ==. val Ride.INPROGRESS

updateAll ::
  Id Ride ->
  Ride ->
  SqlDB m ()
updateAll rideId ride = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideFare =. val (fromIntegral <$> ride.fare),
        RideTotalFare =. val (fromIntegral <$> ride.totalFare),
        RideChargeableDistance =. val (fromIntegral <$> ride.chargeableDistance),
        RideTripEndTime =. val ride.tripEndTime,
        RideTripEndLat =. val (ride.tripEndPos <&> (.lat)),
        RideTripEndLon =. val (ride.tripEndPos <&> (.lon)),
        RideUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideTId ==. val (toKey rideId)

getCountByStatus :: forall m ma. Transactionable ma m => Id Merchant -> Proxy ma -> m [(RideStatus, Int)]
getCountByStatus merchantId _ = do
  Esq.findAll @m @ma $ do
    (ride :& booking) <-
      from $
        table @RideT
          `innerJoin` table @BookingT
            `Esq.on` ( \(ride :& booking) ->
                         ride ^. Ride.RideBookingId ==. booking ^. Booking.BookingTId
                     )
    where_ $ booking ^. BookingProviderId ==. val (toKey merchantId)
    groupBy $ ride ^. RideStatus
    return (ride ^. RideStatus, countRows :: SqlExpr (Esq.Value Int))

updateArrival :: Id Ride -> SqlDB m ()
updateArrival rideId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideDriverArrivalTime =. val (Just now),
        RideUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideTId ==. val (toKey rideId)

countRides :: forall m ma. Transactionable ma m => Id Merchant -> Proxy ma -> m Int
countRides merchantId _ =
  mkCount <$> do
    Esq.findAll @m @ma $ do
      (_ride :& booking) <-
        from $
          table @RideT
            `innerJoin` table @BookingT
              `Esq.on` ( \(ride :& booking) ->
                           ride ^. Ride.RideBookingId ==. booking ^. Booking.BookingTId
                       )
      where_ $ booking ^. BookingProviderId ==. val (toKey merchantId)
      return (countRows :: SqlExpr (Esq.Value Int))
  where
    mkCount [counter] = counter
    mkCount _ = 0

data RideItem = RideItem
  { rideShortId :: ShortId Ride,
    rideDetails :: RideDetails,
    riderDetails :: RiderDetails,
    customerName :: Maybe Text,
    fareDiff :: Maybe Money,
    bookingStatus :: Common.BookingStatus
  }

findAllRideItems ::
  forall m ma.
  Transactionable ma m =>
  Id Merchant ->
  Int ->
  Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Ride) ->
  Maybe DbHash ->
  Maybe DbHash ->
  Maybe Money ->
  UTCTime ->
  Proxy ma ->
  m [RideItem]
findAllRideItems merchantId limitVal offsetVal mbBookingStatus mbRideShortId mbCustomerPhoneDBHash mbDriverPhoneDBHash mbFareDiff now _ = do
  res <- Esq.findAll @m @ma $ do
    booking :& ride :& rideDetails :& riderDetails <-
      from $
        table @BookingT
          `innerJoin` table @RideT
            `Esq.on` ( \(booking :& ride) ->
                         ride ^. Ride.RideBookingId ==. booking ^. Booking.BookingTId
                     )
          `innerJoin` table @RideDetailsT
            `Esq.on` ( \(_ :& ride :& rideDetails) ->
                         ride ^. Ride.RideTId ==. rideDetails ^. RideDetails.RideDetailsId
                     )
          `innerJoin` table @RiderDetailsT
            `Esq.on` ( \(booking :& _ :& _ :& riderDetails) ->
                         booking ^. Booking.BookingRiderId ==. just (riderDetails ^. RiderDetails.RiderDetailsTId)
                     )
    let bookingStatusVal = mkBookingStatusVal ride
    where_ $
      booking ^. BookingProviderId ==. val (toKey merchantId)
        &&. whenJust_ mbBookingStatus (\bookingStatus -> bookingStatusVal ==. val bookingStatus)
        &&. whenJust_ mbRideShortId (\rideShortId -> ride ^. Ride.RideShortId ==. val rideShortId.getShortId)
        &&. whenJust_ mbDriverPhoneDBHash (\hash -> rideDetails ^. RideDetailsDriverNumberHash ==. val (Just hash))
        &&. whenJust_ mbCustomerPhoneDBHash (\hash -> riderDetails ^. RiderDetailsMobileNumberHash ==. val hash)
        &&. whenJust_ mbFareDiff (\fareDiff_ -> (castNumM (ride ^. RideTotalFare) -. castNumM (just (booking ^. BookingEstimatedTotalFare)) >. val (Just fareDiff_)) ||. (castNumM (just (booking ^. BookingEstimatedTotalFare)) -. castNumM (ride ^. RideTotalFare) >. val (Just fareDiff_)))
    orderBy [desc $ ride ^. RideCreatedAt]
    limit $ fromIntegral limitVal
    offset $ fromIntegral offsetVal
    return
      ( ride ^. RideShortId,
        rideDetails,
        riderDetails,
        booking ^. BookingRiderName,
        ride ^. RideTotalFare -. just (booking ^. BookingEstimatedTotalFare),
        bookingStatusVal
      )
  pure $ mkRideItem <$> res
  where
    mkBookingStatusVal ride = do
      -- ride considered as ONGOING_6HRS if ride.status = INPROGRESS, but somehow ride.tripStartTime = Nothing
      let ongoing6HrsCond =
            ride ^. Ride.RideTripStartTime +. just (Esq.interval [Esq.HOUR 6]) <=. val (Just now)
      case_
        [ when_ (ride ^. Ride.RideStatus ==. val Ride.NEW &&. not_ (upcoming6HrsCond ride now)) then_ $ val Common.UPCOMING,
          when_ (ride ^. Ride.RideStatus ==. val Ride.NEW &&. upcoming6HrsCond ride now) then_ $ val Common.UPCOMING_6HRS,
          when_ (ride ^. Ride.RideStatus ==. val Ride.INPROGRESS &&. not_ ongoing6HrsCond) then_ $ val Common.ONGOING,
          when_ (ride ^. Ride.RideStatus ==. val Ride.COMPLETED) then_ $ val Common.COMPLETED,
          when_ (ride ^. Ride.RideStatus ==. val Ride.CANCELLED) then_ $ val Common.CANCELLED
        ]
        (else_ $ val Common.ONGOING_6HRS)

mkRideItem :: (Text, RideDetails, RiderDetails, Maybe Text, Maybe HighPrecMoney, Common.BookingStatus) -> RideItem
mkRideItem (rideShortId, rideDetails, riderDetails, customerName, mbFareDiff_, bookingStatus) = do
  RideItem {rideShortId = ShortId rideShortId, fareDiff = roundToIntegral <$> mbFareDiff_, ..}

upcoming6HrsCond :: SqlExpr (Entity RideT) -> UTCTime -> SqlExpr (Esq.Value Bool)
upcoming6HrsCond ride now = ride ^. Ride.RideCreatedAt +. Esq.interval [Esq.HOUR 6] <=. val now

data StuckRideItem = StuckRideItem
  { rideId :: Id Ride,
    bookingId :: Id Booking,
    driverId :: Id Person
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
      booking ^. BookingProviderId ==. val (toKey merchantId)
        &&. booking ^. BookingTId `in_` valList (toKey <$> bookingIds)
        &&. (ride ^. Ride.RideStatus ==. val Ride.NEW &&. upcoming6HrsCond ride now)
    pure (ride ^. RideTId, booking ^. BookingTId, ride ^. RideDriverId)
  pure $ mkStuckRideItem <$> res
  where
    mkStuckRideItem (rideId, bookingId, driverId) = StuckRideItem {..}
