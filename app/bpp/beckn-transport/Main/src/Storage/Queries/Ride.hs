{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Ride where

import Beckn.External.Encryption
import Beckn.External.Maps.Types (LatLong)
import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Ride as Common
import Domain.Types.Booking.Type as Booking
import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.Ride as Ride
import Domain.Types.RideDetails as RideDetails
import Domain.Types.RiderDetails as RiderDetails
import Storage.Queries.Booking
import Storage.Queries.FullEntityBuilders
import Storage.Tabular.Booking as Booking
import Storage.Tabular.Rating as Rating
import Storage.Tabular.Ride as Ride
import Storage.Tabular.RideDetails as RideDetails
import Storage.Tabular.RiderDetails as RiderDetails

create :: Ride -> SqlDB ()
create dRide = Esq.runTransaction $
  withFullEntity dRide $ \(sRide, _mbSRating) -> do
    Esq.create' @RideT sRide

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

findById :: Transactionable m => Id Ride -> m (Maybe Ride)
findById rideId = Esq.buildDType $ do
  mbFullRideT <- Esq.findOne' $ do
    (ride :& mbRating) <- from fullRideTable
    where_ $ ride ^. RideTId ==. val (toKey rideId)
    pure (ride, mbRating)
  pure $ extractSolidType @Ride <$> mbFullRideT

findActiveByRBId :: Transactionable m => Id Booking -> m (Maybe Ride)
findActiveByRBId rbId = Esq.buildDType $ do
  mbFullRideT <- Esq.findOne' $ do
    (ride :& mbRating) <- from fullRideTable
    where_ $
      ride ^. Ride.RideBookingId ==. val (toKey rbId)
        &&. ride ^. RideStatus !=. val Ride.CANCELLED
    pure (ride, mbRating)
  pure $ extractSolidType @Ride <$> mbFullRideT

findAllCancelledByRBId :: Transactionable m => Id Booking -> m [Ride]
findAllCancelledByRBId bookingId = Esq.buildDType $ do
  fullRidesT <- Esq.findAll' $ do
    (ride :& mbRating) <- from fullRideTable
    where_ $
      ride ^. Ride.RideBookingId ==. val (toKey bookingId)
        &&. ride ^. RideStatus ==. val Ride.CANCELLED
    pure (ride, mbRating)
  pure $ extractSolidType @Ride <$> fullRidesT

findAllByDriverId ::
  Transactionable m =>
  Id Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  m [(Ride, Booking)]
findAllByDriverId driverId mbLimit mbOffset mbOnlyActive = Esq.buildDType $ do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbOnlyActive
  res <- Esq.findAll' $ do
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
      mbFullBooking <- buildFullBooking fullBookingT
      let ride = extractSolidType @Ride fullRideT
      return $ mbFullBooking <&> (ride,)

findOneByDriverId :: Transactionable m => Id Person -> m (Maybe Ride)
findOneByDriverId driverId = Esq.buildDType $ do
  mbFullRideT <- Esq.findOne' $ do
    (ride :& mbRating) <- from fullRideTable
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
    limit 1
    pure (ride, mbRating)
  pure $ extractSolidType @Ride <$> mbFullRideT

findAllRideAPIEntityDataByRBId :: Transactionable m => Id Booking -> m [(Ride, RideDetails)]
findAllRideAPIEntityDataByRBId rbId = Esq.buildDType $ do
  res <- Esq.findAll' $ do
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

getInProgressByDriverId :: Transactionable m => Id Person -> m (Maybe Ride)
getInProgressByDriverId driverId = Esq.buildDType $ do
  mbFullRideT <- Esq.findOne' $ do
    (ride :& mbRating) <- from fullRideTable
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
        &&. ride ^. RideStatus ==. val Ride.INPROGRESS
    pure (ride, mbRating)
  pure $ extractSolidType @Ride <$> mbFullRideT

getInProgressRideIdByDriverId :: Transactionable m => Id Person -> m (Maybe (Id Ride))
getInProgressRideIdByDriverId driverId = do
  id <- Esq.findOne $ do
    ride <- from $ table @RideT
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
        &&. ride ^. RideStatus ==. val Ride.INPROGRESS
    pure $ ride ^. RideId
  pure $ Id <$> id

getActiveByDriverId :: Transactionable m => Id Person -> m (Maybe Ride)
getActiveByDriverId driverId = Esq.buildDType $ do
  mbFullRideT <- Esq.findOne' $ do
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
  SqlDB ()
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
  SqlDB ()
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
  SqlDB ()
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
  SqlDB ()
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
  SqlDB ()
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

getCountByStatus :: Transactionable m => Id Merchant -> m [(RideStatus, Int)]
getCountByStatus merchantId = do
  Esq.findAll $ do
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

updateArrival :: Id Ride -> SqlDB ()
updateArrival rideId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideDriverArrivalTime =. val (Just now),
        RideUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideTId ==. val (toKey rideId)

data RideItem = RideItem
  { rideShortId :: ShortId Ride,
    rideDetails :: RideDetails,
    riderDetails :: RiderDetails,
    customerName :: Maybe Text,
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
  Maybe DbHash ->
  UTCTime ->
  m [RideItem]
findAllRideItems merchantId limitVal offsetVal mbBookingStatus mbRideShortId mbCustomerPhoneDBHash mbDriverPhoneDBHash now = do
  res <- Esq.findAll $ do
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
    orderBy [desc $ ride ^. RideCreatedAt]
    limit $ fromIntegral limitVal
    offset $ fromIntegral offsetVal
    return
      ( ride ^. RideShortId,
        rideDetails,
        riderDetails,
        booking ^. BookingRiderName,
        bookingStatusVal
      )
  pure $ mkRideItem <$> res
  where
    -- ride considered as ONGOING_6HRS if ride.status = INPROGRESS, but somehow ride.tripStartTime = Nothing
    ongoing6HrsCond ride =
      ride ^. Ride.RideTripStartTime +. just (Esq.interval [Esq.HOUR 6]) <=. val (Just now)
    mkBookingStatusVal ride =
      case_
        [ when_ (ride ^. Ride.RideStatus ==. val Ride.NEW) then_ $ val Common.UPCOMING,
          when_ (ride ^. Ride.RideStatus ==. val Ride.INPROGRESS &&. not_ (ongoing6HrsCond ride)) then_ $ val Common.ONGOING,
          when_ (ride ^. Ride.RideStatus ==. val Ride.COMPLETED) then_ $ val Common.COMPLETED,
          when_ (ride ^. Ride.RideStatus ==. val Ride.CANCELLED) then_ $ val Common.CANCELLED
        ]
        (else_ $ val Common.ONGOING_6HRS)

    mkRideItem (rideShortId, rideDetails, riderDetails, customerName, bookingStatus) = do
      RideItem {rideShortId = ShortId rideShortId, ..}
