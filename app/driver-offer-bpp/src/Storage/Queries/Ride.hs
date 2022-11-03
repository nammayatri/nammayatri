{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Ride where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong)
import Beckn.Utils.Common
import Data.Time hiding (getCurrentTime)
import Domain.Types.Booking as Booking
import Domain.Types.Organization
import Domain.Types.Person
import Domain.Types.Ride as Ride
import Storage.Queries.Booking (baseBookingTable)
import Storage.Tabular.Booking as Booking
import Storage.Tabular.Rating as Rating
import Storage.Tabular.Ride as Ride

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
  pure $ extractSolidType <$> mbFullRideT

findActiveByRBId :: Transactionable m => Id Booking -> m (Maybe Ride)
findActiveByRBId rbId = Esq.buildDType $ do
  mbFullRideT <- Esq.findOne' $ do
    (ride :& mbRating) <- from fullRideTable
    where_ $
      ride ^. Ride.RideBookingId ==. val (toKey rbId)
        &&. ride ^. RideStatus !=. val Ride.CANCELLED
    pure (ride, mbRating)
  pure $ extractSolidType <$> mbFullRideT

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
    ((booking :& fromLocation :& toLocation :& fareParams) :& (ride :& mbRating)) <-
      from $
        baseBookingTable
          `innerJoin` fullRideTable
            `Esq.on` ( \((booking :& _ :& _ :& _) :& (ride :& _)) ->
                         ride ^. Ride.RideBookingId ==. booking ^. Booking.BookingTId
                     )
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
        &&. whenTrue_ isOnlyActive (not_ $ ride ^. RideStatus `in_` valList [Ride.COMPLETED, Ride.CANCELLED])
    orderBy [desc $ ride ^. RideCreatedAt]
    limit limitVal
    offset offsetVal
    return ((booking, fromLocation, toLocation, fareParams), (ride, mbRating))

  pure $
    res <&> \(fullBookingT, fullRideT) -> do
      (extractSolidType fullRideT, extractSolidType fullBookingT)

getInProgressByDriverId :: Transactionable m => Id Person -> m (Maybe Ride)
getInProgressByDriverId driverId = Esq.buildDType $ do
  mbFullRideT <- Esq.findOne' $ do
    (ride :& mbRating) <- from fullRideTable
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
        &&. ride ^. RideStatus ==. val Ride.INPROGRESS
    pure (ride, mbRating)
  pure $ extractSolidType <$> mbFullRideT

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
  pure $ extractSolidType <$> mbFullRideT

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
      [ RideChargeableDistance =. val ride.chargeableDistance,
        RideFare =. val ride.fare,
        RideTripEndTime =. val ride.tripEndTime,
        RideTripEndLat =. val (ride.tripEndPos <&> (.lat)),
        RideTripEndLon =. val (ride.tripEndPos <&> (.lon)),
        RideUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideTId ==. val (toKey rideId)

getCountByStatus :: Transactionable m => Id Organization -> m [(RideStatus, Int)]
getCountByStatus orgId = do
  Esq.findAll $ do
    (ride :& booking) <-
      from $
        table @RideT
          `innerJoin` table @BookingT
            `Esq.on` ( \(ride :& booking) ->
                         ride ^. Ride.RideBookingId ==. booking ^. Booking.BookingTId
                     )
    where_ $ booking ^. BookingProviderId ==. val (toKey orgId)
    groupBy $ ride ^. RideStatus
    return (ride ^. RideStatus, countRows :: SqlExpr (Esq.Value Int))

getRidesForDate :: Transactionable m => Id Person -> Day -> m [Ride]
getRidesForDate driverId date = Esq.buildDType $ do
  fullRideType <- Esq.findAll' $ do
    (ride :& rating) <- from fullRideTable
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
        &&. ride ^. RideTripEndTime >=. val (Just minDayTime)
        &&. ride ^. RideTripEndTime <. val (Just maxDayTime)
        &&. ride ^. RideStatus ==. val Ride.COMPLETED
    return (ride, rating)
  return $ extractSolidType <$> fullRideType
  where
    minDayTime = UTCTime (addDays (-1) date) 66600
    maxDayTime = UTCTime date 66600
