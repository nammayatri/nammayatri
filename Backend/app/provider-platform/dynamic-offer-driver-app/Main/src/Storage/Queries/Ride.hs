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
import Data.Time hiding (getCurrentTime)
import Domain.Types.Booking as Booking
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
import Storage.Queries.Booking (baseBookingTable)
import Storage.Tabular.Booking as Booking
import Storage.Tabular.DriverInformation as DriverInfo
import Storage.Tabular.Ride as Ride
import Storage.Tabular.RideDetails as RideDetails
import Storage.Tabular.RiderDetails as RiderDetails

create :: Ride -> SqlDB m ()
create = Esq.create

findById :: forall m ma. Transactionable ma m => Id Ride -> Proxy ma -> m (Maybe Ride)
findById rideId _ = Esq.findOne @m @ma $ do
  ride <- from $ table @RideT
  where_ $ ride ^. RideTId ==. val (toKey rideId)
  pure ride

findActiveByRBId :: forall m ma. Transactionable ma m => Id Booking -> Proxy ma -> m (Maybe Ride)
findActiveByRBId rbId _ = Esq.findOne @m @ma $ do
  ride <- from $ table @RideT
  where_ $
    ride ^. Ride.RideBookingId ==. val (toKey rbId)
      &&. ride ^. RideStatus !=. val Ride.CANCELLED
  pure ride

findAllByDriverId ::
  forall m ma.
  Transactionable ma m =>
  Id Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  Maybe Ride.RideStatus ->
  Proxy ma ->
  m [(Ride, Booking)]
findAllByDriverId driverId mbLimit mbOffset mbOnlyActive mbRideStatus _ = Esq.buildDType $ do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbOnlyActive
  res <- Esq.findAll' @m @ma $ do
    ((booking :& fromLocation :& toLocation :& fareParams) :& ride) <-
      from $
        baseBookingTable
          `innerJoin` table @RideT
            `Esq.on` ( \((booking :& _ :& _ :& _) :& ride) ->
                         ride ^. Ride.RideBookingId ==. booking ^. Booking.BookingTId
                     )
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
        &&. whenTrue_ isOnlyActive (not_ $ ride ^. RideStatus `in_` valList [Ride.COMPLETED, Ride.CANCELLED])
        &&. whenJust_ mbRideStatus (\status -> ride ^. RideStatus ==. val status)
    orderBy [desc $ ride ^. RideCreatedAt]
    limit limitVal
    offset offsetVal
    return ((booking, fromLocation, toLocation, fareParams), ride)

  pure $
    res <&> \(fullBookingT, ride) -> do
      (extractSolidType @Ride ride, extractSolidType @Booking fullBookingT)

findOneByDriverId :: forall m ma. Transactionable ma m => Id Person -> Proxy ma -> m (Maybe Ride)
findOneByDriverId driverId _ = Esq.findOne @m @ma $ do
  ride <- from $ table @RideT
  where_ $
    ride ^. RideDriverId ==. val (toKey driverId)
  limit 1
  pure ride

getInProgressByDriverId :: forall m ma. Transactionable ma m => Id Person -> Proxy ma -> m (Maybe Ride)
getInProgressByDriverId driverId _ = Esq.findOne @m @ma $ do
  ride <- from $ table @RideT
  where_ $
    ride ^. RideDriverId ==. val (toKey driverId)
      &&. ride ^. RideStatus ==. val Ride.INPROGRESS
  pure ride

getInProgressOrNewRideIdAndStatusByDriverId :: forall m ma. Transactionable ma m => Id Person -> Proxy ma -> m (Maybe (Id Ride, RideStatus))
getInProgressOrNewRideIdAndStatusByDriverId driverId _ = do
  mbTuple :: Maybe (Text, RideStatus) <- Esq.findOne @m @ma $ do
    ride <- from $ table @RideT
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
        &&. ride ^. RideStatus `in_` valList [Ride.INPROGRESS, Ride.NEW]
    pure (ride ^. RideId, ride ^. RideStatus)
  pure $ first Id <$> mbTuple

getActiveByDriverId :: forall m ma. Transactionable ma m => Id Person -> Proxy ma -> m (Maybe Ride)
getActiveByDriverId driverId _ = Esq.findOne @m @ma $ do
  ride <- from $ table @RideT
  where_ $
    ride ^. RideDriverId ==. val (toKey driverId)
      &&. ( ride ^. RideStatus ==. val Ride.INPROGRESS
              ||. ride ^. RideStatus ==. val Ride.NEW
          )
  pure ride

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
      [ RideChargeableDistance =. val ride.chargeableDistance,
        RideFare =. val ride.fare,
        RideTripEndTime =. val ride.tripEndTime,
        RideTripEndLat =. val (ride.tripEndPos <&> (.lat)),
        RideTripEndLon =. val (ride.tripEndPos <&> (.lon)),
        RideFareParametersId =. val (toKey <$> ride.fareParametersId),
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

getRidesForDate :: forall m ma. Transactionable ma m => Id Person -> Day -> Proxy ma -> m [Ride]
getRidesForDate driverId date _ = Esq.findAll @m @ma $ do
  ride <- from $ table @RideT
  where_ $
    ride ^. RideDriverId ==. val (toKey driverId)
      &&. ride ^. RideTripEndTime >=. val (Just minDayTime)
      &&. ride ^. RideTripEndTime <. val (Just maxDayTime)
      &&. ride ^. RideStatus ==. val Ride.COMPLETED
  return ride
  where
    minDayTime = UTCTime (addDays (-1) date) 66600
    maxDayTime = UTCTime date 66600

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
        &&. whenJust_ mbFareDiff (\fareDiff_ -> (ride ^. Ride.RideFare -. just (booking ^. Booking.BookingEstimatedFare) >. val (Just fareDiff_)) ||. (just (booking ^. Booking.BookingEstimatedFare) -. ride ^. Ride.RideFare) >. val (Just fareDiff_))
    orderBy [desc $ ride ^. RideCreatedAt]
    limit $ fromIntegral limitVal
    offset $ fromIntegral offsetVal
    return
      ( ride ^. RideShortId,
        rideDetails,
        riderDetails,
        booking ^. BookingRiderName,
        ride ^. Ride.RideFare -. just (booking ^. Booking.BookingEstimatedFare),
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

    mkRideItem (rideShortId, rideDetails, riderDetails, customerName, fareDiff, bookingStatus) = do
      RideItem {rideShortId = ShortId rideShortId, ..}

upcoming6HrsCond :: SqlExpr (Entity RideT) -> UTCTime -> SqlExpr (Esq.Value Bool)
upcoming6HrsCond ride now = ride ^. Ride.RideCreatedAt +. Esq.interval [Esq.HOUR 6] <=. val now

data StuckRideItem = StuckRideItem
  { rideId :: Id Ride,
    bookingId :: Id Booking,
    driverId :: Id Person,
    driverActive :: Bool
  }

findStuckRideItems :: forall m ma. Transactionable ma m => Id Merchant -> [Id Booking] -> UTCTime -> Proxy ma -> m [StuckRideItem]
findStuckRideItems merchantId bookingIds now _ = do
  res <- Esq.findAll @m @ma $ do
    ride :& booking :& driverInfo <-
      from $
        table @RideT
          `innerJoin` table @BookingT
            `Esq.on` ( \(ride :& booking) ->
                         ride ^. Ride.RideBookingId ==. booking ^. Booking.BookingTId
                     )
          `innerJoin` table @DriverInformationT
            `Esq.on` ( \(ride :& _ :& driverInfo) ->
                         ride ^. Ride.RideDriverId ==. driverInfo ^. DriverInfo.DriverInformationDriverId
                     )
    where_ $
      booking ^. BookingProviderId ==. val (toKey merchantId)
        &&. booking ^. BookingTId `in_` valList (toKey <$> bookingIds)
        &&. (ride ^. Ride.RideStatus ==. val Ride.NEW &&. upcoming6HrsCond ride now)
    pure (ride ^. RideTId, booking ^. BookingTId, driverInfo ^. DriverInformationDriverId, driverInfo ^. DriverInformationActive)
  pure $ mkStuckRideItem <$> res
  where
    mkStuckRideItem (rideId, bookingId, driverId, driverActive) = StuckRideItem {..}
