{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Ride where

import Control.Monad.Extra hiding (fromMaybeM)
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Ride as Common
import Data.Either
import qualified Data.HashMap.Strict as HashMap
import Data.Int
import Data.List (zip7)
import Data.Maybe
import Data.Time hiding (getCurrentTime)
import qualified Database.Beam as B
import Database.Beam.Backend (autoSqlValueSyntax)
import qualified Database.Beam.Backend as BeamBackend
import Database.Beam.Postgres
import Domain.Types.Booking as Booking
import Domain.Types.Booking as DBooking
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import Domain.Types.DriverInformation
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.Ride as DR
import Domain.Types.Ride as Ride
import qualified Domain.Types.Ride as DRide
import Domain.Types.RideDetails as RideDetails
import Domain.Types.RiderDetails as RiderDetails
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id, null, traverse_)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Maps.Types (LatLong (..), lat, lon)
import Kernel.Prelude hiding (foldl', map)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.Ride.Table as BeamR
import qualified Storage.Beam.RideDetails as BeamRD
import qualified Storage.Beam.RiderDetails as BeamRDR
import qualified Storage.Queries.Booking as QBooking
import Storage.Queries.Instances.DriverInformation ()
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Storage.Queries.RideDetails ()
import Storage.Queries.RiderDetails ()

data DatabaseWith2 table1 table2 f = DatabaseWith2
  { dwTable1 :: f (B.TableEntity table1),
    dwTable2 :: f (B.TableEntity table2)
  }
  deriving (Generic, B.Database be)

data DatabaseWith4 table1 table2 table3 table4 f = DatabaseWith4
  { dwTable1 :: f (B.TableEntity table1),
    dwTable2 :: f (B.TableEntity table2),
    dwTable3 :: f (B.TableEntity table3),
    dwTable4 :: f (B.TableEntity table4)
  }
  deriving (Generic, B.Database be)

createRide' :: MonadFlow m => Ride -> m ()
createRide' = createWithKV

create :: MonadFlow m => Ride -> m ()
create ride = do
  _ <- whenNothingM_ (QL.findById ride.fromLocation.id) $ do QL.create ride.fromLocation
  _ <- whenNothingM_ (QL.findById ride.toLocation.id) $ do QL.create ride.toLocation
  createRide' ride

createRide :: MonadFlow m => Ride -> m ()
createRide ride = do
  fromLocationMap <- SLM.buildPickUpLocationMapping ride.fromLocation.id ride.id.getId DLM.RIDE
  toLocationMaps <- SLM.buildDropLocationMapping ride.toLocation.id ride.id.getId DLM.RIDE
  QLM.create fromLocationMap >> QLM.create toLocationMaps >> create ride

findById :: MonadFlow m => Id Ride -> m (Maybe Ride)
findById (Id rideId) = findOneWithKV [Se.Is BeamR.id $ Se.Eq rideId]

findAllRidesByDriverId ::
  MonadFlow m =>
  Id Person ->
  m [Ride]
findAllRidesByDriverId (Id driverId) = findAllWithKV [Se.Is BeamR.driverId $ Se.Eq driverId]

findCompletedRideByGHRId :: (MonadFlow m) => Id DDGR.DriverGoHomeRequest -> m (Maybe Ride)
findCompletedRideByGHRId (Id ghrId) = findAllWithOptionsKV [Se.And [Se.Is BeamR.driverGoHomeRequestId $ Se.Eq (Just ghrId), Se.Is BeamR.status $ Se.Eq DRide.COMPLETED]] (Se.Desc BeamR.createdAt) (Just 1) Nothing <&> listToMaybe

findActiveByRBId :: (MonadFlow m) => Id Booking -> m (Maybe Ride)
findActiveByRBId (Id rbId) = findOneWithKV [Se.And [Se.Is BeamR.bookingId $ Se.Eq rbId, Se.Is BeamR.status $ Se.Not $ Se.Eq Ride.CANCELLED]]

findAllRidesWithSeConditionsCreatedAtDesc :: MonadFlow m => [Se.Clause Postgres BeamR.RideT] -> m [Ride]
findAllRidesWithSeConditionsCreatedAtDesc conditions = findAllWithOptionsKV conditions (Se.Desc BeamR.createdAt) Nothing Nothing

findAllDriverInfromationFromRides :: MonadFlow m => [Ride] -> m [DriverInformation]
findAllDriverInfromationFromRides rides = findAllWithKV [Se.And [Se.Is BeamDI.driverId $ Se.In $ getId . DR.driverId <$> rides]]

findAllBookingsWithSeConditions :: MonadFlow m => [Se.Clause Postgres BeamB.BookingT] -> m [Booking]
findAllBookingsWithSeConditions = findAllWithKV

findAllBookingsWithSeConditionsCreatedAtDesc :: MonadFlow m => [Se.Clause Postgres BeamB.BookingT] -> m [Booking]
findAllBookingsWithSeConditionsCreatedAtDesc conditions = findAllWithOptionsKV conditions (Se.Desc BeamB.createdAt) Nothing Nothing

findAllRidesWithSeConditions :: MonadFlow m => [Se.Clause Postgres BeamR.RideT] -> m [Ride]
findAllRidesWithSeConditions = findAllWithKV

findAllRidesBookingsByRideId :: MonadFlow m => Id Merchant -> [Id Ride] -> m [(Ride, Booking)]
findAllRidesBookingsByRideId (Id merchantId) rideIds = do
  rides <- findAllRidesWithSeConditions [Se.Is BeamR.id $ Se.In $ getId <$> rideIds]
  let bookingSeCondition =
        [ Se.And
            [ Se.Is BeamB.id $ Se.In $ getId . DR.bookingId <$> rides,
              Se.Is BeamB.providerId $ Se.Eq merchantId
            ]
        ]
  bookings <- findAllBookingsWithSeConditions bookingSeCondition
  let rideBooking = foldl' (getRideWithBooking bookings) [] rides
  pure rideBooking
  where
    getRideWithBooking bookings acc ride' =
      let bookings' = filter (\x -> x.id == ride'.bookingId) bookings
       in acc <> ((ride',) <$> bookings')

findOneByBookingId :: MonadFlow m => Id Booking -> m (Maybe Ride)
findOneByBookingId (Id bookingId) = findAllWithOptionsKV [Se.Is BeamR.bookingId $ Se.Eq bookingId] (Se.Desc BeamR.createdAt) (Just 1) Nothing <&> listToMaybe

findAllByDriverId :: MonadFlow m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe Ride.RideStatus -> Maybe Day -> m [(Ride, Booking)]
findAllByDriverId (Id driverId) mbLimit mbOffset mbOnlyActive mbRideStatus mbDay = do
  let limitVal = maybe 10 fromInteger mbLimit
      offsetVal = maybe 0 fromInteger mbOffset
      isOnlyActive = Just True == mbOnlyActive
  rides <-
    findAllWithOptionsKV
      [ Se.And
          ( [Se.Is BeamR.driverId $ Se.Eq driverId]
              <> if isOnlyActive
                then [Se.Is BeamR.status $ Se.Not $ Se.In [Ride.COMPLETED, Ride.CANCELLED]]
                else
                  []
                    <> ([Se.Is BeamR.status $ Se.Eq (fromJust mbRideStatus) | isJust mbRideStatus])
                    <> ([Se.And [Se.Is BeamR.updatedAt $ Se.GreaterThanOrEq (minDayTime (fromJust mbDay)), Se.Is BeamR.updatedAt $ Se.LessThanOrEq (maxDayTime (fromJust mbDay))] | isJust mbDay])
          )
      ]
      (Se.Desc BeamR.createdAt)
      (Just limitVal)
      (Just offsetVal)
  bookings <- findAllWithOptionsKV [Se.Is BeamB.id $ Se.In $ getId . DR.bookingId <$> rides] (Se.Desc BeamB.createdAt) Nothing Nothing

  let rideWithBooking = foldl' (getRideWithBooking bookings) [] rides
  pure $ take limitVal rideWithBooking
  where
    getRideWithBooking bookings acc ride =
      let bookings' = filter (\b -> b.id == ride.bookingId) bookings
       in acc <> ((ride,) <$> bookings')
    minDayTime date = UTCTime (addDays (-1) date) 66600
    maxDayTime date = UTCTime date 66600

findOneByDriverId :: MonadFlow m => Id Person -> m (Maybe Ride)
findOneByDriverId (Id personId) = findAllWithOptionsKV [Se.Is BeamR.driverId $ Se.Eq personId] (Se.Desc BeamR.createdAt) (Just 1) Nothing <&> listToMaybe

getInProgressByDriverId :: MonadFlow m => Id Person -> m (Maybe Ride)
getInProgressByDriverId (Id personId) = findOneWithKV [Se.And [Se.Is BeamR.driverId $ Se.Eq personId, Se.Is BeamR.status $ Se.Eq Ride.INPROGRESS]]

getInProgressOrNewRideIdAndStatusByDriverId :: MonadFlow m => Id Person -> m (Maybe (Id Ride, RideStatus))
getInProgressOrNewRideIdAndStatusByDriverId (Id driverId) = do
  ride' <- findOneWithKV [Se.And [Se.Is BeamR.driverId $ Se.Eq driverId, Se.Is BeamR.status $ Se.In [Ride.INPROGRESS, Ride.NEW]]]
  let rideData = (,) <$> (DR.id <$> ride') <*> (DR.status <$> ride')
  pure rideData

getActiveByDriverId :: MonadFlow m => Id Person -> m (Maybe Ride)
getActiveByDriverId (Id personId) =
  findOneWithKV [Se.And [Se.Is BeamR.driverId $ Se.Eq personId, Se.Is BeamR.status $ Se.In [Ride.INPROGRESS, Ride.NEW]]]

getActiveBookingAndRideByDriverId :: MonadFlow m => Id Person -> m [(Ride, Booking)]
getActiveBookingAndRideByDriverId (Id personId) = do
  maybeM
    (return [])
    ( \ride ->
        maybeM
          (return [])
          (\booking -> return [(ride, booking)])
          (QBooking.findById ride.bookingId)
    )
    (findOneWithKV [Se.And [Se.Is BeamR.driverId $ Se.Eq personId, Se.Is BeamR.status $ Se.In [Ride.INPROGRESS, Ride.NEW]]])

updateStatus :: MonadFlow m => Id Ride -> RideStatus -> m ()
updateStatus rideId status = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.status status,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateUiDistanceCalculation :: MonadFlow m => Id Ride -> Maybe Int -> Maybe Int -> m ()
updateUiDistanceCalculation rideId dist1 dist2 = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.uiDistanceCalculationWithAccuracy dist1,
      Se.Set BeamR.uiDistanceCalculationWithoutAccuracy dist2,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateDriverDeviatedFromRoute :: MonadFlow m => Id Ride -> Bool -> m ()
updateDriverDeviatedFromRoute rideId deviation = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.driverDeviatedFromRoute $ Just deviation,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateStartTimeAndLoc :: MonadFlow m => Id Ride -> LatLong -> m ()
updateStartTimeAndLoc rideId point = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.tripStartTime $ Just now,
      Se.Set BeamR.tripStartLat $ Just point.lat,
      Se.Set BeamR.tripStartLon $ Just point.lon,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateOdometerStartReading :: MonadFlow m => Id Ride -> Maybe Int -> m ()
updateOdometerStartReading rideId reading = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.odometerStartReading reading,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

-- updateOdometerEndReading :: MonadFlow m => Id Ride -> Maybe Int -> m()
-- updateOdometerEndReading rideId reading = do
--   now <- getCurrentTime
--   updateOneWithKV
--     [ Se.Set BeamR.odometerEndReading reading,
--       Se.Set BeamR.updatedAt now
--     ]
--     [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateStatusByIds :: MonadFlow m => [Id Ride] -> RideStatus -> m ()
updateStatusByIds rideIds status = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamR.status status,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.In $ getId <$> rideIds)]

updateDistance :: MonadFlow m => Id Person -> HighPrecMeters -> Int -> m ()
updateDistance driverId distance snapCalls = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamR.traveledDistance distance,
      Se.Set BeamR.numberOfSnapToRoadCalls (Just snapCalls),
      Se.Set BeamR.updatedAt now
    ]
    [Se.And [Se.Is BeamR.driverId (Se.Eq $ getId driverId), Se.Is BeamR.status (Se.Eq Ride.INPROGRESS)]]

updateAll :: MonadFlow m => Id Ride -> Ride -> m ()
updateAll rideId ride = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamR.chargeableDistance ride.chargeableDistance,
      Se.Set BeamR.fare ride.fare,
      Se.Set BeamR.tripEndTime ride.tripEndTime,
      Se.Set BeamR.tripEndLat (ride.tripEndPos <&> (.lat)),
      Se.Set BeamR.tripEndLon (ride.tripEndPos <&> (.lon)),
      Se.Set BeamR.fareParametersId (getId <$> ride.fareParametersId),
      Se.Set BeamR.distanceCalculationFailed ride.distanceCalculationFailed,
      Se.Set BeamR.pickupDropOutsideOfThreshold ride.pickupDropOutsideOfThreshold,
      Se.Set BeamR.odometerEndReading ride.odometerEndReading,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

getCountByStatus :: MonadFlow m => Id Merchant -> m [(RideStatus, Int)]
getCountByStatus merchantId = do
  -- Tricky query to be able to insert meaningful Point
  dbConf <- getMasterBeamConfig
  resp <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.aggregate_ (\(ride, _) -> (B.group_ (BeamR.status ride), B.as_ @Int B.countAll_)) $
          B.filter_' (\(_, BeamB.BookingT {..}) -> providerId B.==?. B.val_ (getId merchantId)) $
            do
              ride <- B.all_ (BeamCommon.ride BeamCommon.atlasDB)
              booking <- B.join_' (BeamCommon.booking BeamCommon.atlasDB) (\booking -> BeamB.id booking B.==?. BeamR.bookingId ride)
              pure (ride, booking)
  pure (EulerHS.Prelude.fromRight [] resp)

getRidesForDate :: MonadFlow m => Id Person -> Day -> Seconds -> m [Ride]
getRidesForDate driverId date diffTime = do
  let minDayTime = UTCTime (addDays (-1) date) (86400 - secondsToDiffTime (toInteger diffTime.getSeconds))
  let maxDayTime = UTCTime date (86400 - secondsToDiffTime (toInteger diffTime.getSeconds))
  findAllWithKV
    [ Se.And
        [ Se.Is BeamR.driverId $ Se.Eq $ getId driverId,
          Se.Is BeamR.tripEndTime $ Se.GreaterThanOrEq $ Just minDayTime,
          Se.Is BeamR.tripEndTime $ Se.LessThan $ Just maxDayTime,
          Se.Is BeamR.status $ Se.Eq Ride.COMPLETED
        ]
    ]

updateArrival :: MonadFlow m => Id Ride -> m ()
updateArrival rideId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.driverArrivalTime $ Just now,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

data RideItem = RideItem
  { rideShortId :: ShortId Ride,
    rideCreatedAt :: UTCTime,
    rideDetails :: RideDetails,
    riderDetails :: RiderDetails,
    customerName :: Maybe Text,
    fareDiff :: Maybe Money,
    bookingStatus :: Common.BookingStatus
  }

instance Num (Maybe Money) where
  (-) = liftA2 (-)
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = Just . fromInteger

instance BeamBackend.BeamSqlBackend be => B.HasSqlEqualityCheck be Common.BookingStatus

instance BeamBackend.HasSqlValueSyntax be String => BeamBackend.HasSqlValueSyntax be Common.BookingStatus where
  sqlValueSyntax = autoSqlValueSyntax

findAllRideItems ::
  MonadFlow m =>
  Id Merchant ->
  Int ->
  Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Ride) ->
  Maybe DbHash ->
  Maybe DbHash ->
  Maybe Money ->
  UTCTime ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m [RideItem]
findAllRideItems merchantId limitVal offsetVal mbBookingStatus mbRideShortId mbCustomerPhoneDBHash mbDriverPhoneDBHash mbFareDiff now mbFrom mbTo = do
  dbConf <- getMasterBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (fromIntegral limitVal) $
          B.offset_ (fromIntegral offsetVal) $
            B.filter_'
              ( \(booking, ride, rideDetails, riderDetails) ->
                  booking.providerId B.==?. B.val_ (getId merchantId)
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\rideShortId -> ride.shortId B.==?. B.val_ (getShortId rideShortId)) mbRideShortId
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\hash -> riderDetails.mobileNumberHash B.==?. B.val_ hash) mbCustomerPhoneDBHash
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\hash -> rideDetails.driverNumberHash B.==?. B.val_ (Just hash)) mbDriverPhoneDBHash
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\defaultFrom -> B.sqlBool_ $ ride.createdAt B.>=. B.val_ defaultFrom) mbFrom
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\defaultTo -> B.sqlBool_ $ ride.createdAt B.<=. B.val_ defaultTo) mbTo
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\bookingStatus -> mkBookingStatusVal ride B.==?. B.val_ bookingStatus) mbBookingStatus
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\fareDiff_ -> B.sqlBool_ $ (ride.fare - (B.just_ booking.estimatedFare)) B.>. B.val_ (Just fareDiff_) B.||. (ride.fare - (B.just_ booking.estimatedFare)) B.<. B.val_ (Just fareDiff_)) mbFareDiff
              )
              do
                booking' <- B.all_ (BeamCommon.booking BeamCommon.atlasDB)
                ride' <- B.join_' (BeamCommon.ride BeamCommon.atlasDB) (\ride'' -> BeamR.bookingId ride'' B.==?. BeamB.id booking')
                rideDetails' <- B.join_' (BeamCommon.rideDetails BeamCommon.atlasDB) (\rideDetails'' -> ride'.id B.==?. BeamRD.id rideDetails'')
                riderDetails' <- B.join_' (BeamCommon.rDetails BeamCommon.atlasDB) (\riderDetails'' -> B.just_ (BeamRDR.id riderDetails'') B.==?. BeamB.riderId booking')
                pure (booking', ride', rideDetails', riderDetails')
  res' <- case res of
    Right x -> do
      let bookings = fst' <$> x
          rides = snd' <$> x
          rideDetails = thd' <$> x
          riderDetails = fth' <$> x
      b <- catMaybes <$> mapM fromTType' bookings
      r <- catMaybes <$> mapM fromTType' rides
      rd <- catMaybes <$> mapM fromTType' rideDetails
      rdr <- catMaybes <$> mapM fromTType' riderDetails
      pure $ zip7 (DR.shortId <$> r) (DR.createdAt <$> r) rd rdr (DBooking.riderName <$> b) (liftA2 (-) (DR.fare <$> r) (Just . DBooking.estimatedFare <$> b)) (mkBookingStatus now <$> r)
    Left err -> do
      logError $ "FAILED_TO_FETCH_RIDE_LIST" <> show err
      pure []
  pure $ mkRideItem <$> res'
  where
    mkBookingStatusVal ride =
      B.ifThenElse_ (ride.status B.==. B.val_ Ride.COMPLETED) (B.val_ Common.COMPLETED) $
        B.ifThenElse_ (ride.status B.==. B.val_ Ride.NEW B.&&. B.not_ (ride.createdAt B.<=. B.val_ (addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now))) (B.val_ Common.UPCOMING) $
          B.ifThenElse_ (ride.status B.==. B.val_ Ride.NEW B.&&. (ride.createdAt B.<=. B.val_ (addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now))) (B.val_ Common.UPCOMING_6HRS) $
            B.ifThenElse_ (ride.status B.==. B.val_ Ride.INPROGRESS B.&&. B.not_ (ride.tripStartTime B.<=. B.val_ (Just $ addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now))) (B.val_ Common.ONGOING) $
              B.ifThenElse_ (ride.status B.==. B.val_ Ride.CANCELLED) (B.val_ Common.CANCELLED) (B.val_ Common.ONGOING_6HRS)
    fst' (x, _, _, _) = x
    snd' (_, y, _, _) = y
    thd' (_, _, z, _) = z
    fth' (_, _, _, r) = r
    mkBookingStatus now' ride
      | ride.status == Ride.COMPLETED = Common.COMPLETED
      | ride.status == Ride.NEW && (ride.createdAt) > addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now' = Common.UPCOMING
      | ride.status == Ride.NEW && ride.createdAt <= addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now' = Common.UPCOMING_6HRS
      | ride.status == Ride.INPROGRESS && ride.tripStartTime > Just (addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now') = Common.ONGOING
      | ride.status == Ride.CANCELLED = Common.CANCELLED
      | otherwise = Common.ONGOING_6HRS
    mkRideItem (rideShortId, rideCreatedAt, rideDetails, riderDetails, customerName, fareDiff, bookingStatus) =
      RideItem {..}

data StuckRideItem = StuckRideItem
  { rideId :: Id Ride,
    bookingId :: Id Booking,
    driverId :: Id Person,
    driverActive :: Bool
  }

findStuckRideItems :: MonadFlow m => Id Merchant -> [Id Booking] -> UTCTime -> m [StuckRideItem]
findStuckRideItems (Id merchantId) bookingIds now = do
  let now6HrBefore = addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now
      bookingSeCondition =
        [ Se.And
            [ Se.Is BeamB.providerId $ Se.Eq merchantId,
              Se.Is BeamB.id $ Se.In $ getId <$> bookingIds
            ]
        ]
  bookings <- findAllBookingsWithSeConditions bookingSeCondition
  rides <-
    findAllWithKV
      [ Se.And
          [ Se.Is BeamR.status $ Se.Eq Ride.NEW,
            Se.Is BeamR.createdAt $ Se.LessThanOrEq now6HrBefore,
            Se.Is BeamR.bookingId $ Se.In $ getId . DBooking.id <$> bookings
          ]
      ]
  driverInfos <- findAllDriverInfromationFromRides rides
  let rideBooking = foldl' (getRideWithBooking bookings) [] rides
  let rideBookingDriverInfo = foldl' (getRideWithBookingDriverInfo driverInfos) [] rideBooking
  pure $ mkStuckRideItem <$> rideBookingDriverInfo
  where
    getRideWithBooking bookings acc ride' =
      let bookings' = filter (\x -> x.id == ride'.bookingId) bookings
       in acc <> ((\x -> (ride', x.id)) <$> bookings')

    getRideWithBookingDriverInfo driverInfos acc (ride', booking') =
      let driverInfos' = filter (\x -> x.driverId == ride'.driverId) driverInfos
       in acc <> ((\x -> (ride'.id, booking', x.driverId, x.active)) <$> driverInfos')

    mkStuckRideItem (rideId, bookingId, driverId, driverActive) = StuckRideItem {..}

findLastRideAssigned :: MonadFlow m => Id Person -> m (Maybe Ride)
findLastRideAssigned (Id driverId) = findAllWithOptionsKV [Se.Is BeamR.driverId $ Se.Eq driverId] (Se.Desc BeamR.createdAt) (Just 1) Nothing <&> listToMaybe

findRideBookingsById :: MonadFlow m => Id Merchant -> [Id Booking] -> m (HashMap.HashMap Text (Booking, Maybe DRide.Ride))
findRideBookingsById merchantId bookingIds = do
  bookings <- findBookingsById merchantId bookingIds
  rides <- findRidesByBookingId (bookings <&> (.id))
  let tuple = map (\booking -> (getId booking.id, (booking, Kernel.Prelude.find (\ride -> ride.bookingId == booking.id) rides))) bookings
  pure $ HashMap.fromList tuple

findBookingsById :: MonadFlow m => Id Merchant -> [Id Booking] -> m [Booking]
findBookingsById (Id merchantId) bookingIds = findAllWithKV [Se.And [Se.Is BeamB.providerId $ Se.Eq merchantId, Se.Is BeamB.id $ Se.In $ getId <$> bookingIds]]

findRidesByBookingId :: MonadFlow m => [Id Booking] -> m [DRide.Ride]
findRidesByBookingId bookingIds = findAllWithKV [Se.Is BeamR.bookingId $ Se.In $ getId <$> bookingIds]

findCancelledBookingId :: MonadFlow m => Id Person -> m [Id Booking]
findCancelledBookingId (Id driverId) = findAllWithKV [Se.And [Se.Is BeamR.driverId $ Se.Eq driverId, Se.Is BeamR.status $ Se.Eq Ride.CANCELLED]] <&> (Ride.bookingId <$>)

findRideByRideShortId :: MonadFlow m => ShortId Ride -> m (Maybe Ride)
findRideByRideShortId (ShortId shortId) = findOneWithKV [Se.Is BeamR.shortId $ Se.Eq shortId]

createMapping :: MonadFlow m => Text -> Text -> m [DLM.LocationMapping]
createMapping bookingId rideId = do
  mappings <- QLM.findByEntityId bookingId
  when (null mappings) $ throwError (InternalError "Entity Mappings for Booking Not Found") -- this case should never occur
  let fromLocationMapping = filter (\loc -> loc.order == 0) mappings
      toLocationMappings = filter (\loc -> loc.order /= 0) mappings

  fromLocMap <- listToMaybe fromLocationMapping & fromMaybeM (InternalError "Entity Mappings For FromLocation Not Found")

  when (null toLocationMappings) $ throwError (InternalError "Entity Mappings For ToLocation Not Found")
  let toLocMap = maximumBy (comparing (.order)) toLocationMappings

  fromLocationRideMapping <- SLM.buildPickUpLocationMapping fromLocMap.locationId rideId DLM.RIDE
  toLocationRideMappings <- SLM.buildDropLocationMapping toLocMap.locationId rideId DLM.RIDE

  void $ QLM.create fromLocationRideMapping >> QLM.create toLocationRideMappings
  return [fromLocationRideMapping, toLocationRideMappings]

instance FromTType' BeamR.Ride Ride where
  fromTType' BeamR.RideT {..} = do
    mappings <- QLM.findByEntityId id
    rideMappings <-
      if null mappings
        then do
          void $ QBooking.findById (Id bookingId)
          createMapping bookingId id
        else return mappings
    let fromLocationMapping = filter (\loc -> loc.order == 0) rideMappings
        toLocationMappings = filter (\loc -> loc.order /= 0) rideMappings

    fromLocMap <- listToMaybe fromLocationMapping & fromMaybeM (InternalError "Entity Mappings For FromLocation Not Found")
    fromLocation <- QL.findById fromLocMap.locationId >>= fromMaybeM (InternalError $ "FromLocation not found in ride for fromLocationId: " <> fromLocMap.locationId.getId)

    when (null toLocationMappings) $ throwError (InternalError "Entity Mappings For ToLocation Not Found")
    let toLocMap = maximumBy (comparing (.order)) toLocationMappings
    toLocation <- QL.findById toLocMap.locationId >>= fromMaybeM (InternalError $ "ToLocation not found in ride for toLocationId: " <> toLocMap.locationId.getId)
    tUrl <- parseBaseUrl trackingUrl
    pure $
      Just
        Ride
          { id = Id id,
            bookingId = Id bookingId,
            shortId = ShortId shortId,
            merchantId = Id <$> merchantId,
            driverId = Id driverId,
            tripStartPos = LatLong <$> tripStartLat <*> tripStartLon,
            tripEndPos = LatLong <$> tripEndLat <*> tripEndLon,
            fareParametersId = Id <$> fareParametersId,
            driverGoHomeRequestId = Id <$> driverGoHomeRequestId,
            trackingUrl = tUrl,
            ..
          }

instance ToTType' BeamR.Ride Ride where
  toTType' Ride {..} =
    BeamR.RideT
      { BeamR.id = getId id,
        BeamR.bookingId = getId bookingId,
        BeamR.shortId = getShortId shortId,
        BeamR.merchantId = getId <$> merchantId,
        BeamR.status = status,
        BeamR.driverId = getId driverId,
        BeamR.otp = otp,
        BeamR.trackingUrl = showBaseUrl trackingUrl,
        BeamR.fare = fare,
        BeamR.traveledDistance = traveledDistance,
        BeamR.chargeableDistance = chargeableDistance,
        BeamR.driverArrivalTime = driverArrivalTime,
        BeamR.tripStartTime = tripStartTime,
        BeamR.tripEndTime = tripEndTime,
        BeamR.tripStartLat = lat <$> tripStartPos,
        BeamR.tripEndLat = lat <$> tripEndPos,
        BeamR.tripStartLon = lon <$> tripStartPos,
        BeamR.tripEndLon = lon <$> tripEndPos,
        pickupDropOutsideOfThreshold = pickupDropOutsideOfThreshold,
        BeamR.fareParametersId = getId <$> fareParametersId,
        BeamR.distanceCalculationFailed = distanceCalculationFailed,
        BeamR.createdAt = createdAt,
        BeamR.updatedAt = updatedAt,
        BeamR.driverDeviatedFromRoute = driverDeviatedFromRoute,
        BeamR.numberOfSnapToRoadCalls = numberOfSnapToRoadCalls,
        BeamR.numberOfDeviation = numberOfDeviation,
        BeamR.uiDistanceCalculationWithAccuracy = uiDistanceCalculationWithAccuracy,
        BeamR.uiDistanceCalculationWithoutAccuracy = uiDistanceCalculationWithoutAccuracy,
        BeamR.driverGoHomeRequestId = getId <$> driverGoHomeRequestId,
        BeamR.odometerStartReading = odometerStartReading,
        BeamR.odometerEndReading = odometerEndReading,
        BeamR.endRideOtp = endRideOtp
      }
