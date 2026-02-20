{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.Ride where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Domain.Types.RideDetails as RideDetails
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Clickhouse.RideDetails (findIdsByFleetOwner, findIdsByFleetOwnerAndVehicle)

data RideT f = RideT
  { id :: C f (Id DRide.Ride),
    bookingId :: C f (Id DBooking.Booking),
    shortId :: C f (ShortId DRide.Ride),
    status :: C f (Maybe DRide.RideStatus),
    fare :: C f (Maybe Int),
    driverId :: C f (Maybe (Id DP.Driver)),
    chargeableDistance :: C f (Maybe Int),
    createdAt :: C f UTCTime,
    updatedAt :: C f UTCTime,
    tripEndTime :: C f (Maybe UTCTime),
    tripStartTime :: C f (Maybe UTCTime)
  }
  deriving (Generic)

deriving instance Show Ride

instance ClickhouseValue DRide.RideStatus

rideTTable :: RideT (FieldModification RideT)
rideTTable =
  RideT
    { id = "id",
      bookingId = "booking_id",
      shortId = "short_id",
      status = "status",
      fare = "fare",
      driverId = "driver_id",
      chargeableDistance = "chargeable_distance",
      createdAt = "created_at",
      updatedAt = "updated_at",
      tripEndTime = "trip_end_time",
      tripStartTime = "trip_start_time"
    }

type Ride = RideT Identity

$(TH.mkClickhouseInstances ''RideT 'SELECT_FINAL_MODIFIER)

data RideStats = RideStats
  { totalEarnings :: Int,
    totalDistanceTravelled :: Int,
    completedRides :: Int,
    cancelledRides :: Int,
    totalDuration :: Int,
    driverId' :: Maybe (Id DP.Person)
  }
  deriving (Show)

getCompletedRidesByDriver ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DRide.Ride] ->
  Id DP.Person ->
  UTCTime ->
  UTCTime ->
  m Int
getCompletedRidesByDriver rideIds driverId from to = do
  res <-
    CH.findAll $
      CH.select_ (\ride -> CH.aggregate $ CH.count_ ride.id) $
        CH.filter_
          ( \ride ->
              ride.status CH.==. Just DRide.COMPLETED
                CH.&&. ride.createdAt >=. from
                CH.&&. ride.createdAt <=. to
                CH.&&. ride.driverId CH.==. Just (cast driverId)
                CH.&&. ride.id `in_` rideIds
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE rideTTable)
  pure $ fromMaybe 0 (listToMaybe res)

getRidesByIdAndStatus ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DRide.Ride] ->
  DRide.RideStatus ->
  UTCTime ->
  UTCTime ->
  m Int
getRidesByIdAndStatus rideIds status from to = do
  res <-
    CH.findAll $
      CH.select_ (\ride -> CH.aggregate $ CH.count_ ride.id) $
        CH.filter_
          ( \ride ->
              ride.status CH.==. Just status
                CH.&&. ride.createdAt >=. from
                CH.&&. ride.createdAt <=. to
                CH.&&. ride.id `in_` rideIds
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE rideTTable)
  pure $ fromMaybe 0 (listToMaybe res)

getEarningsByDriver ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DRide.Ride] ->
  Id DP.Person ->
  UTCTime ->
  UTCTime ->
  m Int
getEarningsByDriver rideIds driverId from to = do
  res <-
    CH.findAll $
      CH.select_ (\ride -> CH.aggregate $ CH.sum_ ride.fare) $
        CH.filter_
          ( \ride ->
              ride.status CH.==. Just DRide.COMPLETED
                CH.&&. ride.createdAt >=. from
                CH.&&. ride.createdAt <=. to
                CH.&&. ride.driverId CH.==. Just (cast driverId)
                CH.&&. ride.id `in_` rideIds
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE rideTTable)
  case res of
    [] -> pure 0
    [earnings] -> pure $ fromMaybe 0 earnings
    _ -> throwError $ InternalError "getEarningsByDriver query returns more than 1 response"

getEarningsByIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DRide.Ride] ->
  UTCTime ->
  UTCTime ->
  m Int
getEarningsByIds rideIds from to = do
  res <-
    CH.findAll $
      CH.select_ (\ride -> CH.aggregate $ CH.sum_ ride.fare) $
        CH.filter_
          ( \ride ->
              ride.status CH.==. Just DRide.COMPLETED
                CH.&&. ride.createdAt >=. from
                CH.&&. ride.createdAt <=. to
                CH.&&. ride.id `in_` rideIds
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE rideTTable)
  case res of
    [] -> pure 0
    [earnings] -> pure $ fromMaybe 0 earnings
    _ -> throwError $ InternalError "getEarningsByDriver query returns more than 1 response"

getCompletedRidesStatsByIdsAndDriverId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DRide.Ride] ->
  Maybe (Id DP.Person) ->
  UTCTime ->
  UTCTime ->
  Int ->
  Int ->
  Maybe Bool ->
  Maybe Common.SortOn ->
  m [RideStats]
getCompletedRidesStatsByIdsAndDriverId rideIds mbDriverId from to limit offset sortDesc mbSortOn = do
  let sortBy = case sortDesc of
        Just True -> CH.desc
        _ -> CH.asc
  let sortOn = case mbSortOn of
        Just Common.CANCELLED_RIDES -> (\_ (_, _, _, cancelledRides, _, _) -> sortBy cancelledRides)
        _ -> (\_ (_, _, completedRides, _, _, _) -> sortBy completedRides)
  res <-
    CH.findAll $
      CH.select_
        ( \ride -> do
            let earnings = CH.sum_ ride.fare
            let distanceTravelled = CH.sum_ ride.chargeableDistance
            let completedRides = CH.sum_ $ CH.if_ (ride.status CH.==.. CH.valColumn (Just DRide.COMPLETED)) (CH.valColumn 1) (CH.valColumn 0)
            let cancelledRides = CH.sum_ $ CH.if_ (ride.status CH.==.. CH.valColumn (Just DRide.CANCELLED)) (CH.valColumn 1) (CH.valColumn 0)
            let duration = CH.sum_ (CH.timeDiff ride.createdAt ride.updatedAt)
            CH.groupBy ride.driverId $ \driverId -> do
              (earnings, distanceTravelled, completedRides, cancelledRides, duration, driverId)
        )
        $ CH.orderBy_ sortOn $
          CH.limit_ limit $
            CH.offset_ offset $
              CH.filter_
                ( \ride ->
                    ride.status `in_` [Just DRide.COMPLETED, Just DRide.CANCELLED]
                      CH.&&. ride.createdAt >=. from
                      CH.&&. ride.createdAt <=. to
                      CH.&&. ride.id `in_` rideIds
                      CH.&&. CH.whenJust_ mbDriverId (\driverId -> ride.driverId CH.==. Just (cast driverId))
                )
                (CH.all_ @CH.APP_SERVICE_CLICKHOUSE rideTTable)
  pure $ mkRideStatsByRideStatus <$> res

totalEarningsByFleetOwnerPerVehicle :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> Text -> UTCTime -> UTCTime -> m Int
totalEarningsByFleetOwnerPerVehicle fleetOwnerId vehicleNumber from to = do
  rideIds <- findIdsByFleetOwnerAndVehicle fleetOwnerId vehicleNumber from to
  getEarningsByIds (cast <$> rideIds) from to

totalEarningsByFleetOwnerPerDriver :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> Id DP.Person -> UTCTime -> UTCTime -> m Int
totalEarningsByFleetOwnerPerDriver fleetOwnerId driverId from to = do
  rideIds <- findIdsByFleetOwner fleetOwnerId from to
  getEarningsByDriver (cast <$> rideIds) driverId from to

totalRidesByFleetOwnerPerVehicle :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> Text -> UTCTime -> UTCTime -> m Int
totalRidesByFleetOwnerPerVehicle fleetOwnerId vehicleNumber from to = do
  rideIds <- findIdsByFleetOwnerAndVehicle fleetOwnerId vehicleNumber from to
  getRidesByIdAndStatus (cast <$> rideIds) DRide.COMPLETED from to

totalRidesByFleetOwnerPerDriver :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> Id DP.Person -> UTCTime -> UTCTime -> m Int
totalRidesByFleetOwnerPerDriver fleetOwnerId driverId from to = do
  rideIds <- findIdsByFleetOwner fleetOwnerId from to
  getCompletedRidesByDriver (cast <$> rideIds) driverId from to

totalRidesByFleetOwnerPerVehicleAndDriver :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> Text -> Id DP.Person -> UTCTime -> UTCTime -> m Int
totalRidesByFleetOwnerPerVehicleAndDriver fleetOwnerId vehicleNumber driverId from to = do
  rideIds <- findIdsByFleetOwnerAndVehicle fleetOwnerId vehicleNumber from to
  getCompletedRidesByDriver (cast <$> rideIds) driverId from to

totalEarningsByFleetOwnerPerVehicleAndDriver :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> Text -> Id DP.Person -> UTCTime -> UTCTime -> m Int
totalEarningsByFleetOwnerPerVehicleAndDriver fleetOwnerId vehicleNumber driverId from to = do
  rideIds <- findIdsByFleetOwnerAndVehicle fleetOwnerId vehicleNumber from to
  getEarningsByDriver (cast <$> rideIds) driverId from to

totalRidesStatsInFleet :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> UTCTime -> UTCTime -> m (Int, Int, Int, Int)
totalRidesStatsInFleet fleetOwnerId from to = do
  rideIds <- findIdsByFleetOwner fleetOwnerId from to
  rideStats <- getCompletedRidesStatsByIdsAndDriverId (cast <$> rideIds) Nothing from to (length rideIds) 0 Nothing Nothing
  let (totalEarning, totalDistanceTravelled, completedRides, cancelledRides, _) = getFleetStats rideStats
  pure (totalEarning, totalDistanceTravelled, completedRides, cancelledRides)

fleetStatsByDriver :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => [Id RideDetails.RideDetails] -> Maybe (Id DP.Person) -> UTCTime -> UTCTime -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Common.SortOn -> m [RideStats]
fleetStatsByDriver rideIds mbDriverId from to mbLimit mbOffset mbSortDesc mbSortOn = do
  let limit = min 10 $ fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  getCompletedRidesStatsByIdsAndDriverId (cast <$> rideIds) mbDriverId from to limit offset mbSortDesc mbSortOn

fleetStatsByVehicle :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Text -> Text -> UTCTime -> UTCTime -> m (Int, Int, Int, Int, Int)
fleetStatsByVehicle fleetOwnerId vehicleNumber from to = do
  rideIds <- findIdsByFleetOwnerAndVehicle (Just fleetOwnerId) vehicleNumber from to
  rideStats <- fleetStatsByDriver rideIds Nothing from to Nothing Nothing Nothing Nothing
  pure $ getFleetStats rideStats

mkRideStatsByRideStatus :: (Maybe Int, Maybe Int, Int, Int, Int, Maybe (Id DP.Person)) -> RideStats
mkRideStatsByRideStatus (earnings, distanceTravelled, completedRides_, cancelledRides_, duration, driverId) = RideStats {..}
  where
    totalEarnings = fromMaybe 0 earnings
    totalDistanceTravelled = fromMaybe 0 distanceTravelled
    completedRides = completedRides_
    cancelledRides = cancelledRides_
    totalDuration = duration
    driverId' = driverId

getFleetStats :: [RideStats] -> (Int, Int, Int, Int, Int)
getFleetStats rideStats =
  let totalEarnings = sum $ map (.totalEarnings) rideStats
      totalDistanceTravelled = sum $ map (.totalDistanceTravelled) rideStats
      completedRides = sum $ map (.completedRides) rideStats
      cancelledRides = sum $ map (.cancelledRides) rideStats
      totalDuration = sum $ map (.totalDuration) rideStats
   in (totalEarnings, totalDistanceTravelled, completedRides, cancelledRides, totalDuration)

getAllCompletedRidesByDriverId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DP.Person ->
  UTCTime ->
  UTCTime ->
  m [Ride]
getAllCompletedRidesByDriverId driverId from to =
  CH.findAll $
    CH.select_ (\rd -> CH.notGrouped rd) $
      CH.filter_
        ( \ride ->
            ride.status CH.==. Just DRide.COMPLETED
              CH.&&. ride.createdAt >=. from
              CH.&&. ride.createdAt <=. to
              CH.&&. ride.driverId CH.==. Just (cast driverId)
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE rideTTable)
