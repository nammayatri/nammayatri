{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Allocation.Internal where

import qualified Data.Map as Map
import qualified Data.Time as Time
import qualified Data.Time.Calendar.OrdinalDate as Time
import Domain.Action.Allocation as Alloc
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Booking.BookingLocation as Loc
import Domain.Types.Merchant
import Domain.Types.Person (Driver)
import qualified Domain.Types.RideRequest as SRR
import qualified Domain.Types.Vehicle as Veh
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant.Client
import SharedLogic.DriverPool.Types
import Test.Tasty.HUnit
import Utils.GuidGenerator ()
import Utils.SilentLogger ()

type NotificationStatusMap = (Map (Id SRB.Booking, Id Driver) (NotificationStatus, UTCTime))

org1 :: ShortId Subscriber
org1 = ShortId "Org1"

numRequestsToProcess :: Integer
numRequestsToProcess = 10

allocationTime :: Seconds
allocationTime = 3

notificationTime :: Seconds
notificationTime = 1

reallocationsLimit :: Int
reallocationsLimit = 3

isNotified :: UTCTime -> (NotificationStatus, UTCTime) -> Bool
isNotified _ (Notified, _) = True
isNotified _ _ = False

attemptedNotification ::
  Id SRB.Booking ->
  (Id SRB.Booking, Id Driver) ->
  (NotificationStatus, UTCTime) ->
  Bool
attemptedNotification bookingId (id, _) (status, _) =
  id == bookingId && status `elem` [Rejected, Ignored]

details :: SRB.OneWayBookingDetails
details =
  SRB.OneWayBookingDetails
    { toLocation = bookingStopLocation,
      estimatedDistance = 20000,
      estimatedFinishTime = defaultTime,
      estimatedDuration = 1200
    }

defaultLocationAddress :: Loc.LocationAddress
defaultLocationAddress =
  Loc.LocationAddress
    { street = Nothing,
      city = Nothing,
      state = Nothing,
      country = Nothing,
      building = Nothing,
      areaCode = Nothing,
      area = Nothing
    }

defaultTime :: Time.UTCTime
defaultTime =
  Time.UTCTime
    { utctDay = Time.fromOrdinalDate 2020 120,
      utctDayTime = Time.secondsToDiffTime 40000
    }

defaultUrl :: BaseUrl
defaultUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "api.example.com",
      baseUrlPort = 80,
      baseUrlPath = ""
    }

defaultBookingLocation :: Loc.BookingLocation
defaultBookingLocation =
  Loc.BookingLocation
    { id = "1",
      lat = 9.96,
      lon = 9.96,
      address = defaultLocationAddress,
      createdAt = defaultTime,
      updatedAt = defaultTime
    }

bookingStopLocation :: Loc.BookingLocation
bookingStopLocation =
  defaultBookingLocation
    { Loc.lat = 10.02,
      Loc.lon = 10.02
    }

defaultBooking :: SRB.Booking
defaultBooking =
  SRB.Booking
    { id = Id "1",
      transactionId = "",
      status = SRB.CONFIRMED,
      providerId = Id "",
      bapId = "",
      bapUri = defaultUrl,
      startTime = defaultTime,
      riderId = Just $ Id "",
      fromLocation = defaultBookingLocation,
      vehicleVariant = Veh.SUV,
      estimatedFare = 0,
      discount = Nothing,
      estimatedTotalFare = 0,
      reallocationsCount = 0,
      bookingDetails = SRB.OneWayDetails details,
      riderName = Just "John",
      createdAt = defaultTime,
      updatedAt = defaultTime
    }

addBooking :: Repository -> Id SRB.Booking -> Int -> IO ()
addBooking Repository {..} bookingId reallocationsCount = do
  currTime <- Time.getCurrentTime
  let booking =
        defaultBooking
          { SRB.id = bookingId,
            SRB.status = SRB.CONFIRMED,
            SRB.createdAt = currTime,
            SRB.reallocationsCount = reallocationsCount
          }
  modifyIORef bookingsVar $ Map.insert bookingId booking

updateBooking :: Repository -> Id SRB.Booking -> SRB.BookingStatus -> IO ()
updateBooking Repository {..} bookingId status =
  modifyIORef bookingsVar $ Map.adjust (\booking -> booking{status = status}) bookingId

addRequest :: RequestData -> Repository -> Id SRB.Booking -> IO ()
addRequest requestData Repository {..} bookingId = do
  currentId <- readIORef currentIdVar
  let requestId = Id $ show currentId
  let request =
        RideRequest
          { requestId = requestId,
            bookingId = bookingId,
            requestData = requestData
          }
  modifyIORef currentIdVar (+ 1)
  modifyIORef rideRequestsVar $ Map.insert requestId request

addResponse :: Repository -> Id SRB.Booking -> Id Driver -> Alloc.Response -> IO ()
addResponse repository bookingId driverId status = do
  let driverResponse = DriverResponseType driverId status
  addRequest (DriverResponse driverResponse) repository bookingId

mkDriverPoolResult :: Id Driver -> DriverPoolResult
mkDriverPoolResult driverId =
  DriverPoolResult
    { driverId = driverId,
      distanceToPickup = 100,
      durationToPickup = 600,
      variant = Veh.SUV,
      lat = 0,
      lon = 0
    }

addDriverPool :: Repository -> Map (Id SRB.Booking, PoolBatchNum) [Id Driver] -> IO ()
addDriverPool Repository {..} driversMap = do
  let driverPool = map mkDriverPoolResult <$> driversMap
  writeIORef driverPoolVar driverPool

checkRideStatus :: Repository -> Id SRB.Booking -> SRB.BookingStatus -> IO ()
checkRideStatus Repository {..} bookingId expectedStatus = do
  bookings <- readIORef bookingsVar
  case Map.lookup bookingId bookings of
    Nothing -> assertFailure $ "Booking " <> show (bookingId.getId) <> " not found"
    Just booking -> booking.status @?= expectedStatus

checkNotificationStatus :: Repository -> Id SRB.Booking -> Id Driver -> NotificationStatus -> IO ()
checkNotificationStatus Repository {..} bookingId driverId expectedStatus = do
  notifications <- readIORef notificationStatusVar
  case Map.lookup (bookingId, driverId) notifications of
    Nothing ->
      assertFailure $
        "Notification for bookingId=" <> show (bookingId.getId) <> " and driverId=" <> show (driverId.getId) <> " is not found"
    Just (status, _) -> status @?= expectedStatus

checkFreeNotificationStatus :: Repository -> Id SRB.Booking -> Id Driver -> IO ()
checkFreeNotificationStatus Repository {..} bookingId driverId = do
  notifications <- readIORef notificationStatusVar
  Map.lookup (bookingId, driverId) notifications @?= Nothing

addNotification ::
  Id SRB.Booking ->
  UTCTime ->
  NotificationStatusMap ->
  Id Driver ->
  NotificationStatusMap
addNotification bookingId expiryTime notificationStatuses driverId =
  Map.insert
    (bookingId, driverId)
    (Notified, expiryTime)
    notificationStatuses

updateNotification ::
  Id SRB.Booking ->
  NotificationStatus ->
  NotificationStatusMap ->
  Id Driver ->
  NotificationStatusMap
updateNotification bookingId nStatus notificationStatuses driverId =
  Map.adjust
    (\(_, expiryTime) -> (nStatus, expiryTime))
    (bookingId, driverId)
    notificationStatuses

toCurrentNotification :: ((Id SRB.Booking, Id Driver), (NotificationStatus, UTCTime)) -> CurrentNotification
toCurrentNotification ((_, driverId), (_, expiryTime)) =
  CurrentNotification driverId expiryTime

handle :: Repository -> ServiceHandle IO
handle repository@Repository {..} =
  ServiceHandle
    { getConfiguredNotificationTime = pure notificationTime,
      getConfiguredAllocationTime = pure allocationTime,
      getConfiguredReallocationsLimit = pure reallocationsLimit,
      getRequests = \_ numRides -> do
        rideRequests <- readIORef rideRequestsVar
        let requests = Map.elems rideRequests
        pure $ take (fromIntegral numRides) requests,
      isBatchNumExceedLimit = \_ -> return False,
      getNextDriverPoolBatch = \bookingId -> do
        bnmap <- readIORef batchNumVar
        let bnnum = fromMaybe 0 $ Map.lookup bookingId bnmap
        let newBnNum = bnnum + 1
        writeIORef batchNumVar $ Map.insert bookingId newBnNum bnmap

        -- rsmap <- readIORef radiusStepVar
        -- let rsnum = fromMaybe 0 $ Map.lookup bookingId rsmap

        poolMap <- readIORef driverPoolVar
        let pool = fromMaybe [] $ Map.lookup (bookingId, bnnum) poolMap
        pure pool, -- TODO: real code much more complex, mb we need to create DriverPoolHandler too to test it?
      cleanupDriverPoolBatches = \bookingId -> do
        rsmap <- readIORef radiusStepVar
        writeIORef radiusStepVar $ Map.delete bookingId rsmap
        bnmap <- readIORef batchNumVar
        writeIORef batchNumVar $ Map.delete bookingId bnmap, -- it doesn't clean batches Map, which is not equals to main code
      sendNewRideNotifications = \_ _ -> pure (),
      getCurrentNotifications = \rideId -> do
        notificationStatus <- readIORef notificationStatusVar
        let filtered =
              Map.toList $
                Map.filterWithKey
                  (\(id, _) (notification, _) -> id == rideId && notification == Notified)
                  notificationStatus
        pure $ map toCurrentNotification filtered,
      cleanupOldNotifications = do
        compareTime <- Time.getCurrentTime <&> Time.addUTCTime (-300)
        modifyIORef notificationStatusVar $
          Map.filter (\(_, expiryTime) -> compareTime < expiryTime)
        return 0,
      addNotificationStatuses = \rideId driverIds expiryTime ->
        modifyIORef notificationStatusVar $
          \notificationStatuses ->
            foldl' (addNotification rideId expiryTime) notificationStatuses driverIds,
      updateNotificationStatuses = \rideId nStatus driverIds ->
        modifyIORef notificationStatusVar $
          \notificationStatuses ->
            foldl' (updateNotification rideId nStatus) notificationStatuses driverIds,
      resetLastRejectionTimes = \_ -> pure (),
      getDriversWithNotification = do
        notificationStatus <- readIORef notificationStatusVar
        let filtered = fmap snd $ Map.keys $ Map.filter (\(status, _) -> status == Notified) notificationStatus
        pure filtered,
      assignDriver = \bookingId driverId -> do
        modifyIORef assignmentsVar $ (:) (bookingId, driverId)
        modifyIORef bookingsVar $ Map.adjust (#status .~ SRB.TRIP_ASSIGNED) bookingId
        modifyIORef onRideVar $ (:) driverId,
      cancelBooking = \bookingId _ -> do
        modifyIORef bookingsVar $ Map.adjust (#status .~ SRB.CANCELLED) bookingId
        assignments <- readIORef assignmentsVar
        let driversForBookingId = map snd $ filter (\(rbId, _) -> bookingId == rbId) assignments
        modifyIORef onRideVar $ filter (`notElem` driversForBookingId),
      cleanupNotifications = \rideId ->
        modifyIORef notificationStatusVar $ Map.filterWithKey (\(r, _) _ -> r /= rideId),
      getTopDriversByIdleTime = \count driverIds -> pure $ take count driverIds,
      checkAvailability = \driversPool -> do
        onRide <- readIORef onRideVar
        pure $ filter (\item -> item.driverId `notElem` onRide) driversPool,
      sendRideNotAssignedNotification = \_ _ -> pure (),
      removeRequest = modifyIORef rideRequestsVar . Map.delete,
      addAllocationRequest = \_ -> addRequest Allocation repository,
      getBooking = \bookingId -> do
        bookings <- readIORef bookingsVar
        case Map.lookup bookingId bookings of
          Just booking -> pure booking
          Nothing -> assertFailure $ "Booking " <> show bookingId <> " not found in the map.",
      logEvent = \_ _ -> pure (),
      logDriverEvents = \_ _ _ -> pure (),
      metrics =
        AllocatorMetricsHandle
          { incrementTaskCounter = \_ -> return (),
            incrementFailedTaskCounter = \_ -> return (),
            putTaskDuration = \_ _ -> return (),
            incrementErrorCounter = \_ -> return ()
          }
    }

data Repository = Repository
  { currentIdVar :: IORef Int,
    batchNumVar :: IORef (Map (Id SRB.Booking) PoolBatchNum),
    radiusStepVar :: IORef (Map (Id SRB.Booking) PoolRadiusStep),
    driverPoolVar :: IORef (Map (Id SRB.Booking, PoolBatchNum) [DriverPoolResult]),
    bookingsVar :: IORef (Map (Id SRB.Booking) SRB.Booking),
    rideRequestsVar :: IORef (Map (Id SRR.RideRequest) RideRequest),
    notificationStatusVar :: IORef NotificationStatusMap,
    assignmentsVar :: IORef [(Id SRB.Booking, Id Driver)],
    onRideVar :: IORef [Id Driver]
  }

printRepContent :: Repository -> IO ()
printRepContent Repository {..} = do
  currentId <- readIORef currentIdVar
  print @Text $ "currentIdVar: " <> show currentId
  batchNum <- readIORef batchNumVar
  print @Text $ "batchNumVar: " <> show batchNum
  radiusStep <- readIORef radiusStepVar
  print @Text $ "radiusStepVar: " <> show radiusStep
  driverPool <- readIORef driverPoolVar
  print @Text $ "driverPoolVar: " <> show driverPool
  bookings <- readIORef bookingsVar
  print @Text $ "bookingsVar: " <> show (bookings <&> (.id))
  rideRequests <- readIORef rideRequestsVar
  print @Text $ "rideRequestsVar: " <> show rideRequests
  notificationStatus <- readIORef notificationStatusVar
  print @Text $ "notificationStatusVar: " <> show notificationStatus
  assignments <- readIORef assignmentsVar
  print @Text $ "assignmentsVar: " <> show assignments
  onRide <- readIORef onRideVar
  print @Text $ "onRideVar: " <> show onRide

initRepository :: IO Repository
initRepository = do
  initCurrentId <- newIORef 1
  initBatchNum <- newIORef Map.empty
  initRadiusStep <- newIORef Map.empty
  initDriverPool <- newIORef Map.empty
  initRides <- newIORef Map.empty
  initRideRequest <- newIORef Map.empty
  initNotificationStatus <- newIORef Map.empty
  initAssignments <- newIORef []
  initOnRide <- newIORef []
  let repository =
        Repository
          initCurrentId
          initBatchNum
          initRadiusStep
          initDriverPool
          initRides
          initRideRequest
          initNotificationStatus
          initAssignments
          initOnRide
  pure repository
