{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.LocationUpdates
  ( module Reexport,
    whenWithLocationUpdatesLock,
    buildRideInterpolationHandler,
  )
where

import Data.Time hiding (secondsToNominalDiffTime)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Person
import Domain.Types.Ride
import qualified Domain.Types.RideRoute as RI
import Environment
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance
import Kernel.Utils.Common
import "location-updates" Lib.LocationUpdates as Reexport
import qualified SharedLogic.CallBAP as BP
import SharedLogic.Ride
import qualified Storage.CachedQueries.Merchant.TransporterConfig as MTC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRiderDetails
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error
import qualified Tools.Maps as TMaps

isWithinTolerance :: LatLong -> [LatLong] -> Meters -> Bool
isWithinTolerance pt estimatedRoute routeDeviationThreshold = do
  let minDistance = highPrecMetersToMeters (minimum $ map (distanceBetweenInMeters pt) estimatedRoute)
   in minDistance <= routeDeviationThreshold

checkForDeviation :: Meters -> [LatLong] -> [LatLong] -> Int -> Bool
checkForDeviation _ _ [] deviationCount
  | deviationCount >= 3 = True
  | otherwise = False
checkForDeviation routeDeviationThreshold estimatedRoute (pt : batchWaypoints) deviationCount
  | deviationCount >= 3 = True
  | otherwise = do
    if isWithinTolerance pt estimatedRoute routeDeviationThreshold
      then checkForDeviation routeDeviationThreshold estimatedRoute batchWaypoints 0
      else checkForDeviation routeDeviationThreshold estimatedRoute batchWaypoints (deviationCount + 1)

getTransactionId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Ride -> m Text
getTransactionId ride =
  case ride.transactionId of
    Just tid -> return tid
    Nothing -> do
      booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
      return booking.transactionId

updateSafetyDeviation :: Meters -> Bool -> Maybe Ride -> [LatLong] -> Flow ()
updateSafetyDeviation _ _ Nothing _ = logInfo "No ride found to check deviation"
updateSafetyDeviation safetyThreshold safetyCheckEnabled (Just ride) batchWaypoints = do
  when safetyCheckEnabled $ do
    let rideId = ride.id
    transactionId <- getTransactionId ride
    multipleRoutes :: Maybe [RI.RouteAndDeviationInfo] <- Redis.get $ multipleRouteKey transactionId
    case multipleRoutes of
      Just routes -> do
        let updatedRoutes = map checkRouteForSafetyDeviation routes
        let isSafetyDeviated = all (\route -> RI.safetyDeviation route) updatedRoutes
        Redis.setExp (multipleRouteKey transactionId) updatedRoutes 14400
        fork "Performing safety check" $ do
          when isSafetyDeviated $ performSafetyCheck ride
      Nothing -> do
        logError $ "Multiple routes not found for rideid: " <> getId rideId
  where
    checkRouteForSafetyDeviation routeAndDeviationInfo =
      case routeAndDeviationInfo.routeInfo.points of
        Nothing -> routeAndDeviationInfo
        Just points ->
          routeAndDeviationInfo
            { RI.safetyDeviation = routeAndDeviationInfo.safetyDeviation || (checkForDeviation safetyThreshold points batchWaypoints 0)
            }

updateRouteDeviation :: Meters -> Maybe Ride -> [LatLong] -> Flow Bool
updateRouteDeviation _ Nothing _ = do
  logError "No ride found to check deviation"
  return False
updateRouteDeviation routeDeviationThreshold (Just ride) batchWaypoints = do
  let rideId = ride.id
  transactionId <- getTransactionId ride
  multipleRoutes :: Maybe [RI.RouteAndDeviationInfo] <- Redis.get $ multipleRouteKey transactionId
  case multipleRoutes of
    Just routes ->
      case ride.driverDeviatedFromRoute of
        Just True -> return True
        _ -> do
          let updatedRoutes = map checkRouteForDeviation routes
          let isDeviated = all (\route -> RI.routeDeviation route) updatedRoutes
          Redis.setExp (multipleRouteKey transactionId) updatedRoutes 14400
          when isDeviated $ do
            QRide.updateDriverDeviatedFromRoute rideId True
          return isDeviated
    Nothing -> do
      logError $ "Multiple routes not found for rideid: " <> getId rideId
      return False
  where
    checkRouteForDeviation routeAndDeviationInfo =
      case routeAndDeviationInfo.routeInfo.points of
        Nothing -> routeAndDeviationInfo
        Just points ->
          routeAndDeviationInfo
            { RI.routeDeviation = routeAndDeviationInfo.routeDeviation || (checkForDeviation routeDeviationThreshold points batchWaypoints 0)
            }

checkBatchForDeviation :: Meters -> Maybe Ride -> [LatLong] -> Flow Bool
checkBatchForDeviation _ Nothing _ = do
  logError "No ride found to check deviation"
  return False
checkBatchForDeviation routeDeviationThreshold (Just ride) batchWaypoints = do
  let rideId = ride.id
  transactionId <- getTransactionId ride
  multipleRoutes :: Maybe [RI.RouteAndDeviationInfo] <- Redis.get $ multipleRouteKey transactionId
  case multipleRoutes of
    Just routes -> do
      let isDeviated = all (\route -> checkBatch route) routes
      return isDeviated
    Nothing -> do
      logError $ "Multiple routes not found for rideid: " <> getId rideId
      return False
  where
    checkBatch routeAndDeviationInfo =
      case routeAndDeviationInfo.routeInfo.points of
        Nothing -> False
        Just points -> checkForDeviation routeDeviationThreshold points batchWaypoints 0

getTravelledDistance :: Maybe Ride -> Meters -> Flow Meters
getTravelledDistance Nothing _ = do
  logInfo "No ride found to get travelled distance"
  return 0
getTravelledDistance (Just ride) estimatedDistance = do
  let rideId = ride.id
  transactionId <- getTransactionId ride
  let key = multipleRouteKey transactionId
  multipleRoutes :: Maybe [RI.RouteAndDeviationInfo] <- Redis.get key
  case multipleRoutes of
    Just routes -> do
      let undeviatedRoute = find (not . RI.routeDeviation) routes
      case undeviatedRoute of
        Just route -> do
          let distance = RI.distance $ RI.routeInfo route
          return $ fromMaybe estimatedDistance distance
        Nothing -> do
          logInfo $ "UndeviatedRoute not found for ride" <> show rideId
          return estimatedDistance
    Nothing -> do
      logInfo $ "MultipleRoutes not found for ride" <> show rideId
      return estimatedDistance

getEstimateSliceDistance :: Maybe Ride -> [LatLong] -> Meters -> Flow Meters
getEstimateSliceDistance Nothing batchWaypoints _ = do
  logError "No ride found to get sliced estimated distance"
  return $ highPrecMetersToMeters $ getRouteLinearLength batchWaypoints
getEstimateSliceDistance (Just ride) batchWaypoints routeDeviationThreshold = do
  let rideId = ride.id
  transactionId <- getTransactionId ride
  let key = multipleRouteKey transactionId
  multipleRoutes :: Maybe [RI.RouteAndDeviationInfo] <- Redis.get key
  case multipleRoutes of
    Just routes -> checkDeviationAndGetDistance routes batchWaypoints
    Nothing -> do
      logError $ "MultipleRoutes not found for ride" <> show rideId
      return $ highPrecMetersToMeters $ getRouteLinearLength batchWaypoints
  where
    checkDeviationAndGetDistance _ [] = return 0
    checkDeviationAndGetDistance [] _ = do
      logError "Returning route linear length, no route matched"
      return $ highPrecMetersToMeters $ getRouteLinearLength batchWaypoints
    checkDeviationAndGetDistance (route : rs) wps = do
      case route.routeInfo.points of
        Just pts -> do
          let deviated = checkForDeviation routeDeviationThreshold pts wps 0
          if deviated
            then checkDeviationAndGetDistance rs wps
            else do
              getFinalDistance pts wps
        Nothing -> checkDeviationAndGetDistance rs wps
    getFinalDistance _ _ = return 0 --Implement this

buildRideInterpolationHandler :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Flow (RideInterpolationHandler Person Flow)
buildRideInterpolationHandler merchantId merchantOpCityId isEndRide = do
  transportConfig <- MTC.findByMerchantOpCityId merchantOpCityId Nothing Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getLocalCurrentTime transportConfig.timeDiffFromUtc
  let snapToRoad' =
        if transportConfig.useWithSnapToRoadFallback
          then TMaps.snapToRoadWithFallback merchantId merchantOpCityId
          else snapToRoadWithService
      enableNightSafety = (not isEndRide) && (checkNightSafetyTimeConstraint transportConfig now)
  return $
    mkRideInterpolationHandler
      isEndRide
      (\driverId dist googleSnapCalls osrmSnapCalls -> void (QRide.updateDistance driverId dist googleSnapCalls osrmSnapCalls))
      ( \driverId batchWaypoints -> do
          ride <- QRide.getActiveByDriverId driverId
          updateRouteDeviation transportConfig.routeDeviationThreshold ride batchWaypoints
      )
      ( \driverId batchWaypoints -> do
          ride <- QRide.getActiveByDriverId driverId
          updateSafetyDeviation transportConfig.nightSafetyRouteDeviationThreshold enableNightSafety ride batchWaypoints
      )
      ( \driverId batchWaypoints -> do
          ride <- QRide.getActiveByDriverId driverId
          checkBatchForDeviation transportConfig.routeDeviationThreshold ride batchWaypoints
      )
      ( \driverId estimatedDistance -> do
          ride <- QRide.getActiveByDriverId driverId
          getTravelledDistance ride estimatedDistance
      )
      ( \driverId batchWaypoints -> do
          ride <- QRide.getActiveByDriverId driverId
          getEstimateSliceDistance ride batchWaypoints transportConfig.routeDeviationThreshold
      )
      snapToRoad'
  where
    snapToRoadWithService _ req = do
      resp <- TMaps.snapToRoad merchantId merchantOpCityId req
      return ([Google], Right resp)

    checkNightSafetyTimeConstraint config now = do
      let time = timeToTimeOfDay $ utctDayTime now
      isTimeWithinBounds (secondsToTimeOfDay config.nightSafetyStartTime) (secondsToTimeOfDay config.nightSafetyEndTime) time

whenWithLocationUpdatesLock :: (HedisFlow m r, MonadMask m) => Id Person -> m () -> m ()
whenWithLocationUpdatesLock driverId f = do
  redisLockDriverId <- Redis.tryLockRedis lockKey 60
  if redisLockDriverId
    then do
      logDebug $ lockKey <> " Locked"
      finally
        f
        ( do
            Redis.unlockRedis lockKey
            logDebug $ "DriverId: " <> show driverId <> " Unlocked"
        )
    else do
      logDebug $ lockKey <> " unable to get lock"
      throwError (HitsLimitError 5)
  where
    lockKey = "DriverLocationUpdate:DriverId-" <> driverId.getId

performSafetyCheck :: Ride -> Flow ()
performSafetyCheck ride = do
  let rideId = ride.id
  logInfo $ "Performing safety check for rideId: " <> getId rideId
  when (not ride.safetyAlertTriggered) $ do
    booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
    riderId <-
      booking.riderId
        & fromMaybeM (BookingFieldNotPresent "riderId")
    riderDetails <- runInReplica $ QRiderDetails.findById riderId >>= fromMaybeM (RiderDetailsNotFound rideId.getId)
    void $ QRide.updateSafetyAlertTriggered rideId
    when riderDetails.nightSafetyChecks $ do
      driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
      BP.sendSafetyAlertToBAP booking ride "Route Deviation Detected" driver vehicle
