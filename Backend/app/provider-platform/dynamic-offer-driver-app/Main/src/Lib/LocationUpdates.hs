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

import Data.List (elemIndex)
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
import qualified SharedLogic.TollsDetector as TollsDetector
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
getEstimateSliceDistance (Just ride) batchWaypoints routeDeviationThreshold =
  if (length batchWaypoints) <= 2
    then do
      return $ highPrecMetersToMeters $ getRouteLinearLength batchWaypoints
    else do
      let rideId = ride.id
      transactionId <- getTransactionId ride
      let key = multipleRouteKey transactionId
      multipleRoutes :: Maybe [RI.RouteAndDeviationInfo] <- Redis.get key
      case multipleRoutes of
        Just routes -> checkDeviationAndGetDistance routes
        Nothing -> do
          logError $ "MultipleRoutes not found for ride" <> show rideId
          return $ highPrecMetersToMeters $ getRouteLinearLength batchWaypoints
  where
    checkDeviationAndGetDistance [] = do
      logError "Returning route linear length, no route matched"
      return $ highPrecMetersToMeters $ getRouteLinearLength batchWaypoints
    checkDeviationAndGetDistance (route : rs) = do
      case route.routeInfo.points of
        Just pts -> do
          let deviated = checkForDeviation routeDeviationThreshold pts batchWaypoints 0
          if deviated
            then checkDeviationAndGetDistance rs
            else do
              return $ getFinalDistance pts
        Nothing -> checkDeviationAndGetDistance rs
    getFinalDistance pts =
      let firstWaypoint = head batchWaypoints
          lastWayPoint = head (reverse batchWaypoints)
          sliceStartIndex = getNearestPointIndex firstWaypoint pts
          sliceEndIndex = getNearestPointIndex lastWayPoint pts
          slicedDistance = getSlicedDistance (sliceStartIndex + 1) (sliceEndIndex -1) pts
       in if slicedDistance == 0
            then highPrecMetersToMeters $ getRouteLinearLength batchWaypoints
            else
              let valueS = getSliceDistanceCorrection sliceStartIndex firstWaypoint pts True
                  valueE = getSliceDistanceCorrection sliceEndIndex lastWayPoint pts False
               in slicedDistance + valueS + valueE

getNearestPointIndex :: LatLong -> [LatLong] -> Int
getNearestPointIndex pt routePoints =
  let distanceList = map (\currPoint -> distanceBetweenInMeters currPoint pt) routePoints
      nearestIndex = elemIndex (minimum distanceList) distanceList
   in (fromMaybe (-1) nearestIndex) --safe fromMaybe because we are searching minimum element in the list

--What is there are two nearest points to start and end point?
--So the slice can be different route than what driver might have travelled on
--To solve this we can invalide estimate route points starting se to get more accurate and correct near point

getSlicedDistance :: Int -> Int -> [LatLong] -> Meters
getSlicedDistance startIndex endIndex routePoints =
  let lengthOfRoute = length routePoints
      isIndexInRange = (isWithinIndexRange startIndex lengthOfRoute) && (isWithinIndexRange endIndex lengthOfRoute)
   in if startIndex >= endIndex || not isIndexInRange
        then 0
        else highPrecMetersToMeters $ getRouteLinearLength (sliceIt startIndex endIndex routePoints)
  where
    sliceIt startind endind xs = take (endind - startind + 1) (drop startind xs)
    isWithinIndexRange ind lengthOfObject = (ind >= 0) && (ind < lengthOfObject)

getSliceDistanceCorrection :: Int -> LatLong -> [LatLong] -> Bool -> Meters
getSliceDistanceCorrection index waypoint routePoints isStartPoint
  | index == 0 = getCorrectionValue index (index + 1)
  | index == ((length routePoints) - 1) = getCorrectionValue index (index -1)
  | otherwise =
    let valueLeft = getCorrectionValue index (index -1)
        valueRight = getCorrectionValue index (index + 1)
     in (max valueLeft valueRight)
  where
    getCorrectionValue sInd eInd =
      let (startPoint, endPoint) = ((routePoints !! sInd), (routePoints !! eInd))
          (isPointPerpendicular, intersectionPoint) = checkPerpendicular startPoint endPoint waypoint
       in if not isPointPerpendicular
            then 0
            else
              if isStartPoint
                then highPrecMetersToMeters $ (distanceBetweenInMeters intersectionPoint startPoint) + (distanceBetweenInMeters startPoint (routePoints !! (sInd + 1)))
                else highPrecMetersToMeters $ (distanceBetweenInMeters intersectionPoint startPoint) + (distanceBetweenInMeters startPoint (routePoints !! (sInd - 1)))

-- Ref: https://www.geeksforgeeks.org/point-of-intersection-of-two-lines-formula/
-- Here we are checking if the waypoint is perpendicular on the line being formed by startpoint and endpoint
-- Product of slope of two perpendicular lines is -1
checkPerpendicular :: LatLong -> LatLong -> LatLong -> (Bool, LatLong)
checkPerpendicular startpoint endpoint waypoint =
  let (x1, y1, x2, y2, x3, y3) = (startpoint.lat, startpoint.lon, endpoint.lat, endpoint.lon, waypoint.lat, waypoint.lon)
      m = (x2 - x1) / (y2 - y1)
      m' = (-1.0) / m
      (a1, b1, c1, a2, b2, c2) = (- m, 1.0, m * x1 - y1, - m', 1, m' * x3 - y3)
      x4 = (b1 * c2 - b2 * c1) / (c1 * a2 - c2 * a1)
      y4 = (c1 * a2 - c2 * a1) / (a1 * b2 - a2 * b1)
      intersectionPoint = LatLong x4 y4
      distanceSumStartIntersectionEnd = highPrecMetersToMeters $ (distanceBetweenInMeters startpoint intersectionPoint) + (distanceBetweenInMeters endpoint intersectionPoint)
      distanceStartEnd = highPrecMetersToMeters $ distanceBetweenInMeters startpoint endpoint
   in if abs (distanceSumStartIntersectionEnd - distanceStartEnd) <= 10 then (True, intersectionPoint) else (False, intersectionPoint)

buildRideInterpolationHandler :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Flow (RideInterpolationHandler Person Flow)
buildRideInterpolationHandler merchantId merchantOpCityId isEndRide = do
  transportConfig <- MTC.findByMerchantOpCityId merchantOpCityId Nothing Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getLocalCurrentTime transportConfig.timeDiffFromUtc
  let snapToRoad' shouldRectifyDistantPointsFailure =
        if transportConfig.useWithSnapToRoadFallback
          then TMaps.snapToRoadWithFallback shouldRectifyDistantPointsFailure merchantId merchantOpCityId
          else snapToRoadWithService
      enableNightSafety = (not isEndRide) && (checkNightSafetyTimeConstraint transportConfig now)
  return $
    mkRideInterpolationHandler
      isEndRide
      (\driverId dist googleSnapCalls osrmSnapCalls -> void (QRide.updateDistance driverId dist googleSnapCalls osrmSnapCalls))
      (\driverId tollCharges -> void (QRide.updateTollCharges driverId tollCharges))
      ( \driverId batchWaypoints -> do
          ride <- QRide.getActiveByDriverId driverId
          updateRouteDeviation transportConfig.routeDeviationThreshold ride batchWaypoints
      )
      (TollsDetector.getTollChargesOnRoute merchantOpCityId)
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
      transportConfig.recomputeIfPickupDropNotOutsideOfThreshold
      transportConfig.enableEstimateSlicing
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
