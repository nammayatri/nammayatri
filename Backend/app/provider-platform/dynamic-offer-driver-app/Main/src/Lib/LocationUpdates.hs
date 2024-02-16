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
import Domain.Types.Merchant.TransporterConfig
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
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRiderDetails
import Tools.Error
import qualified Tools.Maps as TMaps

getDeviationForPoint :: LatLong -> [LatLong] -> Meters
getDeviationForPoint pt estimatedRoute =
  highPrecMetersToMeters (minimum $ map (distanceBetweenInMeters pt) estimatedRoute)

getMaxDeviation :: [LatLong] -> [LatLong] -> Meters -> Meters -> Meters -> Meters
getMaxDeviation _ [] _ _ acc = acc
getMaxDeviation estimatedRoute (pt : batchWaypoints) pt1 pt2 acc = do
  let currDeviation = getDeviationForPoint pt estimatedRoute
      currentWindowDeviation = min pt1 (min pt2 currDeviation)
  getMaxDeviation estimatedRoute batchWaypoints pt2 currDeviation (max acc currentWindowDeviation)

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

updateDeviation :: TransporterConfig -> Bool -> Maybe Ride -> [LatLong] -> Flow Bool
updateDeviation _ _ Nothing _ = do
  logInfo "No ride found to check deviation"
  return False
updateDeviation transportConfig safetyCheckEnabled (Just ride) batchWaypoints = do
  let rideId = ride.id
  (alreadyDeviated, safetyAlertAlreadyTriggered) <- getDeviationAndSafetyDetails ride
  if safetyAlertAlreadyTriggered && alreadyDeviated
    then do
      logInfo $ "Safety alert and deviation already triggered for rideId: " <> getId rideId
      return True
    else do
      let routeDeviationThreshold = transportConfig.routeDeviationThreshold
          nightSafetyRouteDeviationThreshold = transportConfig.nightSafetyRouteDeviationThreshold
          key = multipleRouteKey $ getId rideId
          shouldPerformSafetyCheck = safetyCheckEnabled && not safetyAlertAlreadyTriggered
      multipleRoutes :: Maybe [RI.RouteAndDeviationInfo] <- Redis.get key
      case multipleRoutes of
        Just routes -> do
          isRouteDeviated <- checkMultipleRoutesForDeviation routes batchWaypoints routeDeviationThreshold nightSafetyRouteDeviationThreshold rideId shouldPerformSafetyCheck
          updateRouteDeviationDetails isRouteDeviated alreadyDeviated
        Nothing -> do
          isRouteDeviated <- checkForDeviationInSingleRoute batchWaypoints routeDeviationThreshold nightSafetyRouteDeviationThreshold rideId
          updateRouteDeviationDetails isRouteDeviated alreadyDeviated
  where
    updateRouteDeviationDetails :: Bool -> Bool -> Flow Bool
    updateRouteDeviationDetails isRouteDeviated alreadyDeviated = do
      when (isRouteDeviated && not alreadyDeviated) $ do
        logInfo $ "Deviation detected for rideId: " <> getId ride.id
        QRide.updateDriverDeviatedFromRoute ride.id True
      return isRouteDeviated

    getDeviationAndSafetyDetails rideDetails = do
      let alreadyDeviated = fromMaybe False rideDetails.driverDeviatedFromRoute
      return (alreadyDeviated, rideDetails.safetyAlertTriggered)

checkMultipleRoutesForDeviation :: [RI.RouteAndDeviationInfo] -> [LatLong] -> Meters -> Meters -> Id Ride -> Bool -> Flow Bool
checkMultipleRoutesForDeviation routes batchWaypoints routeDeviationThreshold nightSafetyRouteDeviationThreshold rideId safetyCheckEnabled = do
  logInfo $ "Checking for deviation in multiple routes for rideId: " <> getId rideId
  let updatedRoutesInfo = map checkRouteForDeviation routes
  logInfo $ "Updated routes info for rideId: " <> getId rideId <> " is: " <> show updatedRoutesInfo
  setExp (multipleRouteKey $ getId rideId) updatedRoutesInfo 14400
  fork "Performing safety check" $
    when (safetyCheckEnabled && all (\route -> RI.safetyDeviation $ RI.deviationInfo route) updatedRoutesInfo) $ performSafetyCheck rideId
  return $ all (\route -> RI.deviation $ RI.deviationInfo route) updatedRoutesInfo
  where
    checkRouteForDeviation :: RI.RouteAndDeviationInfo -> RI.RouteAndDeviationInfo
    checkRouteForDeviation route@RI.RouteAndDeviationInfo {..} =
      case routeInfo.points of
        Nothing -> route
        Just points ->
          if not safetyCheckEnabled
            then
              route
                { RI.deviationInfo =
                    RI.DeviationInfo
                      { deviation =
                          deviationInfo.deviation || checkForDeviation routeDeviationThreshold points batchWaypoints 0,
                        safetyDeviation = False
                      }
                }
            else do
              case (deviationInfo.safetyDeviation, deviationInfo.deviation) of
                (True, _) -> route
                (False, True) -> do
                  let isSafetyDeviated = checkForDeviation nightSafetyRouteDeviationThreshold points batchWaypoints 0
                  route
                    { RI.deviationInfo =
                        RI.DeviationInfo
                          { deviation = True,
                            safetyDeviation = isSafetyDeviated
                          }
                    }
                (False, False) -> do
                  let deviation = getMaxDeviation points batchWaypoints 0 0 0
                  route
                    { RI.deviationInfo =
                        RI.DeviationInfo
                          { deviation = deviation >= routeDeviationThreshold,
                            safetyDeviation = deviation >= nightSafetyRouteDeviationThreshold
                          }
                    }

checkForDeviationInSingleRoute :: [LatLong] -> Meters -> Meters -> Id Ride -> Flow Bool
checkForDeviationInSingleRoute batchWaypoints routeDeviationThreshold nightSafetyRouteDeviationThreshold rideId = do
  logInfo $ "Checking for deviation in single route for rideId: " <> getId rideId
  let key = searchRequestKey (getId rideId)
  mbRouteInfo :: Maybe RI.RouteInfo <- Redis.get key
  case mbRouteInfo of
    Just routeInfo -> do
      let multipleRoutesEntity =
            [ RI.RouteAndDeviationInfo
                { routeInfo = routeInfo,
                  deviationInfo = RI.DeviationInfo {deviation = False, safetyDeviation = False}
                }
            ]
      checkMultipleRoutesForDeviation multipleRoutesEntity batchWaypoints routeDeviationThreshold nightSafetyRouteDeviationThreshold rideId False
    Nothing -> do
      logWarning $ "Ride route info not found for rideId: " <> getId rideId
      return False

getTravelledDistance :: (CacheFlow m r) => Maybe (Id Ride) -> Meters -> m Meters
getTravelledDistance Nothing _ = do
  logInfo "No ride found to get travelled distance"
  return 0
getTravelledDistance (Just rideId) estimatedDistance = do
  let key = multipleRouteKey $ getId rideId
  multipleRoutes :: Maybe [RI.RouteAndDeviationInfo] <- Redis.get key
  case multipleRoutes of
    Just routes -> do
      let distance = minimum $ map (RI.distance . RI.routeInfo) (filter (not . RI.deviation . RI.deviationInfo) routes)
      return $ fromMaybe estimatedDistance distance
    Nothing -> do
      logInfo $ "MultipleRoutes not found for ride" <> show rideId
      return estimatedDistance

buildRideInterpolationHandler :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Flow (RideInterpolationHandler Person Flow)
buildRideInterpolationHandler merchantId merchantOpCityId isEndRide = do
  transportConfig <- MTC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getLocalCurrentTime transportConfig.timeDiffFromUtc
  let snapToRoad' =
        if transportConfig.useWithSnapToRoadFallback
          then TMaps.snapToRoadWithFallback merchantId merchantOpCityId
          else snapToRoadWithService
      enableNightSafety = checkNightSafetyTimeConstraint transportConfig now
  return $
    mkRideInterpolationHandler
      isEndRide
      (\driverId dist googleSnapCalls osrmSnapCalls -> void (QRide.updateDistance driverId dist googleSnapCalls osrmSnapCalls))
      ( \driverId batchWaypoints -> do
          ride <- QRide.getActiveByDriverId driverId
          updateDeviation transportConfig enableNightSafety ride batchWaypoints
      )
      ( \driverId estimatedDistance -> do
          mRide <- QRide.getInProgressOrNewRideIdAndStatusByDriverId driverId
          getTravelledDistance (mRide <&> fst) estimatedDistance
      )
      snapToRoad'
  where
    snapToRoadWithService req = do
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

performSafetyCheck :: Id Ride -> Flow ()
performSafetyCheck rideId = do
  logInfo $ "Performing safety check for rideId: " <> getId rideId
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  when (not ride.safetyAlertTriggered) $ do
    booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
    riderId <-
      booking.riderId
        & fromMaybeM (BookingFieldNotPresent "riderId")
    riderDetails <- runInReplica $ QRiderDetails.findById riderId >>= fromMaybeM (RiderDetailsNotFound ride.id.getId)
    void $ QRide.updateSafetyAlertTriggered ride.id
    when riderDetails.nightSafetyChecks $ do
      BP.sendSafetyAlertToBAP booking ride "deviation" "Route Deviation Detected"
