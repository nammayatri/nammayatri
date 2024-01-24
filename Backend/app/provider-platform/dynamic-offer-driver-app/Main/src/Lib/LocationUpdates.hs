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

updateDeviation :: TransporterConfig -> Bool -> Maybe (Id Ride) -> [LatLong] -> Flow Bool
updateDeviation _ _ Nothing _ = do
  logInfo "No ride found to check deviation"
  return False
updateDeviation transportConfig safetyCheckEnabled (Just rideId) batchWaypoints = do
  (alreadyDeviated, safetyAlertAlreadyTriggered) <- getDeviationAndSafetyDetails rideId
  if safetyAlertAlreadyTriggered
    then do
      logInfo $ "Safety alert already triggered for rideId: " <> getId rideId
      return True
    else do
      let routeDeviationThreshold = transportConfig.routeDeviationThreshold
          key = searchRequestKey (getId rideId)
      routeInfo :: Maybe RI.RouteInfo <- Redis.get key
      case routeInfo >>= (.points) of
        Just estimatedRoute -> do
          currentlyDeviated <-
            case (safetyCheckEnabled, alreadyDeviated) of
              (True, _) -> do
                let deviation = getMaxDeviation estimatedRoute batchWaypoints 0 0 0
                fork "Performing safety check" $
                  when (deviation >= transportConfig.nightSafetyRouteDeviationThreshold) $ performSafetyCheck rideId
                return $ deviation >= routeDeviationThreshold
              (False, False) -> return $ checkForDeviation routeDeviationThreshold estimatedRoute batchWaypoints 0
              (False, True) -> return True
          case (alreadyDeviated, currentlyDeviated) of
            (True, _) -> return True
            (False, True) -> do
              logInfo $ "Deviation detected for rideId: " <> getId rideId
              QRide.updateDriverDeviatedFromRoute rideId True
              return True
            (False, False) -> do
              logInfo $ "No deviation detected for rideId: " <> getId rideId
              return False
        Nothing -> do
          logWarning $ "Ride route points not found for rideId: " <> getId rideId
          return False
  where
    getDeviationAndSafetyDetails id = do
      mbRideDetails <- QRide.findById id
      case mbRideDetails of
        Nothing -> do
          logWarning $ "Ride not found for rideId: " <> getId id
          return (False, False)
        Just rideDetails -> do
          let alreadyDeviated = fromMaybe False rideDetails.driverDeviatedFromRoute
          return (alreadyDeviated, rideDetails.safetyAlertTriggered)

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
          mRide <- QRide.getInProgressOrNewRideIdAndStatusByDriverId driverId
          updateDeviation transportConfig enableNightSafety (mRide <&> fst) batchWaypoints
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
