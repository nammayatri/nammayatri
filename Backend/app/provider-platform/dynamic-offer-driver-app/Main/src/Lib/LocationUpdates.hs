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

import Control.Applicative ((<|>))
import Domain.Types.Booking
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person
import Domain.Types.Ride
import qualified Domain.Types.RideRoute as RI
import Domain.Types.TransporterConfig
import qualified Domain.Types.Trip as Trip
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
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRiderDetails
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error
import qualified Tools.Maps as TMaps
import qualified Tools.Notifications as TN

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
  logDebug $ "Safety check : " <> show safetyCheckEnabled
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  (alreadyDeviated, safetyAlertAlreadyTriggered) <- getDeviationAndSafetyDetails ride
  if safetyAlertAlreadyTriggered && alreadyDeviated
    then do
      logInfo $ "Safety alert and deviation already triggered for rideId: " <> getId rideId
      return True
    else do
      let routeDeviationThreshold = transportConfig.routeDeviationThreshold
          nightSafetyRouteDeviationThreshold = transportConfig.nightSafetyRouteDeviationThreshold
          key = multipleRouteKey booking.transactionId
          oldKey = multipleRouteKey rideId.getId
          shouldPerformSafetyCheck = safetyCheckEnabled && not safetyAlertAlreadyTriggered
      oldMultipleRoutes :: Maybe [RI.RouteAndDeviationInfo] <- Redis.get oldKey
      whenJust oldMultipleRoutes $ \allRoutes -> do
        Redis.setExp key allRoutes 3600
        Redis.del oldKey
      multipleRoutes :: Maybe [RI.RouteAndDeviationInfo] <- Redis.get key
      case multipleRoutes of
        Just routes -> do
          isRouteDeviated <- checkMultipleRoutesForDeviation routes batchWaypoints routeDeviationThreshold nightSafetyRouteDeviationThreshold ride booking shouldPerformSafetyCheck
          updateRouteDeviationDetails isRouteDeviated alreadyDeviated
        Nothing -> do
          isRouteDeviated <- checkForDeviationInSingleRoute batchWaypoints routeDeviationThreshold nightSafetyRouteDeviationThreshold ride booking
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

checkMultipleRoutesForDeviation :: [RI.RouteAndDeviationInfo] -> [LatLong] -> Meters -> Meters -> Ride -> Booking -> Bool -> Flow Bool
checkMultipleRoutesForDeviation routes batchWaypoints routeDeviationThreshold nightSafetyRouteDeviationThreshold ride booking safetyCheckEnabled = do
  let rideId = ride.id
  logInfo $ "Checking for deviation in multiple routes for rideId: " <> getId rideId
  let updatedRoutesInfo = map checkRouteForDeviation routes
  logInfo $ "Updated routes info for rideId: " <> getId rideId <> " is: " <> show updatedRoutesInfo
  setExp (multipleRouteKey booking.transactionId) updatedRoutesInfo 14400
  fork "Performing safety check" $
    when (safetyCheckEnabled && all (\route -> RI.safetyDeviation $ RI.deviationInfo route) updatedRoutesInfo) $ performSafetyCheck ride booking
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

checkForDeviationInSingleRoute :: [LatLong] -> Meters -> Meters -> Ride -> Booking -> Flow Bool
checkForDeviationInSingleRoute batchWaypoints routeDeviationThreshold nightSafetyRouteDeviationThreshold ride booking = do
  let rideId = ride.id
  logInfo $ "Checking for deviation in single route for rideId: " <> getId rideId
  let key = searchRequestKey booking.transactionId
  mbRouteInfo :: Maybe RI.RouteInfo <- Redis.get key
  case mbRouteInfo of
    Just routeInfo -> do
      let multipleRoutesEntity =
            [ RI.RouteAndDeviationInfo
                { routeInfo = routeInfo,
                  deviationInfo = RI.DeviationInfo {deviation = False, safetyDeviation = False}
                }
            ]
      checkMultipleRoutesForDeviation multipleRoutesEntity batchWaypoints routeDeviationThreshold nightSafetyRouteDeviationThreshold ride booking False
    Nothing -> do
      logWarning $ "Ride route info not found for rideId: " <> getId rideId
      return False

updateTollRouteDeviation :: Id DMOC.MerchantOperatingCity -> Id Person -> Maybe Ride -> [LatLong] -> Flow (Bool, Bool)
updateTollRouteDeviation _ _ Nothing _ = do
  logInfo "No ride found to check deviation"
  return (False, False)
updateTollRouteDeviation merchantOpCityId driverId (Just ride) batchWaypoints = do
  let driverDeviatedToTollRoute = fromMaybe False ride.driverDeviatedToTollRoute
  isTollPresentOnCurrentRoute <- isJust <$> TollsDetector.getTollInfoOnRoute merchantOpCityId (Just driverId) batchWaypoints
  when (isTollPresentOnCurrentRoute && not driverDeviatedToTollRoute) $ do
    QRide.updateDriverDeviatedToTollRoute ride.id isTollPresentOnCurrentRoute
  return (driverDeviatedToTollRoute, isTollPresentOnCurrentRoute)

getTravelledDistanceAndTollInfo :: Id DMOC.MerchantOperatingCity -> Maybe Ride -> Meters -> Maybe (HighPrecMoney, [Text], Bool, Maybe Bool) -> Flow (Meters, Maybe (HighPrecMoney, [Text], Bool, Maybe Bool))
getTravelledDistanceAndTollInfo _ Nothing _ estimatedTollInfo = do
  logInfo "No ride found to get travelled distance"
  return (0, estimatedTollInfo)
getTravelledDistanceAndTollInfo merchantOperatingCityId (Just ride) estimatedDistance estimatedTollInfo = do
  let rideId = ride.id
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let key = multipleRouteKey booking.transactionId
  multipleRoutes :: Maybe [RI.RouteAndDeviationInfo] <- Redis.get key
  case multipleRoutes of
    Just routes -> do
      let undeviatedRoute = find (not . RI.deviation . RI.deviationInfo) routes
      case undeviatedRoute of
        Just route -> do
          let distance = RI.distance $ RI.routeInfo route
              routePoints = RI.points $ RI.routeInfo route
          tollChargesInfo <- join <$> mapM (TollsDetector.getTollInfoOnRoute merchantOperatingCityId Nothing) routePoints
          return $ (fromMaybe estimatedDistance distance, tollChargesInfo <|> estimatedTollInfo)
        Nothing -> do
          logInfo $ "UndeviatedRoute not found for ride" <> show rideId
          return (estimatedDistance, estimatedTollInfo)
    Nothing -> do
      logInfo $ "MultipleRoutes not found for ride" <> show rideId
      return (estimatedDistance, estimatedTollInfo)

buildRideInterpolationHandler :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Flow (RideInterpolationHandler Person Flow)
buildRideInterpolationHandler merchantId merchantOpCityId isEndRide = do
  transportConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let snapToRoad' shouldRectifyDistantPointsFailure =
        if transportConfig.useWithSnapToRoadFallback
          then TMaps.snapToRoadWithFallback shouldRectifyDistantPointsFailure merchantId merchantOpCityId
          else snapToRoadWithService
      enableNightSafety = not isEndRide
      enableSafetyCheckWrtTripCategory = \case
        Trip.Delivery _ -> False
        _ -> True
  return $
    mkRideInterpolationHandler
      isEndRide
      (\driverId dist googleSnapCalls osrmSnapCalls numberOfSelfTuned isDistCalcFailed -> QRide.updateDistance driverId dist googleSnapCalls osrmSnapCalls numberOfSelfTuned isDistCalcFailed)
      (\driverId tollCharges tollNames -> void (QRide.updateTollChargesAndNames driverId tollCharges tollNames))
      ( \driverId batchWaypoints -> do
          ride <- QRide.getActiveByDriverId driverId
          let isSafetyCheckEnabledForTripCategory = maybe True (enableSafetyCheckWrtTripCategory . (.tripCategory)) ride
          routeDeviation <- updateDeviation transportConfig (enableNightSafety && isSafetyCheckEnabledForTripCategory) ride batchWaypoints
          (tollRouteDeviation, isTollPresentOnCurrentRoute) <- updateTollRouteDeviation merchantOpCityId driverId ride batchWaypoints
          return (routeDeviation, tollRouteDeviation, isTollPresentOnCurrentRoute)
      )
      (TollsDetector.getTollInfoOnRoute merchantOpCityId)
      ( \driverId estimatedDistance estimatedTollInfo -> do
          ride <- QRide.getActiveByDriverId driverId
          getTravelledDistanceAndTollInfo merchantOpCityId ride estimatedDistance estimatedTollInfo
      )
      transportConfig.recomputeIfPickupDropNotOutsideOfThreshold
      snapToRoad'
      ( \driverId -> do
          person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
          mbMerchantPN <- CPN.findMatchingMerchantPN merchantOpCityId "TOLL_CROSSED" Nothing Nothing person.language Nothing
          whenJust mbMerchantPN $ \merchantPN -> do
            let entityData = TN.NotifReq {entityId = person.id.getId, title = merchantPN.title, message = merchantPN.body}
            TN.notifyDriverOnEvents person.merchantOperatingCityId person.id person.deviceToken entityData merchantPN.fcmNotificationType
      )
      ( \driverId -> do
          driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
          driverStats <- QDriverStats.findById driver.id >>= fromMaybeM DriverInfoNotFound
          mbRide <- QRide.getActiveByDriverId driverId
          mbBooking <- maybe (return Nothing) (QBooking.findById . bookingId) mbRide
          vehicle <- QVeh.findById driver.id >>= fromMaybeM (DriverWithoutVehicle driver.id.getId)
          BP.sendTollCrossedUpdateToBAP mbBooking mbRide driver driverStats vehicle
      )
  where
    snapToRoadWithService req = do
      resp <- TMaps.snapToRoad merchantId merchantOpCityId req
      return ([Google], Right resp)

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

performSafetyCheck :: Ride -> Booking -> Flow ()
performSafetyCheck ride booking = do
  let rideId = ride.id
  logInfo $ "Performing safety check for rideId: " <> getId rideId
  when (not ride.safetyAlertTriggered) $ do
    riderId <-
      booking.riderId
        & fromMaybeM (BookingFieldNotPresent "riderId")
    riderDetails <- runInReplica $ QRiderDetails.findById riderId >>= fromMaybeM (RiderDetailsNotFound ride.id.getId)
    void $ QRide.updateSafetyAlertTriggered ride.id
    when riderDetails.nightSafetyChecks $ do
      driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
      BP.sendSafetyAlertToBAP booking ride "Route Deviation Detected" driver vehicle
