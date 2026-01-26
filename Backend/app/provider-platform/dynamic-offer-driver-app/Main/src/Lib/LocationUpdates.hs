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
    LocationUpdateFlow,
    whenWithLocationUpdatesLock,
    buildRideInterpolationHandler,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import Domain.Types.Booking
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person
import Domain.Types.Ride
import qualified Domain.Types.RideRoute as RI
import Domain.Types.TransporterConfig
import qualified Domain.Types.Trip as Trip
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import qualified Kernel.Tools.Metrics.CoreMetrics as CoreMetrics
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance
import Kernel.Utils.Common
import "location-updates" Lib.LocationUpdates as Reexport
import Lib.Scheduler (SchedulerType)
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.CallInternalMLPricing as ML
import qualified SharedLogic.External.LocationTrackingService.Types as LT
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
import Tools.Metrics.ARDUBPPMetrics.Types
import qualified Tools.Notifications as TN
import TransactionLogs.Types

type LocationUpdateFlow m r c =
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasLongDurationRetryCfg r c,
    ClickhouseFlow m r,
    EncFlow m r,
    HedisFlow m r,
    MonadMask m,
    HasBPPMetrics m r,
    HasPrettyLogger m r,
    EventStreamFlow m r,
    HasCallStack,
    CoreMetrics.CoreMetrics m,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig],
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
    HasFlowEnv m r '["snapToRoadSnippetThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["droppedPointsThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["osrmMatchThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["maxStraightLineRectificationThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  )

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

updateDeviation :: LocationUpdateFlow m r c => TransporterConfig -> Bool -> Maybe Ride -> [LatLong] -> m Bool
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
      oldMultipleRoutes :: Maybe [RI.RouteAndDeviationInfo] <- Redis.withMasterRedis $ Redis.get oldKey
      whenJust oldMultipleRoutes $ \allRoutes -> do
        Redis.setExp key allRoutes 3600
        Redis.del oldKey
      multipleRoutes :: Maybe [RI.RouteAndDeviationInfo] <- Redis.withMasterRedis $ Redis.get key
      case multipleRoutes of
        Just routes -> do
          isRouteDeviated <- checkMultipleRoutesForDeviation routes batchWaypoints routeDeviationThreshold nightSafetyRouteDeviationThreshold ride booking shouldPerformSafetyCheck
          updateRouteDeviationDetails isRouteDeviated alreadyDeviated
        Nothing -> do
          isRouteDeviated <- checkForDeviationInSingleRoute batchWaypoints routeDeviationThreshold nightSafetyRouteDeviationThreshold ride booking
          updateRouteDeviationDetails isRouteDeviated alreadyDeviated
  where
    updateRouteDeviationDetails :: LocationUpdateFlow m r c => Bool -> Bool -> m Bool
    updateRouteDeviationDetails isRouteDeviated alreadyDeviated = do
      when (isRouteDeviated && not alreadyDeviated) $ do
        logInfo $ "Deviation detected for rideId: " <> getId ride.id
        QRide.updateDriverDeviatedFromRoute ride.id True
      return isRouteDeviated

    getDeviationAndSafetyDetails rideDetails = do
      let alreadyDeviated = fromMaybe False rideDetails.driverDeviatedFromRoute
      return (alreadyDeviated, rideDetails.safetyAlertTriggered)

checkMultipleRoutesForDeviation :: LocationUpdateFlow m r c => [RI.RouteAndDeviationInfo] -> [LatLong] -> Meters -> Meters -> Ride -> Booking -> Bool -> m Bool
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

checkForDeviationInSingleRoute :: LocationUpdateFlow m r c => [LatLong] -> Meters -> Meters -> Ride -> Booking -> m Bool
checkForDeviationInSingleRoute batchWaypoints routeDeviationThreshold nightSafetyRouteDeviationThreshold ride booking = do
  let rideId = ride.id
  logInfo $ "Checking for deviation in single route for rideId: " <> getId rideId
  let key = searchRequestKey booking.transactionId
  mbRouteInfo :: Maybe RI.RouteInfo <- Redis.withMasterRedis $ Redis.get key
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

updateTollRouteDeviation :: LocationUpdateFlow m r c => Id DMOC.MerchantOperatingCity -> Id Person -> Maybe Ride -> [LatLong] -> m (Bool, Bool)
updateTollRouteDeviation _ _ Nothing _ = do
  logInfo "No ride found to check deviation"
  return (False, False)
updateTollRouteDeviation merchantOpCityId driverId (Just ride) batchWaypoints = do
  let driverDeviatedToTollRoute = fromMaybe False ride.driverDeviatedToTollRoute
  isTollPresentOnCurrentRoute <- isJust <$> TollsDetector.getTollInfoOnRoute merchantOpCityId (Just driverId) batchWaypoints
  when (isTollPresentOnCurrentRoute && not driverDeviatedToTollRoute) $ do
    QRide.updateDriverDeviatedToTollRoute ride.id isTollPresentOnCurrentRoute
  return (driverDeviatedToTollRoute, isTollPresentOnCurrentRoute)

getTravelledDistanceAndTollInfo :: LocationUpdateFlow m r c => Id DMOC.MerchantOperatingCity -> Maybe Ride -> Meters -> Maybe (HighPrecMoney, [Text], [Text], Bool, Maybe Bool) -> m (Meters, Maybe (HighPrecMoney, [Text], [Text], Bool, Maybe Bool))
getTravelledDistanceAndTollInfo _ Nothing _ estimatedTollInfo = do
  logInfo "No ride found to get travelled distance"
  return (0, estimatedTollInfo)
getTravelledDistanceAndTollInfo merchantOperatingCityId (Just ride) estimatedDistance estimatedTollInfo = do
  let rideId = ride.id
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let key = multipleRouteKey booking.transactionId
  multipleRoutes :: Maybe [RI.RouteAndDeviationInfo] <- Redis.withMasterRedis $ Redis.get key
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

buildRideInterpolationHandler :: LocationUpdateFlow m r c => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe (Id Ride) -> Bool -> Maybe Integer -> m (RideInterpolationHandler Person m)
buildRideInterpolationHandler merchantId merchantOpCityId rideId isEndRide mbBatchSize = do
  transportConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let snapToRoad' shouldRectifyDistantPointsFailure =
        if transportConfig.useWithSnapToRoadFallback
          then TMaps.snapToRoadWithFallback shouldRectifyDistantPointsFailure merchantId merchantOpCityId False (fmap getId rideId)
          else snapToRoadWithService (fmap getId rideId)
      enableNightSafety = not isEndRide
      enableSafetyCheckWrtTripCategory = \case
        Trip.Delivery _ -> False
        _ -> True
  return $
    mkRideInterpolationHandler
      (fromMaybe transportConfig.normalRideBulkLocUpdateBatchSize mbBatchSize) -- keeping batch size of 98 by default for bulk location updates to trigger snapToRoad
      98
      isEndRide
      (\driverId dist googleSnapCalls osrmSnapCalls numberOfSelfTuned isDistCalcFailed -> QRide.updateDistance driverId dist googleSnapCalls osrmSnapCalls numberOfSelfTuned isDistCalcFailed)
      (\driverId tollCharges tollNames tollIds -> void (QRide.updateTollChargesAndNamesAndIds driverId tollCharges tollNames tollIds))
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
    snapToRoadWithService rideId' req = do
      resp <- TMaps.snapToRoad merchantId merchantOpCityId rideId' req
      return ([Google], Right resp)

whenWithLocationUpdatesLock :: (HedisFlow m r, MonadMask m) => Id Person -> m a -> m a
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

performSafetyCheck :: LocationUpdateFlow m r c => Ride -> Booking -> m ()
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
      BP.sendSafetyAlertToBAP booking ride Enums.DEVIATION driver vehicle
