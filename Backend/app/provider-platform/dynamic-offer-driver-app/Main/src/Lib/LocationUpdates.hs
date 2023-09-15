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

import Domain.Action.Beckn.Search
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant.MerchantServiceConfig as DOSC
import Domain.Types.Person
import Domain.Types.Ride
import qualified Domain.Types.RideRoute as RI
import Environment
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance
import Kernel.Utils.Common
import qualified Kernel.Utils.Time as TT
import "location-updates" Lib.LocationUpdates as Reexport
import qualified SharedLogic.Ride as SRide
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QOMC
import qualified Storage.CachedQueries.Merchant.TransporterConfig as MTC
import qualified Storage.Queries.Ride as QRide
import Tools.Error

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

updateDeviation :: (HedisFlow m r, CacheFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Meters -> Maybe (Id Ride) -> [LatLong] -> m Bool
updateDeviation _ Nothing _ = do
  logInfo "No ride found to check deviation"
  return False
updateDeviation routeDeviationThreshold (Just rideId) batchWaypoints = do
  logWarning "Updating Deviation"
  let key = searchRequestKey (getId rideId)
  routeInfo :: Maybe RI.RouteInfo <- Redis.get key
  case routeInfo >>= (.points) of
    Just estimatedRoute ->
      if checkForDeviation routeDeviationThreshold estimatedRoute batchWaypoints 0
        then do
          logInfo $ "Deviation detected for rideId: " <> show rideId
          QRide.updateDriverDeviatedFromRoute rideId True
          return True
        else do
          logInfo $ "No deviation detected for rideId: " <> show rideId
          return False
    Nothing -> do
      logWarning $ "Ride route points not found for rideId: " <> show rideId
      return False

buildRideInterpolationHandler :: Id Merchant -> Bool -> Flow (RideInterpolationHandler Person Flow)
buildRideInterpolationHandler merchantId isEndRide = do
  transportConfig <- md "MTC.findByMerchantId merchantId" $ MTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  orgMapsConfig <- md "QOMC.findByMerchantId merchantId" $ QOMC.findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  orgMapsServiceConfig <-
    md "QOMC.findByMerchantIdAndService merchantId" $
      QOMSC.findByMerchantIdAndService merchantId (DOSC.MapsService orgMapsConfig.snapToRoad)
        >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Maps" (show orgMapsConfig.snapToRoad))
  md "case orgMapsServiceConfig.serviceConfig of" $
    case orgMapsServiceConfig.serviceConfig of
      DOSC.MapsServiceConfig cfg ->
        return $
          mkRideInterpolationHandler
            isEndRide
            cfg
            (\driverId dist snapCalls -> void (QRide.updateDistance driverId dist snapCalls))
            ( \driverId batchWaypoints -> do
                mRide <- md "SRide.getInProgressOrNewRideIdAndStatusByDriverId" $ SRide.getInProgressOrNewRideIdAndStatusByDriverId driverId
                md "updateDeviation" $ updateDeviation transportConfig.routeDeviationThreshold (mRide <&> fst) batchWaypoints
            )
      _ -> throwError $ InternalError "Unknown Service Config"
  where
    md tag f = do
      (res, duration) <- TT.measureDuration f
      logError $ (tag <> " : ") <> show duration
      pure res

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
