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
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person as Person
-- import Kernel.Types.Common

import qualified Domain.Types.RideRoute as RI
import Environment
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance
import Kernel.Utils.Common
import "location-updates" Lib.LocationUpdates as Reexport
import qualified SharedLogic.Ride as SRide
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QOMC
import qualified Storage.Queries.Ride as QRide
import Tools.Error

isWithinTolerance :: LatLong -> [LatLong] -> Meters -> Bool
isWithinTolerance pt estimatedRoute tolerance =
  let minDistance = highPrecMetersToMeters (minimum $ map (distanceBetweenInMeters pt) estimatedRoute)
   in minDistance <= tolerance

checkForDeviation :: Meters -> [LatLong] -> [LatLong] -> Int -> Bool
checkForDeviation _ _ [] _ = False
checkForDeviation tolerance estimatedRoute (pt : batchWaypoints) deviationCount
  | deviationCount >= 3 = True
  | length batchWaypoints < 2 = False
  | otherwise = do
    if isWithinTolerance pt estimatedRoute tolerance
      then checkForDeviation tolerance estimatedRoute batchWaypoints (deviationCount + 1)
      else checkForDeviation tolerance estimatedRoute batchWaypoints deviationCount

updateDeviation :: (HedisFlow m r, CacheFlow m r, EsqDBReplicaFlow m r, EncFlow m r, HasField "toleranceEarthMetres" r Meters) => Id Person -> [LatLong] -> m ()
updateDeviation driverId batchWaypoints = do
  ride <- SRide.getInProgressOrNewRideIdAndStatusByDriverId driverId
  tolerance <- asks (.toleranceEarthMetres)
  case ride of
    Just (rideId, _) -> do
      let key = searchRequestKey (getId rideId)
      routeInfo :: RI.RouteInfo <- Redis.get key >>= fromMaybeM (RideDoesNotExist $ getId rideId)
      case routeInfo.points of
        Just estimatedRoute ->
          if checkForDeviation tolerance estimatedRoute batchWaypoints 0
            then do
              logInfo $ "Deviation detected for driverId: " <> show driverId
              QRide.updateNumDeviation driverId True
            else do
              logInfo $ "No deviation detected for driverId: " <> show driverId
        Nothing -> logInfo $ "Ride route points not found for rideId: " <> show rideId
    Nothing -> logInfo $ "Ride not found for driverId: " <> show driverId

buildRideInterpolationHandler :: Id Merchant -> Bool -> Flow (RideInterpolationHandler Person.Person Flow)
buildRideInterpolationHandler orgId isEndRide = do
  orgMapsConfig <- QOMC.findByMerchantId orgId >>= fromMaybeM (MerchantServiceUsageConfigNotFound orgId.getId)
  orgMapsServiceConfig <-
    QOMSC.findByMerchantIdAndService orgId (DOSC.MapsService orgMapsConfig.snapToRoad)
      >>= fromMaybeM (MerchantServiceConfigNotFound orgId.getId "Maps" (show orgMapsConfig.snapToRoad))
  case orgMapsServiceConfig.serviceConfig of
    DOSC.MapsServiceConfig cfg ->
      return $
        mkRideInterpolationHandler
          isEndRide
          cfg
          (\driverId dist -> void (QRide.updateDistance driverId dist))
          (\driverId batchWaypoints -> void (updateDeviation driverId batchWaypoints))
    _ -> throwError $ InternalError "Unknown Service Config"

whenWithLocationUpdatesLock :: (HedisFlow m r, MonadMask m) => Id DP.Person -> m () -> m ()
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
