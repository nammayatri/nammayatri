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

import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant.MerchantServiceConfig as DOSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.Prelude
import Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import "location-updates" Lib.LocationUpdates as Reexport
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Maps as Maps

buildRideInterpolationHandler :: Id Merchant -> Bool -> Id DRide.Ride -> Maybe (Maps.SMapsService 'Maps.SnapToRoad) -> Flow (RideInterpolationHandler Person.Person Flow)
buildRideInterpolationHandler orgId isEndRide rideId mbMapsService = do
  mapsService <- Maps.pickServiceWithDefault @'Maps.SnapToRoad mbMapsService orgId rideId
  orgMapsServiceConfig <-
    QOMSC.findByMerchantIdAndService orgId (DOSC.MapsService mapsService.getStrictMapsService)
      >>= fromMaybeM (MerchantServiceConfigNotFound orgId.getId "Maps" (show mapsService))
  case orgMapsServiceConfig.serviceConfig of
    DOSC.MapsServiceConfig cfg ->
      return $
        mkRideInterpolationHandler isEndRide cfg $
          \driverId dist -> void (QRide.updateDistance driverId dist)
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
