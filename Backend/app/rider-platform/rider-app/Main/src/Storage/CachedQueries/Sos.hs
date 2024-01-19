{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.Sos
  ( findByRideIdAndStatus,
    clearCache,
    findByRideIdAndStatusList,
    clearCacheList,
  )
where

import Domain.Types.Ride
import Domain.Types.Sos as DSos
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Sos as Queries

findByRideIdAndStatus :: (CacheFlow m r, EsqDBFlow m r) => Id Ride -> DSos.SosStatus -> m (Maybe DSos.Sos)
findByRideIdAndStatus rideId status = do
  Hedis.safeGet (makeIdKey rideId status) >>= \case
    Just a -> pure a
    Nothing -> flip whenJust (cacheSosIdByRideIdAndStatus rideId status) /=<< Queries.findByRideIdAndStatus rideId status

cacheSosIdByRideIdAndStatus :: (CacheFlow m r) => Id Ride -> DSos.SosStatus -> DSos.Sos -> m ()
cacheSosIdByRideIdAndStatus rideId status sos = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey rideId status
  Hedis.setExp idKey sos expTime

makeIdKey :: Id Ride -> DSos.SosStatus -> Text
makeIdKey rideId status = "CachedQueries:Sos:RideId-" <> rideId.getId <> "Status-" <> show status

clearCache :: (CacheFlow m r) => Id Ride -> DSos.SosStatus -> m ()
clearCache rideId status = Hedis.del $ makeIdKey rideId status

findByRideIdAndStatusList :: (CacheFlow m r, EsqDBFlow m r) => [DSos.SosStatus] -> Id Ride -> m (Maybe DSos.Sos)
findByRideIdAndStatusList status rideId = do
  Hedis.safeGet (makeIdKeyList rideId status) >>= \case
    Just a -> pure a
    Nothing -> flip whenJust (cacheSosIdByRideIdAndStatusList rideId status) /=<< Queries.findByRideIdinStatusList status rideId

cacheSosIdByRideIdAndStatusList :: (CacheFlow m r) => Id Ride -> [DSos.SosStatus] -> DSos.Sos -> m ()
cacheSosIdByRideIdAndStatusList rideId status sos = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKeyList rideId status
  Hedis.setExp idKey sos expTime

makeIdKeyList :: Id Ride -> [DSos.SosStatus] -> Text
makeIdKeyList rideId status = "CachedQueries:Sos:RideId-" <> rideId.getId <> "Status-" <> show status

clearCacheList :: (CacheFlow m r) => Id Ride -> [DSos.SosStatus] -> m ()
clearCacheList rideId status = Hedis.del $ makeIdKeyList rideId status
