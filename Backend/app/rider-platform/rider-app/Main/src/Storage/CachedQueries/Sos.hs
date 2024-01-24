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
    cacheSosIdByRideId,
  )
where

import Domain.Types.Ride
import Domain.Types.Sos as DSos
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Sos as Queries

findByRideIdAndStatus :: (CacheFlow m r, EsqDBFlow m r) => Id Ride -> [DSos.SosStatus] -> m (Maybe DSos.Sos)
findByRideIdAndStatus rideId status = do
  Hedis.safeGet (makeIdKey rideId) >>= \case
    Just a -> pure a
    Nothing -> flip whenJust (cacheSosIdByRideId rideId) /=<< (listToMaybe <$> Queries.findByRideIdinStatusList (Just 1) (Just 0) status rideId)

cacheSosIdByRideId :: (CacheFlow m r) => Id Ride -> DSos.Sos -> m ()
cacheSosIdByRideId rideId sos = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey rideId
  Hedis.setExp idKey sos expTime

makeIdKey :: Id Ride -> Text
makeIdKey rideId = "CachedQueries:Sos:RideId-" <> rideId.getId

clearCache :: (CacheFlow m r) => Id Ride -> m ()
clearCache rideId = Hedis.del $ makeIdKey rideId
