{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.Sos
  ( findByRideId,
    clearCache,
    cacheSosIdByRideId,
    mockSosKey,
  )
where

import qualified Domain.Types.Person as Person
import Domain.Types.Ride
import Domain.Types.Sos as DSos
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common

findByRideId :: (CacheFlow m r, EsqDBFlow m r) => Id Ride -> m (Maybe DSos.Sos)
findByRideId rideId = do
  Hedis.safeGet $ makeIdKey rideId

cacheSosIdByRideId :: (CacheFlow m r) => Id Ride -> DSos.Sos -> m ()
cacheSosIdByRideId rideId sos = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey rideId
  Hedis.setExp idKey sos expTime

makeIdKey :: Id Ride -> Text
makeIdKey rideId = "CachedQueries:Sos:RideId-" <> rideId.getId

clearCache :: (CacheFlow m r) => Id Ride -> m ()
clearCache rideId = Hedis.del $ makeIdKey rideId

mockSosKey :: Id Person.Person -> Text
mockSosKey personId = "mock-sos-" <> getId personId
