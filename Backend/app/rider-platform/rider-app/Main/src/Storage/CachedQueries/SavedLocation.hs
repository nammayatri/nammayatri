{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Storage.CachedQueries.SavedLocation where

import Domain.Types.Person (Person)
import Domain.Types.SavedReqLocation
import Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Queries.SavedReqLocation as Queries

findByLatLonAndRiderId :: (CacheFlow m r, EsqDBFlow m r) => Id Person -> LatLong -> m (Maybe SavedReqLocation)
findByLatLonAndRiderId personId latLong =
  Hedis.safeGet (makeIdKey latLong personId) >>= \case
    Just a -> return a
    Nothing -> flip whenJust (cacheSavedLocation latLong personId) /=<< Queries.findByLatLonAndRiderId personId latLong

cacheSavedLocation :: CacheFlow m r => LatLong -> Id Person -> SavedReqLocation -> m ()
cacheSavedLocation latLong personId savedLocation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeIdKey latLong personId
  Hedis.setExp key savedLocation expTime

deleteSavedLocation :: CacheFlow m r => LatLong -> Id Person -> m ()
deleteSavedLocation latLong personId = Hedis.del (makeIdKey latLong personId)

makeIdKey :: LatLong -> Id Person -> Text
makeIdKey latLong personId = "CachedQueries:SavedLocation:-" <> show latLong <> show personId
