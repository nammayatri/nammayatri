{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.Person
  ( findCityInfoById,
    updateCityInfoById,
    findPersonStatsById,
    clearPSCache,
  )
where

import Domain.Action.UI.Person
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person
import Domain.Types.PersonStats
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Beckn.Context (City)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Person as Queries
import qualified Storage.Queries.PersonStats as QPS

findCityInfoById :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id Person -> m (Maybe PersonCityInformation)
findCityInfoById personId = do
  Hedis.safeGet (makeIdKey personId) >>= \case
    Just a -> pure a
    Nothing -> flip whenJust cachePersonCityInfo /=<< Queries.findCityInfoById personId

updateCityInfoById :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id Person -> City -> Id DMOC.MerchantOperatingCity -> m ()
updateCityInfoById personId city merchantOperatingCityId = do
  Queries.updateCityInfoById personId city merchantOperatingCityId
  clearCache personId

cachePersonCityInfo :: (CacheFlow m r, MonadFlow m) => PersonCityInformation -> m ()
cachePersonCityInfo personCityInfo = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey personCityInfo.personId
  Hedis.setExp idKey personCityInfo expTime

clearCache :: (CacheFlow m r, MonadFlow m) => Id Person -> m ()
clearCache personId = do
  Hedis.del (makeIdKey personId)

makeIdKey :: Id Person -> Text
makeIdKey personId = "CachedQueries:Person:PersonCityInformation-" <> personId.getId

findPersonStatsById :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id Person -> m (Maybe PersonStats)
findPersonStatsById personId = do
  Hedis.safeGet (makePSIdKey personId) >>= \case
    Just a -> pure a
    Nothing -> flip whenJust cachePersonStats /=<< QPS.findByPersonId personId

cachePersonStats :: (CacheFlow m r, MonadFlow m) => PersonStats -> m ()
cachePersonStats personStats = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makePSIdKey personStats.personId
  Hedis.setExp idKey personStats expTime

clearPSCache :: (CacheFlow m r, MonadFlow m) => Id Person -> m ()
clearPSCache personId = do
  Hedis.del (makePSIdKey personId)

makePSIdKey :: Id Person -> Text
makePSIdKey personId = "CachedQueries:Person:PersonStats-" <> personId.getId
