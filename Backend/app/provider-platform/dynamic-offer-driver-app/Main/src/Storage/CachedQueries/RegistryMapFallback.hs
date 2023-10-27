{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.RegistryMapFallback where

import Domain.Types.RegistryMapFallback
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import Storage.Queries.RegistryMapFallback as Queries

findBySubscriberId :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Text -> m [RegistryMapFallback]
findBySubscriberId subscriberId =
  Hedis.safeGet (makeSubscriberIdKey subscriberId) >>= \case
    Just a -> return a
    Nothing -> cacheRegistryMapFallbacks (makeSubscriberIdKey subscriberId) /=<< Queries.findBySubscriberId subscriberId

findByUniqueId :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Text -> m [RegistryMapFallback]
findByUniqueId uniqueId =
  Hedis.safeGet (makeUniqueIdKey uniqueId) >>= \case
    Just a -> return a
    Nothing -> cacheRegistryMapFallbacks (makeUniqueIdKey uniqueId) /=<< Queries.findByUniqueId uniqueId

findBySubscriberIdAndUniqueId :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Text -> Text -> m (Maybe RegistryMapFallback)
findBySubscriberIdAndUniqueId subId uniqueId =
  Hedis.safeGet (makeSubscriberIdAndUniqueIdKey subId uniqueId) >>= \case
    Just a -> return a
    Nothing -> cacheRegistryMapFallback (makeSubscriberIdAndUniqueIdKey subId uniqueId) /=<< Queries.findBySubscriberIdAndUniqueId subId uniqueId

cacheRegistryMapFallbacks :: CacheFlow m r => Text -> [RegistryMapFallback] -> m ()
cacheRegistryMapFallbacks key registryMap = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp key registryMap expTime

cacheRegistryMapFallback :: CacheFlow m r => Text -> Maybe RegistryMapFallback -> m ()
cacheRegistryMapFallback key registryMap = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp key registryMap expTime

makeSubscriberIdKey :: Text -> Text
makeSubscriberIdKey subscriberId = "driver-offer:CachedQueries:RegistryMapFallback:SubscriberId-" <> subscriberId

makeUniqueIdKey :: Text -> Text
makeUniqueIdKey uniqueId = "driver-offer:CachedQueries:RegistryMapFallback:UniqueId-" <> uniqueId

makeSubscriberIdAndUniqueIdKey :: Text -> Text -> Text
makeSubscriberIdAndUniqueIdKey subscriberId uniqueId = "driver-offer:CachedQueries:RegistryMapFallback:SubscriberId-" <> subscriberId <> "UniqueId-" <> uniqueId
