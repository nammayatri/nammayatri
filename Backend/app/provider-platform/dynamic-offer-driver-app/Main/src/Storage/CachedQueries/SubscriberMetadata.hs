{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.SubscriberMetadata where

import Domain.Types.SubscriberMetadata
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.SubscriberMetadata as Queries

findById :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id SubscriberMetadata -> m (Maybe SubscriberMetadata)
findById subscriberMetadataId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeSubscriberMetadataByIdKey subscriberMetadataId) >>= \case
    Just a -> pure a
    Nothing -> flip whenJust (cacheSubscriberMetadataById subscriberMetadataId) /=<< Queries.findById subscriberMetadataId

clearSubscriberMetadataByIdCache :: (CacheFlow m r) => Id SubscriberMetadata -> m ()
clearSubscriberMetadataByIdCache = Hedis.withCrossAppRedis . Hedis.del . makeSubscriberMetadataByIdKey

cacheSubscriberMetadataById :: (CacheFlow m r) => Id SubscriberMetadata -> SubscriberMetadata -> m ()
cacheSubscriberMetadataById subscriberMetadataId subscriberMetadata = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeSubscriberMetadataByIdKey subscriberMetadataId) subscriberMetadata expTime

makeSubscriberMetadataByIdKey :: Id SubscriberMetadata -> Text
makeSubscriberMetadataByIdKey subscriberMetadataId = "driver-offer:CachedQueries:SubscriberMetadata:Id-" <> getId subscriberMetadataId
