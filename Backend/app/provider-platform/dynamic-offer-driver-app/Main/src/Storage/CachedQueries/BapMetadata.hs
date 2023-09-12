{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Storage.CachedQueries.BapMetadata where

import Domain.Types.BapMetadata
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.BapMetadata as Queries

findById :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id BapMetadata -> m (Maybe BapMetadata)
findById bapMetadataId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeBapMetadataByIdKey bapMetadataId) >>= \case
    Just a -> pure a
    Nothing -> flip whenJust (cacheBapMetadataById bapMetadataId) /=<< Queries.findById bapMetadataId

clearBapMetadataByIdCache :: (CacheFlow m r) => Id BapMetadata -> m ()
clearBapMetadataByIdCache = Hedis.withCrossAppRedis . Hedis.del . makeBapMetadataByIdKey

cacheBapMetadataById :: (CacheFlow m r) => Id BapMetadata -> BapMetadata -> m ()
cacheBapMetadataById bapMetadataId bapMetadata = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeBapMetadataByIdKey bapMetadataId) bapMetadata expTime

makeBapMetadataByIdKey :: Id BapMetadata -> Text
makeBapMetadataByIdKey bapMetadataId = "do:CQ:BM:Id-" <> getId bapMetadataId
