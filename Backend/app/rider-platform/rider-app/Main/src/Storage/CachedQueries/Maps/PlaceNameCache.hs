{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Maps.PlaceNameCache where

import Domain.Types.Maps.PlaceNameCache
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common (CacheFlow, MonadFlow)
import qualified Storage.Queries.Maps.PlaceNameCache as Queries

findPlaceByPlaceId :: (CacheFlow m r, Esq.EsqDBFlow m r) => Text -> m [PlaceNameCache]
findPlaceByPlaceId placeId =
  Hedis.safeGet (makePlaceIdKey placeId) >>= \case
    Just a -> return a
    Nothing -> cachedPlaceByPlaceId placeId /=<< Queries.findPlaceByPlaceId placeId

findPlaceByGeoHash :: (CacheFlow m r, Esq.EsqDBFlow m r) => Text -> m [PlaceNameCache]
findPlaceByGeoHash geoHash =
  Hedis.safeGet (makeGeoHashIdKey geoHash) >>= \case
    Just a -> return a
    Nothing -> cachedPlaceByGeoHash geoHash /=<< Queries.findPlaceByGeoHash geoHash

create :: MonadFlow m => PlaceNameCache -> m ()
create = Queries.create

-- test with empty list
cachedPlaceByPlaceId :: CacheFlow m r => Text -> [PlaceNameCache] -> m ()
cachedPlaceByPlaceId placeId placeNameCached = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let placeIdKey = makePlaceIdKey placeId
  Hedis.setExp placeIdKey placeNameCached expTime

cachedPlaceByGeoHash :: CacheFlow m r => Text -> [PlaceNameCache] -> m ()
cachedPlaceByGeoHash geoHash placeNameCached = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let geoHashIdKey = makeGeoHashIdKey geoHash
  Hedis.setExp geoHashIdKey placeNameCached expTime

makePlaceIdKey :: Text -> Text
makePlaceIdKey placeId = "CachedQueries:Maps:PlaceId-" <> placeId

makeGeoHashIdKey :: Text -> Text
makeGeoHashIdKey geoHash = "CachedQueries:Maps:GeoHash-" <> geoHash
