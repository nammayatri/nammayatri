{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.RoutePolylines (getByRouteIdAndCity, getByRouteIdsAndCity) where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.RoutePolylines
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.RoutePolylines as Queries

getByRouteIdsAndCity ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  [Text] ->
  Id DMOC.MerchantOperatingCity ->
  m [RoutePolylines]
getByRouteIdsAndCity routeIds merchantOperatingCityId = do
  catMaybes <$> mapM (\routeId -> getByRouteIdAndCity routeId merchantOperatingCityId) routeIds

getByRouteIdAndCity ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  Text ->
  Id DMOC.MerchantOperatingCity ->
  m (Maybe RoutePolylines)
getByRouteIdAndCity routeId merchantOperatingCityId = do
  let key = makeRouteIdAndCityKey routeId merchantOperatingCityId
  Hedis.safeGet key >>= \case
    Just routePolylines -> pure $ Just routePolylines
    Nothing -> do
      result <- Queries.getByRouteIdAndCity routeId merchantOperatingCityId
      flip whenJust (cacheRoutePolylines key) result
      pure result

cacheRoutePolylines :: (CacheFlow m r) => Text -> RoutePolylines -> m ()
cacheRoutePolylines key routePolylines = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp key routePolylines expTime

makeRouteIdAndCityKey :: Text -> Id DMOC.MerchantOperatingCity -> Text
makeRouteIdAndCityKey routeId merchantOperatingCityId =
  "CachedQueries:RoutePolylines:RouteId:" <> routeId <> ":City:" <> merchantOperatingCityId.getId
