{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.StationsExtraInformation (getByStationIdAndCity, getBystationIdsAndCity) where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.StationsExtraInformation
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.StationsExtraInformation as Queries

-- This is a special function for pubilc transport data make sure to not use this when your number of stations are less.
getBystationIdsAndCity ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  [Text] ->
  Id DMOC.MerchantOperatingCity ->
  m [StationsExtraInformation]
getBystationIdsAndCity stationIds merchantOperatingCityId = do
  let cityKey = makeCityKey merchantOperatingCityId
  Hedis.safeGet cityKey >>= \case
    Just allStationsForCity -> do
      -- Filter the cached stations for the requested station IDs
      let filteredStations = filter (\station -> station.stationId `elem` stationIds) allStationsForCity
      pure filteredStations
    Nothing -> do
      -- Cache miss: fetch all stations for the city from DB and cache them
      allStationsForCity <- Queries.getAllStationsByCity merchantOperatingCityId
      cacheAllStationsForCity cityKey allStationsForCity
      -- Filter for the requested station IDs
      let filteredStations = filter (\station -> station.stationId `elem` stationIds) allStationsForCity
      pure filteredStations

getByStationIdAndCity ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  Text ->
  Id DMOC.MerchantOperatingCity ->
  m (Maybe StationsExtraInformation)
getByStationIdAndCity stationId merchantOperatingCityId = do
  let key = makeStationIdAndCityKey stationId merchantOperatingCityId
  Hedis.safeGet key >>= \case
    Just stationsExtraInformation -> pure $ Just stationsExtraInformation
    Nothing -> do
      result <- Queries.findByStationIdAndCity stationId merchantOperatingCityId
      flip whenJust (cacheStationsExtraInformation key) result
      pure result

cacheStationsExtraInformation :: (CacheFlow m r) => Text -> StationsExtraInformation -> m ()
cacheStationsExtraInformation key stationsExtraInformation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp key stationsExtraInformation expTime

makeStationIdAndCityKey :: Text -> Id DMOC.MerchantOperatingCity -> Text
makeStationIdAndCityKey stationId merchantOperatingCityId =
  "CachedQueries:StationsExtraInformation:StationId:" <> stationId <> ":City:" <> merchantOperatingCityId.getId

makeCityKey :: Id DMOC.MerchantOperatingCity -> Text
makeCityKey merchantOperatingCityId =
  "CachedQueries:StationsExtraInformation:AllStations:City:" <> merchantOperatingCityId.getId

cacheAllStationsForCity :: (CacheFlow m r) => Text -> [StationsExtraInformation] -> m ()
cacheAllStationsForCity key allStations = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp key allStations expTime
