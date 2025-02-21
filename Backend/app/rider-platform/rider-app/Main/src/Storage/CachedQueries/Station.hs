{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.Station
  ( findByStationCode,
    findByStationCodeAndIntegratedBPPConfigId,
    findById,
  )
where

import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Station
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Station as Queries

findByStationCode :: (CacheFlow m r, EsqDBFlow m r) => Text -> Id DIBC.IntegratedBPPConfig -> m (Maybe Station)
findByStationCode stationCode integratedBPPConfigId = do
  let key = makeStationCodeKey stationCode
  Hedis.safeGet key >>= \case
    Just a -> return $ Just a
    Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheByStationCode /=<< Queries.findByStationCode stationCode integratedBPPConfigId

makeStationCodeKey :: Text -> Text
makeStationCodeKey stationCode = "CachedQueries:Station:StationCode:" <> stationCode

cacheByStationCode :: (CacheFlow m r) => Station -> m ()
cacheByStationCode station = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeStationCodeKey station.code
  Hedis.setExp key station expTime

findByStationCodeAndIntegratedBPPConfigId :: (CacheFlow m r, EsqDBFlow m r) => Text -> Id DIBC.IntegratedBPPConfig -> m (Maybe Station)
findByStationCodeAndIntegratedBPPConfigId stationCode integratedBPPConfigId = do
  let key = makeStationCodeAndIntegratedBPPConfigIdKey stationCode integratedBPPConfigId
  Hedis.safeGet key >>= \case
    Just a -> return $ Just a
    Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheByStationCodeAndIntegratedBPPConfigId /=<< Queries.findByStationCode stationCode integratedBPPConfigId

makeStationCodeAndIntegratedBPPConfigIdKey :: Text -> Id DIBC.IntegratedBPPConfig -> Text
makeStationCodeAndIntegratedBPPConfigIdKey stationCode integratedBPPConfigId = "CachedQueries:Station:StationCode:" <> stationCode <> ":integratedBPPConfigId:" <> integratedBPPConfigId.getId

cacheByStationCodeAndIntegratedBPPConfigId :: (CacheFlow m r) => Station -> m ()
cacheByStationCodeAndIntegratedBPPConfigId station = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeStationCodeAndIntegratedBPPConfigIdKey station.code station.integratedBppConfigId
  Hedis.setExp key station expTime

findById :: (CacheFlow m r, EsqDBFlow m r) => Id Station -> m (Maybe Station)
findById stationId = do
  let key = makeStationIdKey stationId
  Hedis.safeGet key >>= \case
    Just a -> return $ Just a
    Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheById /=<< Queries.findById stationId

makeStationIdKey :: Id Station -> Text
makeStationIdKey stationId = "CachedQueries:Station:StationId:" <> stationId.getId

cacheById :: (CacheFlow m r) => Station -> m ()
cacheById station = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeStationIdKey station.id
  Hedis.setExp key station expTime
