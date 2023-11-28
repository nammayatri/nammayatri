{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.ConfigMapping where

import Control.Monad
import Data.Time
import Domain.Types.Merchant.ConfigMapping
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Vehicle.Variant (Variant)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Id (Id)
import Kernel.Utils.Logging (logError)
import qualified Storage.Queries.Merchant.ConfigMapping as Queries

getConfigMapId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> Meters -> Maybe Variant -> UTCTime -> Text -> m (Maybe (Id ConfigMapping))
getConfigMapId merchantOperatingCityId distance mbVarType currTime tableName = do
  let timeOfDay = localTimeOfDay $ utcToLocalTime (TimeZone 0 False "") currTime
  cachedData :: (Maybe [ConfigMapping]) <- Hedis.safeGet $ makeConfigMappingKey merchantOperatingCityId mbVarType tableName
  case cachedData of
    Just configs -> do
      let res = filter (\c -> c.startDistance <= distance && c.endDistance > distance && c.startTime <= timeOfDay && c.endTime > timeOfDay) configs
      case res of
        [config'] -> return $ Just config'.configMapId
        [] -> callDBAndCacheData merchantOperatingCityId distance mbVarType timeOfDay tableName
        (config : _) -> do
          logError $ "More than once canfig found for merchantOperatingCityId: " <> show merchantOperatingCityId <> " distance: " <> show distance <> " mbVarType: " <> show mbVarType <> " timeOfDay: " <> show timeOfDay <> " tableName: " <> show tableName <> " configs: " <> show configs
          return $ Just config.configMapId
    Nothing -> callDBAndCacheData merchantOperatingCityId distance mbVarType timeOfDay tableName

callDBAndCacheData :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> Meters -> Maybe Variant -> TimeOfDay -> Text -> m (Maybe (Id ConfigMapping))
callDBAndCacheData merchantOperatingCityId distance mbVarType timeOfDay tableName = do
  cfg' <- Queries.getConfigMapId merchantOperatingCityId distance mbVarType timeOfDay tableName
  case cfg' of
    Just cfg -> do
      let key = makeConfigMappingKey merchantOperatingCityId mbVarType tableName
      val <-
        Hedis.safeGet key >>= \case
          Just cfgs -> return $ Just (cfg : cfgs)
          Nothing -> return $ Just [cfg]
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.setExp key val expTime
      return $ Just cfg.configMapId
    Nothing -> return Nothing

-- matchingRecords :: [ConfigMapping] <- mapMaybeM Hedis.safeGet matchingKeys
-- let res = listToMaybe $ sortOn (Down . (.distance)) (filter (\recrd -> recrd.distance <= distance && recrd.startTime <= timeOfDay && recrd.endTime > timeOfDay) matchingRecords)
-- case res of
--   Just r -> return $ Just r.configMapId
--   Nothing -> do
--     cfg' <- Queries.getConfigMapId merchantOperatingCityId distance mbVarType timeOfDay tableName
--     case cfg' of
--       Just cfg -> do
--         let key = makeConfigMappingKey merchantOperatingCityId mbVarType tableName
--         val <- Hedis.safeGet key >>= \case
--           Just cfgs -> return $ Just (cfg : cfgs)
--           Nothing -> return $ Just [cfg]
--         expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
--         Hedis.setExp key val expTime
--         return $ Just cfg.configMapId
--       Nothing -> return Nothing

makeConfigMappingKey :: Id MerchantOperatingCity -> Maybe Variant -> Text -> Text
makeConfigMappingKey mocId mbVarType tableName = "driver-offer:CachedQueries:ConfigMapping:MerchantOpCityId-" <> mocId.getId <> ":VarType-" <> maybe "" show mbVarType <> ":TableName-" <> tableName

-- makeConfigMappingFetchPattern :: Id MerchantOperatingCity -> Maybe Variant -> Text -> Text
-- makeConfigMappingFetchPattern mocId mbVarType tableName = "*driver-offer:CachedQueries:ConfigMapping:MerchantOpCityId-" <> mocId.getId <> "*" <> ":VarType-" <> maybe "" show mbVarType <> "*" <> ":TableName-" <> tableName <> "*"

-- -- getMatchingKeys :: HedisFlow m env => Text -> m [Text] --CMTODO: Ask from where will this get connection from Piyush.
-- -- getMatchingKeys pattrn = map cs <$> HedisC.keys $ (cs pattrn :: BS.ByteString)

-- getMatchingKeys :: HedisFlow m env => Text -> m [Text] --CMTODO: Ask from where will this get connection from Piyush.
-- getMatchingKeys _ = undefined
