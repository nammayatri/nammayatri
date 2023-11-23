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
--import qualified Data.ByteString.Internal as BS
--import Data.String.Conversions (cs)

import Control.Monad.Extra (mapMaybeM)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Time
import Domain.Types.Merchant.ConfigMapping
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Vehicle.Variant (Variant)
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Id (Id)
import qualified Storage.Queries.Merchant.ConfigMapping as Queries

getConfigMapId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> Meters -> Maybe Variant -> UTCTime -> Text -> m (Maybe (Id ConfigMapping))
getConfigMapId merchantOperatingCityId distance mbVarType currTime tableName = do
  let timeOfDay = localTimeOfDay $ utcToLocalTime (TimeZone 0 False "") currTime
  matchingKeys <- getMatchingKeys $ makeConfigMappingFetchPattern merchantOperatingCityId mbVarType tableName
  matchingRecords :: [ConfigMapping] <- mapMaybeM Hedis.safeGet matchingKeys
  let res = listToMaybe $ sortOn (Down . (.distance)) (filter (\recrd -> recrd.distance <= distance && recrd.startTime <= timeOfDay && recrd.endTime > timeOfDay) matchingRecords)
  case res of
    Just r -> return $ Just r.configMapId
    Nothing -> do
      cfg' <- Queries.getConfigMapId merchantOperatingCityId distance mbVarType timeOfDay tableName
      case cfg' of
        Just cfg -> do
          let key = makeConfigMappingKey merchantOperatingCityId cfg.distance mbVarType (cfg.startTime) (cfg.endTime) tableName
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp key cfg expTime
          return $ Just cfg.configMapId
        Nothing -> return Nothing

makeConfigMappingKey :: Id MerchantOperatingCity -> Meters -> Maybe Variant -> TimeOfDay -> TimeOfDay -> Text -> Text
makeConfigMappingKey mocId (Meters distance) mbVarType startTime endTime tableName = "driver-offer:CachedQueries:ConfigMapping:MerchantOpCityId-" <> mocId.getId <> ":Distance-" <> show distance <> ":VarType-" <> maybe "" show mbVarType <> ":StartTime-" <> show startTime <> ":EndTime-" <> show endTime <> ":TableName-" <> tableName

makeConfigMappingFetchPattern :: Id MerchantOperatingCity -> Maybe Variant -> Text -> Text
makeConfigMappingFetchPattern mocId mbVarType tableName = "*driver-offer:CachedQueries:ConfigMapping:MerchantOpCityId-" <> mocId.getId <> "*" <> ":VarType-" <> maybe "" show mbVarType <> "*" <> ":TableName-" <> tableName <> "*"

-- getMatchingKeys :: HedisFlow m env => Text -> m [Text] --CMTODO: Ask from where will this get connection from Piyush.
-- getMatchingKeys pattrn = map cs <$> HedisC.keys $ (cs pattrn :: BS.ByteString)

getMatchingKeys :: HedisFlow m env => Text -> m [Text] --CMTODO: Ask from where will this get connection from Piyush.
getMatchingKeys _ = undefined
