{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.RiderConfig
  ( create,
    findByMerchantOperatingCityId,
    findByMerchantOperatingCityIdInRideFlow,
    clearCache,
  )
where

import Data.Aeson as A
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.RiderConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.RiderConfig as Queries
import qualified Tools.DynamicLogic as DynamicLogic

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => RiderConfig -> m ()
create = Queries.create

findByMerchantOperatingCityIdInRideFlow ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  Id MerchantOperatingCity ->
  [LYT.ConfigVersionMap] ->
  m (Maybe RiderConfig)
findByMerchantOperatingCityIdInRideFlow id configInExperimentVersions =
  findByMerchantOperatingCityId id (Just configInExperimentVersions)

findByMerchantOperatingCityId ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  Id MerchantOperatingCity ->
  Maybe [LYT.ConfigVersionMap] ->
  m (Maybe RiderConfig)
findByMerchantOperatingCityId id mbConfigInExperimentVersions = do
  version <- DynamicLogic.getConfigVersion (cast id) mbConfigInExperimentVersions LYT.RiderConfig
  cachedConfig <- Hedis.safeGet (makeMerchantOperatingCityIdKey id version)
  case cachedConfig of
    Just riderConfig -> return (Just riderConfig)
    Nothing -> fetchAndCacheConfig version
  where
    fetchAndCacheConfig version = do
      allLogics <- DynamicLogic.getConfigLogic (cast id) version LYT.RiderConfig
      mbConfig <- Queries.findByMerchantOperatingCityId id
      maybe (return Nothing) (processConfig allLogics version) mbConfig

    processConfig allLogics version cfg = do
      let configWrapper = LYT.Config cfg Nothing
      response <- try @_ @SomeException $ LYTU.runLogics allLogics configWrapper
      case response of
        Left e -> do
          logError $ "Error in running logics for rider config: " <> show e
          return $ Just cfg
        Right resp -> do
          case (fromJSON resp.result :: Result (LYT.Config RiderConfig)) of
            Success result -> do
              cacheRiderConfig version result.config
              return $ Just result.config
            A.Error err -> do
              logError $ "Error in running logics for rider config: " <> show err
              return $ Just cfg

cacheRiderConfig :: (CacheFlow m r) => Int -> RiderConfig -> m ()
cacheRiderConfig version riderConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let riderConfigKey = makeMerchantOperatingCityIdKey riderConfig.merchantOperatingCityId version
  Hedis.setExp riderConfigKey riderConfig expTime

makeMerchantOperatingCityIdKey :: Id MerchantOperatingCity -> Int -> Text
makeMerchantOperatingCityIdKey id version = "CachedQueries:RiderConfig:MerchantOperatingCityId-" <> id.getId <> "-V-" <> show version

clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> Maybe Int -> m ()
clearCache merchanOperatingCityId mbVersion = do
  case mbVersion of
    -- clear all version cache here
    Nothing -> Hedis.del (makeMerchantOperatingCityIdKey merchanOperatingCityId 1) -- should we clear cache for all versions?
    Just version -> Hedis.del (makeMerchantOperatingCityIdKey merchanOperatingCityId version)
