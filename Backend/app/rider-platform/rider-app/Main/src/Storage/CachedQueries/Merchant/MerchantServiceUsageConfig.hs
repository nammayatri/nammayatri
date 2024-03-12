{-# LANGUAGE TypeApplications #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantServiceUsageConfig
  ( create,
    findByMerchantOperatingCityId,
    clearCache,
    updateMerchantServiceUsageConfig,
  )
where

import qualified Client.Main as CM
import Control.Lens
import qualified Data.Aeson as DA
import Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as DAKM
import Data.Aeson.Lens
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Domain.Types.Common
import Domain.Types.Merchant.MerchantServiceUsageConfig
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified EulerHS.Language as L
import qualified GHC.List as GL
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Queries.SystemConfigs as KSQS
import qualified Kernel.Types.Cac as KTC
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.Merchant.MerchantServiceUsageConfig as Queries
import qualified System.Environment as Se
import System.Random

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantServiceUsageConfig -> m ()
create = Queries.create

findByMerchantOperatingCityId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id MerchantOperatingCity -> m (Maybe MerchantServiceUsageConfig)
findByMerchantOperatingCityId id = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe [] (.useCAC) systemConfigs
  if "merchant_service_usage_config" `GL.elem` useCACConfig
    then do
      gen <- newStdGen
      let toss = fst (randomR (1, 100) gen :: (Int, StdGen))
      logDebug $ "The toss value is for MerchantServiceUsageConfig " <> show toss
      context <- liftIO $ CM.hashMapToString $ HashMap.fromList [(Text.pack "merchantOperatingCityId", DA.String (getId id))]
      mbTenant <- liftIO $ Se.lookupEnv "RIDER_TENANT"
      let tenant = fromMaybe "atlas_app_v2" mbTenant
      config <- liftIO $ CM.evalExperimentAsString tenant context toss
      logDebug $ "The fetched config is : " <> Text.pack config
      let formattedConfig = config ^@.. _Object . reindexed (KTC.dropPrefixFromConfig (Text.pack "merchantServiceUsageConfig:")) (itraversed . indices (Text.isPrefixOf "merchantServiceUsageConfig:" . DAK.toText))
      logDebug $ "res8 = " <> show formattedConfig
      logDebug $ "The json version of res8 : " <> show (DA.Object $ DAKM.fromList formattedConfig)
      let mbParsedConfig = DA.Object (DAKM.fromList formattedConfig) ^? _JSON :: Maybe (MerchantServiceUsageConfigD 'Unsafe)
      logDebug $ "The decoded Merchant service usage config is 1 : " <> show mbParsedConfig
      case mbParsedConfig of
        Just cfg -> return $ Just $ coerce @(MerchantServiceUsageConfigD 'Unsafe) @MerchantServiceUsageConfig cfg
        _ -> do
          logError $ "Could not find the config for tenant " <> Text.pack tenant <> " for the stated merchantOperatingCityId. Trying to restart Client with backup...."
          cfg <- KSQS.findById $ Text.pack tenant
          mbHost <- liftIO $ Se.lookupEnv "CAC_HOST"
          mbInterval <- liftIO $ Se.lookupEnv "CAC_INTERVAL"
          KTC.initializeCACThroughConfig CM.createClientFromConfig (fromMaybe (error "Config not found in db for merchantServiceUsageConfig") cfg) tenant (fromMaybe "http://localhost:8080" mbHost) (fromMaybe 10 (readMaybe =<< mbInterval))
          Just <$> getMerchantServiceUsageConfigFromCACStrict id toss
    else getCfgFromCache id

getMerchantServiceUsageConfigFromCACStrict :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> m MerchantServiceUsageConfig
getMerchantServiceUsageConfigFromCACStrict id' toss = do
  context <- liftIO $ CM.hashMapToString $ HashMap.fromList [(Text.pack "merchantOperatingCityId", DA.String (getId id'))]
  tenant <- liftIO $ Se.lookupEnv "RIDER_TENANT"
  config <- liftIO $ CM.evalExperimentAsString (fromMaybe "atlas_app_v2" tenant) context toss
  let formattedConfig = config ^@.. _Object . reindexed (KTC.dropPrefixFromConfig (Text.pack "merchantServiceUsageConfig:")) (itraversed . indices (Text.isPrefixOf "merchantServiceUsageConfig:" . DAK.toText))
  let parsedConfig = DA.Object (DAKM.fromList formattedConfig) ^? _JSON :: Maybe (MerchantServiceUsageConfigD 'Unsafe)
  return $ coerce @(MerchantServiceUsageConfigD 'Unsafe) @MerchantServiceUsageConfig $ fromMaybe (error $ Text.pack $ "Could not find merchantServiceUsageConfig corresponding to the stated merchantOperatingCityId : " <> show id') parsedConfig

getCfgFromCache :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id MerchantOperatingCity -> m (Maybe MerchantServiceUsageConfig)
getCfgFromCache id =
  Hedis.safeGet (makeMerchantOperatingCityIdKey id) >>= \case
    Just a -> return . Just $ coerce @(MerchantServiceUsageConfigD 'Unsafe) @MerchantServiceUsageConfig a
    Nothing -> flip whenJust cacheMerchantServiceUsageConfig /=<< Queries.findByMerchantOperatingCityId id

cacheMerchantServiceUsageConfig :: (CacheFlow m r) => MerchantServiceUsageConfig -> m ()
cacheMerchantServiceUsageConfig merchantServiceUsageConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantOperatingCityIdKey merchantServiceUsageConfig.merchantOperatingCityId
  Hedis.setExp idKey (coerce @MerchantServiceUsageConfig @(MerchantServiceUsageConfigD 'Unsafe) merchantServiceUsageConfig) expTime

makeMerchantOperatingCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOperatingCityIdKey id = "CachedQueries:MerchantServiceUsageConfig:MerchantOperatingCityId-" <> id.getId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache merchanOperatingCityId = do
  Hedis.del (makeMerchantOperatingCityIdKey merchanOperatingCityId)

updateMerchantServiceUsageConfig :: (CacheFlow m r, EsqDBFlow m r) => MerchantServiceUsageConfig -> m ()
updateMerchantServiceUsageConfig = Queries.updateMerchantServiceUsageConfig
