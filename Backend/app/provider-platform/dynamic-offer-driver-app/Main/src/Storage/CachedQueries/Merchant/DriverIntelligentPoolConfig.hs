{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.DriverIntelligentPoolConfig
  ( create,
    clearCache,
    findByMerchantOpCityId,
    update,
  )
where

import Client.Main as CM
import Control.Lens.Combinators
import Control.Lens.Fold
import Data.Aeson as DA
import qualified Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as DAKM
import Data.Aeson.Lens
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HashMap
import Data.Text as Text
import qualified Domain.Types.Cac as DTC
import Domain.Types.Common
import Domain.Types.Merchant.DriverIntelligentPoolConfig
import Domain.Types.Merchant.MerchantOperatingCity
import qualified EulerHS.Language as L
import qualified GHC.List as GL
import Kernel.Beam.Lib.Utils (pushToKafka)
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Queries.SystemConfigs as KSQS
import Kernel.Tools.Metrics.CoreMetrics.Types (incrementSystemConfigsFailedCounter)
import Kernel.Types.Cac
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.Merchant.DriverIntelligentPoolConfig as Queries
import qualified System.Environment as SE
import qualified System.Environment as Se
import System.Random

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverIntelligentPoolConfig -> m ()
create = Queries.create

getDriverIntelligentPoolConfigFromDB :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe DriverIntelligentPoolConfig)
getDriverIntelligentPoolConfigFromDB id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
    Just a -> return . Just $ coerce @(DriverIntelligentPoolConfigD 'Unsafe) @DriverIntelligentPoolConfig a
    Nothing -> flip whenJust cacheDriverIntelligentPoolConfig /=<< Queries.findByMerchantOpCityId id

getConfigFromCACStrict :: (CacheFlow m r, EsqDBFlow m r) => Maybe Text -> Maybe Text -> String -> Id MerchantOperatingCity -> m (Maybe DriverIntelligentPoolConfig)
getConfigFromCACStrict srId idName dipcCond id = do
  tenant <- liftIO (SE.lookupEnv "TENANT") <&> fromMaybe "atlas_driver_offer_bpp_v2"
  gen <- newStdGen
  let (toss', _) = randomR (1, 100) gen :: (Int, StdGen)
  toss <-
    maybe
      (pure toss')
      ( \srId' -> do
          Hedis.withCrossAppRedis (Hedis.safeGet (makeCACDriverIntelligentPoolConfigKey srId')) >>= \case
            Just (a :: Int) -> pure a
            Nothing -> do
              _ <- cacheToss srId' toss'
              pure toss'
      )
      srId
  contextValue <- liftIO $ CM.evalExperimentAsString tenant dipcCond toss
  let res' = contextValue ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "driverIntelligentPoolConfig:") (itraversed . indices (Text.isPrefixOf "driverIntelligentPoolConfig:" . DAK.toText))
      res = DA.Object (DAKM.fromList res') ^? _JSON :: (Maybe DriverIntelligentPoolConfig)
  let result =
        maybe
          ( do
              incrementSystemConfigsFailedCounter "cac_driver_intelligent_pool_config_parse_error"
              getDriverIntelligentPoolConfigFromDB id
          )
          (pure . Just)
          res
  when (isJust srId) do
    variantIds <- liftIO $ CM.getVariants tenant dipcCond toss
    let idName' = fromMaybe (error "idName not found") idName
        cacData = CACData (fromJust srId) idName' (Text.pack dipcCond) "driverIntelligentPoolConfig" (Text.pack (show variantIds))
    pushToKafka cacData "cac-data" ""
  result

cacFallbackHelper :: (CacheFlow m r, EsqDBFlow m r) => Maybe Text -> Maybe Text -> String -> Id MerchantOperatingCity -> m (Maybe DriverIntelligentPoolConfig)
cacFallbackHelper srId idName context id = do
  mbHost <- liftIO $ Se.lookupEnv "CAC_HOST"
  mbInterval <- liftIO $ Se.lookupEnv "CAC_INTERVAL"
  tenant <- liftIO (SE.lookupEnv "TENANT") <&> fromMaybe "atlas_driver_offer_bpp_v2"
  config <- KSQS.findById $ Text.pack tenant
  _ <- initializeCACThroughConfig CM.createClientFromConfig (fromMaybe (error "Config not found for DriverIntelligentPoolConfig in db") config) tenant (fromMaybe "http://localhost:8080" mbHost) (fromMaybe 10 (readMaybe =<< mbInterval))
  getConfigFromCACStrict srId idName context id

getDriverIntelligentPoolConfigFromCAC :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe Text -> Maybe Text -> m (Maybe DriverIntelligentPoolConfig)
getDriverIntelligentPoolConfigFromCAC id srId idName = do
  dipcCond <- liftIO $ CM.hashMapToString $ HashMap.fromList [(pack "merchantOperatingCityId", DA.String (getId id))]
  gen <- newStdGen
  let (toss', _) = randomR (1, 100) gen :: (Int, StdGen)
  toss <-
    maybe
      (pure toss')
      ( \srId' -> do
          Hedis.withCrossAppRedis (Hedis.safeGet (makeCACDriverIntelligentPoolConfigKey srId')) >>= \case
            Just (a :: Int) -> pure a
            Nothing -> do
              _ <- cacheToss srId' toss'
              pure toss'
      )
      srId
  tenant <- liftIO (SE.lookupEnv "TENANT") <&> fromMaybe "atlas_driver_offer_bpp_v2"
  contextValue <- liftIO $ CM.evalExperimentAsString tenant dipcCond toss
  let res' = contextValue ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "driverIntelligentPoolConfig:") (itraversed . indices (Text.isPrefixOf "driverIntelligentPoolConfig:" . DAK.toText))
      res = DA.Object (DAKM.fromList res') ^? _JSON :: (Maybe DriverIntelligentPoolConfig)
  maybe
    (logDebug ("DriverIntelligentPoolConfig from CAC Not Parsable: " <> show res' <> " for tenant " <> Text.pack tenant) >> cacFallbackHelper srId idName dipcCond id)
    ( \res'' -> do
        when (isJust srId) do
          variantIds <- liftIO $ CM.getVariants tenant dipcCond toss
          let idName' = fromMaybe (error "idName not found") idName
              cacData = CACData (fromJust srId) idName' (Text.pack dipcCond) "driverIntelligentPoolConfig" (Text.pack (show variantIds))
          pushToKafka cacData "cac-data" ""
        (pure . Just) res''
    )
    res

getConfigFromInMemory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe Text -> Maybe Text -> m (Maybe DriverIntelligentPoolConfig)
getConfigFromInMemory id srId idName = do
  tenant <- liftIO $ Se.lookupEnv "TENANT"
  dipc <- L.getOption (DTC.DriverIntelligentPoolConfig id.getId)
  isExp <- liftIO $ CM.isExperimentsRunning (fromMaybe "atlas_driver_offer_bpp_v2" tenant)
  bool
    ( maybe
        ( getDriverIntelligentPoolConfigFromCAC id Nothing Nothing
            >>= ( \config -> do
                    when (isJust config) do
                      L.setOption (DTC.DriverIntelligentPoolConfig id.getId) (fromJust config)
                    pure config
                )
        )
        ( \config' -> do
            isUpdateReq <- DTC.updateConfig DTC.LastUpdatedDriverPoolConfig
            if isUpdateReq
              then do
                config <- getDriverIntelligentPoolConfigFromCAC id Nothing Nothing
                when (isJust config) do
                  L.setOption (DTC.DriverIntelligentPoolConfig id.getId) (fromJust config)
                pure config
              else pure $ Just config'
        )
        dipc
    )
    (getDriverIntelligentPoolConfigFromCAC id srId idName)
    isExp

findByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe Text -> Maybe Text -> m (Maybe DriverIntelligentPoolConfig)
findByMerchantOpCityId id srId idName = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe [] (.useCAC) systemConfigs
  config <-
    if "driver_intelligent_pool_config" `GL.elem` useCACConfig
      then do
        logDebug $ "Getting driverIntelligentPoolConfig from CAC for merchantOperatingCity:" <> getId id
        getConfigFromInMemory id srId idName
      else do
        logDebug $ "Getting driverIntelligentPoolConfig from DB for merchantOperatingCity:" <> getId id
        getDriverIntelligentPoolConfigFromDB id
  logDebug $ "merchantOperatingCity for merchantOperatingCity:" <> getId id <> " is:" <> show config
  pure config

cacheDriverIntelligentPoolConfig :: CacheFlow m r => DriverIntelligentPoolConfig -> m ()
cacheDriverIntelligentPoolConfig cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantOpCityIdKey cfg.merchantOperatingCityId
  Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey (coerce @DriverIntelligentPoolConfig @(DriverIntelligentPoolConfigD 'Unsafe) cfg) expTime

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey id = "driver-offer:CachedQueries:DriverIntelligentPoolConfig:MerchantOperatingCityId-" <> id.getId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache = Hedis.withCrossAppRedis . Hedis.del . makeMerchantOpCityIdKey

update :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverIntelligentPoolConfig -> m ()
update = Queries.update

makeCACDriverIntelligentPoolConfigKey :: Text -> Text
makeCACDriverIntelligentPoolConfigKey id = "driver-offer:CAC:CachedQueries-" <> id

cacheToss :: (CacheFlow m r) => Text -> Int -> m ()
cacheToss srId toss = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeCACDriverIntelligentPoolConfigKey srId) toss expTime
