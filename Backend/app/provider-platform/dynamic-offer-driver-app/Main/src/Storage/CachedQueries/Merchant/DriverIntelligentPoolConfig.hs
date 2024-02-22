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
import Data.Aeson as DA
-- import Data.Aeson.Types as DAT
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HashMap
import Data.Text as Text
import Domain.Types.Common
import Domain.Types.Merchant.DriverIntelligentPoolConfig
import Domain.Types.Merchant.MerchantOperatingCity
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Queries.SystemConfigs as KSQS
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.Merchant.DriverIntelligentPoolConfig as Queries
import qualified System.Environment as SE
import System.Random
import qualified Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as DAKM
import Control.Lens.Combinators
import Control.Lens.Fold
import Data.Aeson.Lens

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverIntelligentPoolConfig -> m ()
create = Queries.create

getDriverIntelligentPoolConfigFromDB :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe DriverIntelligentPoolConfig)
getDriverIntelligentPoolConfigFromDB id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
    Just a -> return . Just $ coerce @(DriverIntelligentPoolConfigD 'Unsafe) @DriverIntelligentPoolConfig a
    Nothing -> flip whenJust cacheDriverIntelligentPoolConfig /=<< Queries.findByMerchantOpCityId id

dropDriverIntelligentPoolConfig :: Key -> Key
dropDriverIntelligentPoolConfig text =
  -- DAK.fromText $ Text.drop 10 (Text.pack $ show text)
  case Text.stripPrefix "driverIntelligentPoolConfig:" (DAK.toText text) of
    Just a -> DAK.fromText a
    Nothing -> text


getConfigFromCACStrict :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m DriverIntelligentPoolConfig
getConfigFromCACStrict merchantOpCityId = do
  dipcCond <- liftIO $ CM.hashMapToString $ HashMap.fromList ([(pack "merchantOperatingCityId", DA.String (getId merchantOpCityId))])
  tenant <- liftIO (SE.lookupEnv "DRIVER_TENANT") >>= pure . fromMaybe "atlas_driver_offer_bpp_v2"
  gen <- newStdGen
  let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
  contextValue <- liftIO $ CM.evalExperimentAsString tenant dipcCond toss
  let res' = (contextValue ^@.. _Value . _Object . reindexed dropDriverIntelligentPoolConfig (itraversed . indices (\k -> Text.isPrefixOf "driverIntelligentPoolConfig:" (DAK.toText k))))
      res = (DA.Object $ DAKM.fromList res') ^? _JSON :: (Maybe DriverIntelligentPoolConfig)
  pure . fromMaybe (error "error in fetching the context value driverIntelligentPoolConfig: ") $ res

initializeCACThroughConfig :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m DriverIntelligentPoolConfig
initializeCACThroughConfig id = do
  host <- liftIO $ SE.lookupEnv "CAC_HOST"
  interval' <- liftIO $ SE.lookupEnv "CAC_INTERVAL"
  interval <- pure $ fromMaybe 10 (readMaybe =<< interval')
  tenant <- liftIO (SE.lookupEnv "DRIVER_TENANT") >>= pure . fromMaybe "atlas_driver_offer_bpp_v2"
  config <- KSQS.findById' $ Text.pack tenant
  status <- liftIO $ CM.createClientFromConfig tenant interval (Text.unpack config.configValue) (fromMaybe "http://localhost:8080" host)
  case status of
    0 -> getConfigFromCACStrict id
    _ -> error $ "error in creating the client for tenant" <> Text.pack tenant <> " retrying again"

getDriverIntelligentPoolConfigFromCAC :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m DriverIntelligentPoolConfig
getDriverIntelligentPoolConfigFromCAC id = do
  dipcCond <- liftIO $ CM.hashMapToString $ HashMap.fromList ([(pack "merchantOperatingCityId", DA.String (getId id))])
  gen <- newStdGen
  let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
  tenant <- liftIO (SE.lookupEnv "DRIVER_TENANT") >>= pure . fromMaybe "driver_offer_bpp_v2"
  contextValue <- liftIO $ CM.evalExperimentAsString tenant dipcCond toss
  let res' = (contextValue ^@.. _Value . _Object . reindexed dropDriverIntelligentPoolConfig (itraversed . indices (\k -> Text.isPrefixOf "driverIntelligentPoolConfig:" (DAK.toText k))))
      res = (DA.Object $ DAKM.fromList res') ^? _JSON :: (Maybe DriverIntelligentPoolConfig)
  maybe (initializeCACThroughConfig id ) pure res
  
  --     logDebug $ "dipc: the fetched context value is " <> show contextValue'
  --     --value <- liftIO $ (CM.hashMapToString (fromMaybe (HashMap.fromList [(pack "defaultKey", DA.String (Text.pack ("defaultValue")))]) contextValue))
  --     valueHere <- buildDipcType contextValue'
  --     logDebug $ "dipc: he build context value is1 " <> show valueHere
  --     cacheDriverIntelligentPoolConfig valueHere
  --     pure $ Just valueHere
  -- where
  --   buildDipcType cv = case (DAT.parse jsonToDriverIntelligentPoolConfig cv) of
  --     DA.Success dipc -> pure $ dipc
  --     DA.Error err ->
  --       error $ (pack "error in parsing the context value for driverPoolConfig ") <> (pack err)

findByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe DriverIntelligentPoolConfig)
findByMerchantOpCityId id = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (.useCAC) systemConfigs
  case useCACConfig of
    False -> getDriverIntelligentPoolConfigFromDB id
    True -> Just <$> (getDriverIntelligentPoolConfigFromCAC id)

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
