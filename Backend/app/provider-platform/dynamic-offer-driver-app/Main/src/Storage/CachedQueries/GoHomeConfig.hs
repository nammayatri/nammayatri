{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.GoHomeConfig where

import qualified Client.Main as CM
import Control.Lens.Combinators
import Control.Lens.Fold
import Control.Monad
import Data.Aeson as DA
import qualified Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as DAKM
import Data.Aeson.Lens
import Data.HashMap.Strict as HashMap
import Data.Text as Text
import Domain.Types.GoHomeConfig
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person as DP
import EulerHS.Language as L (getOption)
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Queries.SystemConfigs as KSQS
import Kernel.Types.Cac
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.GoHomeConfig as Queries
import qualified System.Environment ()
import qualified System.Environment as Se
import System.Random
import Tools.Error (GenericError (..))

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => GoHomeConfig -> m ()
create = Queries.create

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => GoHomeConfig -> m ()
create = Queries.create


-- getGoHomeConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> m GoHomeConfig
-- getGoHomeConfig id toss = do
--   confCond <- liftIO $ CM.hashMapToString $ HashMap.fromList [(pack "merchantOperatingCityId", DA.String (getId id))]
--   logDebug $ "goHomeConfig Cond: " <> show confCond
--   tenant <- liftIO $ Se.lookupEnv "DRIVER_TENANT"
--   context' <- liftIO $ CM.evalExperiment (fromMaybe "driver_offer_bpp_v2" tenant) confCond toss
--   logDebug $ "goHomeConfig: " <> show context'
--   ans <- case context' of
--     Left err -> do
--       host <- liftIO $ Se.lookupEnv "CAC_HOST"
--       interval' <- liftIO $ Se.lookupEnv "CAC_INTERVAL"
--       let interval = case interval' of
--             Just a -> fromMaybe 10 (readMaybe a)
--             Nothing -> 10
--       logError $ Text.pack "error in fetching the context value " <> Text.pack err
--       c <- KSQS.findById' $ Text.pack (fromMaybe "driver_offer_bpp_v2" tenant)
--       status <- liftIO $ CM.createClientFromConfig (fromMaybe "driver_offer_bpp_v2" tenant) interval (Text.unpack c.configValue) (fromMaybe "http://localhost:8080" host)
--       case status of
--         0 -> do
--           logDebug $ "client created for tenant" <> maybe "driver_offer_bpp_v2" Text.pack tenant
--           getGoHomeConfig id toss
--         _ -> error $ "error in creating the client for tenant" <> maybe "driver_offer_bpp_v2" Text.pack tenant <> " retrying again"
--     Right contextValue' ->
--       case DAT.parse jsonToGoHomeConfig contextValue' of
--         Success dpc -> pure dpc
--         DAT.Error err -> error $ pack "error in parsing the context value for go home config " <> pack err
--   -- pure $ Just ans
--   logDebug $ "goHomeConfig: " <> show ans
--   pure ans

-- dropGoHomeConfig :: Key -> Key
-- dropGoHomeConfig text =
--   -- DAK.fromText $ Text.drop 10 (Text.pack $ show text)
--   case Text.stripPrefix "goHomeConfig:" (DAK.toText text) of
--     Just a -> DAK.fromText a
--     Nothing -> text

-- initializeCACThroughConfig :: (CacheFlow m r, EsqDBFlow m r) => m ()
-- initializeCACThroughConfig = do
--   host <- liftIO $ Se.lookupEnv "CAC_HOST"
--   interval' <- liftIO $ Se.lookupEnv "CAC_INTERVAL"
--   interval <- pure $ fromMaybe 10 (readMaybe =<< interval')
--   tenant <- liftIO (Se.lookupEnv "DRIVER_TENANT") >>= pure . fromMaybe "atlas_driver_offer_bpp_v2"
--   config <- KSQS.findById' $ Text.pack tenant
--   status <- liftIO $ CM.createClientFromConfig tenant interval (Text.unpack config.configValue) (fromMaybe "http://localhost:8080" host)
--   case status of
--     0 -> pure ()
--     _ -> error $ "error in creating the client for tenant" <> Text.pack tenant <> " retrying again"

getGoHomeConfigFromCACStrict :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> m GoHomeConfig
getGoHomeConfigFromCACStrict id' toss = do
  context <- liftIO $ CM.hashMapToString $ HashMap.fromList [(pack "merchantOperatingCityId", DA.String (getId id'))]
  tenant <- liftIO $ Se.lookupEnv "DRIVER_TENANT"
  config <- liftIO $ CM.evalExperimentAsString (fromMaybe "driver_offer_bpp_v2" tenant) context toss
  let res8 = (config ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "goHomeConfig:") (itraversed . indices (\k -> Text.isPrefixOf "goHomeConfig:" (DAK.toText k))))
      res9 = (DA.Object $ DAKM.fromList res8) ^? _JSON :: Maybe GoHomeConfig
  maybe (error ("Could not find Go-To config corresponding to the stated merchant id" <> show id')) pure res9

createThroughConfigHelper :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> m GoHomeConfig
createThroughConfigHelper id' toss = do
  mbHost <- liftIO $ Se.lookupEnv "CAC_HOST"
  mbInterval <- liftIO $ Se.lookupEnv "CAC_INTERVAL"
  tenant <- liftIO (Se.lookupEnv "DRIVER_TENANT") >>= pure . fromMaybe "atlas_driver_offer_bpp_v2"
  config <- KSQS.findById' $ Text.pack tenant
  _ <- initializeCACThroughConfig CM.createClientFromConfig config.configValue tenant (fromMaybe "http://localhost:8080" mbHost) (fromMaybe 10 (readMaybe =<< mbInterval))
  getGoHomeConfigFromCACStrict id' toss

getGoHomeConfigFromCAC :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> m GoHomeConfig
getGoHomeConfigFromCAC id' toss = do
  context <- liftIO $ CM.hashMapToString $ HashMap.fromList [(pack "merchantOperatingCityId", DA.String (getId id'))]
  tenant <- liftIO $ Se.lookupEnv "DRIVER_TENANT"
  config <- liftIO $ CM.evalExperimentAsString (fromMaybe "driver_offer_bpp_v2" tenant) context toss
  let res8 = (config ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "goHomeConfig:") (itraversed . indices (\k -> Text.isPrefixOf "goHomeConfig:" (DAK.toText k))))
      res9 = (DA.Object $ DAKM.fromList res8) ^? _JSON :: Maybe GoHomeConfig
  maybe (createThroughConfigHelper id' toss) pure res9

getGoHomeConfigFromDB :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m GoHomeConfig
getGoHomeConfigFromDB id = do
  logDebug $ "Fetching goHomeConfig from DB"
  Hedis.safeGet (makeGoHomeKey id) >>= \case
    Just cfg -> return cfg
    Nothing -> do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      cfg <- fromMaybeM (InternalError ("Could not find Go-To config corresponding to the stated merchant id" <> show id)) =<< Queries.findByMerchantOpCityId id
      Hedis.setExp (makeGoHomeKey id) cfg expTime
      return cfg

findByMerchantOpCityId :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe (Id DP.Person) -> m GoHomeConfig
findByMerchantOpCityId id (Just personId) = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (\sc -> sc.useCAC) systemConfigs
  case useCACConfig of
    False -> getGoHomeConfigFromDB id
    True -> do
      tenant <- liftIO $ Se.lookupEnv "DRIVER_TENANT"
      isExp <- liftIO $ CM.isExperimentsRunning (fromMaybe "driver_offer_bpp_v2" tenant)
      case isExp of
        True -> do
          Hedis.withCrossAppRedis (Hedis.safeGet $ makeCACGoHomeConfigKey personId) >>= \case
            (Just (a :: Int)) -> do
              getGoHomeConfigFromCAC id a
            Nothing -> do
              gen <- newStdGen
              let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
              logDebug $ "the toss value is for goHomeConfig " <> show toss
              _ <- cacheToss personId toss
              getGoHomeConfigFromCAC id toss
        False -> getGoHomeConfigFromCAC id 1
findByMerchantOpCityId id Nothing = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (\sc -> sc.useCAC) systemConfigs
  case useCACConfig of
    True -> do
      gen <- newStdGen
      let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
      logDebug $ "the toss value is for goHomeConfig " <> show toss
      getGoHomeConfigFromCAC id toss
    False -> getGoHomeConfigFromDB id

makeGoHomeKey :: Id MerchantOperatingCity -> Text
makeGoHomeKey id = "driver-offer:CachedQueries:GoHomeConfig:MerchantOpCityId-" <> id.getId

cacheToss :: (CacheFlow m r) => Id Person -> Int -> m ()
cacheToss personId toss = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeCACGoHomeConfigKey personId) toss expTime

makeCACGoHomeConfigKey :: Id Person -> Text
makeCACGoHomeConfigKey id = "driver-offer:CAC:CachedQueries:GoHomeConfig:PersonId-" <> id.getId
