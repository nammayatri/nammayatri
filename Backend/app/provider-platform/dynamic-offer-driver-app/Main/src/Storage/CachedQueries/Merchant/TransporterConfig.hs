{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.TransporterConfig
  ( create,
    findByMerchantOpCityId,
    clearCache,
    update,
    updateFCMConfig,
    updateReferralLinkPassword,
  )
where

import qualified Client.Main as CM
-- import Domain.Types.Merchant.DriverPoolConfig as DPC

-- import Data.Aeson.Key
-- import qualified Data.Time.Clock as DTC
-- import qualified Data.ByteString.Lazy.Char8 as BL
-- import qualified Data.Text.Encoding as DTE

-- import qualified Data.ByteString.Lazy.Char8 as BSL

import Control.Lens.Combinators
import Control.Lens.Fold
import qualified Data.Aeson as DA
import qualified Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as DAKM
import Data.Aeson.Lens
import Data.Aeson.Types as DAT
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Domain.Types.Common
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Merchant.TransporterConfig
import Domain.Types.Person
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Queries.SystemConfigs as KSQS
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.Merchant.TransporterConfig as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => TransporterConfig -> m ()
create = Queries.create
import qualified System.Environment as Se
import System.Random

dropTransporterConfig :: Key -> Key
dropTransporterConfig text =
  -- DAK.fromText $ Text.drop 10 (Text.pack $ show text)
  case Text.stripPrefix "transporterConfig:" (DAK.toText text) of
    Just a -> DAK.fromText a
    Nothing -> text

getConfigFromCACStrict :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> m TransporterConfig
getConfigFromCACStrict merchantOpCityId toss = do
  dpcCond <- liftIO $ CM.hashMapToString $ HashMap.fromList ([(Text.pack "merchantOperatingCityId", DA.String (getId merchantOpCityId))])
  tenant <- liftIO (Se.lookupEnv "DRIVER_TENANT") >>= pure . fromMaybe "atlas_driver_offer_bpp_v2"
  contextValue <- liftIO $ CM.evalExperimentAsString tenant dpcCond toss
  let res' = (contextValue ^@.. _Value . _Object . reindexed dropTransporterConfig (itraversed . indices (\k -> Text.isPrefixOf "transporterConfig:" (DAK.toText k))))
      res = (DA.Object $ DAKM.fromList res') ^? _JSON :: (Maybe TransporterConfig)
  pure . fromMaybe (error "error in fetching the context value") $ res

initializeCACThroughConfig :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> m TransporterConfig
initializeCACThroughConfig merchantOpCityId toss = do
  host <- liftIO $ Se.lookupEnv "CAC_HOST"
  interval' <- liftIO $ Se.lookupEnv "CAC_INTERVAL"
  interval <- pure $ fromMaybe 10 (readMaybe =<< interval')
  tenant <- liftIO (Se.lookupEnv "DRIVER_TENANT") >>= pure . fromMaybe "atlas_driver_offer_bpp_v2"
  config <- KSQS.findById' $ Text.pack tenant
  status <- liftIO $ CM.createClientFromConfig tenant interval (Text.unpack config.configValue) (fromMaybe "http://localhost:8080" host)
  case status of
    0 -> getConfigFromCACStrict merchantOpCityId toss
    _ -> error $ "error in creating the client for tenant" <> Text.pack tenant <> " retrying again"

getConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> m TransporterConfig
getConfig id toss = do
  confCond <- liftIO $ CM.hashMapToString $ HashMap.fromList [(Text.pack "merchantOperatingCityId", DA.String (getId id))]
  tenant <- liftIO (Se.lookupEnv "DRIVER_TENANT") >>= pure . fromMaybe "driver_offer_bpp_v2"
  contextValue <- liftIO $ CM.evalExperimentAsString tenant confCond toss
  let res' = (contextValue ^@.. _Value . _Object . reindexed dropTransporterConfig (itraversed . indices (\k -> Text.isPrefixOf "transporterConfig:" (DAK.toText k))))
      res = (DA.Object $ DAKM.fromList res') ^? _JSON :: (Maybe TransporterConfig)
  maybe (initializeCACThroughConfig id toss) pure res

-- context' <- liftIO $ CM.evalExperiment (fromMaybe "driver_offer_bpp_v2" tenant) confCond toss
-- logDebug $ "transporterConfig: " <> show context'
-- ans <- case context' of
--   Left err -> do
--     host <- liftIO $ Se.lookupEnv "CAC_HOST"
--     interval' <- liftIO $ Se.lookupEnv "CAC_INTERVAL"
--     let interval = case interval' of
--           Just a -> fromMaybe 10 (readMaybe a)
--           Nothing -> 10
--     logError $ Text.pack "error in fetching the context value " <> Text.pack err
--     c <- KSQS.findById' $ Text.pack (fromMaybe "driver_offer_bpp_v2" tenant)
--     status <- liftIO $ CM.createClientFromConfig (fromMaybe "driver_offer_bpp_v2" tenant) interval (Text.unpack c.configValue) (fromMaybe "http://localhost:8080" host)
--     case status of
--       0 -> do
--         logDebug $ "client created for tenant" <> maybe "driver_offer_bpp_v2" Text.pack tenant
--         getConfig id toss
--       _ -> error $ "error in creating the client for tenant" <> maybe "driver_offer_bpp_v2" Text.pack tenant <> " retrying again"
--   Right contextValue' ->
--     case DAT.parse jsonToTransporterConfig contextValue' of
--       Success dpc -> pure $ Just dpc
--       DAT.Error err -> error $ Text.pack "error in parsing the context value for transporter config " <> Text.pack err
-- -- pure $ Just ans
-- logDebug $ "transporterConfig: " <> show ans
-- pure ans

getTransporterConfigFromDB :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe TransporterConfig)
getTransporterConfigFromDB id = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
    Just a -> return . Just $ coerce @(TransporterConfigD 'Unsafe) @TransporterConfig a
    Nothing -> flip whenJust cacheTransporterConfig /=<< Queries.findByMerchantOpCityId id

findByMerchantOpCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe (Id Person) -> m (Maybe TransporterConfig)
findByMerchantOpCityId id mPersonId = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (.useCAC) systemConfigs
  if useCACConfig
    then Just <$> (findByMerchantOpCityIdCAC id mPersonId)
    else getTransporterConfigFromDB id

findByMerchantOpCityIdCAC :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe (Id Person) -> m TransporterConfig
findByMerchantOpCityIdCAC id (Just personId) = do
  tenant <- liftIO $ Se.lookupEnv "DRIVER_TENANT"
  isExp <- liftIO $ CM.isExperimentsRunning (fromMaybe "driver_offer_bpp_v2" tenant)
  ( if isExp
      then
        ( do
            Hedis.withCrossAppRedis (Hedis.safeGet $ makeCACTransporterConfigKey personId) >>= \case
              Just (a :: Int) -> do
                getConfig id a
              Nothing -> do
                gen <- newStdGen
                let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
                logDebug $ "the toss value is for transporter config " <> show toss
                _ <- cacheToss personId toss
                getConfig id toss
        )
      else
        ( do
            getConfig id 1
        )
    )
findByMerchantOpCityIdCAC id Nothing = do
  gen <- newStdGen
  let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
  logDebug $ "the toss value is for transporter config " <> show toss
  getConfig id toss

cacheTransporterConfig :: (CacheFlow m r) => TransporterConfig -> m ()
cacheTransporterConfig cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantOpCityIdKey cfg.merchantOperatingCityId
  Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey (coerce @TransporterConfig @(TransporterConfigD 'Unsafe) cfg) expTime

cacheToss :: (CacheFlow m r) => Id Person -> Int -> m ()
cacheToss personId toss = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeCACTransporterConfigKey personId) toss expTime

makeCACTransporterConfigKey :: Id Person -> Text
makeCACTransporterConfigKey id = "driver-offer:CAC:CachedQueries:TransporterConfig:PersonId-" <> id.getId

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey id = "driver-offer:CachedQueries:TransporterConfig:MerchantOperatingCityId-" <> id.getId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache = Hedis.withCrossAppRedis . Hedis.del . makeMerchantOpCityIdKey

updateFCMConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> BaseUrl -> Text -> m ()
updateFCMConfig = Queries.updateFCMConfig

updateReferralLinkPassword :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> m ()
updateReferralLinkPassword = Queries.updateReferralLinkPassword

update :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => TransporterConfig -> m ()
update = Queries.update
