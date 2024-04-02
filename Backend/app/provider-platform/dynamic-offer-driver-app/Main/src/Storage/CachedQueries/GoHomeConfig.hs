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
import qualified Domain.Types.Cac as DTC
import Domain.Types.GoHomeConfig
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import EulerHS.Language as L (getOption, setOption)
import qualified GHC.List as GL
import Kernel.Beam.Lib.Utils (pushToKafka)
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

getGoHomeConfigFromCACStrict :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Int -> Maybe Text -> Maybe Text -> String -> m GoHomeConfig
getGoHomeConfigFromCACStrict toss stickyId idName context = do
  tenant <- liftIO $ Se.lookupEnv "TENANT"
  config <- liftIO $ CM.evalExperimentAsString (fromMaybe "driver_offer_bpp_v2" tenant) context toss
  let res8 = config ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "goHomeConfig:") (itraversed . indices (Text.isPrefixOf "goHomeConfig:" . DAK.toText))
      res9 = DA.Object (DAKM.fromList res8) ^? _JSON :: Maybe GoHomeConfig
  maybe
    (error "Could not find Go-To config")
    ( \res'' -> do
        when (isJust stickyId) do
          variantIds <- liftIO $ CM.getVariants (fromMaybe "driver_offer_bpp_v2" tenant) context toss
          let idName' = fromMaybe (error "idName not found") idName
              cacData = CACData (fromJust stickyId) idName' (Text.pack context) "goHomeConfig" (Text.pack (show variantIds))
          pushToKafka cacData "cac-data" ""
        pure res''
    )
    res9

createThroughConfigHelper :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Int -> Maybe Text -> Maybe Text -> String -> m GoHomeConfig
createThroughConfigHelper toss stickyId idName context = do
  mbHost <- liftIO $ Se.lookupEnv "CAC_HOST"
  mbInterval <- liftIO $ Se.lookupEnv "CAC_INTERVAL"
  tenant <- liftIO (Se.lookupEnv "TENANT") <&> fromMaybe "atlas_driver_offer_bpp_v2"
  config <- KSQS.findById $ Text.pack tenant
  _ <- initializeCACThroughConfig CM.createClientFromConfig (fromMaybe (error "config not found for goHomeConfig in db") config) tenant (fromMaybe "http://localhost:8080" mbHost) (fromMaybe 10 (readMaybe =<< mbInterval))
  getGoHomeConfigFromCACStrict toss stickyId idName context

getGoHomeConfigFromCAC :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> Maybe Text -> Maybe Text -> m GoHomeConfig
getGoHomeConfigFromCAC id' toss stickyId idName = do
  context <- liftIO $ CM.hashMapToString $ HashMap.fromList [(pack "merchantOperatingCityId", DA.String (getId id'))]
  tenant <- liftIO $ Se.lookupEnv "TENANT"
  config <- liftIO $ CM.evalExperimentAsString (fromMaybe "driver_offer_bpp_v2" tenant) context toss
  let res8 = config ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "goHomeConfig:") (itraversed . indices (Text.isPrefixOf "goHomeConfig:" . DAK.toText))
      res9 = DA.Object (DAKM.fromList res8) ^? _JSON :: Maybe GoHomeConfig
  maybe
    (logDebug ("GoHomeConfig from CAC Not Parsable: " <> show res8 <> " for tenant: " <> Text.pack (fromMaybe "driver_offer_bpp_v2" tenant)) >> createThroughConfigHelper toss stickyId idName context)
    ( \res'' -> do
        when (isJust stickyId) do
          variantIds <- liftIO $ CM.getVariants (fromMaybe "driver_offer_bpp_v2" tenant) context toss
          let idName' = fromMaybe (error "idName not found") idName
              cacData = CACData (fromJust stickyId) idName' (Text.pack context) "goHomeConfig" (Text.pack (show variantIds))
          pushToKafka cacData "cac-data" ""
        pure res''
    )
    res9

getGoHomeConfigFromDB :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m GoHomeConfig
getGoHomeConfigFromDB id = do
  logDebug "Fetching goHomeConfig from DB"
  Hedis.safeGet (makeGoHomeKey id) >>= \case
    Just cfg -> return cfg
    Nothing -> do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      cfg <- fromMaybeM (InternalError ("Could not find Go-To config corresponding to the stated merchant id" <> show id)) =<< Queries.findByMerchantOpCityId id
      Hedis.setExp (makeGoHomeKey id) cfg expTime
      return cfg

findByMerchantOpCityId :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe Text -> Maybe Text -> m GoHomeConfig
findByMerchantOpCityId id stickyId idName = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe [] (.useCAC) systemConfigs
  if "go_home_config" `GL.elem` useCACConfig
    then do
      tenant <- liftIO $ Se.lookupEnv "TENANT"
      isExp <- liftIO $ CM.isExperimentsRunning (fromMaybe "driver_offer_bpp_v2" tenant)
      if isExp
        then do
          Hedis.withCrossAppRedis (Hedis.safeGet $ makeCACGoHomeConfigKey stickyId) >>= \case
            (Just (a :: Int)) -> do
              getGoHomeConfigFromCAC id a stickyId idName
            Nothing -> do
              gen <- newStdGen
              let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
              when (isJust stickyId) do
                cacheToss stickyId toss
              getGoHomeConfigFromCAC id toss stickyId idName
        else getConfigsFromMemory id
    else getGoHomeConfigFromDB id

getConfigsFromMemory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m GoHomeConfig
getConfigsFromMemory id = do
  ghc <- L.getOption (DTC.GoHomeConfig id.getId)
  maybe
    (L.setOption (DTC.GoHomeConfig id.getId) /=<< getGoHomeConfigFromCAC id 1 Nothing Nothing)
    ( \ghc' -> do
        isUpdateReq <- DTC.updateConfig DTC.LastUpdatedGoHomeConfig
        if isUpdateReq
          then do
            ghc'' <- getGoHomeConfigFromCAC id 1 Nothing Nothing
            L.setOption (DTC.GoHomeConfig id.getId) ghc''
            pure ghc''
          else pure ghc'
    )
    ghc

makeGoHomeKey :: Id MerchantOperatingCity -> Text
makeGoHomeKey id = "driver-offer:CachedQueries:GoHomeConfig:MerchantOpCityId-" <> id.getId

cacheToss :: (CacheFlow m r) => Maybe Text -> Int -> m ()
cacheToss stickyId toss = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeCACGoHomeConfigKey stickyId) toss expTime

makeCACGoHomeConfigKey :: Maybe Text -> Text
makeCACGoHomeConfigKey id = "driver-offer:CAC:CachedQueries-" <> show id
