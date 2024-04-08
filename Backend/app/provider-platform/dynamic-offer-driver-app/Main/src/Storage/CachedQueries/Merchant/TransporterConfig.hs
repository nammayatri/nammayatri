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
import Control.Lens.Combinators
import Control.Lens.Fold
import Data.Aeson as A
import qualified Data.Aeson as DA
import Data.Aeson.Key as DAK
import Data.Aeson.KeyMap as DAKM
import Data.Aeson.Lens
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Domain.Types.Cac as DTC
import Domain.Types.Common
import Domain.Types.Location (dummyToLocationData)
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Merchant.TransporterConfig
import qualified EulerHS.Language as L
import qualified GHC.List as GL
import Kernel.Beam.Lib.Utils (pushToKafka)
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude as KP
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Queries.SystemConfigs as KSQS
import Kernel.Types.Cac
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.Merchant.TransporterConfig as Queries
import qualified System.Environment as Se
import System.Random

valueToType :: FromJSON a => A.Value -> a
valueToType val = case A.fromJSON val of
  A.Success a -> a
  A.Error _ -> error "Not a string"

valueToText :: Value -> Text
valueToText val = case val of
  String text -> text
  _ -> error "Not a string"

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => TransporterConfig -> m ()
create = Queries.create

getTransporterConfigFromCACStrict :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> m TransporterConfig
getTransporterConfigFromCACStrict id' toss = do
  context <- liftIO $ CM.hashMapToString $ HashMap.fromList [(Text.pack "merchantOperatingCityId", DA.String (getId id'))]
  tenant <- liftIO $ Se.lookupEnv "TENANT"
  config <- liftIO $ CM.evalExperimentAsString (fromMaybe "driver_offer_bpp_v2" tenant) context toss
  let res' =
        config
          ^@.. _Value
            . _Object
            . reindexed
              (dropPrefixFromConfig "transporterConfig:")
              ( itraversed
                  . indices (Text.isPrefixOf "transporterConfig:" . DAK.toText)
              )
      res'' = parsingMiddleware $ DAKM.fromList res'
      res = DA.Object res'' ^? _JSON :: Maybe TransporterConfig
  maybe (throwError (InternalError ("Could not find TransporterConfig config corresponding to the stated merchant id" <> id'.getId))) pure res

createThroughConfigHelper :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> m TransporterConfig
createThroughConfigHelper id' toss = do
  mbHost <- liftIO $ Se.lookupEnv "CAC_HOST"
  mbInterval <- liftIO $ Se.lookupEnv "CAC_INTERVAL"
  tenant <- liftIO (Se.lookupEnv "TENANT") <&> fromMaybe "atlas_driver_offer_bpp_v2"
  config <- KSQS.findById $ Text.pack tenant
  _ <- initializeCACThroughConfig CM.createClientFromConfig (fromMaybe (error "config not found for transporterConfig in db") config) tenant (fromMaybe "http://localhost:8080" mbHost) (fromMaybe 10 (readMaybe =<< mbInterval))
  getTransporterConfigFromCACStrict id' toss

parsingMiddleware :: DAKM.KeyMap Value -> DAKM.KeyMap Value
parsingMiddleware km =
  let newObject'' =
        DAKM.mapWithKey
          ( \k v -> case DAK.toText k of
              "dummyFromLocation" -> toJSON (fromMaybe dummyToLocationData (fromJSONHelper v))
              "dummyToLocation" -> toJSON (fromMaybe dummyToLocationData (fromJSONHelper v))
              _ -> v
          )
          km
      fcmUrl = valueToType <$> DAKM.lookup "fcmUrl" newObject''
      fcmServiceAccount = valueToText <$> DAKM.lookup "fcmServiceAccount" newObject''
      fcmTokenKeyPrefix = valueToText <$> DAKM.lookup "fcmTokenKeyPrefix" newObject''
      fcmConfig = FCM.FCMConfig <$> fcmUrl <*> fcmServiceAccount <*> fcmTokenKeyPrefix
      newObject = KP.foldr DAKM.delete newObject'' ["fcmUrl", "fcmServiceAccount", "fcmTokenKeyPrefix"]
   in DAKM.insert "fcmConfig" (toJSON fcmConfig) newObject

getConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> Maybe Text -> Maybe Text -> m TransporterConfig
getConfig id toss stickId idName = do
  confCond <- liftIO $ CM.hashMapToString $ HashMap.fromList [(Text.pack "merchantOperatingCityId", DA.String (getId id))]
  tenant <- liftIO $ Se.lookupEnv "TENANT"
  config <- liftIO $ CM.evalExperimentAsString (fromMaybe "driver_offer_bpp_v2" tenant) confCond toss
  let res' =
        config
          ^@.. _Value
            . _Object
            . reindexed
              (dropPrefixFromConfig "transporterConfig:")
              ( itraversed
                  . indices (Text.isPrefixOf "transporterConfig:" . DAK.toText)
              )
      res'' = parsingMiddleware $ DAKM.fromList res'
      res = DA.Object res'' ^? _JSON :: Maybe TransporterConfig
  maybe
    (logDebug ("TransporterConfig from CAC Not Parsable: " <> show res' <> "for tenant" <> Text.pack (fromMaybe "driver_offer_bpp_v2" tenant)) >> createThroughConfigHelper id toss)
    ( \res''' -> do
        when (isJust stickId) do
          variantIds <- liftIO $ CM.getVariants (fromMaybe "driver_offer_bpp_v2" tenant) confCond toss
          let idName' = fromMaybe (error "idName not found") idName
              cacData = CACData (fromJust stickId) idName' (Text.pack confCond) "transporterConfig" (Text.pack (show variantIds))
          pushToKafka cacData "cac-data" ""
        pure res'''
    )
    res

getConfigFromMemory :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> m TransporterConfig
getConfigFromMemory id toss = do
  value <- L.getOption (DTC.TransporterConfig id.getId)
  maybe
    ( getConfig id toss Nothing Nothing
        >>= ( \config -> do
                L.setOption (DTC.TransporterConfig id.getId) config
                pure config
            )
    )
    ( \config' -> do
        isUpdateReq <- DTC.updateConfig DTC.LastUpdatedTransporterConfig
        if isUpdateReq
          then do
            config <- getConfig id toss Nothing Nothing
            L.setOption (DTC.TransporterConfig id.getId) config
            pure config
          else pure config'
    )
    value

getTransporterConfigFromDB :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe TransporterConfig)
getTransporterConfigFromDB id = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
    Just a -> return . Just $ coerce @(TransporterConfigD 'Unsafe) @TransporterConfig a
    Nothing -> flip whenJust cacheTransporterConfig /=<< Queries.findByMerchantOpCityId id

findByMerchantOpCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe Text -> Maybe Text -> m (Maybe TransporterConfig)
findByMerchantOpCityId id mbstickId idName = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe [] (.useCAC) systemConfigs
  config <-
    if "transporter_config" `GL.elem` useCACConfig
      then do
        logDebug $ "Getting transporterConfig from CAC for merchatOperatingCity:" <> getId id
        Just <$> findByMerchantOpCityIdCAC id mbstickId idName
      else do
        logDebug $ "Getting transporterConfig from DB for merchatOperatingCity:" <> getId id
        getTransporterConfigFromDB id
  logDebug $ "transporterConfig we recieved for merchantOperatingCityId:" <> getId id <> " is:" <> show config
  pure config

findByMerchantOpCityIdCAC :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe Text -> Maybe Text -> m TransporterConfig
findByMerchantOpCityIdCAC id (Just stickId) idName = do
  tenant <- liftIO $ Se.lookupEnv "TENANT"
  isExp <- liftIO $ CM.isExperimentsRunning (fromMaybe "driver_offer_bpp_v2" tenant)
  ( if isExp
      then
        ( do
            Hedis.withCrossAppRedis (Hedis.safeGet $ makeCACTransporterConfigKey stickId) >>= \case
              Just (a :: Int) -> do
                getConfig id a (Just stickId) idName
              Nothing -> do
                gen <- newStdGen
                let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
                _ <- cacheToss stickId toss
                getConfig id toss (Just stickId) idName
        )
      else
        ( do
            getConfigFromMemory id 1
        )
    )
findByMerchantOpCityIdCAC id Nothing _ = do
  gen <- newStdGen
  let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
  getConfig id toss Nothing Nothing

cacheTransporterConfig :: (CacheFlow m r) => TransporterConfig -> m ()
cacheTransporterConfig cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantOpCityIdKey cfg.merchantOperatingCityId
  Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey (coerce @TransporterConfig @(TransporterConfigD 'Unsafe) cfg) expTime

cacheToss :: (CacheFlow m r) => Text -> Int -> m ()
cacheToss stickId toss = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeCACTransporterConfigKey stickId) toss expTime

makeCACTransporterConfigKey :: Text -> Text
makeCACTransporterConfigKey id = "driver-offer:CAC:CachedQueries-" <> id

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
