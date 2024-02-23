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

-- import Kernel.Types.Cac

import Control.Lens.Combinators
import Control.Lens.Fold
import qualified Data.Aeson as DA
import Data.Aeson.Key as DAK
import Data.Aeson.KeyMap as DAKM
import Data.Aeson.Lens
import Data.Aeson.Types as DAT
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Domain.Types.Common
import Domain.Types.Location (dummyToLocationData)
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Merchant.TransporterConfig
import Domain.Types.Person
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types (Language)
import Kernel.Prelude as KP
import qualified Kernel.Storage.Hedis as Hedis
-- import qualified Data.ByteString.Lazy.Char8 as BSL

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

initializeCACThroughConfig :: (CacheFlow m r, EsqDBFlow m r) => m ()
initializeCACThroughConfig = do
  host <- liftIO $ Se.lookupEnv "CAC_HOST"
  interval' <- liftIO $ Se.lookupEnv "CAC_INTERVAL"
  interval <- pure $ fromMaybe 10 (readMaybe =<< interval')
  tenant <- liftIO (Se.lookupEnv "DRIVER_TENANT") >>= pure . fromMaybe "atlas_driver_offer_bpp_v2"
  config <- KSQS.findById' $ Text.pack tenant
  status <- liftIO $ CM.createClientFromConfig tenant interval (Text.unpack config.configValue) (fromMaybe "http://localhost:8080" host)
  case status of
    0 -> pure ()
    _ -> error $ "error in creating the client for tenant" <> Text.pack tenant

getTransporterConfigFromCACStrict :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> m TransporterConfig
getTransporterConfigFromCACStrict id' toss = do
  context <- liftIO $ CM.hashMapToString $ HashMap.fromList [(Text.pack "merchantOperatingCityId", DA.String (getId id'))]
  tenant <- liftIO $ Se.lookupEnv "DRIVER_TENANT"
  config <- liftIO $ CM.evalExperimentAsString (fromMaybe "driver_offer_bpp_v2" tenant) context toss
  let res' = (config ^@.. _Value . _Object . reindexed dropTransporterConfig (itraversed . indices (\k -> Text.isPrefixOf "transporterConfig:" (DAK.toText k))))
      res'' = parsingMiddleware $ DAKM.fromList res'
      res = (DA.Object res'') ^? _JSON :: Maybe TransporterConfig
  maybe (error ("Could not find TransporterConfig config corresponding to the stated merchant id" <> show id')) pure res

createThroughConfigHelper :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> m TransporterConfig
createThroughConfigHelper id' toss = do
  _ <- initializeCACThroughConfig
  getTransporterConfigFromCACStrict id' toss

-- getFcmConfigKeys :: [(Key, Value)] -> [(Key,Value)]
-- getFcmConfigKeys [] = []
-- getFcmConfigKeys ((key', val):xs) = case DAK.toText key' of
--   "fcmUrl" -> ("fcmUrl", val) : getFcmConfigKeys xs
--   "fcmServiceAccount" -> ("fcmServiceAccount", val) : getFcmConfigKeys xs
--   "fcmTokenKeyPrefix" -> ("fcmTokenKeyPrefix", val) : getFcmConfigKeys xs
--   _ ->  getFcmConfigKeys xs

-- getFcmConfig :: [(Key,Value)] -> (Key, Value)
-- getFcmConfig fcm = do
--   let keymap = DAKM.fromList fcm
--   let fcmConfig = fromMaybe (error "Missing FCM Config here.") (FCM.FCMConfig <$> (valueToType <$> (DAKM.lookup "fcmUrl" keymap )) <*> (valueToText <$> (DAKM.lookup "fcmServiceAccount" keymap)) <*> (valueToText <$> (DAKM.lookup "fcmTokenKeyPrefix" keymap)))
--   ("fcmConfig",toJSON fcmConfig)

parsingMiddleware :: DAKM.KeyMap Value -> DAKM.KeyMap Value
parsingMiddleware km =
  let newObject'' =
        DAKM.mapWithKey
          ( \k v -> case DAK.toText k of
              "languagesToBeTranslated" -> toJSON $ (readWithInfo v :: [Language])
              "notificationRetryEligibleErrorCodes" -> toJSON (valueToTextList v)
              "specialDrivers" -> toJSON (valueToTextList v)
              "specialLocationTags" -> toJSON (valueToTextList v)
              "dummyFromLocation" -> toJSON (fromMaybe dummyToLocationData (valueToMaybe v))
              "dummyToLocation" -> toJSON (fromMaybe dummyToLocationData (valueToMaybe v))
              _ -> v
          )
          km
      newObject' =
        DAKM.mapKeyVal
          (\key'' -> key'')
          ( \val -> case val of
              String s -> case ((Text.isInfixOf "{" s) && (Text.isInfixOf ":" s)) of
                True -> case (CM.convertTextToObject (Text.replace "'" "\"" s)) of
                  Left _ -> String s
                  Right obj' -> Object obj'
                False -> String s
              x -> x
          )
          newObject''
      fcmUrl = fromMaybe (error "fcmURL missing ") (valueToType <$> (DAKM.lookup "fcmUrl" newObject'))
      fcmServiceAccount = fromMaybe (error "fcmServiceAccount missing ") (valueToText <$> (DAKM.lookup "fcmServiceAccount" newObject'))
      fcmTokenKeyPrefix = fromMaybe (error "fcmTokenKeyPrefix missing ") (valueToText <$> (DAKM.lookup "fcmTokenKeyPrefix" newObject'))
      fcmConfig = FCM.FCMConfig fcmUrl fcmServiceAccount fcmTokenKeyPrefix
      newObject = KP.foldr DAKM.delete newObject' ["fcmUrl", "fcmServiceAccount", "fcmTokenKeyPrefix"]
   in DAKM.insert "fcmConfig" (toJSON fcmConfig) newObject

-- stringValueToObject :: [(Key, Value)] -> [(Key,Value)]
-- stringValueToObject [] =  []
-- stringValueToObject ((key',val):xs) = case DAK.toText key' of
--   "languagesToBeTranslated"  -> ( "languagesToBeTranslated",toJSON $ (readWithInfo val :: [Language])) : stringValueToObject xs
--   "notificationRetryEligibleErrorCodes"  -> ("notificationRetryEligibleErrorCodes",toJSON ( valueToTextList val)) : stringValueToObject xs
--   "specialDrivers"  -> ("specialDrivers",toJSON ( valueToTextList val)) : stringValueToObject xs
--   "specialLocationTags"  -> ("specialLocationTags",toJSON ( valueToTextList val)) : stringValueToObject xs
--   "dummyFromLocation"  -> ("dummyFromLocation", toJSON ( fromMaybe dummyToLocationData (valueToMaybe val))) : stringValueToObject xs
--   "dummyToLocation"  -> ("dummyToLocation", toJSON ( fromMaybe dummyToLocationData (valueToMaybe val))) : stringValueToObject xs
-- "availabilityTimeWindowOption" -> ("availabilityTimeWindowOption", toJSON ((valueToType v) :: SWC.SlidingWindowOptions)) : stringValueToObject xs
-- "acceptanceRatioWindowOption" -> ("acceptanceRatioWindowOption", toJSON ((valueToType v) :: SWC.SlidingWindowOptions)) : stringValueToObject xs
-- "cancellationAndRideFrequencyRatioWindowOption" -> ("cancellationAndRideFrequencyRatioWindowOption", toJSON ((valueToType v) :: SWC.SlidingWindowOptions)) : stringValueToObject xs
-- "minQuotesToQualifyForIntelligentPoolWindowOption" -> ("minQuotesToQualifyForIntelligentPoolWindowOption", toJSON ((valueToType v) :: SWC.SlidingWindowOptions)) : stringValueToObject xs
-- _ -> (key',val) : stringValueToObject xs

getConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> m TransporterConfig
getConfig id toss = do
  confCond <- liftIO $ CM.hashMapToString $ HashMap.fromList [(Text.pack "merchantOperatingCityId", DA.String (getId id))]
  tenant <- liftIO $ Se.lookupEnv "DRIVER_TENANT"
  config <- liftIO $ CM.evalExperimentAsString (fromMaybe "driver_offer_bpp_v2" tenant) confCond toss
  let res' = (config ^@.. _Value . _Object . reindexed dropTransporterConfig (itraversed . indices (\k -> Text.isPrefixOf "transporterConfig:" (DAK.toText k))))
      res'' = parsingMiddleware $ DAKM.fromList res'
      res = (DA.Object res'') ^? _JSON :: Maybe TransporterConfig
  maybe (createThroughConfigHelper id toss) pure res

-- ans <- case context' of
--   Left err -> do
--     host <- liftIO $ Se.lookupEnv "CAC_HOST"
--     interval' <- liftIO $ Se.lookupEnv "CAC_INTERVAL"
--     let interval = case interval' of
--           Just a -> fromMaybe 10 (readMaybe a)
--           Nothing -> 10
--     logError $ Text.pack "error in fetching the context value " <> Text.pack err
--     c <- KSQS.findById' $ Text.pack (fromMaybe "driver_offer_bpp_v2" tenant)
--     logDebug $ "config value from db for tenant" <> show c
--     status <- liftIO $ CM.createClientFromConfig (fromMaybe "driver_offer_bpp_v2" tenant) interval (Text.unpack c.configValue) (fromMaybe "http://localhost:8080" host)
--     case status of
--       0 -> do
--         logDebug $ "client created for tenant" <> maybe "driver_offer_bpp_v2" Text.pack tenant
--         getConfig id toss
--       _ -> error $ "error in creating the client for tenant" <> maybe "driver_offer_bpp_v2" Text.pack tenant <> " retrying again"
--   Right contextValue' -> do
--     let x = fromJSONCAC (CACValue (Object contextValue'))
--     case x of
--       Success dpc -> pure $ Just dpc
--       DA.Error err -> error $ Text.pack "error in parsing the context value for transporter config " <> Text.pack err

-- case DAT.parse jsonToTransporterConfig contextValue' of
--   Success dpc -> pure $ Just dpc
--   DAT.Error err -> error $ Text.pack "error in parsing the context value for transporter config " <> Text.pack err
-- pure $ Just ans

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
    then Just <$> findByMerchantOpCityIdCAC id mPersonId
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
