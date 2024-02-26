{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverPool.Config where

import Client.Main as CM
-- import Domain.Types.Merchant.DriverPoolConfig as DPC

-- import Data.Aeson.Key
-- import qualified Data.Time.Clock as DTC
-- import qualified Data.ByteString.Lazy.Char8 as BL
-- import qualified Data.Text.Encoding as DTE
-- import Data.Aeson.Types as DAT

import Control.Lens.Combinators
import Control.Lens.Fold
import Data.Aeson as DA
import qualified Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as DAKM
import Data.Aeson.Lens
import Data.HashMap.Strict as HashMap
import Data.Text as Text hiding (find)
import Domain.Types.Merchant.DriverPoolConfig
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.Language as L (getOption)
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import qualified Kernel.Storage.Queries.SystemConfigs as KSQS
import Kernel.Types.Cac
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Error
import Kernel.Utils.Logging
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as CDP
import qualified System.Environment as SE
import System.Random

data CancellationScoreRelatedConfig = CancellationScoreRelatedConfig
  { popupDelayToAddAsPenalty :: Maybe Seconds,
    thresholdCancellationScore :: Maybe Int,
    minRidesForCancellationScore :: Maybe Int
  }
  deriving (Generic)

getDriverPoolConfigFromDB :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe Variant.Variant -> Meters -> m DriverPoolConfig
getDriverPoolConfigFromDB merchantOpCityId Nothing dist = do
  configs <- CDP.findAllByMerchantOpCityId merchantOpCityId
  getDefaultDriverPoolConfig configs dist
getDriverPoolConfigFromDB merchantOpCityId (Just vehicle) dist = do
  configs <- CDP.findAllByMerchantOpCityId merchantOpCityId
  let mbApplicableConfig = find (filterByDistAndDveh (Just vehicle) dist) configs
  case configs of
    [] -> throwError $ InvalidRequest "DriverPoolConfig not found"
    _ ->
      case mbApplicableConfig of
        Just applicableConfig -> return applicableConfig
        Nothing -> getDefaultDriverPoolConfig configs dist

-- dropDriverPoolConfig :: Key -> Key
-- dropDriverPoolConfig text =
--   -- DAK.fromText $ Text.drop 10 (Text.pack $ show text)
--   case Text.stripPrefix "driverPoolConfig:" (DAK.toText text) of
--     Just a -> DAK.fromText a
--     Nothing -> text

stringValueToObject :: [(Key, Value)] -> [(Key, Value)]
stringValueToObject [] = []
stringValueToObject ((k, v) : xs) =
  case DAK.toText k of
    "distanceBasedBatchSplit" -> ("distanceBasedBatchSplit", toJSON ((readWithInfo' v) :: [BatchSplitByPickupDistance])) : stringValueToObject xs
    _ -> (k, v) : stringValueToObject xs

getConfigFromCACStrict :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe Variant.Variant -> Meters -> m DriverPoolConfig
getConfigFromCACStrict merchantOpCityId mbvt dist = do
  dpcCond <- liftIO $ CM.hashMapToString $ HashMap.fromList ([(pack "merchantOperatingCityId", DA.String (getId merchantOpCityId)), (pack "tripDistance", DA.String (Text.pack (show dist)))] ++ (bool [] [(pack "variant", DA.String (Text.pack (show $ fromJust mbvt)))] (isJust mbvt)))
  logDebug $ "the context value is " <> show dpcCond
  tenant <- liftIO (SE.lookupEnv "DRIVER_TENANT") >>= pure . fromMaybe "atlas_driver_offer_bpp_v2"
  gen <- newStdGen
  let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
  config <- liftIO $ CM.evalExperimentAsString tenant dpcCond toss
  let res' = (config ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "driverPoolConfig:") (itraversed . indices (\k -> Text.isPrefixOf "driverPoolConfig:" (DAK.toText k))))
      res'' = stringValueToObject res'
      res = (DA.Object $ DAKM.fromList res'') ^? _JSON :: (Maybe DriverPoolConfig)
  maybe (error "error in fetching the context value driverPoolConfig: ") pure res

-- initializeCACThroughConfig :: (CacheFlow m r, EsqDBFlow m r) => m ()
-- initializeCACThroughConfig = do
--   host <- liftIO $ SE.lookupEnv "CAC_HOST"
--   interval' <- liftIO $ SE.lookupEnv "CAC_INTERVAL"
--   interval <- pure $ fromMaybe 10 (readMaybe =<< interval')
--   tenant <- liftIO (SE.lookupEnv "DRIVER_TENANT") >>= pure . fromMaybe "atlas_driver_offer_bpp_v2"
--   config <- KSQS.findById' $ Text.pack tenant
--   status <- liftIO $ CM.createClientFromConfig tenant interval (Text.unpack config.configValue) (fromMaybe "http://localhost:8080" host)
--   case status of
--     0 -> pure ()
--     _ -> error $ "error in creating the client for tenant" <> Text.pack tenant <> " retrying again"

helper :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe Variant.Variant -> Meters -> m DriverPoolConfig
helper merchantOpCityId mbvt dist = do
  tenant <- liftIO (SE.lookupEnv "DRIVER_TENANT") >>= pure . fromMaybe "atlas_driver_offer_bpp_v2"
  config <- KSQS.findById' $ Text.pack tenant
  _ <- initializeCACThroughConfig CM.createClientFromConfig config.configValue
  getConfigFromCACStrict merchantOpCityId mbvt dist

getDriverPoolConfigFromCAC :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe Variant.Variant -> Meters -> m DriverPoolConfig
getDriverPoolConfigFromCAC merchantOpCityId mbvt dist = do
  dpcCond <- liftIO $ CM.hashMapToString $ HashMap.fromList ([(pack "merchantOperatingCityId", DA.String (getId merchantOpCityId)), (pack "tripDistance", DA.String (Text.pack (show dist)))] ++ (bool [] [(pack "variant", DA.String (Text.pack (show $ fromJust mbvt)))] (isJust mbvt)))
  tenant <- liftIO (SE.lookupEnv "DRIVER_TENANT") >>= pure . fromMaybe "atlas_driver_offer_bpp_v2"
  gen <- newStdGen
  let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
  contextValue <- liftIO $ CM.evalExperimentAsString tenant dpcCond toss
  let res' = (contextValue ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "driverPoolConfig:") (itraversed . indices (\k -> Text.isPrefixOf "driverPoolConfig:" (DAK.toText k))))
      res'' = stringValueToObject res'
      res = (DA.Object $ DAKM.fromList res'') ^? _JSON :: (Maybe DriverPoolConfig)

  maybe (helper merchantOpCityId mbvt dist) pure res

-- case contextValue of
--   Left err -> do
--     host <- liftIO $ SE.lookupEnv "CAC_HOST"
--     interval' <- liftIO $ SE.lookupEnv "CAC_INTERVAL"
--     let interval = case interval' of
--           Just a -> fromMaybe 10 (readMaybe a)
--           Nothing -> 10
--     logError $ Text.pack "error in fetching the context value " <> Text.pack err
--     config <- KSQS.findById' $ Text.pack (fromMaybe "driver_offer_bpp_v2" tenant)
--     case config of
--       Just c -> do
--         logDebug $ "config value from db for tenant" <> show c
--         status <- liftIO $ CM.createClientFromConfig (fromMaybe "driver_offer_bpp_v2" tenant) interval (Text.unpack c.configValue) (fromMaybe "http://localhost:8080" host)
--         case status of
--           0 -> do
--             logDebug $ "client created for tenant" <> maybe "driver_offer_bpp_v2" Text.pack tenant
--             getDriverPoolConfig merchantOpCityId mbvt dist
--           _ -> error $ "error in creating the client for tenant" <> maybe "driver_offer_bpp_v2" Text.pack tenant <> " retrying again"
--       Nothing -> error $ "error in fetching the config value from db for tenant" <> maybe "driver_offer_bpp_v2" Text.pack tenant
--   Right contextValue' -> do
--     logDebug $ "the fetched context value is " <> show contextValue'
--     --value <- liftIO $ (CM.hashMapToString (fromMaybe (HashMap.fromList [(pack "defaultKey", DA.String (Text.pack ("defaultValue")))]) contextValue))
--     valueHere <- buildDpcType contextValue'
--     logDebug $ "the build context value is1 " <> show valueHere
--     return valueHere
-- where
--   buildDpcType cv =
--     -- pure $ fromJSONShy (ShyValue (Object cv))
--     case DAT.parse jsonToDriverPoolConfig cv of
--       Success dpc -> pure dpc
--       Error err -> do
--         logError $ pack "error in parsing the context value for driverpoolConfig " <> pack err
--         getDriverPoolConfigFromDB merchantOpCityId mbvt dist

getDriverPoolConfig ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe Variant.Variant ->
  Meters ->
  m DriverPoolConfig
getDriverPoolConfig merchantOpCityId mbvt dist = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (\sc -> sc.useCAC) systemConfigs
  case useCACConfig of
    False -> getDriverPoolConfigFromDB merchantOpCityId mbvt dist
    True -> getDriverPoolConfigFromCAC merchantOpCityId mbvt dist

filterByDistAndDveh :: Maybe Variant.Variant -> Meters -> DriverPoolConfig -> Bool
filterByDistAndDveh mbVehicle_ dist cfg =
  dist >= cfg.tripDistance && cfg.vehicleVariant == mbVehicle_

getDefaultDriverPoolConfig :: (EsqDBFlow m r) => [DriverPoolConfig] -> Meters -> m DriverPoolConfig
getDefaultDriverPoolConfig configs dist = do
  find (filterByDistAndDveh Nothing dist) configs
    & fromMaybeM (InvalidRequest "DriverPool default config not found")
