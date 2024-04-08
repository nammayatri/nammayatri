{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverPool.Config where

import qualified Client.Main as CM
import Control.Lens.Combinators
import Control.Lens.Fold
import Data.Aeson as DA
import qualified Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as DAKM
import Data.Aeson.Lens
import qualified Data.HashMap.Strict as HashMap
import Data.Text as Text hiding (find)
import qualified Domain.Types.Cac as DTC
import qualified Domain.Types.Common as DTC
import Domain.Types.DriverPoolConfig
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.ServiceTierType as DVST
import EulerHS.Language as L (getOption)
import qualified EulerHS.Language as L
import qualified GHC.List as GL
import Kernel.Beam.Lib.Utils (pushToKafka)
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude as KP
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Queries.SystemConfigs as KSQS
import Kernel.Types.Cac
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Error
import Kernel.Utils.Logging
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as CDP
import qualified System.Environment as SE
import qualified System.Environment as Se
import System.Random

data CancellationScoreRelatedConfig = CancellationScoreRelatedConfig
  { popupDelayToAddAsPenalty :: Maybe Seconds,
    thresholdCancellationScore :: Maybe Int,
    minRidesForCancellationScore :: Maybe Int
  }
  deriving (Generic)

getSearchDriverPoolConfig ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe Meters ->
  m DriverPoolConfig
getSearchDriverPoolConfig merchantOpCityId mbDist = do
  let serviceTier = Nothing
      tripCategory = "All"
  getDriverPoolConfigHelper merchantOpCityId serviceTier tripCategory mbDist Nothing Nothing

getDriverPoolConfigFromDB ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe DVST.ServiceTierType ->
  String ->
  Maybe Meters ->
  m DriverPoolConfig
getDriverPoolConfigFromDB merchantOpCityId serviceTier tripCategory mbDist = do
  let distance = fromMaybe 0 mbDist
  configs <- CDP.findAllByMerchantOpCityId merchantOpCityId
  let mbApplicableConfig = find (filterByDistAndDveh serviceTier (Text.pack tripCategory) distance) configs
  case configs of
    [] -> throwError $ InvalidRequest $ "DriverPool Configs not found for MerchantOperatingCity: " <> merchantOpCityId.getId
    _ ->
      case mbApplicableConfig of
        Just applicableConfig -> return applicableConfig
        Nothing -> do
          let alternativeConfigs = find (filterByDistAndDveh serviceTier "All" distance) configs
          case alternativeConfigs of
            Just cfg -> return cfg
            Nothing -> findDriverPoolConfig configs Nothing "All" distance

getConfigFromCACStrict :: (CacheFlow m r, EsqDBFlow m r) => Maybe Text -> Maybe Text -> String -> m DriverPoolConfig
getConfigFromCACStrict srId idName context = do
  tenant <- liftIO (SE.lookupEnv "TENANT") <&> fromMaybe "atlas_driver_offer_bpp_v2"
  gen <- newStdGen
  let (toss', _) = randomR (1, 100) gen :: (Int, StdGen)
  toss <-
    maybe
      (pure toss')
      ( \srId' -> do
          Hedis.withCrossAppRedis (Hedis.safeGet (makeCACDriverPoolConfigKey srId')) >>= \case
            Just (a :: Int) -> pure a
            Nothing -> do
              _ <- cacheToss srId' toss'
              pure toss'
      )
      srId
  config <- liftIO $ CM.evalExperimentAsString tenant context toss
  let res' = config ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "driverPoolConfig:") (itraversed . indices (Text.isPrefixOf "driverPoolConfig:" . DAK.toText))
      res = DA.Object (DAKM.fromList res') ^? _JSON :: Maybe DriverPoolConfig
  maybe
    (error "error in fetching the context value driverPoolConfig: ")
    ( \res'' -> do
        when (isJust srId) do
          variantIds <- liftIO $ CM.getVariants tenant context toss
          let idName' = fromMaybe (error "idName not found") idName
              cacData = CACData (fromJust srId) idName' (Text.pack context) "driverPoolConfig" (Text.pack (show variantIds))
          pushToKafka cacData "cac-data" ""
        pure res''
    )
    res

helper :: (CacheFlow m r, EsqDBFlow m r) => Maybe Text -> Maybe Text -> String -> m DriverPoolConfig
helper srId idName context = do
  mbHost <- liftIO $ Se.lookupEnv "CAC_HOST"
  mbInterval <- liftIO $ Se.lookupEnv "CAC_INTERVAL"
  tenant <- liftIO (SE.lookupEnv "TENANT") <&> fromMaybe "atlas_driver_offer_bpp_v2"
  config <- KSQS.findById $ Text.pack tenant
  _ <- initializeCACThroughConfig CM.createClientFromConfig (fromMaybe (error "Config not found in db for driverPoolConfig") config) tenant (fromMaybe "http://localhost:8080" mbHost) (fromMaybe 10 (readMaybe =<< mbInterval))
  getConfigFromCACStrict srId idName context

getDriverPoolConfigFromCAC :: (CacheFlow m r, EsqDBFlow m r) => String -> Maybe Text -> Maybe Text -> m DriverPoolConfig
getDriverPoolConfigFromCAC dpcCond srId idName = do
  tenant <- liftIO (SE.lookupEnv "TENANT") <&> fromMaybe "atlas_driver_offer_bpp_v2"
  gen <- newStdGen
  let (toss', _) = randomR (1, 100) gen :: (Int, StdGen)
  toss <-
    maybe
      (pure toss')
      ( \srId' -> do
          Hedis.withCrossAppRedis (Hedis.safeGet (makeCACDriverPoolConfigKey srId')) >>= \case
            Just (a :: Int) -> pure a
            Nothing -> do
              _ <- cacheToss srId' toss'
              pure toss'
      )
      srId
  contextValue <- liftIO $ CM.evalExperimentAsString tenant dpcCond toss
  let res' = contextValue ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "driverPoolConfig:") (itraversed . indices (Text.isPrefixOf "driverPoolConfig:" . DAK.toText))
      res = DA.Object (DAKM.fromList res') ^? _JSON :: (Maybe DriverPoolConfig)
  maybe
    (logDebug ("DriverPoolConfig from CAC: " <> show res' <> " for tenant " <> Text.pack tenant) >> helper srId idName dpcCond)
    ( \res'' -> do
        when (isJust srId) do
          variantIds <- liftIO $ CM.getVariants tenant dpcCond toss
          let idName' = fromMaybe (error "idName not found") idName
              cacData = CACData (fromJust srId) idName' (Text.pack dpcCond) "driverPoolConfig" (Text.pack (show variantIds))
          pushToKafka cacData "cac-data" ""
        pure res''
    )
    res

doubleToInt :: Double -> Int
doubleToInt = round

getConfigFromInMemory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe DVST.ServiceTierType -> String -> Meters -> Maybe Text -> Maybe Text -> m DriverPoolConfig
getConfigFromInMemory id mbvst tripCategory dist srId idName = do
  tenant <- liftIO $ Se.lookupEnv "TENANT"
  let roundeDist = doubleToInt (fromIntegral (dist.getMeters + 500) / 1000)
  dpc <- L.getOption (DTC.DriverPoolConfig id.getId (show mbvst) tripCategory roundeDist)
  isExp <- liftIO $ CM.isExperimentsRunning (fromMaybe "driver_offer_bpp_v2" tenant)
  dpcCond <- liftIO $ CM.hashMapToString $ HashMap.fromList ([(pack "merchantOperatingCityId", DA.String (getId id)), (pack "tripCategory", DA.String (Text.pack tripCategory)), (pack "tripDistance", DA.String (Text.pack (show dist)))] <> [("vehicleVariant", DA.String (Text.pack (show (fromJust mbvst)))) | isJust mbvst])
  cfg <-
    bool
      ( maybe
          ( getDriverPoolConfigFromCAC dpcCond Nothing Nothing
              >>= ( \config -> do
                      L.setOption (DTC.DriverPoolConfig id.getId (show mbvst) tripCategory roundeDist) config
                      pure config
                  )
          )
          ( \config' -> do
              isUpdateReq <- DTC.updateConfig DTC.LastUpdatedDriverPoolConfig
              if isUpdateReq
                then do
                  config <- getDriverPoolConfigFromCAC dpcCond Nothing Nothing
                  L.setOption (DTC.DriverPoolConfig id.getId (show mbvst) tripCategory roundeDist) config
                  pure config
                else pure config'
          )
          dpc
      )
      (getDriverPoolConfigFromCAC dpcCond srId idName)
      isExp
  if tripCategory == Text.unpack cfg.tripCategory && mbvst == cfg.vehicleVariant
    then pure cfg
    else do
      logDebug $ "Did not find driverPoolConfig for tripCategory " <> Text.pack tripCategory <> " and serviceTier " <> show mbvst <> " merchantOperatingCityid" <> show id
      dpcCond' <- liftIO $ CM.hashMapToString $ HashMap.fromList ([(pack "merchantOperatingCityId", DA.String (getId id)), (pack "tripCategory", "All"), (pack "tripDistance", DA.String (Text.pack (show dist)))] <> [("vehicleVariant", DA.String (Text.pack (show (fromJust mbvst)))) | isJust mbvst])
      cfg' <- getDriverPoolConfigFromCAC dpcCond' srId idName
      if cfg'.vehicleVariant == mbvst
        then pure cfg'
        else do
          logDebug $ "Did not find driverPoolConfig for tripCategory ALL" <> " and serviceTier " <> show mbvst <> " merchantOperatingCityid" <> show id
          dpcCond'' <- liftIO $ CM.hashMapToString $ HashMap.fromList [(pack "merchantOperatingCityId", DA.String (getId id)), (pack "tripCategory", "All"), (pack "tripDistance", DA.String (Text.pack (show dist)))]
          getDriverPoolConfigFromCAC dpcCond'' srId idName

getDriverPoolConfigHelper ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe DVST.ServiceTierType ->
  String ->
  Maybe Meters ->
  Maybe Text ->
  Maybe Text ->
  m DriverPoolConfig
getDriverPoolConfigHelper merchantOpCityId serviceTier tripCategory mbDist srId idName = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe [] (.useCAC) systemConfigs
  if "driver_pool_config" `GL.elem` useCACConfig
    then do
      logDebug $ "Getting driverPoolConfig from CAC for merchatOperatingCity:" <> getId merchantOpCityId
      getConfigFromInMemory merchantOpCityId serviceTier tripCategory (fromMaybe 0 mbDist) srId idName
    else do
      logDebug $ "Getting driverPoolConfig from DB for merchatOperatingCity:" <> getId merchantOpCityId
      getDriverPoolConfigFromDB merchantOpCityId serviceTier tripCategory mbDist

getDriverPoolConfig ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  DVST.ServiceTierType ->
  DTC.TripCategory ->
  Maybe Meters ->
  Maybe Text ->
  Maybe Text ->
  m DriverPoolConfig
getDriverPoolConfig merchantOpCityId serviceTier tripCategory tripDistance srId idName = do
  config <- getDriverPoolConfigHelper merchantOpCityId (Just serviceTier) (show tripCategory) tripDistance srId idName
  logDebug $ "driverPoolConfig we recieved for merchantOpCityId:" <> getId merchantOpCityId <> " and serviceTier:" <> show serviceTier <> " and tripCategory:" <> show tripCategory <> " and tripDistance:" <> show tripDistance <> " is:" <> show config
  pure config

filterByDistAndDveh :: Maybe DVST.ServiceTierType -> Text -> Meters -> DriverPoolConfig -> Bool
filterByDistAndDveh serviceTier tripCategory dist cfg =
  dist >= cfg.tripDistance && cfg.vehicleVariant == serviceTier && cfg.tripCategory == tripCategory

findDriverPoolConfig :: (EsqDBFlow m r) => [DriverPoolConfig] -> Maybe DVST.ServiceTierType -> Text -> Meters -> m DriverPoolConfig
findDriverPoolConfig configs serviceTier tripCategory dist = do
  find (filterByDistAndDveh serviceTier tripCategory dist) configs
    & fromMaybeM (InvalidRequest $ "DriverPool Config not found: " <> show serviceTier <> show tripCategory <> show dist)

makeCACDriverPoolConfigKey :: Text -> Text
makeCACDriverPoolConfigKey id = "driver-offer:CAC:CachedQueries-" <> id

cacheToss :: (CacheFlow m r) => Text -> Int -> m ()
cacheToss srId toss = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeCACDriverPoolConfigKey srId) toss expTime
