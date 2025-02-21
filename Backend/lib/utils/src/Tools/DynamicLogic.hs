module Tools.DynamicLogic where

import Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub)
import Kernel.Prelude
import Kernel.Randomizer
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Types.TimeBound
import Kernel.Utils.Common
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicElement as DALE
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout as DALR
import qualified Lib.Yudhishthira.Storage.CachedQueries.TimeBoundConfig as CTBC
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import Lib.Yudhishthira.Types
import Lib.Yudhishthira.Types.AppDynamicLogicRollout

-- import Data.Aeson as A

findOneConfig :: forall a m r. (FromJSON a, ToJSON a, BeamFlow m r) => Id MerchantOperatingCity -> LogicDomain -> Maybe [ConfigVersionMap] -> Maybe Value -> (m (Maybe a)) -> m (Maybe a)
findOneConfig merchantOpCityId cfgDomain mbConfigInExperimentVersions extraDimensions getConfigFromDBFunc = do
  currentTime <- getCurrentTime
  let extraDimensionsWithTime = fmap (\dims -> case dims of A.Object obj -> A.Object (KM.insert "currentTime" (toJSON currentTime) obj); _ -> A.Object (KM.fromList [("currentTime", toJSON currentTime)])) extraDimensions
  mbVersion <- getConfigVersion merchantOpCityId mbConfigInExperimentVersions cfgDomain
  cachedConfig :: Maybe a <- Hedis.safeGet (makeConfigCacheKey merchantOpCityId mbVersion cfgDomain)
  case cachedConfig of
    Just cfg -> return $ Just cfg
    Nothing -> fetchAndCacheConfig mbVersion extraDimensionsWithTime
  where
    fetchAndCacheConfig mbVersion extraDimensionsWithTime = do
      allLogics <- getConfigLogic merchantOpCityId mbVersion cfgDomain
      mbConfig <- getConfigFromDBFunc
      config <- maybe (return Nothing) (\cfg -> Just <$> processConfig allLogics mbVersion extraDimensionsWithTime cfg) mbConfig
      cacheConfig (makeConfigCacheKey merchantOpCityId mbVersion cfgDomain) config
      return config

findAllConfigs :: forall a m r. (FromJSON a, ToJSON a, BeamFlow m r) => Id MerchantOperatingCity -> LogicDomain -> Maybe [ConfigVersionMap] -> Maybe Value -> (m [a]) -> m [a]
findAllConfigs merchantOpCityId cfgDomain mbConfigInExperimentVersions extraDimensions getConfigFromDBFunc = do
  currentTime <- getCurrentTime
  let extraDimensionsWithTime = fmap (\dims -> case dims of A.Object obj -> A.Object (KM.insert "currentTime" (toJSON currentTime) obj); _ -> A.Object (KM.fromList [("currentTime", toJSON currentTime)])) extraDimensions
  mbVersion <- getConfigVersion merchantOpCityId mbConfigInExperimentVersions cfgDomain
  cachedConfig :: Maybe [a] <- Hedis.safeGet (makeConfigCacheKey merchantOpCityId mbVersion cfgDomain)
  case cachedConfig of
    Just cfgs -> return cfgs
    Nothing -> fetchAndCacheConfig mbVersion extraDimensionsWithTime
  where
    fetchAndCacheConfig mbVersion extraDimensionsWithTime = do
      allLogics <- getConfigLogic merchantOpCityId mbVersion cfgDomain
      listConfig <- getConfigFromDBFunc
      allConfigs <- mapM (processConfig allLogics mbVersion extraDimensionsWithTime) listConfig
      cacheConfig (makeConfigCacheKey merchantOpCityId mbVersion cfgDomain) allConfigs
      return allConfigs

findOneConfigWithCacheKey :: forall a m r. (FromJSON a, ToJSON a, BeamFlow m r) => Id MerchantOperatingCity -> LogicDomain -> Maybe [ConfigVersionMap] -> Maybe Value -> (m (Maybe a)) -> Text -> m (Maybe a)
findOneConfigWithCacheKey merchantOpCityId cfgDomain mbConfigInExperimentVersions extraDimensions getConfigFromDBFunc cacheKey = do
  currentTime <- getCurrentTime
  let extraDimensionsWithTime = fmap (\dims -> case dims of A.Object obj -> A.Object (KM.insert "currentTime" (toJSON currentTime) obj); _ -> A.Object (KM.fromList [("currentTime", toJSON currentTime)])) extraDimensions
  mbVersion <- getConfigVersion merchantOpCityId mbConfigInExperimentVersions cfgDomain
  cachedConfig <- Hedis.safeGet (makeConfigCacheKeyWithPrefix cacheKey mbVersion)
  case cachedConfig of
    Just cfg -> return $ Just cfg
    Nothing -> fetchAndCacheConfig mbVersion extraDimensionsWithTime
  where
    fetchAndCacheConfig mbVersion extraDimensionsWithTime = do
      allLogics <- getConfigLogic merchantOpCityId mbVersion cfgDomain
      mbConfig <- getConfigFromDBFunc
      config <- maybe (return Nothing) (\cfg -> Just <$> processConfig allLogics mbVersion extraDimensionsWithTime cfg) mbConfig
      cacheConfig (makeConfigCacheKeyWithPrefix cacheKey mbVersion) config
      return config

findAllConfigsWithCacheKey :: forall a m r. (FromJSON a, ToJSON a, BeamFlow m r) => Id MerchantOperatingCity -> LogicDomain -> Maybe [ConfigVersionMap] -> Maybe Value -> (m [a]) -> Text -> m [a]
findAllConfigsWithCacheKey merchantOpCityId cfgDomain mbConfigInExperimentVersions extraDimensions getConfigFromDBFunc cacheKey = do
  currentTime <- getCurrentTime
  let extraDimensionsWithTime = fmap (\dims -> case dims of A.Object obj -> A.Object (KM.insert "currentTime" (toJSON currentTime) obj); _ -> A.Object (KM.fromList [("currentTime", toJSON currentTime)])) extraDimensions
  mbVersion <- getConfigVersion merchantOpCityId mbConfigInExperimentVersions cfgDomain
  cachedConfig :: Maybe [a] <- Hedis.safeGet (makeConfigCacheKeyWithPrefix cacheKey mbVersion)
  case cachedConfig of
    Just cfgs -> return cfgs
    Nothing -> fetchAndCacheConfig mbVersion extraDimensionsWithTime
  where
    fetchAndCacheConfig mbVersion extraDimensionsWithTime = do
      allLogics <- getConfigLogic merchantOpCityId mbVersion cfgDomain
      listConfig <- getConfigFromDBFunc
      allConfigs <- mapM (processConfig allLogics mbVersion extraDimensionsWithTime) listConfig
      cacheConfig (makeConfigCacheKeyWithPrefix cacheKey mbVersion) allConfigs
      return allConfigs

processConfig :: forall a m r. (FromJSON a, ToJSON a, BeamFlow m r) => [A.Value] -> Maybe Int -> Maybe Value -> a -> m a
processConfig allLogics mbVersion extraDimensions cfg = do
  let configWrapper = Config cfg extraDimensions 0
  response <- try @_ @SomeException $ LYTU.runLogics allLogics configWrapper
  case response of
    Left e -> do
      logError $ "Error in running logics for rider config for version: " <> show mbVersion <> " and error: " <> show e
      return cfg
    Right resp -> do
      case (fromJSON (resp.result) :: Result (Config a)) of
        A.Success result -> do
          return result.config
        A.Error err -> do
          logError $ "Error in running logics for rider config: " <> show err
          return cfg

getConfigVersion ::
  BeamFlow m r =>
  Id MerchantOperatingCity ->
  Maybe [ConfigVersionMap] ->
  LogicDomain ->
  m (Maybe Int)
getConfigVersion merchantOpCityId mbConfigInExperimentVersions domain = do
  case mbConfigInExperimentVersions of
    Nothing -> selectVersionForUnboundedConfigs merchantOpCityId domain Nothing
    Just configInExperimentVersions -> do
      let configVersionMap = find (\a -> a.config == domain) configInExperimentVersions
      return $ configVersionMap <&> (.version)

getConfigLogic ::
  BeamFlow m r =>
  Id MerchantOperatingCity ->
  Maybe Int ->
  LogicDomain ->
  m [A.Value]
getConfigLogic merchantOpCityId mbVersion configDomain = do
  mbBaseRollout <- DALR.findBaseRolloutByMerchantOpCityAndDomain (cast merchantOpCityId) configDomain
  baseLogics <- maybe (return []) (\rollout -> DALE.findByDomainAndVersion configDomain rollout.version) mbBaseRollout
  when (null baseLogics) $ logError $ "Base logic not found for merchantOpCityId: " <> show merchantOpCityId <> " and configDomain: " <> show configDomain
  case (mbVersion, mbBaseRollout) of
    (Just version, Just rollout) | version == rollout.version -> return $ baseLogics <&> (.logic)
    (Just version, _) -> do
      experimentLogic <- DALE.findByDomainAndVersion configDomain version
      when (null experimentLogic) $ logError $ "Experiment logic not found for merchantOpCityId: " <> show merchantOpCityId <> " and configDomain: " <> show configDomain <> " and version: " <> show version
      let logics = baseLogics <> experimentLogic
      return $ logics <&> (.logic)
    (Nothing, _) -> return $ baseLogics <&> (.logic)

getAppDynamicLogic ::
  BeamFlow m r =>
  Id MerchantOperatingCity ->
  LogicDomain ->
  UTCTime ->
  Maybe Int ->
  Maybe Int ->
  m ([A.Value], Maybe Int)
getAppDynamicLogic merchantOpCityId domain localTime mbVersion mbToss = do
  mbFinalVersion <- pure mbVersion |<|>| selectAppDynamicLogicVersion merchantOpCityId domain localTime mbToss
  case mbFinalVersion of
    Just version -> do
      logics <- DALE.findByDomainAndVersion domain version
      when (null logics) $ logError $ "No dynamic logic found for merchantOpCityId: " <> show merchantOpCityId <> " and domain: " <> show domain <> " and version: " <> show version
      return (logics <&> (.logic), Just version)
    Nothing -> do
      logWarning $ "Missing Version, No dynamic logic found for merchantOpCityId: " <> show merchantOpCityId <> " and domain: " <> show domain
      return ([], Nothing)

selectVersionForUnboundedConfigs ::
  BeamFlow m r =>
  Id MerchantOperatingCity ->
  LogicDomain ->
  Maybe Int ->
  m (Maybe Int)
selectVersionForUnboundedConfigs merchantOpCityId domain mbToss = do
  mbConfigs <- DALR.findByMerchantOpCityAndDomain (cast merchantOpCityId) domain
  configs <- if null mbConfigs then DALR.findByMerchantOpCityAndDomain (Id "default") domain else return mbConfigs
  let applicapleConfigs = filter (\cfg -> cfg.timeBounds == "Unbounded") configs
  mbSelectedConfig <- chooseLogic applicapleConfigs mbToss
  return $ mbSelectedConfig <&> (.version)

isExperimentRunning :: BeamFlow m r => Id MerchantOperatingCity -> LogicDomain -> m Bool
isExperimentRunning merchantOpCityId domain = do
  mbConfigs <- DALR.findByMerchantOpCityAndDomain (cast merchantOpCityId) domain
  return $ any (\cfg -> cfg.percentageRollout /= 100 && cfg.percentageRollout /= 0) mbConfigs

selectAppDynamicLogicVersion ::
  BeamFlow m r =>
  Id MerchantOperatingCity ->
  LogicDomain ->
  UTCTime ->
  Maybe Int ->
  m (Maybe Int)
selectAppDynamicLogicVersion merchantOpCityId domain localTime mbToss = do
  mbConfigs <- DALR.findByMerchantOpCityAndDomain (cast merchantOpCityId) domain
  configs <- if null mbConfigs then DALR.findByMerchantOpCityAndDomain (Id "default") domain else return mbConfigs
  allTimeBoundConfigs <- CTBC.findByCityAndDomain (cast merchantOpCityId) domain
  let boundedTimeBoundConfigs = findBoundedDomain (filter (\cfg -> cfg.timeBounds /= Unbounded) allTimeBoundConfigs) localTime
  let applicapleConfigs =
        case boundedTimeBoundConfigs of -- if not bounded config found, return all configs with Unbounded timeBounds
          [] -> unboundedConfigs configs
          (x : _) -> do
            let boundedConfigs = filter (\cfg -> cfg.timeBounds == x.name) configs
            if null boundedConfigs
              then unboundedConfigs configs
              else boundedConfigs
  mbSelectedConfig <- chooseLogic applicapleConfigs mbToss
  return $ mbSelectedConfig <&> (.version)
  where
    unboundedConfigs = filter (\cfg -> cfg.timeBounds == "Unbounded")

cumulativeRollout :: [AppDynamicLogicRollout] -> [(AppDynamicLogicRollout, Int)]
cumulativeRollout logics = scanl1 addPercentages $ zip logics (map (.percentageRollout) logics)
  where
    addPercentages (_, p1) (logic2, p2) = (logic2, p1 + p2)

chooseLogic :: MonadFlow m => [AppDynamicLogicRollout] -> Maybe Int -> m (Maybe AppDynamicLogicRollout)
chooseLogic logics mbToss = do
  let cumulative = cumulativeRollout logics
  toss <- maybe (getRandomInRange (1, 100 :: Int)) pure mbToss
  return $ findLogic toss cumulative

-- Function to find the logic corresponding to the random number
findLogic :: Int -> [(AppDynamicLogicRollout, Int)] -> Maybe AppDynamicLogicRollout
findLogic _ [] = Nothing
findLogic randVal ((logic, cumPercent) : xs)
  | randVal <= cumPercent = Just logic
  | otherwise = findLogic randVal xs

getConfigVersionMapForStickiness ::
  BeamFlow m r =>
  Id MerchantOperatingCity ->
  m [ConfigVersionMap]
getConfigVersionMapForStickiness merchantOpCityId = do
  allConfigsRollouts <- DALR.fetchAllConfigsByMerchantOpCityId merchantOpCityId -- TODO: change to fetchAllBaseConfigsByMerchantOpCityId and cache it
  let configsInExperiment :: [LogicDomain] = nub $ map (.domain) $ filter (\rollout -> rollout.percentageRollout /= 100 && rollout.isBaseVersion == Just True) allConfigsRollouts
  mbConfigVersionMap <- mapM getVersion configsInExperiment
  let configVersionMap :: [ConfigVersionMap] = catMaybes mbConfigVersionMap
  return configVersionMap
  where
    getVersion domain = do
      mbVersion <- selectVersionForUnboundedConfigs merchantOpCityId domain Nothing
      case (mbVersion, domain) of
        (Just version, RIDER_CONFIG _) -> return $ Just $ ConfigVersionMap domain version
        (Just version, DRIVER_CONFIG _) -> return $ Just $ ConfigVersionMap domain version
        _ -> return Nothing

cacheConfig :: (ToJSON a, CacheFlow m r) => Text -> a -> m ()
cacheConfig configKey config = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp configKey config expTime

makeConfigCacheKey :: Id MerchantOperatingCity -> Maybe Int -> LogicDomain -> Text
makeConfigCacheKey cityId mbVersion configDomain = "CachedQueries: " <> show configDomain <> "-MerchantOperatingCityId:" <> cityId.getId <> "-V:" <> show mbVersion

makeConfigCacheKeyWithPrefix :: Text -> Maybe Int -> Text
makeConfigCacheKeyWithPrefix prefix mbVersion = prefix <> "-V:" <> show mbVersion

clearConfigCache :: BeamFlow m r => Id MerchantOperatingCity -> LogicDomain -> Maybe Int -> m ()
clearConfigCache merchanOperatingCityId configDomain mbVersion = do
  case mbVersion of
    Nothing -> do
      rollouts <- DALR.findByMerchantOpCityAndDomain merchanOperatingCityId configDomain
      forM_ rollouts $ \rollout -> do
        Hedis.del (makeConfigCacheKey merchanOperatingCityId (Just rollout.version) configDomain)
      Hedis.del (makeConfigCacheKey merchanOperatingCityId Nothing configDomain)
    Just version -> Hedis.del (makeConfigCacheKey merchanOperatingCityId (Just version) configDomain)

clearConfigCacheWithPrefix :: BeamFlow m r => Text -> Id MerchantOperatingCity -> LogicDomain -> Maybe Int -> m ()
clearConfigCacheWithPrefix prefix merchanOperatingCityId configDomain mbVersion = do
  case mbVersion of
    Nothing -> do
      rollouts <- DALR.findByMerchantOpCityAndDomain merchanOperatingCityId configDomain
      forM_ rollouts $ \rollout -> do
        Hedis.del (makeConfigCacheKeyWithPrefix prefix (Just rollout.version))
      Hedis.del (makeConfigCacheKeyWithPrefix prefix Nothing)
    Just version -> Hedis.del (makeConfigCacheKeyWithPrefix prefix (Just version))
