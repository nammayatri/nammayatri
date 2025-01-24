module Tools.DynamicLogic where

import Data.Aeson as A
import Data.List (nub)
import Kernel.Prelude
import Kernel.Randomizer
import Kernel.Types.Id
import Kernel.Types.TimeBound
import Kernel.Utils.Common
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicElement as DALE
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout as DALR
import qualified Lib.Yudhishthira.Storage.CachedQueries.TimeBoundConfig as CTBC
import Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.AppDynamicLogicRollout

getConfigVersion ::
  BeamFlow m r =>
  Id MerchantOperatingCity ->
  Maybe [LYT.ConfigVersionMap] ->
  LYT.ConfigType ->
  m Int
getConfigVersion merchantOpCityId mbConfigInExperimentVersions configType = do
  case mbConfigInExperimentVersions of
    Nothing -> do
      mbVersion <- selectVersionForUnboundedConfigs merchantOpCityId (LYT.CONFIG configType)
      return $ fromMaybe 1 mbVersion
    Just configInExperimentVersions -> do
      let configVersionMap = find (\a -> a.config == configType) configInExperimentVersions
      case configVersionMap of
        Nothing -> return 1
        Just versionMap -> return versionMap.version

getConfigLogic ::
  BeamFlow m r =>
  Id MerchantOperatingCity ->
  Int ->
  LYT.ConfigType ->
  m [A.Value]
getConfigLogic merchantOpCityId version configType = do
  baseLogics <- DALE.findByDomainAndVersion (LYT.CONFIG configType) 1
  when (null baseLogics) $ logError $ "Base logic not found for merchantOpCityId: " <> show merchantOpCityId <> " and configType: " <> show configType <> " and version: 1"
  case version of
    1 -> return $ baseLogics <&> (.logic)
    _ -> do
      experimentLogic <- DALE.findByDomainAndVersion (LYT.CONFIG configType) version
      when (null experimentLogic) $ logError $ "Experiment logic not found for merchantOpCityId: " <> show merchantOpCityId <> " and configType: " <> show configType <> " and version: " <> show version
      let logics = baseLogics <> experimentLogic
      return $ logics <&> (.logic)

getAppDynamicLogic ::
  BeamFlow m r =>
  Id MerchantOperatingCity ->
  LYT.LogicDomain ->
  UTCTime ->
  Maybe Int ->
  m ([A.Value], Maybe Int)
getAppDynamicLogic merchantOpCityId domain localTime mbVersion = do
  mbFinalVersion <- pure mbVersion |<|>| selectAppDynamicLogicVersion merchantOpCityId domain localTime
  case mbFinalVersion of
    Just version -> do
      logics <- DALE.findByDomainAndVersion domain version
      when (null logics) $ logError $ "No dynamic logic found for merchantOpCityId: " <> show merchantOpCityId <> " and domain: " <> show domain <> " and version: " <> show version
      return (logics <&> (.logic), Just version)
    Nothing -> do
      logWarning $ "No dynamic logic found for merchantOpCityId: " <> show merchantOpCityId <> " and domain: " <> show domain
      return ([], Nothing)

selectVersionForUnboundedConfigs ::
  BeamFlow m r =>
  Id MerchantOperatingCity ->
  LYT.LogicDomain ->
  m (Maybe Int)
selectVersionForUnboundedConfigs merchantOpCityId domain = do
  mbConfigs <- DALR.findByMerchantOpCityAndDomain (cast merchantOpCityId) domain
  configs <- if null mbConfigs then DALR.findByMerchantOpCityAndDomain (Id "default") domain else return mbConfigs
  let applicapleConfigs = filter (\cfg -> cfg.timeBounds == "Unbounded") configs
  mbSelectedConfig <- chooseLogic applicapleConfigs
  return $ mbSelectedConfig <&> (.version)

selectAppDynamicLogicVersion ::
  BeamFlow m r =>
  Id MerchantOperatingCity ->
  LYT.LogicDomain ->
  UTCTime ->
  m (Maybe Int)
selectAppDynamicLogicVersion merchantOpCityId domain localTime = do
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
  mbSelectedConfig <- chooseLogic applicapleConfigs
  return $ mbSelectedConfig <&> (.version)
  where
    unboundedConfigs = filter (\cfg -> cfg.timeBounds == "Unbounded")

cumulativeRollout :: [AppDynamicLogicRollout] -> [(AppDynamicLogicRollout, Int)]
cumulativeRollout logics = scanl1 addPercentages $ zip logics (map (.percentageRollout) logics)
  where
    addPercentages (_, p1) (logic2, p2) = (logic2, p1 + p2)

chooseLogic :: MonadFlow m => [AppDynamicLogicRollout] -> m (Maybe AppDynamicLogicRollout)
chooseLogic logics = do
  let cumulative = cumulativeRollout logics
  toss <- getRandomInRange (1, 100 :: Int)
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
  allConfigsRollouts <- DALR.fetchAllConfigsByMerchantOpCityId merchantOpCityId
  let configsInExperiment :: [LogicDomain] = nub $ map (.domain) $ filter (\rollout -> rollout.percentageRollout /= 100 && rollout.percentageRollout /= 0 && rollout.version == 1) allConfigsRollouts
  now <- getCurrentTime
  mbConfigVersionMap <- mapM (getVersion now) configsInExperiment
  let configVersionMap :: [ConfigVersionMap] = catMaybes mbConfigVersionMap
  return configVersionMap
  where
    getVersion now domain = do
      mbVersion <- selectAppDynamicLogicVersion merchantOpCityId domain now
      case (mbVersion, domain) of
        (Just version, CONFIG cfgType) -> return $ Just $ ConfigVersionMap cfgType version
        _ -> return Nothing
