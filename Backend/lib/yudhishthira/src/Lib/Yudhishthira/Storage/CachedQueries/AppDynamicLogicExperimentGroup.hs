module Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicExperimentGroup where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import qualified Kernel.Types.Id
import Kernel.Utils.Logging (logDebug)
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicExperimentGroup as Queries
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicExperimentGroup as DT

-- | v1 scope: only always-on ("Unbounded") groups are selectable (window
-- schedules are keyed per domain, whereas a group spans domains).
unboundedTimeBound :: Text
unboundedTimeBound = "Unbounded"

-- | Active, selectable experiment groups for a city: not discarded/reverted,
-- always-on, and with a positive rollout percentage. Two-tier cached (in-mem +
-- Redis) because it is read on every config resolution (the group-centric gate
-- and the group-toss input). Depends only on the registry, so it is invalidated
-- only on a group registry change ('clearActiveGroupsCache').
getActiveGroupsCached ::
  BeamFlow.BeamFlow m r =>
  Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
  m [DT.AppDynamicLogicExperimentGroup]
getActiveGroupsCached cityId =
  IM.withInMemCache (activeGroupsInMemKey cityId) 10800 $ do
    mbCached <- Hedis.safeGet (activeGroupsRedisKey cityId)
    case mbCached of
      Just groups -> pure groups
      Nothing -> do
        allGroups <- Queries.findAllByMerchantOpCityId cityId
        let activeGroups = filter isActive allGroups
        expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
        Hedis.setExp (activeGroupsRedisKey cityId) activeGroups expTime
        logDebug $ "CP Log: [ACTIVE_GROUPS_REFILL] city=" <> Kernel.Types.Id.getId cityId <> " groups=" <> show (map (\g -> (g.groupName, g.rolloutPercentage)) activeGroups)
        pure activeGroups
  where
    isActive g =
      g.experimentStatus /= Just Lib.Yudhishthira.Types.DISCARDED
        && g.experimentStatus /= Just Lib.Yudhishthira.Types.REVERTED
        && g.timeBounds == unboundedTimeBound
        && g.rolloutPercentage > 0

clearActiveGroupsCache :: BeamFlow.BeamFlow m r => Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> m ()
clearActiveGroupsCache cityId = do
  logDebug $ "CP Log: [ACTIVE_GROUPS_DELETE] city=" <> Kernel.Types.Id.getId cityId
  Hedis.runInMultiCloudRedisWrite $ Hedis.del $ activeGroupsRedisKey cityId
  IM.refreshInMem $ activeGroupsInMemInfix cityId

activeGroupsRedisKey :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Text
activeGroupsRedisKey cityId = "yudhishthira-CachedQueries:AppDynamicLogicExperimentGroup:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId cityId <> ":ActiveGroups"

activeGroupsInMemKey :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> [Text]
activeGroupsInMemKey cityId = ["AppDynamicLogicExperimentGroup:ActiveGroups", Kernel.Types.Id.getId cityId]

activeGroupsInMemInfix :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Text
activeGroupsInMemInfix cityId = "AppDynamicLogicExperimentGroup:ActiveGroups:" <> Kernel.Types.Id.getId cityId
