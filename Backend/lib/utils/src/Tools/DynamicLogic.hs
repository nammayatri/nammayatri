module Tools.DynamicLogic where

import Data.Aeson as A
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
