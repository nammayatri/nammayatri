-- | Config getter: re-exports getConfig for external use.
module Storage.ConfigPilot.Interface.Getter
  ( getConfig,
    getConfigImpl,
    invalidateConfigInMem,
    configPilotInMemKey,
  )
where

import qualified Data.Aeson as A
import Data.Time (timeToTimeOfDay, utctDayTime)
import Kernel.Prelude
import Kernel.Randomizer
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics.Types (CoreMetrics)
import Kernel.Types.CacheFlow (HasInMemEnv)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime, throwError)
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout as CADLR
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicElementExtra as CADLE
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType)
import Storage.Beam.Yudhishthira ()
import Storage.ConfigPilot.Interface.Types (ConfigDimensions (..))
import Tools.Error

getConfigImpl ::
  forall configTypeDimensions b m r.
  (ConfigDimensions configTypeDimensions, FromJSON b, ToJSON b, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  configTypeDimensions ->
  LYT.Config b ->
  LYT.LogicDomain ->
  Id LYT.MerchantOperatingCity ->
  m b
getConfigImpl _dimensions wrappedConfig logicDomain merchantOpCityId = do
  activeElementVersions <- getActiveRolloutVersionsWithToss
  allActiveElements <- CADLE.findByDomainAndVersions Nothing Nothing logicDomain activeElementVersions
  let baseLogics = map (.logic) allActiveElements
  resp <- LYTU.runLogics baseLogics wrappedConfig
  case A.fromJSON resp.result of
    A.Success (cfg :: LYT.Config b) -> pure cfg.config
    A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to the config. " <> show e
  where
    getActiveRolloutVersionsWithToss = do
      allActiveRollouts <- CADLR.findActiveByMerchantOpCityAndDomain merchantOpCityId logicDomain
      let nonBaseRollouts = filter (\r -> r.isBaseVersion /= Just True) allActiveRollouts
      let cumulativeRollouts = buildCumulativeRollouts nonBaseRollouts
      toss <- getRandomInRange (1, 100 :: Int)
      let selectedRollout = find (\(_, cumulativePerc) -> toss <= cumulativePerc) cumulativeRollouts
      let baseRollout = find (\rollout -> rollout.isBaseVersion == Just True) allActiveRollouts
      let selectedRollouts = maybeToList (fst <$> selectedRollout) <> maybeToList baseRollout
      pure $ (.version) <$> selectedRollouts
    buildCumulativeRollouts rollouts =
      snd $
        foldl'
          ( \(prevPerc, acc) rollout ->
              let newPerc = prevPerc + rollout.percentageRollout
               in (newPerc, acc <> [(rollout, newPerc)])
          )
          (0, [])
          rollouts

-- | Build the in-mem cache key from dimensions. Uses 'dimensionsCacheKey' so
-- that every field in the dimensions record is automatically included.
configPilotInMemKey :: ConfigDimensions a => a -> [Text]
configPilotInMemKey dims = ["ConfigPilot", show (getConfigType dims), dimensionsCacheKey dims]

-- | Invalidate the in-mem cache for a config type across all pods.
-- Sets a Redis key that the background cleanup thread reads to clear matching in-mem entries.
invalidateConfigInMem :: (MonadFlow m, CacheFlow m r, CoreMetrics m, HasInMemEnv r) => ConfigType -> m ()
invalidateConfigInMem cfgType = do
  now <- getCurrentTime
  let val = A.object ["forceCleanupTimestamp" A..= timeOfDayFromUTCTime now, "forceCleanupKeyPrefix" A..= ("ConfigPilot:" <> show cfgType :: Text)]
  Hedis.setExp "inmem:force:cleanup:timeofday" val 600
  where
    timeOfDayFromUTCTime t =
      let dayTime = utctDayTime t
       in timeToTimeOfDay dayTime
