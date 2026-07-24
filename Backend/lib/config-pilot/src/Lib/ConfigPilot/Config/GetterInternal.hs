-- | Config getter: re-exports getConfig for external use.
module Lib.ConfigPilot.Config.GetterInternal
  ( getConfigImpl,
    selectActiveElementVersions,
    invalidateConfigInMem,
    configPilotInMemKey,
    PersonIdKey (..),
    TxnIdKey (..),
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import Data.Time (timeToTimeOfDay, utctDayTime)
import qualified EulerHS.Language as L
import EulerHS.Types (OptionEntity)
import Kernel.Prelude
import Kernel.Randomizer
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Tools.Metrics.CoreMetrics.Types (CoreMetrics)
import Kernel.Types.CacheFlow (HasInMemEnv)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, MonadFlow, getCurrentTime, logDebug, throwError)
import Lib.ConfigPilot.Interface.Types (ConfigDimensions (..))
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout as CADLR
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicElementExtra as CADLE
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType)

-- | Key for storing personId in EulerHS option local context.
data PersonIdKey = PersonIdKey
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity PersonIdKey Text

-- | Key for storing txnId in EulerHS option local context.
data TxnIdKey = TxnIdKey
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity TxnIdKey Text

-- | Per-request selection of the active dynamic-logic element versions for a
-- config domain.
selectActiveElementVersions ::
  forall m r.
  BeamFlow.BeamFlow m r =>
  LYT.LogicDomain ->
  Id LYT.MerchantOperatingCity ->
  m [Int]
selectActiveElementVersions logicDomain merchantOpCityId = do
  experimentRunning <- CADLR.isExperimentRunningCached merchantOpCityId logicDomain
  if not experimentRunning
    then do
      logDebug $ "CP Log: [SELECT_NO_EXPERIMENT] domain=" <> show logicDomain <> " mocId=" <> merchantOpCityId.getId <> " -> [] (base resolved in fill)"
      pure []
    else do
      mTxnId <- L.getOptionLocal TxnIdKey
      logDebug $ "CP Log: [SELECT_START] domain=" <> show logicDomain <> " mocId=" <> merchantOpCityId.getId <> " path=" <> maybe "fresh-toss" (const "txn-sticky") mTxnId <> " txnId=" <> show mTxnId
      case mTxnId of
        Just txnId -> getTxnIdStickyVersions txnId
        Nothing -> getActiveRolloutVersionsWithToss
  where
    mkTxnIdConfigStickyKey txnId = "sticky_config_versions:" <> txnId <> ":" <> show logicDomain

    getTxnIdStickyVersions txnId = do
      let stickyKey = mkTxnIdConfigStickyKey txnId
      mVersions <- Hedis.get stickyKey
      case mVersions of
        Just versions -> do
          logDebug $ "CP Log: [SELECT_TXN_HIT] domain=" <> show logicDomain <> " stickyKey=" <> stickyKey <> " versions=" <> show versions
          pure versions
        Nothing -> do
          versions <- getActiveRolloutVersionsWithToss
          Hedis.setExp stickyKey versions 7200
          logDebug $ "CP Log: [SELECT_TXN_NEW] domain=" <> show logicDomain <> " stickyKey=" <> stickyKey <> " frozeVersions=" <> show versions <> " ttlSec=7200"
          pure versions

    getActiveRolloutVersionsWithToss = do
      allActiveRollouts <- CADLR.findActiveByMerchantOpCityAndDomain merchantOpCityId logicDomain
      let nonBaseRollouts = filter (\r -> r.isBaseVersion /= Just True) allActiveRollouts
      let cumulativeRollouts = buildCumulativeRollouts nonBaseRollouts
      toss <- getRandomInRange (1, 100 :: Int)
      let selectedRollout = find (\(_, cumulativePerc) -> toss <= cumulativePerc) cumulativeRollouts
      let baseRollout = find (\rollout -> rollout.isBaseVersion == Just True) allActiveRollouts
      let selectedRollouts = maybeToList (fst <$> selectedRollout) <> maybeToList baseRollout
      logDebug $
        "CP Log: [SELECT_TOSS] domain=" <> show logicDomain
          <> " toss="
          <> show toss
          <> " cumulativeBuckets="
          <> show (map (\(r, cum) -> (r.version, r.percentageRollout, cum)) cumulativeRollouts)
          <> " base="
          <> show ((.version) <$> baseRollout)
          <> " expVersionPicked="
          <> show ((.version) . fst <$> selectedRollout)
          <> " finalVersions="
          <> show ((.version) <$> selectedRollouts)
      when (null allActiveRollouts) $ logDebug $ "CP Log: [SELECT_NO_ROLLOUTS] domain=" <> show logicDomain <> " mocId=" <> merchantOpCityId.getId <> " -> serving raw default (no logic applied)"
      when (isNothing baseRollout) $ logDebug $ "CP Log: [SELECT_NO_BASE] domain=" <> show logicDomain <> " mocId=" <> merchantOpCityId.getId <> " -> no base version; only experiment (if any) applied"
      logDebug $ "[GETCONFIG_SELECT] domain=" <> show logicDomain <> " active=" <> show (map (\r -> (r.version, r.isBaseVersion)) allActiveRollouts) <> " toss=" <> show toss <> " base=" <> show ((.version) <$> baseRollout) <> " selected=" <> show ((.version) <$> selectedRollouts)
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

-- | Core config-fetching logic: given the already-selected element versions,
-- fetch those elements and apply their JSON-patch logics to the wrapped config.
getConfigImpl ::
  forall b m r.
  (FromJSON b, ToJSON b, BeamFlow.BeamFlow m r) =>
  [Int] ->
  LYT.Config b ->
  LYT.LogicDomain ->
  Id LYT.MerchantOperatingCity ->
  m b
getConfigImpl activeElementVersions wrappedConfig logicDomain merchantOpCityId = do
  effectiveVersions <- case activeElementVersions of
    [] -> do
      mbBase <- CADLR.findBaseRolloutByMerchantOpCityAndDomain merchantOpCityId logicDomain
      case mbBase of
        Just base -> do
          logDebug $ "CP Log: [APPLY_BASE] domain=" <> show logicDomain <> " baseVersion=" <> show base.version
          pure [base.version]
        Nothing -> do
          logDebug $ "CP Log: [APPLY_NO_BASE] domain=" <> show logicDomain <> " -> no base rollout; config returned unchanged"
          pure []
    versions -> pure versions
  allActiveElements <- CADLE.findByDomainAndVersions Nothing Nothing logicDomain effectiveVersions
  let baseLogics = map (.logic) allActiveElements
  logDebug $ "CP Log: [APPLY] domain=" <> show logicDomain <> " mocId=" <> merchantOpCityId.getId <> " versions=" <> show effectiveVersions <> " elementsFound=" <> show (length allActiveElements)
  resp <- LYTU.runLogics baseLogics wrappedConfig
  case A.fromJSON resp.result of
    A.Success (cfg :: LYT.Config b) -> do
      logDebug $ "ConfigPilot read path: successfully applied JSON patch to config for logicDomain: " <> show logicDomain <> ", merchantOpCityId: " <> merchantOpCityId.getId <> ". Output: " <> decodeUtf8 (A.encode cfg.config)
      pure cfg.config
    A.Error e -> do
      logDebug $ "ConfigPilot read path: error applying JSON patch to config for logicDomain: " <> show logicDomain <> ", merchantOpCityId: " <> merchantOpCityId.getId <> ". Error: " <> show e
      throwError $ InvalidRequest $ "Error occurred while applying JSON patch to the config. " <> show e

-- | Build the in-mem cache key from dimensions. Uses 'dimensionsCacheKey' so
-- that every field in the dimensions record is automatically included.
configPilotInMemKey :: ConfigDimensions a => a -> [Text]
configPilotInMemKey dims = ["ConfigPilot", show (getConfigType dims), dimensionsCacheKey dims]

-- | Invalidate both cache layers for a config type across all pods: the in-mem
-- L1 (shudhi sidecar + Redis cleanup flag) and the Redis L2 hash bucket.
invalidateConfigInMem :: (MonadFlow m, CacheFlow m r, CoreMetrics m, HasInMemEnv r) => ConfigType -> m ()
invalidateConfigInMem cfgType = do
  let keyPrefix :: Text = "ConfigPilot:" <> show cfgType
  now <- getCurrentTime
  let val = A.object ["forceCleanupTimestamp" .= timeOfDayFromUTCTime now, "forceCleanupKeyPrefix" .= keyPrefix]
  Hedis.setExp "inmem:force:cleanup:timeofday" val 600
  Hedis.delRedisCacheBucket keyPrefix -- L2: drop the cross-pod Redis hash bucket for this config type in one DEL
  logDebug $ "Redis Key Deleted : " <> keyPrefix
  IM.refreshInMem keyPrefix -- L1: clear in-mem across pods via the shudhi sidecar (the Redis flag above is the TTL-based fallback)
  logDebug $ "InMemKey Key Deleted : " <> keyPrefix
  where
    timeOfDayFromUTCTime t =
      let dayTime = utctDayTime t
       in timeToTimeOfDay dayTime
