-- | Config getter: re-exports getConfig for external use.
module Lib.ConfigPilot.Config.GetterInternal
  ( getConfigImpl,
    selectActiveElementVersions,
    invalidateConfigInMem,
    configPilotInMemKey,
    getCodeVersion,
    PersonIdKey (..),
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Time (timeToTimeOfDay, utctDayTime)
import qualified EulerHS.Language as L
import EulerHS.Types (OptionEntity)
import Kernel.Beam.Types (TxnIdKey (..))
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
import qualified Lib.Yudhishthira.Tools.TimeBoundRollout as LYTB
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType)
import System.Environment (lookupEnv)

-- | Key for storing personId in EulerHS option local context.
data PersonIdKey = PersonIdKey
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity PersonIdKey Text

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
    mkTxnIdConfigStickyKey txnId = "sticky_config_versions:" <> txnId <> ":" <> show logicDomain <> ":" <> merchantOpCityId.getId

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
      -- Narrow the experiment rollouts to the current time window.
      nonBaseRollouts <-
        LYTB.filterByActiveTimeBound merchantOpCityId logicDomain $
          filter (\r -> r.isBaseVersion /= Just True) allActiveRollouts
      -- Independent per-experiment selection: every live experiment (percentageRollout > 0) gets
      -- its OWN toss in 1..100 and wins iff its toss <= its percentageRollout. All winners are
      -- applied, base-first then by ascending version, so the newest version overrides on any
      -- field written by more than one winner (see getConfigImpl). Independent draws keep the
      -- experiments statistically independent -- unlike a single shared toss, whose winner
      -- populations would be nested (a lower-% experiment a strict subset of a higher-% one).
      wonRollouts <-
        filterM
          ( \r ->
              if r.percentageRollout <= 0
                then pure False
                else (\t -> t <= r.percentageRollout) <$> getRandomInRange (1, 100 :: Int)
          )
          nonBaseRollouts
      let winners = sortOn (.version) wonRollouts
      let baseRollout = find (\rollout -> rollout.isBaseVersion == Just True) allActiveRollouts
      let selectedRollouts = maybeToList baseRollout <> winners
      when (null allActiveRollouts) $ logDebug $ "CP Log: [SELECT_NO_ROLLOUTS] domain=" <> show logicDomain <> " mocId=" <> merchantOpCityId.getId <> " -> serving raw default (no logic applied)"
      when (isNothing baseRollout) $ logDebug $ "CP Log: [SELECT_NO_BASE] domain=" <> show logicDomain <> " mocId=" <> merchantOpCityId.getId <> " -> no base version; only experiment (if any) applied"
      logDebug $ "[GETCONFIG_SELECT] domain=" <> show logicDomain <> " candidates=" <> show (map (\r -> (r.version, r.percentageRollout)) nonBaseRollouts) <> " base=" <> show ((.version) <$> baseRollout) <> " winners=" <> show (map (.version) winners) <> " selected=" <> show (map (.version) selectedRollouts)
      pure $ (.version) <$> selectedRollouts

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
  let orderedElements = concatMap (\v -> sortOn (.order) (filter (\e -> e.version == v) allActiveElements)) effectiveVersions
      baseLogics = map (.logic) orderedElements
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
-- that every field in the dimensions record automatically included.
configPilotInMemKey :: ConfigDimensions a => a -> [Text]
configPilotInMemKey dims = ["ConfigPilot", show (getConfigType dims), dimensionsCacheKey dims]

getCodeVersion :: MonadIO m => m Text
getCodeVersion = do
  mbVersion <- liftIO $ lookupEnv "DEPLOYMENT_VERSION"
  pure $ case mbVersion of
    Just v | not (null v) -> T.pack v
    _ -> "DEV"

-- | Invalidate both cache layers for a config type across all pods: the in-mem
-- L1 (shudhi sidecar + Redis cleanup flag) and the Redis L2 hash bucket.
invalidateConfigInMem :: (MonadFlow m, CacheFlow m r, CoreMetrics m, HasInMemEnv r) => ConfigType -> m ()
invalidateConfigInMem cfgType = do
  let keyPrefix :: Text = "ConfigPilot:" <> show cfgType
  now <- getCurrentTime
  let val = A.object ["forceCleanupTimestamp" .= timeOfDayFromUTCTime now, "forceCleanupKeyPrefix" .= keyPrefix]
  Hedis.setExp "inmem:force:cleanup:timeofday" val 600
  Hedis.delRedisCacheBucket keyPrefix -- L2 under caller's keyModifier
  Hedis.withCrossAppRedis $ do
    Hedis.setExp "inmem:force:cleanup:timeofday" val 600
    Hedis.delRedisCacheBucket keyPrefix -- L2 unprefixed (kafka-consumers / cross-app readers)
  logDebug $ "Redis Key Deleted (prefixed + unprefixed): " <> keyPrefix
  IM.refreshInMem keyPrefix -- L1: clear in-mem across pods via the shudhi sidecar (the Redis flag above is the TTL-based fallback)
  logDebug $ "InMemKey Key Deleted : " <> keyPrefix
  where
    timeOfDayFromUTCTime t =
      let dayTime = utctDayTime t
       in timeToTimeOfDay dayTime
