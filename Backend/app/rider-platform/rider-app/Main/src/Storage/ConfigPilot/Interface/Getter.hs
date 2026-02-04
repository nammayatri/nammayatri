-- | Config getter: re-exports getConfig for external use.
module Storage.ConfigPilot.Interface.Getter
  ( getConfig,
    getConfigImpl,
  )
where

import qualified Data.Aeson as A
import Kernel.Prelude
import Kernel.Randomizer
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, throwError)
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout as CADLR
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicElementExtra as CADLE
import Storage.Beam.Yudhishthira ()
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import Storage.ConfigPilot.Interface.Types (ConfigDimensions (..))
import Tools.Error

getConfigImpl ::
  forall configTypeDimensions b m r.
  (ConfigDimensions configTypeDimensions, FromJSON b, ToJSON b, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  configTypeDimensions ->
  LYT.Config b ->
  Id LYT.MerchantOperatingCity ->
  Maybe Text ->
  m b
getConfigImpl domensions wrappedConfig merchantOpCityId mTxnId = do
  let configType = getConfigType domensions
  activeElementVersions <-
    case mTxnId of
      Just txnId -> getTxnIdStickyVersions txnId configType
      Nothing -> getActiveRolloutVersionsWithToss configType
  allActiveElements <- CADLE.findByDomainAndVersions Nothing Nothing (LYT.RIDER_CONFIG configType) activeElementVersions
  let baseLogics = map (.logic) allActiveElements
  resp <- LYTU.runLogics baseLogics wrappedConfig
  case A.fromJSON resp.result of
    A.Success (cfg :: LYT.Config b) -> pure cfg.config
    A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to the config. " <> show e
  where
    mkTxnIdConfigStickyKey txnId configType = "sticky_config_versions:" <> txnId <> ":" <> show configType

    getTxnIdStickyVersions txnId configType = do
      mVersions <- Hedis.get (mkTxnIdConfigStickyKey txnId configType)
      case mVersions of
        Just versions -> pure versions
        Nothing -> do
          versions <- getActiveRolloutVersionsWithToss configType
          Hedis.setExp (mkTxnIdConfigStickyKey txnId configType) versions 7200
          pure versions

    getActiveRolloutVersionsWithToss configType = do
      allActiveRollouts <- CADLR.findByMerchantOpCityAndDomain merchantOpCityId (LYT.RIDER_CONFIG configType) -- includes base rollout as well
      let cumulativeRollouts = buildCumulativeRollouts allActiveRollouts
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