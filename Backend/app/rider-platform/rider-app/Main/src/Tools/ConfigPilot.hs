{-# OPTIONS_GHC -Wno-deprecations -Wno-orphans #-}

module Tools.ConfigPilot where

import qualified ConfigPilotFrontend.Types as CPT
import qualified Data.Aeson as A
import Data.List (sortOn)
import Data.String.Conversions (cs)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.UiRiderConfig as DTU
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types as LYTU
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.Exophone as CQExo
import qualified Storage.CachedQueries.FRFSConfig as CQFRFS
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CQMPN
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CQPC
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQR
import qualified Storage.CachedQueries.MerchantConfig as CQMC
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as CQRRN
import qualified Storage.CachedQueries.UiRiderConfig as SCU
import Storage.ConfigPilot.Config.BecknConfig (BecknConfigDimensions (..))
import Storage.ConfigPilot.Config.Exophone (ExophoneDimensions (..))
import Storage.ConfigPilot.Config.FRFSConfig (FRFSConfigDimensions (..))
import Storage.ConfigPilot.Config.MerchantConfig (MerchantConfigDimensions (..))
import Storage.ConfigPilot.Config.MerchantPushNotification (MerchantPushNotificationDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))
import Storage.ConfigPilot.Config.PayoutConfig (PayoutDimensions (..))
import Storage.ConfigPilot.Config.RideRelatedNotificationConfig (RideRelatedNotificationConfigDimensions (..))
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Getter (invalidateConfigInMem)
import Storage.ConfigPilot.Interface.Types (getConfig, getConfigList)
import qualified Storage.Queries.BecknConfig as SQBC
import qualified Storage.Queries.MerchantServiceConfig as SQMSC
import qualified Storage.Queries.UiRiderConfig as SQU
import Storage.Queries.UiRiderConfigExtra ()
import qualified Tools.DynamicLogic as DynamicLogic

returnConfigs :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => LYTU.LogicDomain -> Id LYTU.MerchantOperatingCity -> Id LYTU.Merchant -> Kernel.Types.Beckn.Context.City -> m LYTU.TableDataResp
returnConfigs cfgType merchantOpCityId merchantId opCity = do
  case cfgType of
    LYTU.RIDER_CONFIG LYTU.RiderConfig -> do
      riderCfg <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOpCityId.getId})
      return LYTU.TableDataResp {configs = map A.toJSON (maybeToList riderCfg)}
    LYTU.RIDER_CONFIG LYTU.PayoutConfig -> do
      payoutCfg <- getConfigList (PayoutDimensions {merchantOperatingCityId = merchantOpCityId.getId, vehicleCategory = Nothing, isPayoutEnabled = Nothing, payoutEntity = Nothing})
      return LYTU.TableDataResp {configs = map A.toJSON payoutCfg}
    LYTU.RIDER_CONFIG LYTU.RideRelatedNotificationConfig -> do
      rideRelatedNotificationCfg <- getConfigList (RideRelatedNotificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, timeDiffEvent = Nothing})
      return LYTU.TableDataResp {configs = map A.toJSON rideRelatedNotificationCfg}
    LYTU.RIDER_CONFIG LYTU.MerchantConfig -> do
      merchantCfg <- getConfigList (MerchantConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId})
      return LYTU.TableDataResp {configs = map A.toJSON merchantCfg}
    LYTU.RIDER_CONFIG LYTU.MerchantServiceUsageConfig -> do
      msucCfg <- getConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId})
      return LYTU.TableDataResp {configs = map A.toJSON (maybeToList msucCfg)}
    LYTU.RIDER_CONFIG LYTU.MerchantServiceConfig -> do
      mscCfgs <- getConfigList (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, merchantId = merchantId.getId, serviceName = Nothing})
      return LYTU.TableDataResp {configs = map A.toJSON mscCfgs}
    LYTU.RIDER_CONFIG LYTU.BecknConfig -> do
      bcCfgs <- getConfigList (BecknConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, merchantId = merchantId.getId, domain = Nothing, vehicleCategory = Nothing})
      return LYTU.TableDataResp {configs = map A.toJSON bcCfgs}
    LYTU.RIDER_CONFIG LYTU.MerchantPushNotification -> do
      mpnCfgs <- getConfig (MerchantPushNotificationDimensions {merchantOperatingCityId = merchantOpCityId.getId})
      return LYTU.TableDataResp {configs = map A.toJSON mpnCfgs}
    LYTU.RIDER_CONFIG LYTU.Exophone -> do
      exoCfgs <- getConfigList (ExophoneDimensions {merchantOperatingCityId = merchantOpCityId.getId, callService = Nothing})
      return LYTU.TableDataResp {configs = map A.toJSON exoCfgs}
    LYTU.RIDER_CONFIG LYTU.FRFSConfig -> do
      frfsConfig <- getConfig (FRFSConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId})
      return LYTU.TableDataResp {configs = map A.toJSON (maybeToList frfsConfig)}
    LYTU.UI_RIDER dt pt -> do
      let uiConfigReq = LYTU.UiConfigRequest {os = dt, platform = pt, merchantId = getId merchantId, city = opCity, language = Nothing, bundle = Nothing, toss = Nothing}
      mbUiConfigInfo <- SCU.findUiConfig uiConfigReq (cast merchantOpCityId) True
      return LYTU.TableDataResp {configs = map A.toJSON (maybeToList (fst <$> mbUiConfigInfo))}
    _ -> throwError $ InvalidRequest "Unsupported config type."

handleConfigDBUpdate :: (BeamFlow m r, EsqDBFlow m r, CacheFlow m r) => Id LYTU.MerchantOperatingCity -> LYTU.ConcludeReq -> [A.Value] -> Maybe (Id LYTU.Merchant) -> Kernel.Types.Beckn.Context.City -> m ()
handleConfigDBUpdate merchantOpCityId concludeReq baseLogics mbMerchantId opCity = do
  case concludeReq.domain of
    LYTU.RIDER_CONFIG LYTU.RiderConfig -> do
      handleConfigUpdate (normalizeMaybeFetch CQR.findByMerchantOperatingCityId) (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.RiderConfig) >> invalidateConfigInMem LYTU.RiderConfig) CQR.updateByPrimaryKey (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.PayoutConfig -> do
      handleConfigUpdate (\mocId' -> CQPC.findAllByMerchantOpCityId mocId' (Just [])) (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.PayoutConfig) >> invalidateConfigInMem LYTU.PayoutConfig) CQPC.updateByPrimaryKey (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.RideRelatedNotificationConfig -> do
      handleConfigUpdate (\mocId' -> CQRRN.findAllByMerchantOperatingCityId mocId' (Just [])) (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.RideRelatedNotificationConfig) >> invalidateConfigInMem LYTU.RideRelatedNotificationConfig) CQRRN.updateByPrimaryKey (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.MerchantConfig -> do
      handleConfigUpdateWithExtraDimensions (\mocId' _ -> CQMC.findAllByMerchantOperatingCityId mocId' (Just [])) (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.MerchantConfig) >> invalidateConfigInMem LYTU.MerchantConfig) CQMC.updateByPrimaryKey (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.MerchantPushNotification -> do
      handleConfigUpdate (\mocId' -> CQMPN.findAllByMerchantOpCityId mocId' (Just [])) (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.MerchantPushNotification) >> invalidateConfigInMem LYTU.MerchantPushNotification) CQMPN.updateByPrimaryKey (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.FRFSConfig -> do
      handleConfigUpdate (\mocId' -> normalizeMaybeFetch (\m -> CQFRFS.findByMerchantOperatingCityId m (Just [])) mocId') (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.FRFSConfig) >> invalidateConfigInMem LYTU.FRFSConfig) CQFRFS.updateByPrimaryKey (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.MerchantServiceUsageConfig -> do
      handleConfigUpdateViaJson (normalizeMaybeFetchJson CQMSUC.findByMerchantOperatingCityId) (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.MerchantServiceUsageConfig) >> invalidateConfigInMem LYTU.MerchantServiceUsageConfig) CQMSUC.updateMerchantServiceUsageConfig (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.MerchantServiceConfig -> do
      handleConfigUpdateViaJson SQMSC.findAllByMerchantOperatingCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.MerchantServiceConfig) >> invalidateConfigInMem LYTU.MerchantServiceConfig) CQMSC.upsertMerchantServiceConfig (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.BecknConfig -> do
      handleConfigUpdateViaJson (SQBC.findAllByMerchantOperatingCityId . Just) (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.BecknConfig) >> invalidateConfigInMem LYTU.BecknConfig) CQBC.updateByPrimaryKey (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.Exophone -> do
      handleConfigUpdateViaJson CQExo.findAllByMerchantOperatingCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.Exophone) >> invalidateConfigInMem LYTU.Exophone) CQExo.updateByPrimaryKey (cast merchantOpCityId)
    LYTU.UI_RIDER dt pt -> do
      let uiConfigReq = LYTU.UiConfigRequest {os = dt, platform = pt, merchantId = maybe "" getId mbMerchantId, city = opCity, language = Nothing, bundle = Nothing, toss = Nothing}
      handleConfigUpdateWithExtraDimensionsUi SQU.getUiConfig (SCU.clearCache (cast merchantOpCityId) dt pt) SCU.updateByPrimaryKey (cast merchantOpCityId) uiConfigReq
    _ -> throwError $ InvalidRequest $ "Logic Domain not supported" <> show concludeReq.domain
  where
    convertToConfigWrapper :: [a] -> [LYTU.Config a]
    convertToConfigWrapper configs =
      zipWith
        (\id cfg -> cfg {LYTU.identifier = id})
        [0 ..]
        (map (\cfg -> LYTU.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) configs)

    applyPatchToConfig :: forall b m. (FromJSON b, MonadFlow m, ToJSON b) => [LYTU.Config b] -> m [LYTU.Config b]
    applyPatchToConfig configWrapper = do
      patchedConfigs <-
        mapM
          ( \c@(LYTU.Config _ ed identifier) -> do
              logicRanResp <- LYTU.runLogics baseLogics c
              case (A.fromJSON (logicRanResp.result) :: A.Result (LYTU.Config b)) of
                A.Success nCfg -> pure $ LYTU.Config nCfg.config ed identifier -- identifier is in JSON logic as well, need to handle that properly (it shouldn't be there, thats just placeholder) its used only for figuring our which particular rows of a config are updated using a rule
                A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to the config. " <> show e
          )
          configWrapper
      pure patchedConfigs

    getConfigsToUpdate :: (MonadFlow m, Eq a, Show a, FromJSON a, ToJSON a) => [LYTU.Config a] -> [LYTU.Config a] -> m [a]
    getConfigsToUpdate configWrapper cfgs = do
      let sortedCfgs = sortOn LYTU.identifier cfgs
      pure $
        catMaybes $
          zipWith
            ( \cfg1 cfg2 ->
                if cfg1.identifier == cfg2.identifier && cfg1.config /= cfg2.config
                  then Just cfg2.config
                  else Nothing
            )
            configWrapper
            sortedCfgs

    handleConfigUpdate ::
      (MonadFlow m, FromJSON a, ToJSON a, Eq a, Show a) =>
      (Id MerchantOperatingCity -> m [a]) -> -- Fetch function
      m () -> -- Cache clearing function
      (a -> m ()) -> -- Update function
      Id MerchantOperatingCity ->
      m ()
    handleConfigUpdate fetchFunc clearCacheFunc updateFunc merchantOpCityId' = do
      configs <- fetchFunc merchantOpCityId'
      let configWrapper = convertToConfigWrapper configs
      logDebug $ "baic logic b0: " <> (cs (A.encode configWrapper) :: Text)
      patchedConfigs <- applyPatchToConfig configWrapper
      logDebug $ "baic logic b01: " <> (cs (A.encode patchedConfigs) :: Text)
      configsToUpdate <- getConfigsToUpdate configWrapper patchedConfigs
      logDebug $ "config to update" <> (cs (A.encode configsToUpdate) :: Text)
      mapM_ updateFunc configsToUpdate
      clearCacheFunc

    handleConfigUpdateWithExtraDimensions ::
      (MonadFlow m, FromJSON a, ToJSON a, Eq a, Show a) =>
      (Id MerchantOperatingCity -> Bool -> m [a]) -> -- Fetch function
      m () -> -- Cache clearing function
      (a -> m ()) -> -- Update function
      Id MerchantOperatingCity ->
      m ()
    handleConfigUpdateWithExtraDimensions fetchFunc clearCacheFunc updateFunc merchantOpCityId' = do
      configs <- fetchFunc merchantOpCityId' True
      let configWrapper = convertToConfigWrapper configs
      patchedConfigs <- applyPatchToConfig configWrapper
      configsToUpdate <- getConfigsToUpdate configWrapper patchedConfigs
      mapM_ updateFunc configsToUpdate
      clearCacheFunc

    handleConfigUpdateWithExtraDimensionsUi ::
      (MonadFlow m) =>
      (LYTU.UiConfigRequest -> Id MerchantOperatingCity -> m (Maybe DTU.UiRiderConfig)) -> -- Fetch function
      m () -> -- Cache clearing function
      (DTU.UiRiderConfig -> m ()) -> -- Update function
      Id MerchantOperatingCity ->
      LYTU.UiConfigRequest ->
      m ()
    handleConfigUpdateWithExtraDimensionsUi fetchFunc clearCacheFunc updateFunc merchantOpCityId' uiConfigReq' = do
      uiConfig :: DTU.UiRiderConfig <- fetchFunc uiConfigReq' merchantOpCityId' >>= fromMaybeM (InvalidRequest "No default found for UiRiderConfig")
      let configWrapper = convertToConfigWrapper [uiConfig.config]
      patchedConfigs <- applyPatchToConfig configWrapper
      configsToUpdate <- getConfigsToUpdate configWrapper patchedConfigs
      let configsToUpdate' :: [DTU.UiRiderConfig] = zipWith (\cfg newConfig -> cfg {DTU.config = newConfig}) [uiConfig] configsToUpdate
      mapM_ updateFunc configsToUpdate'
      clearCacheFunc

    normalizeMaybeFetch :: (MonadFlow m, FromJSON a, ToJSON a, Eq a, Show a) => (Id MerchantOperatingCity -> m (Maybe a)) -> Id MerchantOperatingCity -> m [a]
    normalizeMaybeFetch fetchFunc merchantOpCityId' = do
      result <- fetchFunc merchantOpCityId'
      pure $ maybeToList result
    handleConfigUpdateViaJson ::
      (MonadFlow m, FromJSON a, ToJSON a) =>
      (Id MerchantOperatingCity -> m [a]) ->
      m () ->
      (a -> m ()) ->
      Id MerchantOperatingCity ->
      m ()
    handleConfigUpdateViaJson fetchFunc clearCacheFunc updateFunc merchantOpCityId' = do
      configs <- fetchFunc merchantOpCityId'
      let configWrapper = convertToConfigWrapper configs
      patchedConfigs <- applyPatchToConfig configWrapper
      configsToUpdateRes <- getConfigsToUpdateViaJson configWrapper patchedConfigs
      mapM_ updateFunc configsToUpdateRes
      clearCacheFunc

    getConfigsToUpdateViaJson :: (MonadFlow m, FromJSON a, ToJSON a) => [LYTU.Config a] -> [LYTU.Config a] -> m [a]
    getConfigsToUpdateViaJson configWrapper cfgs = do
      let sortedCfgs = sortOn LYTU.identifier cfgs
      pure $
        catMaybes $
          zipWith
            ( \cfg1 cfg2 ->
                if cfg1.identifier == cfg2.identifier && A.toJSON cfg1.config /= A.toJSON cfg2.config
                  then Just cfg2.config
                  else Nothing
            )
            configWrapper
            sortedCfgs

    normalizeMaybeFetchJson :: (MonadFlow m) => (Id MerchantOperatingCity -> m (Maybe a)) -> Id MerchantOperatingCity -> m [a]
    normalizeMaybeFetchJson fetchFunc merchantOpCityId' = do
      result <- fetchFunc merchantOpCityId'
      pure $ maybeToList result

getTSServiceUrl :: (CoreMetrics m, MonadFlow m, CPT.HasTSServiceConfig m r) => m BaseUrl
getTSServiceUrl = do
  tsServiceConfig <- asks (.tsServiceConfig)
  pure tsServiceConfig.url
