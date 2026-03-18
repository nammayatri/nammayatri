{-# OPTIONS_GHC -Wno-deprecations -Wno-orphans #-}

module Tools.ConfigPilot where

import qualified ConfigPilotFrontend.Types as CPT
import qualified Data.Aeson as A
import Data.List (sortOn)
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
import qualified Storage.CachedQueries.FRFSConfig as SCFRFS
import qualified Storage.CachedQueries.Merchant.PayoutConfig as SCMPC
import qualified Storage.CachedQueries.MerchantConfig as SCMC
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as SCRRN
import qualified Storage.CachedQueries.UiRiderConfig as SCU
import Storage.ConfigPilot.Config.BecknConfig (BecknConfigDimensions (..))
import Storage.ConfigPilot.Config.Exophone (ExophoneDimensions (..))
import Storage.ConfigPilot.Config.MerchantPushNotification (MerchantPushNotificationDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.BecknConfig as SQBC
import qualified Storage.Queries.Exophone as SQExo
import qualified Storage.Queries.FRFSConfig as SQFRFS
import qualified Storage.Queries.MerchantConfig as SQMC
import qualified Storage.Queries.MerchantPushNotification as SQMPN
import qualified Storage.Queries.MerchantServiceConfig as SQMSC
import qualified Storage.Queries.MerchantServiceUsageConfig as SQMSUC
import qualified Storage.Queries.PayoutConfig as SQPC
import qualified Storage.Queries.RideRelatedNotificationConfig as SQRRN
import qualified Storage.Queries.RiderConfig as SQR
import qualified Storage.Queries.UiRiderConfig as SQU
import qualified Tools.DynamicLogic as DynamicLogic

returnConfigs :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => LYTU.LogicDomain -> Id LYTU.MerchantOperatingCity -> Id LYTU.Merchant -> Kernel.Types.Beckn.Context.City -> m LYTU.TableDataResp
returnConfigs cfgType merchantOpCityId merchantId opCity = do
  case cfgType of
    LYTU.RIDER_CONFIG LYTU.RiderConfig -> do
      riderCfg <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOpCityId.getId})
      return LYTU.TableDataResp {configs = map A.toJSON (maybeToList riderCfg)}
    LYTU.RIDER_CONFIG LYTU.PayoutConfig -> do
      payoutCfg <- SCMPC.findAllByMerchantOpCityId (cast merchantOpCityId) (Just [])
      return LYTU.TableDataResp {configs = map A.toJSON payoutCfg}
    LYTU.RIDER_CONFIG LYTU.RideRelatedNotificationConfig -> do
      rideRelatedNotificationCfg <- SCRRN.findAllByMerchantOperatingCityId (cast merchantOpCityId) (Just [])
      return LYTU.TableDataResp {configs = map A.toJSON rideRelatedNotificationCfg}
    LYTU.RIDER_CONFIG LYTU.MerchantConfig -> do
      merchantCfg <- SCMC.findAllByMerchantOperatingCityId (cast merchantOpCityId) (Just [])
      return LYTU.TableDataResp {configs = map A.toJSON merchantCfg}
    LYTU.RIDER_CONFIG LYTU.MerchantServiceUsageConfig -> do
      msucCfg <- getConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId})
      return LYTU.TableDataResp {configs = map A.toJSON (maybeToList msucCfg)}
    LYTU.RIDER_CONFIG LYTU.MerchantServiceConfig -> do
      mscCfgs <- getConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, serviceName = Nothing})
      return LYTU.TableDataResp {configs = map A.toJSON mscCfgs}
    LYTU.RIDER_CONFIG LYTU.BecknConfig -> do
      bcCfgs <- getConfig (BecknConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, domain = Nothing, vehicleCategory = Nothing})
      return LYTU.TableDataResp {configs = map A.toJSON bcCfgs}
    LYTU.RIDER_CONFIG LYTU.MerchantPushNotification -> do
      mpnCfgs <- getConfig (MerchantPushNotificationDimensions {merchantOperatingCityId = merchantOpCityId.getId})
      return LYTU.TableDataResp {configs = map A.toJSON mpnCfgs}
    LYTU.RIDER_CONFIG LYTU.Exophone -> do
      exoCfgs <- getConfig (ExophoneDimensions {merchantOperatingCityId = merchantOpCityId.getId, callService = Nothing})
      return LYTU.TableDataResp {configs = map A.toJSON exoCfgs}
    LYTU.RIDER_CONFIG LYTU.FRFSConfig -> do
      frfsConfig <- SCFRFS.findByMerchantOperatingCityId (cast merchantOpCityId) (Just [])
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
      handleConfigUpdate (normalizeMaybeFetch SQR.findByMerchantOperatingCityId) (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.RiderConfig)) SQR.updateByPrimaryKey (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.PayoutConfig -> do
      handleConfigUpdate SQPC.findAllByMerchantOpCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.PayoutConfig)) SQPC.updateByPrimaryKey (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.RideRelatedNotificationConfig -> do
      handleConfigUpdate SQRRN.findAllByMerchantOperatingCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.RideRelatedNotificationConfig)) SQRRN.updateByPrimaryKey (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.MerchantConfig -> do
      handleConfigUpdateWithExtraDimensions SQMC.findAllByMerchantOperatingCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.MerchantConfig)) SQMC.updateByPrimaryKey (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.MerchantPushNotification -> do
      handleConfigUpdate SQMPN.findAllByMerchantOpCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.MerchantPushNotification)) SQMPN.updateByPrimaryKey (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.FRFSConfig -> do
      handleConfigUpdate (normalizeMaybeFetch SQFRFS.findByMerchantOperatingCityId) (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.FRFSConfig)) SQFRFS.updateByPrimaryKey (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.MerchantServiceUsageConfig -> do
      handleConfigUpdateViaJson (normalizeMaybeFetchJson SQMSUC.findByMerchantOperatingCityId) (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.MerchantServiceUsageConfig)) SQMSUC.updateMerchantServiceUsageConfig (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.MerchantServiceConfig -> do
      handleConfigUpdateViaJson SQMSC.findAllByMerchantOperatingCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.MerchantServiceConfig)) SQMSC.upsertMerchantServiceConfig (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.BecknConfig -> do
      handleConfigUpdateViaJson (SQBC.findAllByMerchantOperatingCityId . Just) (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.BecknConfig)) SQBC.updateByPrimaryKey (cast merchantOpCityId)
    LYTU.RIDER_CONFIG LYTU.Exophone -> do
      handleConfigUpdateViaJson SQExo.findAllByMerchantOperatingCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYTU.RIDER_CONFIG LYTU.Exophone)) SQExo.updateByPrimaryKey (cast merchantOpCityId)
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
      patchedConfigs <- mapM (LYTU.runLogics baseLogics) configWrapper
      mapM
        ( \resp ->
            case (A.fromJSON (resp.result) :: A.Result (LYTU.Config b)) of
              A.Success cfg -> pure cfg
              A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to the config. " <> show e
        )
        patchedConfigs

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
      patchedConfigs <- applyPatchToConfig configWrapper
      configsToUpdate <- getConfigsToUpdate configWrapper patchedConfigs
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

    -- | Variant of handleConfigUpdate that uses JSON-based equality comparison.
    -- Useful for config types that don't derive Eq.
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
