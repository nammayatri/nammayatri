{-# OPTIONS_GHC -Wno-deprecations #-}

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
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as SCMMPN
import qualified Storage.CachedQueries.Merchant.PayoutConfig as SCMPC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.MerchantConfig as SCMC
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as SCRRN
import qualified Storage.CachedQueries.UiRiderConfig as SCU
import qualified Storage.Queries.MerchantConfig as SQMC
import qualified Storage.Queries.MerchantPushNotification as SQMPN
import qualified Storage.Queries.PayoutConfig as SQPC
import qualified Storage.Queries.RideRelatedNotificationConfig as SQRRN
import qualified Storage.Queries.RiderConfig as SQR
import qualified Storage.Queries.UiRiderConfig as SQU
import qualified Tools.DynamicLogic as DynamicLogic

returnConfigs :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => LYTU.ConfigType -> Id LYTU.MerchantOperatingCity -> Id LYTU.Merchant -> Kernel.Types.Beckn.Context.City -> m LYTU.TableDataResp
returnConfigs cfgType merchantOpCityId merchantId opCity = do
  case cfgType of
    LYTU.RiderConfig -> do
      riderCfg <- QRC.findByMerchantOperatingCityId (cast merchantOpCityId) (Just [])
      return LYTU.TableDataResp {configs = map A.toJSON (maybeToList riderCfg)}
    LYTU.PayoutConfig -> do
      payoutCfg <- SCMPC.findAllByMerchantOpCityId (cast merchantOpCityId) (Just [])
      return LYTU.TableDataResp {configs = map A.toJSON payoutCfg}
    LYTU.RideRelatedNotificationConfig -> do
      rideRelatedNotificationCfg <- SCRRN.findAllByMerchantOperatingCityId (cast merchantOpCityId) (Just [])
      return LYTU.TableDataResp {configs = map A.toJSON rideRelatedNotificationCfg}
    LYTU.MerchantConfig -> do
      merchantCfg <- SCMC.findAllByMerchantOperatingCityId (cast merchantOpCityId) (Just [])
      return LYTU.TableDataResp {configs = map A.toJSON merchantCfg}
    LYTU.MerchantPushNotification -> do
      merchantPushNotification <- SCMMPN.findAllByMerchantOpCityId (cast merchantOpCityId) (Just [])
      return LYTU.TableDataResp {configs = map A.toJSON merchantPushNotification}
    (LYTU.UiConfig dt pt) -> do
      let uiConfigReq = LYTU.UiConfigRequest {os = dt, platform = pt, merchantId = getId merchantId, city = opCity, language = Nothing, bundle = Nothing, toss = Nothing}
      (mbUiConfig, _) <- SCU.findUiConfig uiConfigReq (cast merchantOpCityId) True
      return LYTU.TableDataResp {configs = map A.toJSON (maybeToList mbUiConfig)}
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
    LYTU.RIDER_CONFIG (LYTU.UiConfig dt pt) -> do
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

getTSServiceUrl :: (CoreMetrics m, MonadFlow m, CPT.HasTSServiceConfig m r) => m BaseUrl
getTSServiceUrl = do
  tsServiceConfig <- asks (.tsServiceConfig)
  pure tsServiceConfig.url
