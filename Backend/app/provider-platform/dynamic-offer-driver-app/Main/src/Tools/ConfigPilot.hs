{-# OPTIONS_GHC -Wno-deprecations #-}

module Tools.ConfigPilot where

import qualified ConfigPilotFrontend.Types as CPT
import qualified Data.Aeson as A
import Data.List (sortOn)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.UiDriverConfig as DTU
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as LYTBF
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicElement as LTSCADLE
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicElement as LTSQADLE
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types.AppDynamicLogicElement as LYTADLE
import qualified Lib.Yudhishthira.Types.ConfigPilot as LYTC
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as SCMDPC
import qualified Storage.CachedQueries.Merchant.MerchantMessage as SCMM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as SCMMPN
import qualified Storage.CachedQueries.Merchant.PayoutConfig as SCMP
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCMTC
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as SCR
import qualified Storage.CachedQueries.UiDriverConfig as SCU
import qualified Storage.Queries.DriverPoolConfig as SCMD
import qualified Storage.Queries.MerchantMessage as SQM
import qualified Storage.Queries.MerchantPushNotification as SQMPN
import qualified Storage.Queries.PayoutConfig as SCP
import qualified Storage.Queries.RideRelatedNotificationConfig as SQR
import qualified Storage.Queries.TransporterConfig as SCMT
import qualified Storage.Queries.UiDriverConfig as SQU
import qualified Tools.DynamicLogic as DynamicLogic

returnConfigs :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => LYT.LogicDomain -> Id LYT.MerchantOperatingCity -> Id LYT.Merchant -> Kernel.Types.Beckn.Context.City -> m LYT.TableDataResp
returnConfigs logicDomain merchantOpCityId merchantId opCity = do
  case logicDomain of
    LYT.DRIVER_CONFIG LYT.DriverPoolConfig -> do
      driverPoolCfg <- SCMDPC.findAllByMerchantOpCityId (cast merchantOpCityId) (Just []) Nothing
      return LYT.TableDataResp {configs = map A.toJSON driverPoolCfg}
    LYT.DRIVER_CONFIG LYT.TransporterConfig -> do
      transporterCfg <- SCMTC.getTransporterConfigFromDB (cast merchantOpCityId)
      return LYT.TableDataResp {configs = map A.toJSON (maybeToList transporterCfg)}
    LYT.DRIVER_CONFIG LYT.PayoutConfig -> do
      payoutCfg <- SCMP.findAllByMerchantOpCityId (cast merchantOpCityId) (Just [])
      return LYT.TableDataResp {configs = map A.toJSON payoutCfg}
    LYT.DRIVER_CONFIG LYT.RideRelatedNotificationConfig -> do
      rideRelatedNotificationCfg <- SCR.findAllByMerchantOperatingCityId (cast merchantOpCityId) (Just [])
      return LYT.TableDataResp {configs = map A.toJSON rideRelatedNotificationCfg}
    LYT.DRIVER_CONFIG LYT.MerchantMessage -> do
      merchantMessage <- SCMM.findAllByMerchantOpCityId (cast merchantOpCityId) (Just [])
      return LYT.TableDataResp {configs = map A.toJSON merchantMessage}
    LYT.DRIVER_CONFIG LYT.MerchantPushNotification -> do
      merchantPushNotification <- SCMMPN.findAllByMerchantOpCityId (cast merchantOpCityId) (Just [])
      return LYT.TableDataResp {configs = map A.toJSON merchantPushNotification}
    LYT.UI_DRIVER dt pt -> do
      let uiConfigReq = LYT.UiConfigRequest {os = dt, platform = pt, merchantId = getId merchantId, city = opCity, language = Nothing, bundle = Nothing, toss = Nothing}
      mbConfigInfo <- SCU.findUIConfig uiConfigReq (cast merchantOpCityId) True
      return LYT.TableDataResp {configs = map A.toJSON (maybeToList (fst <$> mbConfigInfo))}
    _ -> throwError $ InvalidRequest "Unsupported config type."

handleConfigDBUpdate :: (BeamFlow m r, EsqDBFlow m r, CacheFlow m r) => Id LYT.MerchantOperatingCity -> LYT.ConcludeReq -> [A.Value] -> Maybe (Id LYT.Merchant) -> Kernel.Types.Beckn.Context.City -> m ()
handleConfigDBUpdate merchantOpCityId concludeReq baseLogics mbMerchantId opCity = do
  case concludeReq.domain of
    LYT.DRIVER_CONFIG LYT.DriverPoolConfig -> do
      handleConfigUpdateWithExtraDimensions SCMD.findAllByMerchantOpCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYT.DRIVER_CONFIG LYT.DriverPoolConfig)) SCMD.updateByPrimaryKey (cast merchantOpCityId)
    LYT.DRIVER_CONFIG LYT.TransporterConfig -> do
      handleConfigUpdate (normalizeMaybeFetch SCMT.findByMerchantOpCityId) (SCMTC.clearCache (cast merchantOpCityId)) SCMT.update (cast merchantOpCityId)
    LYT.DRIVER_CONFIG LYT.PayoutConfig -> do
      handleConfigUpdate SCP.findAllByMerchantOpCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYT.DRIVER_CONFIG LYT.PayoutConfig)) SCP.updateByPrimaryKey (cast merchantOpCityId)
    LYT.DRIVER_CONFIG LYT.RideRelatedNotificationConfig -> do
      handleConfigUpdate SQR.findAllByMerchantOperatingCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYT.DRIVER_CONFIG LYT.RideRelatedNotificationConfig)) SQR.updateByPrimaryKey (cast merchantOpCityId)
    LYT.DRIVER_CONFIG LYT.MerchantMessage -> do
      handleConfigUpdate SQM.findAllByMerchantOpCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYT.DRIVER_CONFIG LYT.MerchantMessage)) SQM.updateByPrimaryKey (cast merchantOpCityId)
    LYT.DRIVER_CONFIG LYT.MerchantPushNotification -> do
      handleConfigUpdate SQMPN.findAllByMerchantOpCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYT.DRIVER_CONFIG LYT.MerchantPushNotification)) SQMPN.updateByPrimaryKey (cast merchantOpCityId)
    LYT.UI_DRIVER dt pt -> do
      let uiConfigReq = LYT.UiConfigRequest {os = dt, platform = pt, merchantId = maybe "" getId mbMerchantId, city = opCity, language = Nothing, bundle = Nothing, toss = Nothing}
      handleConfigUpdateWithExtraDimensionsUi SQU.findUIConfig (SCU.clearCache (cast merchantOpCityId) dt pt) SCU.updateByPrimaryKey (cast merchantOpCityId) uiConfigReq concludeReq.version concludeReq.domain
    _ -> throwError $ InvalidRequest $ "Logic Domain not supported" <> show concludeReq.domain
  where
    convertToConfigWrapper :: [a] -> [LYT.Config a]
    convertToConfigWrapper configs =
      zipWith
        (\id cfg -> cfg {LYT.identifier = id})
        [0 ..]
        (map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) configs)

    applyPatchToConfig :: forall b m. (FromJSON b, MonadFlow m, ToJSON b) => [LYT.Config b] -> m [LYT.Config b]
    applyPatchToConfig configWrapper = do
      patchedConfigs <- mapM (LYTU.runLogics baseLogics) configWrapper
      mapM
        ( \resp ->
            case (A.fromJSON (resp.result) :: A.Result (LYT.Config b)) of
              A.Success cfg -> pure cfg
              A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to the config. " <> show e
        )
        patchedConfigs

    getConfigsToUpdate :: (MonadFlow m, Eq a, Show a, FromJSON a, ToJSON a) => [LYT.Config a] -> [LYT.Config a] -> m [a]
    getConfigsToUpdate configWrapper cfgs = do
      let sortedCfgs = sortOn LYT.identifier cfgs
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
      (Maybe Int -> Maybe Int -> Id MerchantOperatingCity -> m [a]) -> -- Fetch function
      m () -> -- Cache clearing function
      (a -> m ()) -> -- Update function
      Id MerchantOperatingCity ->
      m ()
    handleConfigUpdateWithExtraDimensions fetchFunc clearCacheFunc updateFunc merchantOpCityId' = do
      configs <- fetchFunc Nothing Nothing merchantOpCityId'
      let configWrapper = convertToConfigWrapper configs
      patchedConfigs <- applyPatchToConfig configWrapper
      configsToUpdate <- getConfigsToUpdate configWrapper patchedConfigs
      mapM_ updateFunc configsToUpdate
      clearCacheFunc

    handleConfigUpdateWithExtraDimensionsUi ::
      (MonadFlow m, LYTBF.BeamFlow m r) =>
      (LYT.UiConfigRequest -> Id MerchantOperatingCity -> m (Maybe DTU.UiDriverConfig)) -> -- Fetch function
      m () -> -- Cache clearing function
      (DTU.UiDriverConfig -> m ()) -> -- Update function
      Id MerchantOperatingCity ->
      LYT.UiConfigRequest ->
      Kernel.Prelude.Int ->
      LYT.LogicDomain ->
      m ()
    handleConfigUpdateWithExtraDimensionsUi fetchFunc clearCacheFunc updateFunc merchantOpCityId' uiConfigReq' version domain = do
      uiConfig :: DTU.UiDriverConfig <- fetchFunc uiConfigReq' merchantOpCityId' >>= fromMaybeM (InvalidRequest "No default found for UiDriverConfig")
      let configWrapper = convertToConfigWrapper [uiConfig.config]
      patchedConfigs <- applyPatchToConfig configWrapper
      let extractedPatchedConfigElement :: [Value] = fmap LYTC.config patchedConfigs
      appDynamicLogicElement <- LTSQADLE.findByPrimaryKey domain 0 version >>= fromMaybeM (InvalidRequest $ "No AppDynamicLogicElement found for domain " <> show domain <> " and version " <> show version)
      let updatedAppDynamicLogicElement :: LYTADLE.AppDynamicLogicElement = appDynamicLogicElement {LYTADLE.patchedElement = listToMaybe extractedPatchedConfigElement}
      configsToUpdate <- getConfigsToUpdate configWrapper patchedConfigs
      let configsToUpdate' :: [DTU.UiDriverConfig] = zipWith (\cfg newConfig -> cfg {DTU.config = newConfig}) [uiConfig] configsToUpdate
      LTSQADLE.updateByPrimaryKey updatedAppDynamicLogicElement
      LTSCADLE.clearCache domain
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
