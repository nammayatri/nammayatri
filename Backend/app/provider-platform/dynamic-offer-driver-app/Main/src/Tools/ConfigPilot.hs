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
import qualified Lib.Yudhishthira.Storage.Queries.TagActionNotificationConfig as SQTANC
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types.AppDynamicLogicElement as LYTADLE
import qualified Lib.Yudhishthira.Types.ConfigPilot as LYTC
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.CoinsConfig as CQCC
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.FleetOwnerDocumentVerificationConfig as CQFODVC
import qualified Storage.CachedQueries.GoHomeConfig as CQGHC
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as SCMDPC
import qualified Storage.CachedQueries.Merchant.MerchantMessage as SCMM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as SCMMPN
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.Merchant.PayoutConfig as SCMP
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCMTC
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as SCR
import qualified Storage.CachedQueries.UiDriverConfig as SCU
import qualified Storage.Queries.Coins.CoinsConfig as SQCC
import qualified Storage.Queries.DocumentVerificationConfig as SQDVC
import qualified Storage.Queries.DriverPoolConfig as SCMD
import qualified Storage.Queries.FleetOwnerDocumentVerificationConfig as SQFODVC
import qualified Storage.Queries.GoHomeConfig as SQGHC
import qualified Storage.Queries.LeaderBoardConfigs as SQLBC
import qualified Storage.Queries.MerchantMessage as SQM
import qualified Storage.Queries.MerchantPushNotification as SQMPN
import qualified Storage.Queries.MerchantServiceConfigExtra as SQMSCE
import qualified Storage.Queries.MerchantServiceUsageConfig as SQMSUC
import qualified Storage.Queries.PayoutConfig as SCP
import qualified Storage.Queries.ReminderConfig as SQRMC
import qualified Storage.Queries.RideRelatedNotificationConfig as SQR
import qualified Storage.Queries.ScheduledPayoutConfig as SQSPC
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
    LYT.DRIVER_CONFIG LYT.MerchantServiceUsageConfigDriver -> do
      msuc <- CQMSUC.findByMerchantOpCityId (cast merchantOpCityId)
      return LYT.TableDataResp {configs = map A.toJSON (maybeToList msuc)}
    LYT.DRIVER_CONFIG LYT.DocumentVerificationConfig -> do
      dvCfg <- CQDVC.findAllByMerchantOpCityId (cast merchantOpCityId) (Just [])
      return LYT.TableDataResp {configs = map A.toJSON dvCfg}
    LYT.DRIVER_CONFIG LYT.GoHomeConfig -> do
      goHomeCfg <- CQGHC.findByMerchantOpCityId (cast merchantOpCityId)
      return LYT.TableDataResp {configs = [A.toJSON goHomeCfg]}
    LYT.DRIVER_CONFIG LYT.LeaderBoardConfig -> do
      lbCfg <- SQLBC.findAllByMerchantOpCityId (cast merchantOpCityId)
      return LYT.TableDataResp {configs = map A.toJSON lbCfg}
    LYT.DRIVER_CONFIG LYT.ReminderConfig -> do
      reminderCfg <- SQRMC.findAllByMerchantOpCityId (cast merchantOpCityId)
      return LYT.TableDataResp {configs = map A.toJSON reminderCfg}
    LYT.DRIVER_CONFIG LYT.ScheduledPayoutConfig -> do
      spCfg <- SQSPC.findAllByMerchantOpCityId (cast merchantOpCityId)
      return LYT.TableDataResp {configs = map A.toJSON spCfg}
    LYT.DRIVER_CONFIG LYT.TagActionNotificationConfig -> do
      tanCfg <- SQTANC.findAllByMerchantOperatingCityId (cast merchantOpCityId)
      return LYT.TableDataResp {configs = map A.toJSON tanCfg}
    LYT.DRIVER_CONFIG LYT.FleetOwnerDocumentVerificationConfig -> do
      fodvCfg <- CQFODVC.findAllByMerchantOpCityId (cast merchantOpCityId) (Just [])
      return LYT.TableDataResp {configs = map A.toJSON fodvCfg}
    LYT.DRIVER_CONFIG LYT.CoinsConfig -> do
      coinsCfg <- CQCC.findAllByMerchantOptCityId (cast merchantOpCityId)
      return LYT.TableDataResp {configs = map A.toJSON coinsCfg}
    LYT.DRIVER_CONFIG LYT.MerchantServiceConfig -> do
      mscCfg <- SQMSCE.findAllMerchantOpCityId (cast merchantOpCityId)
      return LYT.TableDataResp {configs = map A.toJSON mscCfg}
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
    LYT.DRIVER_CONFIG LYT.MerchantServiceUsageConfigDriver -> do
      handleConfigUpdateViaJson (\mocId' -> maybeToList <$> SQMSUC.findByMerchantOpCityId mocId') (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYT.DRIVER_CONFIG LYT.MerchantServiceUsageConfigDriver)) CQMSUC.updateMerchantServiceUsageConfig (cast merchantOpCityId)
    LYT.DRIVER_CONFIG LYT.DocumentVerificationConfig -> do
      handleConfigUpdateViaJson (\mocId' -> SQDVC.findAllByMerchantOpCityId Nothing Nothing mocId') (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYT.DRIVER_CONFIG LYT.DocumentVerificationConfig)) SQDVC.updateByPrimaryKey (cast merchantOpCityId)
    LYT.DRIVER_CONFIG LYT.GoHomeConfig -> do
      handleConfigUpdateViaJson (\mocId' -> maybeToList <$> SQGHC.findByMerchantOpCityId mocId') (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYT.DRIVER_CONFIG LYT.GoHomeConfig)) SQGHC.updateByPrimaryKey (cast merchantOpCityId)
    LYT.DRIVER_CONFIG LYT.LeaderBoardConfig -> do
      handleConfigUpdateViaJson SQLBC.findAllByMerchantOpCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYT.DRIVER_CONFIG LYT.LeaderBoardConfig)) SQLBC.updateByPrimaryKey (cast merchantOpCityId)
    LYT.DRIVER_CONFIG LYT.ScheduledPayoutConfig -> do
      handleConfigUpdate SQSPC.findAllByMerchantOpCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYT.DRIVER_CONFIG LYT.ScheduledPayoutConfig)) SQSPC.updateByPrimaryKey (cast merchantOpCityId)
    LYT.DRIVER_CONFIG LYT.TagActionNotificationConfig -> do
      handleConfigUpdateViaJson (\mocId' -> SQTANC.findAllByMerchantOperatingCityId (cast mocId')) (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYT.DRIVER_CONFIG LYT.TagActionNotificationConfig)) SQTANC.updateByPrimaryKey (cast merchantOpCityId)
    LYT.DRIVER_CONFIG LYT.FleetOwnerDocumentVerificationConfig -> do
      handleConfigUpdateViaJson (\mocId' -> SQFODVC.findAllByMerchantOpCityId Nothing Nothing mocId') (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYT.DRIVER_CONFIG LYT.FleetOwnerDocumentVerificationConfig)) SQFODVC.updateByPrimaryKey (cast merchantOpCityId)
    LYT.DRIVER_CONFIG LYT.ReminderConfig -> do
      handleConfigUpdateViaJson SQRMC.findAllByMerchantOpCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYT.DRIVER_CONFIG LYT.ReminderConfig)) SQRMC.updateByPrimaryKey (cast merchantOpCityId)
    LYT.DRIVER_CONFIG LYT.CoinsConfig -> do
      handleConfigUpdateViaJson SQCC.findAllByMerchantOptCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYT.DRIVER_CONFIG LYT.CoinsConfig)) SQCC.updateByPrimaryKey (cast merchantOpCityId)
    LYT.DRIVER_CONFIG LYT.MerchantServiceConfig -> do
      handleConfigUpdateViaJson SQMSCE.findAllMerchantOpCityId (DynamicLogic.deleteConfigHashKey (cast merchantOpCityId) (LYT.DRIVER_CONFIG LYT.MerchantServiceConfig)) (\cfg -> SQMSCE.upsertMerchantServiceConfig cfg (cast merchantOpCityId)) (cast merchantOpCityId)
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

    -- For configs that do not derive Eq (compared via their JSON encoding instead).
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

    getConfigsToUpdateViaJson :: (MonadFlow m, FromJSON a, ToJSON a) => [LYT.Config a] -> [LYT.Config a] -> m [a]
    getConfigsToUpdateViaJson configWrapper cfgs = do
      let sortedCfgs = sortOn LYT.identifier cfgs
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

getTSServiceUrl :: (CoreMetrics m, MonadFlow m, CPT.HasTSServiceConfig m r) => m BaseUrl
getTSServiceUrl = do
  tsServiceConfig <- asks (.tsServiceConfig)
  pure tsServiceConfig.url
