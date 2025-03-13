{-# OPTIONS_GHC -Wno-deprecations #-}

module Tools.ConfigPilot where

import qualified Data.Aeson as A
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
-- import qualified Storage.Queries.UiDriverConfig as QUiC
import EulerHS.Prelude hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as SCMDPC
import qualified Storage.CachedQueries.Merchant.MerchantMessage as SCMM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as SCMMPN
import qualified Storage.CachedQueries.Merchant.PayoutConfig as SCMP
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCMTC
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as SCR
import qualified Storage.Queries.DriverPoolConfig as SCMD
import qualified Storage.Queries.MerchantMessage as SQM
import qualified Storage.Queries.MerchantPushNotification as SQMPN
import qualified Storage.Queries.PayoutConfig as SCP
import qualified Storage.Queries.RideRelatedNotificationConfig as SQR
import qualified Storage.Queries.TransporterConfig as SCMT
import qualified Tools.DynamicLogic as DynamicLogic

returnConfigs :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => LYT.ConfigType -> Id LYT.MerchantOperatingCity -> m LYT.TableDataResp
returnConfigs cfgType merchantOpCityId = do
  case cfgType of
    LYT.DriverPoolConfig -> do
      driverPoolCfg <- SCMDPC.findAllByMerchantOpCityId (cast merchantOpCityId) (Just []) Nothing
      return LYT.TableDataResp {configs = map A.toJSON driverPoolCfg}
    LYT.TransporterConfig -> do
      transporterCfg <- SCMTC.getTransporterConfigFromDB (cast merchantOpCityId)
      return LYT.TableDataResp {configs = map A.toJSON (maybeToList transporterCfg)}
    LYT.PayoutConfig -> do
      payoutCfg <- SCMP.findAllByMerchantOpCityId (cast merchantOpCityId) (Just [])
      return LYT.TableDataResp {configs = map A.toJSON payoutCfg}
    LYT.RideRelatedNotificationConfig -> do
      rideRelatedNotificationCfg <- SCR.findAllByMerchantOperatingCityId (cast merchantOpCityId) (Just [])
      return LYT.TableDataResp {configs = map A.toJSON rideRelatedNotificationCfg}
    LYT.MerchantMessage -> do
      merchantMessage <- SCMM.findAllByMerchantOpCityId (cast merchantOpCityId) (Just [])
      return LYT.TableDataResp {configs = map A.toJSON merchantMessage}
    LYT.MerchantPushNotification -> do
      merchantPushNotification <- SCMMPN.findAllByMerchantOpCityId (cast merchantOpCityId) (Just [])
      return LYT.TableDataResp {configs = map A.toJSON merchantPushNotification}
    _ -> throwError $ InvalidRequest "Unsupported config type."

handleConfigDBUpdate :: (BeamFlow m r, EsqDBFlow m r, CacheFlow m r) => Id LYT.MerchantOperatingCity -> LYT.ConcludeReq -> [A.Value] -> m ()
handleConfigDBUpdate merchantOpCityId concludeReq baseLogics = do
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

    normalizeMaybeFetch :: (MonadFlow m, FromJSON a, ToJSON a, Eq a, Show a) => (Id MerchantOperatingCity -> m (Maybe a)) -> Id MerchantOperatingCity -> m [a]
    normalizeMaybeFetch fetchFunc merchantOpCityId' = do
      result <- fetchFunc merchantOpCityId'
      pure $ maybeToList result
