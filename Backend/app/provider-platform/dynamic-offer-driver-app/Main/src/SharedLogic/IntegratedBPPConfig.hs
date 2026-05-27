module SharedLogic.IntegratedBPPConfig
  ( findMaybeIntegratedBPPConfig,
    findIntegratedBPPConfig,
    findFirstIbppConfigByCityAndVehicle,
    findAllIntegratedBPPConfig,
    findAllIntegratedBPPConfigAcrossCities,
    getBaseUrl,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types.IntegratedBPPConfig
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.IntegratedBPPConfig as QIBC
import qualified Storage.Queries.MerchantOperatingCity as QMOC
import Tools.Error

findMaybeIntegratedBPPConfig ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe (Id IntegratedBPPConfig) ->
  Id MerchantOperatingCity ->
  Text ->
  PlatformType ->
  m (Maybe IntegratedBPPConfig)
findMaybeIntegratedBPPConfig mbIntegratedBPPConfigId merchantOperatingCityId vehicleCategory platformType =
  case mbIntegratedBPPConfigId of
    Just configId ->
      IM.withInMemCache ["IntegratedBPPConfig", getId configId] 3600 $
        QIBC.findById configId
    Nothing ->
      IM.withInMemCache ["IntegratedBPPConfig", getId merchantOperatingCityId, vehicleCategory, show platformType] 3600 $ do
        moc <- QMOC.findById merchantOperatingCityId >>= fromMaybeM (InvalidRequest "Operating City not found")
        listToMaybe <$> QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) (Just moc.city) (Just vehicleCategory) platformType

findIntegratedBPPConfig ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe (Id IntegratedBPPConfig) ->
  Id MerchantOperatingCity ->
  Text ->
  PlatformType ->
  m IntegratedBPPConfig
findIntegratedBPPConfig mbIntegratedBPPConfigId merchantOperatingCityId vehicleCategory platformType =
  findMaybeIntegratedBPPConfig mbIntegratedBPPConfigId merchantOperatingCityId vehicleCategory platformType
    >>= fromMaybeM IntegratedBPPConfigNotFound

findFirstIbppConfigByCityAndVehicle ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  Text ->
  m IntegratedBPPConfig
findFirstIbppConfigByCityAndVehicle merchantOpCityId vehicleCategory = do
  moc <- QMOC.findById merchantOpCityId >>= fromMaybeM (InvalidRequest "Operating City not found")
  configs <- QIBC.findByDomainAndCityAndVehicleCategoryAnyPlatform (show Spec.FRFS) (Just moc.city) (Just vehicleCategory)
  listToMaybe configs & fromMaybeM IntegratedBPPConfigNotFound

findAllIntegratedBPPConfig ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  Text ->
  PlatformType ->
  m [IntegratedBPPConfig]
findAllIntegratedBPPConfig merchantOperatingCityId vehicleCategory platformType = do
  moc <- QMOC.findById merchantOperatingCityId >>= fromMaybeM (InvalidRequest "Operating City not found")
  QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) (Just moc.city) (Just vehicleCategory) platformType

findAllIntegratedBPPConfigAcrossCities ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  PlatformType ->
  m [IntegratedBPPConfig]
findAllIntegratedBPPConfigAcrossCities vehicleCategory platformType =
  QIBC.findAllByPlatformAndVehicleCategory (show Spec.FRFS) (Just vehicleCategory) platformType

getBaseUrl :: (MonadFlow m) => IntegratedBPPConfig -> m BaseUrl
getBaseUrl cfg = case cfg.providerConfig of
  DIRECT directCfg -> directCfg.baseUrl & fromMaybeM (InvalidRequest "baseUrl not configured in IntegratedBPPConfig")
  ONDC _ -> throwError $ InvalidRequest "Base URL is not applicable for ONDC provider config in IntegratedBPPConfig"
