module SharedLogic.IntegratedBPPConfig where

import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types.IntegratedBPPConfig
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude
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
findMaybeIntegratedBPPConfig mbIntegratedBPPConfigId merchantOperatingCityId vehicleCategory platformType = do
  mbById <- maybe (pure Nothing) QIBC.findById mbIntegratedBPPConfigId
  case mbById of
    Just cfg -> pure $ Just cfg
    Nothing -> do
      moc <- QMOC.findById merchantOperatingCityId >>= fromMaybeM (InvalidRequest "Operating City not found")
      listToMaybe <$> QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) (Just moc.city) vehicleCategory platformType

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

findAllIntegratedBPPConfig ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  Text ->
  PlatformType ->
  m [IntegratedBPPConfig]
findAllIntegratedBPPConfig merchantOperatingCityId vehicleCategory platformType = do
  moc <- QMOC.findById merchantOperatingCityId >>= fromMaybeM (InvalidRequest "Operating City not found")
  QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) (Just moc.city) vehicleCategory platformType

findAllIntegratedBPPConfigAcrossCities ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  PlatformType ->
  m [IntegratedBPPConfig]
findAllIntegratedBPPConfigAcrossCities vehicleCategory platformType =
  QIBC.findAllByPlatformAndVehicleCategory (show Spec.FRFS) vehicleCategory platformType
