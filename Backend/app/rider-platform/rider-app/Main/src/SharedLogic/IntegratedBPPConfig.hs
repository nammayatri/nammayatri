module SharedLogic.IntegratedBPPConfig where

import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.OnDemand.Enums as Enums
import Domain.Types.IntegratedBPPConfig
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.IntegratedBPPConfig as QIBC
import Tools.Error

findMaybeIntegratedBPPConfig ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe (Id IntegratedBPPConfig) ->
  Id MerchantOperatingCity ->
  Enums.VehicleCategory ->
  PlatformType ->
  m (Maybe IntegratedBPPConfig)
findMaybeIntegratedBPPConfig mbIntegratedBPPConfigId merchantOperatingCityId vehicleCategory platformType =
  let fallback = QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCityId vehicleCategory platformType
   in maybe fallback (\id -> QIBC.findById id |<|>| fallback) mbIntegratedBPPConfigId

findMaybeIntegratedBPPConfigFromEntity ::
  (HasField "integratedBppConfigId" r (Maybe (Id IntegratedBPPConfig)), EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  r ->
  Id MerchantOperatingCity ->
  Enums.VehicleCategory ->
  PlatformType ->
  m (Maybe IntegratedBPPConfig)
findMaybeIntegratedBPPConfigFromEntity entity merchantOperatingCityId vehicleCategory platformType =
  let fallback = QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCityId vehicleCategory platformType
   in maybe fallback (\id -> QIBC.findById id |<|>| fallback) entity.integratedBppConfigId

findMaybeIntegratedBPPConfigFromAgency ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Text ->
  Id MerchantOperatingCity ->
  Enums.VehicleCategory ->
  PlatformType ->
  m (Maybe IntegratedBPPConfig)
findMaybeIntegratedBPPConfigFromAgency agencyId merchantOperatingCityId vehicleCategory platformType =
  let fallback = QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCityId vehicleCategory platformType
   in maybe fallback (\agencyId' -> QIBC.findByAgencyId agencyId' |<|>| fallback) agencyId

findIntegratedBPPConfigById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id IntegratedBPPConfig ->
  m IntegratedBPPConfig
findIntegratedBPPConfigById integratedBPPConfigId = QIBC.findById integratedBPPConfigId >>= fromMaybeM IntegratedBPPConfigNotFound

findIntegratedBPPConfig ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe (Id IntegratedBPPConfig) ->
  Id MerchantOperatingCity ->
  Enums.VehicleCategory ->
  PlatformType ->
  m IntegratedBPPConfig
findIntegratedBPPConfig mbIntegratedBPPConfigId merchantOperatingCityId vehicleCategory platformType =
  findMaybeIntegratedBPPConfig mbIntegratedBPPConfigId merchantOperatingCityId vehicleCategory platformType
    >>= fromMaybeM IntegratedBPPConfigNotFound

findIntegratedBPPConfigFromEntity ::
  (HasField "integratedBppConfigId" a (Id IntegratedBPPConfig), EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  a ->
  m IntegratedBPPConfig
findIntegratedBPPConfigFromEntity entity =
  QIBC.findById entity.integratedBppConfigId >>= fromMaybeM IntegratedBPPConfigNotFound

findIntegratedBPPConfigFromAgency ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Text ->
  Id MerchantOperatingCity ->
  Enums.VehicleCategory ->
  PlatformType ->
  m IntegratedBPPConfig
findIntegratedBPPConfigFromAgency agencyName merchantOperatingCityId vehicleCategory platformType =
  findMaybeIntegratedBPPConfigFromAgency agencyName merchantOperatingCityId vehicleCategory platformType
    >>= fromMaybeM IntegratedBPPConfigNotFound

findAllIntegratedBPPConfig ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  Enums.VehicleCategory ->
  PlatformType ->
  m [IntegratedBPPConfig]
findAllIntegratedBPPConfig merchantOperatingCityId vehicleCategory platformType =
  QIBC.findAllByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCityId vehicleCategory platformType

fetchFirstIntegratedBPPConfigRightResult ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [IntegratedBPPConfig] ->
  (IntegratedBPPConfig -> m a) ->
  m (Maybe a)
fetchFirstIntegratedBPPConfigRightResult integratedBPPConfigs handler =
  foldrM
    ( \integratedBPPConfig result ->
        if isNothing result
          then do
            try @_ @SomeException (handler integratedBPPConfig) >>= \case
              Left _ -> return Nothing
              Right res' -> return (Just res')
          else return result
    )
    Nothing
    integratedBPPConfigs

fetchFirstIntegratedBPPConfigResult ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [IntegratedBPPConfig] ->
  (IntegratedBPPConfig -> m [a]) ->
  m [a]
fetchFirstIntegratedBPPConfigResult integratedBPPConfigs handler =
  foldrM
    ( \integratedBPPConfig result ->
        if null result
          then do
            try @_ @SomeException (handler integratedBPPConfig) >>= \case
              Left _ -> return []
              Right res' -> return res'
          else return result
    )
    []
    integratedBPPConfigs

fetchAllIntegratedBPPConfigResult ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [IntegratedBPPConfig] ->
  (IntegratedBPPConfig -> m [a]) ->
  m [a]
fetchAllIntegratedBPPConfigResult integratedBPPConfigs handler = do
  result <-
    mapM
      ( \integratedBPPConfig ->
          try @_ @SomeException (handler integratedBPPConfig) >>= \case
            Left _ -> return []
            Right res' -> return res'
      )
      integratedBPPConfigs
  return $ concat result

findAllIntegratedBPPConfigAcrossCities ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Enums.VehicleCategory ->
  PlatformType ->
  m [IntegratedBPPConfig]
findAllIntegratedBPPConfigAcrossCities vehicleCategory platformType = do
  QIBC.findAllByPlatformAndVehicleCategory (show Spec.FRFS) vehicleCategory platformType
