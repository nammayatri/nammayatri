{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.IntegratedBPPConfigExtra where

import qualified BecknV2.OnDemand.Enums
import Data.List (sortBy)
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.IntegratedBPPConfig as Beam
import Storage.Queries.OrphanInstances.IntegratedBPPConfig

-- | Which row an agency key names when several rows share it (the shared-cab feed has a MULTIMODAL row for journeys and an
-- APPLICATION row for the driver proxy and session): the one row of the caller's platform type. An agency key that is
-- shared by several rows of the same platform (one per city, as the bus and metro feeds are) names no single row, so it
-- answers Nothing and the caller falls back to its own city lookup, exactly as the generated findOne did.
pickAgencyRow :: Domain.Types.IntegratedBPPConfig.PlatformType -> (a -> (Domain.Types.IntegratedBPPConfig.PlatformType, Text)) -> [a] -> Maybe a
pickAgencyRow preferred key rows = case filter ((== preferred) . fst . key) rows of
  [row] -> Just row
  [] -> case rows of
    [row] -> Just row
    _ -> Nothing
  _ -> Nothing

-- | The row only if it is of the platform the caller needs (the driver proxy needs its APPLICATION row; a key that has only
-- a MULTIMODAL row names no proxy config).
onlyOfPlatform :: Domain.Types.IntegratedBPPConfig.PlatformType -> (a -> Domain.Types.IntegratedBPPConfig.PlatformType) -> Maybe a -> Maybe a
onlyOfPlatform platform platformOf = mfilter ((== platform) . platformOf)

findByAgencyIdDeterministic ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Text ->
  Domain.Types.IntegratedBPPConfig.PlatformType ->
  m (Maybe Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig)
findByAgencyIdDeterministic agencyKey preferred =
  pickAgencyRow preferred (\c -> (c.platformType, Kernel.Types.Id.getId c.id)) <$> findAllWithKV [Se.Is Beam.agencyKey $ Se.Eq agencyKey]

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  m [Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig]
findAllByMerchantOperatingCityId merchantOperatingCityId =
  findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findAllByDomainAndCityAndVehicleCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Text ->
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  BecknV2.OnDemand.Enums.VehicleCategory ->
  Domain.Types.IntegratedBPPConfig.PlatformType ->
  m [Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig]
findAllByDomainAndCityAndVehicleCategory domain merchantOperatingCityId vehicleCategory platformType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory,
          Se.Is Beam.platformType $ Se.Eq platformType
        ]
    ]

findByDomainAndCityAndVehicleCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Text ->
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  BecknV2.OnDemand.Enums.VehicleCategory ->
  Domain.Types.IntegratedBPPConfig.PlatformType ->
  m (Maybe Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig)
findByDomainAndCityAndVehicleCategory domain merchantOperatingCityId vehicleCategory platformType =
  fmap (listToMaybe . sortBy (\a b -> compare b.createdAt a.createdAt)) (findAllByDomainAndCityAndVehicleCategory domain merchantOperatingCityId vehicleCategory platformType)

findAllByPlatformAndVehicleCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Text ->
  BecknV2.OnDemand.Enums.VehicleCategory ->
  Domain.Types.IntegratedBPPConfig.PlatformType ->
  m [Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig]
findAllByPlatformAndVehicleCategory domain vehicleCategory platformType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory,
          Se.Is Beam.platformType $ Se.Eq platformType
        ]
    ]
