{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.IntegratedBPPConfigExtra where

import qualified BecknV2.OnDemand.Enums
import Control.Lens ((^?), _head)
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
  fmap ((^? _head) . sortBy (\a b -> compare b.createdAt a.createdAt)) (findAllByDomainAndCityAndVehicleCategory domain merchantOperatingCityId vehicleCategory platformType)

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
