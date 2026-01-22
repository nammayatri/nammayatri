{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PayoutSplitConfig (module Storage.Queries.PayoutSplitConfig, module ReExport) where

import qualified Data.Text
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PayoutSplitConfig
import qualified Domain.Types.VehicleVariant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Types.SpecialLocation
import qualified Sequelize as Se
import qualified Storage.Beam.PayoutSplitConfig as Beam
import Storage.Queries.PayoutSplitConfigExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PayoutSplitConfig.PayoutSplitConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PayoutSplitConfig.PayoutSplitConfig] -> m ())
createMany = traverse_ create

findAllByAreasCityAndVariant ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Lib.Types.SpecialLocation.Area] -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.VehicleVariant.VehicleVariant -> m ([Domain.Types.PayoutSplitConfig.PayoutSplitConfig]))
findAllByAreasCityAndVariant area merchantOperatingCityId vehicleVariant = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.area $ Se.In area,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleVariant $ Se.Eq vehicleVariant
        ]
    ]

findAllDistinctPayoutVendorsByCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.PayoutSplitConfig.PayoutSplitConfig]))
findAllDistinctPayoutVendorsByCity merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Lib.Types.SpecialLocation.Area -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.VehicleVariant.VehicleVariant -> Data.Text.Text -> m (Maybe Domain.Types.PayoutSplitConfig.PayoutSplitConfig))
findByPrimaryKey area merchantOperatingCityId vehicleVariant vendorId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.area $ Se.Eq area,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleVariant $ Se.Eq vehicleVariant,
          Se.Is Beam.vendorId $ Se.Eq vendorId
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PayoutSplitConfig.PayoutSplitConfig -> m ())
updateByPrimaryKey (Domain.Types.PayoutSplitConfig.PayoutSplitConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.bankDetails bankDetails, Se.Set Beam.vendorSplitAmount vendorSplitAmount, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.area $ Se.Eq area,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleVariant $ Se.Eq vehicleVariant,
          Se.Is Beam.vendorId $ Se.Eq vendorId
        ]
    ]
