{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Discount (module Storage.Queries.Discount, module ReExport) where

import qualified Domain.Types.Discount
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Discount as Beam
import Storage.Queries.DiscountExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Discount.Discount -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Discount.Discount] -> m ())
createMany = traverse_ create

findAllEnabledByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Bool -> m [Domain.Types.Discount.Discount])
findAllEnabledByMerchantOpCityId merchantOperatingCityId enabled = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.enabled $ Se.Eq enabled
        ]
    ]

findByMerchantOpCityIdAndType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> Kernel.Prelude.Bool -> m [Domain.Types.Discount.Discount])
findByMerchantOpCityIdAndType merchantOperatingCityId discountType enabled = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.discountType $ Se.Eq discountType,
          Se.Is Beam.enabled $ Se.Eq enabled
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Discount.Discount -> m (Maybe Domain.Types.Discount.Discount))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Discount.Discount -> m ())
updateByPrimaryKey (Domain.Types.Discount.Discount {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.config config,
      Se.Set Beam.discountType discountType,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.paymentMode paymentMode,
      Se.Set Beam.planId (Kernel.Types.Id.getId <$> planId),
      Se.Set Beam.validFrom validFrom,
      Se.Set Beam.validTo validTo,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
