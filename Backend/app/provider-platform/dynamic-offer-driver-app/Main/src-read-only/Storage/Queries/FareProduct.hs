{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareProduct (module Storage.Queries.FareProduct, module ReExport) where

import qualified Domain.Types.Common
import qualified Domain.Types.FarePolicy
import qualified Domain.Types.FareProduct
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Types.SpecialLocation
import qualified Sequelize as Se
import qualified Storage.Beam.FareProduct as Beam
import Storage.Queries.FareProductExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareProduct.FareProduct -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FareProduct.FareProduct] -> m ())
createMany = traverse_ create

findAllFareProductByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FarePolicy.FarePolicy -> m [Domain.Types.FareProduct.FareProduct])
findAllFareProductByFarePolicyId farePolicyId = do findAllWithKV [Se.And [Se.Is Beam.farePolicyId $ Se.Eq (Kernel.Types.Id.getId farePolicyId)]]

findAllFareProductByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Bool -> m [Domain.Types.FareProduct.FareProduct])
findAllFareProductByMerchantOpCityId merchantOperatingCityId enabled = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.enabled $ Se.Eq enabled
        ]
    ]

findAllUnboundedByMerchantOpCityIdVariantArea ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Lib.Types.SpecialLocation.Area -> Domain.Types.Common.TripCategory -> Domain.Types.Common.ServiceTierType -> Kernel.Types.TimeBound.TimeBound -> Kernel.Prelude.Bool -> [Domain.Types.FareProduct.SearchSource] -> m [Domain.Types.FareProduct.FareProduct])
findAllUnboundedByMerchantOpCityIdVariantArea merchantOperatingCityId area tripCategory vehicleServiceTier timeBounds enabled searchSource = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.area $ Se.Eq area,
          Se.Is Beam.tripCategory $ Se.Eq tripCategory,
          Se.Is Beam.vehicleVariant $ Se.Eq vehicleServiceTier,
          Se.Is Beam.timeBounds $ Se.Eq timeBounds,
          Se.Is Beam.enabled $ Se.Eq enabled,
          Se.Is Beam.searchSource $ Se.In searchSource
        ]
    ]

findAllUnboundedFareProductForArea ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Lib.Types.SpecialLocation.Area -> Kernel.Types.TimeBound.TimeBound -> Kernel.Prelude.Bool -> [Domain.Types.FareProduct.SearchSource] -> m [Domain.Types.FareProduct.FareProduct])
findAllUnboundedFareProductForArea merchantOperatingCityId area timeBounds enabled searchSource = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.area $ Se.Eq area,
          Se.Is Beam.timeBounds $ Se.Eq timeBounds,
          Se.Is Beam.enabled $ Se.Eq enabled,
          Se.Is Beam.searchSource $ Se.In searchSource
        ]
    ]

findUnboundedByMerchantOpCityIdVariantArea ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Lib.Types.SpecialLocation.Area -> Domain.Types.Common.TripCategory -> Domain.Types.Common.ServiceTierType -> Kernel.Types.TimeBound.TimeBound -> Kernel.Prelude.Bool -> [Domain.Types.FareProduct.SearchSource] -> m (Maybe Domain.Types.FareProduct.FareProduct))
findUnboundedByMerchantOpCityIdVariantArea merchantOperatingCityId area tripCategory vehicleServiceTier timeBounds enabled searchSource = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.area $ Se.Eq area,
          Se.Is Beam.tripCategory $ Se.Eq tripCategory,
          Se.Is Beam.vehicleVariant $ Se.Eq vehicleServiceTier,
          Se.Is Beam.timeBounds $ Se.Eq timeBounds,
          Se.Is Beam.enabled $ Se.Eq enabled,
          Se.Is Beam.searchSource $ Se.In searchSource
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FareProduct.FareProduct -> m (Maybe Domain.Types.FareProduct.FareProduct))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareProduct.FareProduct -> m ())
updateByPrimaryKey (Domain.Types.FareProduct.FareProduct {..}) = do
  updateWithKV
    [ Se.Set Beam.area area,
      Se.Set Beam.disableRecompute disableRecompute,
      Se.Set Beam.disableTollRecompute disableTollRecompute,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.farePolicyId (Kernel.Types.Id.getId farePolicyId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.searchSource searchSource,
      Se.Set Beam.timeBounds timeBounds,
      Se.Set Beam.tripCategory tripCategory,
      Se.Set Beam.vehicleVariant vehicleServiceTier
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
