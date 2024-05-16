{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareProduct (module Storage.Queries.FareProduct, module ReExport) where

import qualified Domain.Action.UI.FareProduct
import qualified Domain.Types.Common
import qualified Domain.Types.FareProduct
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.ServiceTierType
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Types.SpecialLocation
import qualified Sequelize as Se
import qualified Storage.Beam.FareProduct as Beam
import Storage.Queries.FareProductExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.FareProduct.FareProduct -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.FareProduct.FareProduct] -> m ())
createMany = traverse_ create

findAllBoundedByMerchantOpCityIdVariantArea ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> Lib.Types.SpecialLocation.Area -> Domain.Types.ServiceTierType.ServiceTierType -> Domain.Types.Common.TripCategory -> Domain.Action.UI.FareProduct.TimeBound -> Kernel.Prelude.Bool -> m [Domain.Types.FareProduct.FareProduct])
findAllBoundedByMerchantOpCityIdVariantArea (Kernel.Types.Id.Id merchantOperatingCityId) area vehicleServiceTier tripCategory timeBounds enabled = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.area $ Se.Eq area,
          Se.Is Beam.vehicleVariant $ Se.Eq vehicleServiceTier,
          Se.Is Beam.tripCategory $ Se.Eq tripCategory,
          Se.Is Beam.timeBounds $ Se.Eq timeBounds,
          Se.Is Beam.enabled $ Se.Eq enabled
        ]
    ]

findAllBoundedFareProductForVariants ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> Lib.Types.SpecialLocation.Area -> Domain.Types.Common.TripCategory -> Domain.Action.UI.FareProduct.TimeBound -> Kernel.Prelude.Bool -> m [Domain.Types.FareProduct.FareProduct])
findAllBoundedFareProductForVariants (Kernel.Types.Id.Id merchantOperatingCityId) area tripCategory timeBounds enabled = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.area $ Se.Eq area,
          Se.Is Beam.tripCategory $ Se.Eq tripCategory,
          Se.Is Beam.timeBounds $ Se.Eq timeBounds,
          Se.Is Beam.enabled $ Se.Eq enabled
        ]
    ]

findAllFareProductByMerchantOpCityId ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Bool -> m [Domain.Types.FareProduct.FareProduct])
findAllFareProductByMerchantOpCityId (Kernel.Types.Id.Id merchantOperatingCityId) enabled = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.enabled $ Se.Eq enabled
        ]
    ]

findAllUnboundedFareProductForVariants ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> Lib.Types.SpecialLocation.Area -> Domain.Types.Common.TripCategory -> Domain.Action.UI.FareProduct.TimeBound -> Kernel.Prelude.Bool -> m [Domain.Types.FareProduct.FareProduct])
findAllUnboundedFareProductForVariants (Kernel.Types.Id.Id merchantOperatingCityId) area tripCategory timeBounds enabled = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.area $ Se.Eq area,
          Se.Is Beam.tripCategory $ Se.Eq tripCategory,
          Se.Is Beam.timeBounds $ Se.Eq timeBounds,
          Se.Is Beam.enabled $ Se.Eq enabled
        ]
    ]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.FareProduct.FareProduct -> m (Maybe Domain.Types.FareProduct.FareProduct))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.FareProduct.FareProduct -> m ())
updateByPrimaryKey (Domain.Types.FareProduct.FareProduct {..}) = do
  updateWithKV
    [ Se.Set Beam.area area,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.farePolicyId (Kernel.Types.Id.getId farePolicyId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.timeBounds timeBounds,
      Se.Set Beam.tripCategory tripCategory,
      Se.Set Beam.vehicleVariant vehicleServiceTier
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
