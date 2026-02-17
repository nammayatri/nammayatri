{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DomainDiscountConfig where

import qualified Domain.Types.Common
import qualified Domain.Types.DomainDiscountConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified SharedLogic.Type
import qualified Storage.Beam.DomainDiscountConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DomainDiscountConfig.DomainDiscountConfig -> m ())
create = createWithKV

findAllByMerchantOpCityIdAndBillingCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> SharedLogic.Type.BillingCategory -> m ([Domain.Types.DomainDiscountConfig.DomainDiscountConfig]))
findAllByMerchantOpCityIdAndBillingCategory merchantOperatingCityId billingCategory = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.billingCategory $ Se.Eq billingCategory
        ]
    ]

findByMerchantOpCityIdAndDomainAndBillingCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> SharedLogic.Type.BillingCategory -> Domain.Types.Common.ServiceTierType -> m (Maybe Domain.Types.DomainDiscountConfig.DomainDiscountConfig))
findByMerchantOpCityIdAndDomainAndBillingCategory merchantOperatingCityId domain billingCategory vehicleServiceTier = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.billingCategory $ Se.Eq billingCategory,
          Se.Is Beam.vehicleServiceTier $ Se.Eq vehicleServiceTier
        ]
    ]

instance FromTType' Beam.DomainDiscountConfig Domain.Types.DomainDiscountConfig.DomainDiscountConfig where
  fromTType' (Beam.DomainDiscountConfigT {..}) = do
    pure $
      Just
        Domain.Types.DomainDiscountConfig.DomainDiscountConfig
          { billingCategory = billingCategory,
            createdAt = createdAt,
            discountPercentage = discountPercentage,
            domain = domain,
            enabled = enabled,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            updatedAt = updatedAt,
            vehicleServiceTier = vehicleServiceTier,
            merchantId = Kernel.Types.Id.Id <$> merchantId
          }

instance ToTType' Beam.DomainDiscountConfig Domain.Types.DomainDiscountConfig.DomainDiscountConfig where
  toTType' (Domain.Types.DomainDiscountConfig.DomainDiscountConfig {..}) = do
    Beam.DomainDiscountConfigT
      { Beam.billingCategory = billingCategory,
        Beam.createdAt = createdAt,
        Beam.discountPercentage = discountPercentage,
        Beam.domain = domain,
        Beam.enabled = enabled,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.updatedAt = updatedAt,
        Beam.vehicleServiceTier = vehicleServiceTier,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId
      }
