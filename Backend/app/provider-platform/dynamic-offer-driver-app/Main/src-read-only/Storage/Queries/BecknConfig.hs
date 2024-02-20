{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BecknConfig where

import qualified Domain.Types.BecknConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Servant.Client.Core
import qualified Storage.Beam.BecknConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.BecknConfig.BecknConfig -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.BecknConfig.BecknConfig] -> m ()
createMany = traverse_ create

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig -> m (Maybe (Domain.Types.BecknConfig.BecknConfig))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByMerchantIdAndDomain :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Text -> m ([Domain.Types.BecknConfig.BecknConfig])
findByMerchantIdAndDomain merchantId domain = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.domain $ Se.Eq domain
        ]
    ]

findByMerchantIdDomainAndVehicle :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Text -> Domain.Types.BecknConfig.VehicleCategory -> m (Maybe (Domain.Types.BecknConfig.BecknConfig))
findByMerchantIdDomainAndVehicle merchantId domain vehicleCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig -> m (Maybe (Domain.Types.BecknConfig.BecknConfig))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.BecknConfig.BecknConfig -> m ()
updateByPrimaryKey Domain.Types.BecknConfig.BecknConfig {..} = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.cancellationFeeAmount cancellationFeeAmount,
      Se.Set Beam.cancellationFeePercentage cancellationFeePercentage,
      Se.Set Beam.collectedBy collectedBy,
      Se.Set Beam.domain domain,
      Se.Set Beam.gatewayUrl $ Kernel.Prelude.showBaseUrl gatewayUrl,
      Se.Set Beam.paymentParamsJson paymentParamsJson,
      Se.Set Beam.registryUrl $ Kernel.Prelude.showBaseUrl registryUrl,
      Se.Set Beam.settlementType settlementType,
      Se.Set Beam.staticTermsUrl $ (Kernel.Prelude.fmap showBaseUrl) staticTermsUrl,
      Se.Set Beam.subscriberId subscriberId,
      Se.Set Beam.subscriberUrl $ Kernel.Prelude.showBaseUrl subscriberUrl,
      Se.Set Beam.uniqueKeyId uniqueKeyId,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.BecknConfig Domain.Types.BecknConfig.BecknConfig where
  fromTType' Beam.BecknConfigT {..} = do
    gatewayUrl' <- Kernel.Prelude.parseBaseUrl gatewayUrl
    registryUrl' <- Kernel.Prelude.parseBaseUrl registryUrl
    staticTermsUrl' <- (Kernel.Prelude.maybe (return Kernel.Prelude.Nothing) (Kernel.Prelude.fmap Kernel.Prelude.Just . parseBaseUrl)) staticTermsUrl
    subscriberUrl' <- Kernel.Prelude.parseBaseUrl subscriberUrl

    pure $
      Just
        Domain.Types.BecknConfig.BecknConfig
          { cancellationFeeAmount = cancellationFeeAmount,
            cancellationFeePercentage = cancellationFeePercentage,
            collectedBy = collectedBy,
            domain = domain,
            gatewayUrl = gatewayUrl',
            id = Kernel.Types.Id.Id id,
            paymentParamsJson = paymentParamsJson,
            registryUrl = registryUrl',
            settlementType = settlementType,
            staticTermsUrl = staticTermsUrl',
            subscriberId = subscriberId,
            subscriberUrl = subscriberUrl',
            uniqueKeyId = uniqueKeyId,
            vehicleCategory = vehicleCategory,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.BecknConfig Domain.Types.BecknConfig.BecknConfig where
  toTType' Domain.Types.BecknConfig.BecknConfig {..} = do
    Beam.BecknConfigT
      { Beam.cancellationFeeAmount = cancellationFeeAmount,
        Beam.cancellationFeePercentage = cancellationFeePercentage,
        Beam.collectedBy = collectedBy,
        Beam.domain = domain,
        Beam.gatewayUrl = Kernel.Prelude.showBaseUrl gatewayUrl,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.paymentParamsJson = paymentParamsJson,
        Beam.registryUrl = Kernel.Prelude.showBaseUrl registryUrl,
        Beam.settlementType = settlementType,
        Beam.staticTermsUrl = (Kernel.Prelude.fmap showBaseUrl) staticTermsUrl,
        Beam.subscriberId = subscriberId,
        Beam.subscriberUrl = Kernel.Prelude.showBaseUrl subscriberUrl,
        Beam.uniqueKeyId = uniqueKeyId,
        Beam.vehicleCategory = vehicleCategory,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
