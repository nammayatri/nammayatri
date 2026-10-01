{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BapMetadata where

import qualified Data.Text
import qualified Domain.Types.BapMetadata
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BapMetadata as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BapMetadata.BapMetadata -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BapMetadata.BapMetadata] -> m ())
createMany = traverse_ create

findBySubscriberIdDomainMerchantAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.BapMetadata.BapMetadata -> Data.Text.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> m (Maybe Domain.Types.BapMetadata.BapMetadata))
findBySubscriberIdDomainMerchantAndCity id domain merchantId merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id),
          Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Data.Text.Text -> Kernel.Types.Id.Id Domain.Types.BapMetadata.BapMetadata -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> m (Maybe Domain.Types.BapMetadata.BapMetadata))
findByPrimaryKey domain id merchantId merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id),
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BapMetadata.BapMetadata -> m ())
updateByPrimaryKey (Domain.Types.BapMetadata.BapMetadata {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.enableOndcScheduledRideSupport enableOndcScheduledRideSupport,
      Se.Set Beam.logoUrl (Kernel.Prelude.fmap showBaseUrl logoUrl),
      Se.Set Beam.name name,
      Se.Set Beam.offlineContract offlineContract,
      Se.Set Beam.staticTermsUrl (Kernel.Prelude.fmap showBaseUrl staticTermsUrl),
      Se.Set Beam.supportEmail supportEmail,
      Se.Set Beam.supportPhone supportPhone,
      Se.Set Beam.supportUrl (Kernel.Prelude.fmap showBaseUrl supportUrl),
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id),
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)
        ]
    ]

instance FromTType' Beam.BapMetadata Domain.Types.BapMetadata.BapMetadata where
  fromTType' (Beam.BapMetadataT {..}) = do
    logoUrl' <- Kernel.Prelude.maybe (return Kernel.Prelude.Nothing) (Kernel.Prelude.fmap Kernel.Prelude.Just . parseBaseUrl) logoUrl
    staticTermsUrl' <- Kernel.Prelude.maybe (return Kernel.Prelude.Nothing) (Kernel.Prelude.fmap Kernel.Prelude.Just . parseBaseUrl) staticTermsUrl
    supportUrl' <- Kernel.Prelude.maybe (return Kernel.Prelude.Nothing) (Kernel.Prelude.fmap Kernel.Prelude.Just . parseBaseUrl) supportUrl
    pure $
      Just
        Domain.Types.BapMetadata.BapMetadata
          { domain = domain,
            enableOndcScheduledRideSupport = enableOndcScheduledRideSupport,
            id = Kernel.Types.Id.Id id,
            logoUrl = logoUrl',
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            name = name,
            offlineContract = offlineContract,
            staticTermsUrl = staticTermsUrl',
            supportEmail = supportEmail,
            supportPhone = supportPhone,
            supportUrl = supportUrl',
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.BapMetadata Domain.Types.BapMetadata.BapMetadata where
  toTType' (Domain.Types.BapMetadata.BapMetadata {..}) = do
    Beam.BapMetadataT
      { Beam.domain = domain,
        Beam.enableOndcScheduledRideSupport = enableOndcScheduledRideSupport,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.logoUrl = Kernel.Prelude.fmap showBaseUrl logoUrl,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.name = name,
        Beam.offlineContract = offlineContract,
        Beam.staticTermsUrl = Kernel.Prelude.fmap showBaseUrl staticTermsUrl,
        Beam.supportEmail = supportEmail,
        Beam.supportPhone = supportPhone,
        Beam.supportUrl = Kernel.Prelude.fmap showBaseUrl supportUrl,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
