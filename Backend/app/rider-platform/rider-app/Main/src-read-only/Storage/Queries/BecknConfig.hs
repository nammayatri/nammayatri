{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BecknConfig where

import qualified Domain.Types.BecknConfig
import qualified Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BecknConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BecknConfig.BecknConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BecknConfig.BecknConfig] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig -> m (Maybe Domain.Types.BecknConfig.BecknConfig))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByMerchantIdAndDomain ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Text -> m [Domain.Types.BecknConfig.BecknConfig])
findByMerchantIdAndDomain merchantId domain = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId), Se.Is Beam.domain $ Se.Eq domain]]

findByMerchantIdDomainAndVehicle ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Text -> Domain.Types.BecknConfig.VehicleCategory -> m (Maybe Domain.Types.BecknConfig.BecknConfig))
findByMerchantIdDomainAndVehicle merchantId domain vehicleCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig -> m (Maybe Domain.Types.BecknConfig.BecknConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BecknConfig.BecknConfig -> m ())
updateByPrimaryKey (Domain.Types.BecknConfig.BecknConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.domain domain,
      Se.Set Beam.subscriberId subscriberId,
      Se.Set Beam.subscriberUrl (Kernel.Prelude.showBaseUrl subscriberUrl),
      Se.Set Beam.gatewayUrl (Kernel.Prelude.showBaseUrl gatewayUrl),
      Se.Set Beam.registryUrl (Kernel.Prelude.showBaseUrl registryUrl),
      Se.Set Beam.uniqueKeyId uniqueKeyId,
      Se.Set Beam.settlementType settlementType,
      Se.Set Beam.paymentParamsJson paymentParamsJson,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.collectedBy collectedBy,
      Se.Set Beam.staticTermsUrl (Kernel.Prelude.fmap showBaseUrl staticTermsUrl),
      Se.Set Beam.buyerFinderFee buyerFinderFee,
      Se.Set Beam.settlementWindow settlementWindow,
      Se.Set Beam.searchTTLSec searchTTLSec,
      Se.Set Beam.selectTTLSec selectTTLSec,
      Se.Set Beam.initTTLSec initTTLSec,
      Se.Set Beam.confirmTTLSec confirmTTLSec,
      Se.Set Beam.confirmBufferTTLSec confirmBufferTTLSec,
      Se.Set Beam.bapIFSC bapIFSC,
      Se.Set Beam.statusTTLSec statusTTLSec,
      Se.Set Beam.trackTTLSec trackTTLSec,
      Se.Set Beam.ratingTTLSec ratingTTLSec,
      Se.Set Beam.cancelTTLSec cancelTTLSec,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.BecknConfig Domain.Types.BecknConfig.BecknConfig where
  fromTType' (Beam.BecknConfigT {..}) = do
    subscriberUrl' <- Kernel.Prelude.parseBaseUrl subscriberUrl
    gatewayUrl' <- Kernel.Prelude.parseBaseUrl gatewayUrl
    registryUrl' <- Kernel.Prelude.parseBaseUrl registryUrl
    staticTermsUrl' <- Kernel.Prelude.maybe (return Kernel.Prelude.Nothing) (Kernel.Prelude.fmap Kernel.Prelude.Just . parseBaseUrl) staticTermsUrl
    pure $
      Just
        Domain.Types.BecknConfig.BecknConfig
          { id = Kernel.Types.Id.Id id,
            domain = domain,
            subscriberId = subscriberId,
            subscriberUrl = subscriberUrl',
            gatewayUrl = gatewayUrl',
            registryUrl = registryUrl',
            uniqueKeyId = uniqueKeyId,
            settlementType = settlementType,
            paymentParamsJson = paymentParamsJson,
            vehicleCategory = vehicleCategory,
            collectedBy = collectedBy,
            staticTermsUrl = staticTermsUrl',
            buyerFinderFee = buyerFinderFee,
            settlementWindow = settlementWindow,
            searchTTLSec = searchTTLSec,
            selectTTLSec = selectTTLSec,
            initTTLSec = initTTLSec,
            confirmTTLSec = confirmTTLSec,
            confirmBufferTTLSec = confirmBufferTTLSec,
            bapIFSC = bapIFSC,
            statusTTLSec = statusTTLSec,
            trackTTLSec = trackTTLSec,
            ratingTTLSec = ratingTTLSec,
            cancelTTLSec = cancelTTLSec,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.BecknConfig Domain.Types.BecknConfig.BecknConfig where
  toTType' (Domain.Types.BecknConfig.BecknConfig {..}) = do
    Beam.BecknConfigT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.domain = domain,
        Beam.subscriberId = subscriberId,
        Beam.subscriberUrl = Kernel.Prelude.showBaseUrl subscriberUrl,
        Beam.gatewayUrl = Kernel.Prelude.showBaseUrl gatewayUrl,
        Beam.registryUrl = Kernel.Prelude.showBaseUrl registryUrl,
        Beam.uniqueKeyId = uniqueKeyId,
        Beam.settlementType = settlementType,
        Beam.paymentParamsJson = paymentParamsJson,
        Beam.vehicleCategory = vehicleCategory,
        Beam.collectedBy = collectedBy,
        Beam.staticTermsUrl = Kernel.Prelude.fmap showBaseUrl staticTermsUrl,
        Beam.buyerFinderFee = buyerFinderFee,
        Beam.settlementWindow = settlementWindow,
        Beam.searchTTLSec = searchTTLSec,
        Beam.selectTTLSec = selectTTLSec,
        Beam.initTTLSec = initTTLSec,
        Beam.confirmTTLSec = confirmTTLSec,
        Beam.confirmBufferTTLSec = confirmBufferTTLSec,
        Beam.bapIFSC = bapIFSC,
        Beam.statusTTLSec = statusTTLSec,
        Beam.trackTTLSec = trackTTLSec,
        Beam.ratingTTLSec = ratingTTLSec,
        Beam.cancelTTLSec = cancelTTLSec,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
