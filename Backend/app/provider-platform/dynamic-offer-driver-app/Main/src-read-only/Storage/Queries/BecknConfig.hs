{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.BecknConfig where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.BecknConfig
import qualified Storage.Beam.BecknConfig as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified BecknV2.OnDemand.Enums
import qualified Domain.Types.Merchant
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BecknConfig.BecknConfig -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BecknConfig.BecknConfig] -> m ())
createMany = traverse_ create
findAllByMerchantId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> m ([Domain.Types.BecknConfig.BecknConfig]))
findAllByMerchantId merchantId = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId)]]
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig -> m (Maybe Domain.Types.BecknConfig.BecknConfig))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
findByMerchantIdAndDomain :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                             (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Text -> m ([Domain.Types.BecknConfig.BecknConfig]))
findByMerchantIdAndDomain merchantId domain = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId), Se.Is Beam.domain $ Se.Eq domain]]
findByMerchantIdDomainAndVehicle :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                    (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> m (Maybe Domain.Types.BecknConfig.BecknConfig))
findByMerchantIdDomainAndVehicle merchantId domain vehicleCategory = do findOneWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
                                                                                               Se.Is Beam.domain $ Se.Eq domain,
                                                                                               Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory]]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig -> m (Maybe Domain.Types.BecknConfig.BecknConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BecknConfig.BecknConfig -> m ())
updateByPrimaryKey (Domain.Types.BecknConfig.BecknConfig {..}) = do {_now <- getCurrentTime;
                                                                     updateWithKV [Se.Set Beam.buyerFinderFee buyerFinderFee,
                                                                                   Se.Set Beam.collectedBy collectedBy,
                                                                                   Se.Set Beam.domain domain,
                                                                                   Se.Set Beam.gatewayUrl (Kernel.Prelude.showBaseUrl gatewayUrl),
                                                                                   Se.Set Beam.multimodalOnSearchTTLSec multimodalOnSearchTTLSec,
                                                                                   Se.Set Beam.onCancelTTLSec onCancelTTLSec,
                                                                                   Se.Set Beam.onConfirmTTLSec onConfirmTTLSec,
                                                                                   Se.Set Beam.onInitTTLSec onInitTTLSec,
                                                                                   Se.Set Beam.onSearchTTLSec onSearchTTLSec,
                                                                                   Se.Set Beam.onSelectTTLSec onSelectTTLSec,
                                                                                   Se.Set Beam.onStatusTTLSec onStatusTTLSec,
                                                                                   Se.Set Beam.onTrackTTLSec onTrackTTLSec,
                                                                                   Se.Set Beam.onUpdateTTLSec onUpdateTTLSec,
                                                                                   Se.Set Beam.paymentParamsJson paymentParamsJson,
                                                                                   Se.Set Beam.ppfEnabled ppfEnabled,
                                                                                   Se.Set Beam.registryUrl (Kernel.Prelude.showBaseUrl registryUrl),
                                                                                   Se.Set Beam.settlementType settlementType,
                                                                                   Se.Set Beam.settlementWindow settlementWindow,
                                                                                   Se.Set Beam.staticTermsUrl ((Kernel.Prelude.fmap showBaseUrl) staticTermsUrl),
                                                                                   Se.Set Beam.subscriberId subscriberId,
                                                                                   Se.Set Beam.subscriberUrl (Kernel.Prelude.showBaseUrl subscriberUrl),
                                                                                   Se.Set Beam.uniqueKeyId uniqueKeyId,
                                                                                   Se.Set Beam.vehicleCategory vehicleCategory,
                                                                                   Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                                   Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
                                                                                   Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



instance FromTType' Beam.BecknConfig Domain.Types.BecknConfig.BecknConfig
    where fromTType' (Beam.BecknConfigT {..}) = do {gatewayUrl' <- Kernel.Prelude.parseBaseUrl gatewayUrl;
                                                    registryUrl' <- Kernel.Prelude.parseBaseUrl registryUrl;
                                                    staticTermsUrl' <- ((Kernel.Prelude.maybe (return Kernel.Prelude.Nothing) (Kernel.Prelude.fmap Kernel.Prelude.Just . parseBaseUrl))) staticTermsUrl;
                                                    subscriberUrl' <- Kernel.Prelude.parseBaseUrl subscriberUrl;
                                                    pure $ Just Domain.Types.BecknConfig.BecknConfig{buyerFinderFee = buyerFinderFee,
                                                                                                     collectedBy = collectedBy,
                                                                                                     domain = domain,
                                                                                                     gatewayUrl = gatewayUrl',
                                                                                                     id = Kernel.Types.Id.Id id,
                                                                                                     multimodalOnSearchTTLSec = multimodalOnSearchTTLSec,
                                                                                                     onCancelTTLSec = onCancelTTLSec,
                                                                                                     onConfirmTTLSec = onConfirmTTLSec,
                                                                                                     onInitTTLSec = onInitTTLSec,
                                                                                                     onSearchTTLSec = onSearchTTLSec,
                                                                                                     onSelectTTLSec = onSelectTTLSec,
                                                                                                     onStatusTTLSec = onStatusTTLSec,
                                                                                                     onTrackTTLSec = onTrackTTLSec,
                                                                                                     onUpdateTTLSec = onUpdateTTLSec,
                                                                                                     paymentParamsJson = paymentParamsJson,
                                                                                                     ppfEnabled = ppfEnabled,
                                                                                                     registryUrl = registryUrl',
                                                                                                     settlementType = settlementType,
                                                                                                     settlementWindow = settlementWindow,
                                                                                                     staticTermsUrl = staticTermsUrl',
                                                                                                     subscriberId = subscriberId,
                                                                                                     subscriberUrl = subscriberUrl',
                                                                                                     uniqueKeyId = uniqueKeyId,
                                                                                                     vehicleCategory = vehicleCategory,
                                                                                                     merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                                     merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
                                                                                                     createdAt = createdAt,
                                                                                                     updatedAt = updatedAt}}
instance ToTType' Beam.BecknConfig Domain.Types.BecknConfig.BecknConfig
    where toTType' (Domain.Types.BecknConfig.BecknConfig {..}) = do Beam.BecknConfigT{Beam.buyerFinderFee = buyerFinderFee,
                                                                                      Beam.collectedBy = collectedBy,
                                                                                      Beam.domain = domain,
                                                                                      Beam.gatewayUrl = Kernel.Prelude.showBaseUrl gatewayUrl,
                                                                                      Beam.id = Kernel.Types.Id.getId id,
                                                                                      Beam.multimodalOnSearchTTLSec = multimodalOnSearchTTLSec,
                                                                                      Beam.onCancelTTLSec = onCancelTTLSec,
                                                                                      Beam.onConfirmTTLSec = onConfirmTTLSec,
                                                                                      Beam.onInitTTLSec = onInitTTLSec,
                                                                                      Beam.onSearchTTLSec = onSearchTTLSec,
                                                                                      Beam.onSelectTTLSec = onSelectTTLSec,
                                                                                      Beam.onStatusTTLSec = onStatusTTLSec,
                                                                                      Beam.onTrackTTLSec = onTrackTTLSec,
                                                                                      Beam.onUpdateTTLSec = onUpdateTTLSec,
                                                                                      Beam.paymentParamsJson = paymentParamsJson,
                                                                                      Beam.ppfEnabled = ppfEnabled,
                                                                                      Beam.registryUrl = Kernel.Prelude.showBaseUrl registryUrl,
                                                                                      Beam.settlementType = settlementType,
                                                                                      Beam.settlementWindow = settlementWindow,
                                                                                      Beam.staticTermsUrl = (Kernel.Prelude.fmap showBaseUrl) staticTermsUrl,
                                                                                      Beam.subscriberId = subscriberId,
                                                                                      Beam.subscriberUrl = Kernel.Prelude.showBaseUrl subscriberUrl,
                                                                                      Beam.uniqueKeyId = uniqueKeyId,
                                                                                      Beam.vehicleCategory = vehicleCategory,
                                                                                      Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                      Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
                                                                                      Beam.createdAt = createdAt,
                                                                                      Beam.updatedAt = updatedAt}



