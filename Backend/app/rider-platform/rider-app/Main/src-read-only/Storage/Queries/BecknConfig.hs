{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.BecknConfig (module Storage.Queries.BecknConfig, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.BecknConfigExtra as ReExport
import qualified Domain.Types.BecknConfig
import qualified Storage.Beam.BecknConfig as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BecknConfig.BecknConfig -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BecknConfig.BecknConfig] -> m ())
createMany = traverse_ create
findAllByMerchantOperatingCityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                    (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> m ([Domain.Types.BecknConfig.BecknConfig]))
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)]
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig -> m (Maybe Domain.Types.BecknConfig.BecknConfig))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
findByMerchantIdAndDomain :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                             (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Text -> m ([Domain.Types.BecknConfig.BecknConfig]))
findByMerchantIdAndDomain merchantId domain = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId), Se.Is Beam.domain $ Se.Eq domain]]
findByMerchantIdDomainandMerchantOperatingCityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                                    (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> m ([Domain.Types.BecknConfig.BecknConfig]))
findByMerchantIdDomainandMerchantOperatingCityId merchantId domain merchantOperatingCityId = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
                                                                                                                       Se.Is Beam.domain $ Se.Eq domain,
                                                                                                                       Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)]]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig -> m (Maybe Domain.Types.BecknConfig.BecknConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BecknConfig.BecknConfig -> m ())
updateByPrimaryKey (Domain.Types.BecknConfig.BecknConfig {..}) = do {_now <- getCurrentTime;
                                                                     updateWithKV [Se.Set Beam.bapIFSC bapIFSC,
                                                                                   Se.Set Beam.buyerFinderFee buyerFinderFee,
                                                                                   Se.Set Beam.cancelTTLSec cancelTTLSec,
                                                                                   Se.Set Beam.collectedBy collectedBy,
                                                                                   Se.Set Beam.confirmBufferTTLSec confirmBufferTTLSec,
                                                                                   Se.Set Beam.confirmTTLSec confirmTTLSec,
                                                                                   Se.Set Beam.domain domain,
                                                                                   Se.Set Beam.gatewayUrl (Kernel.Prelude.showBaseUrl gatewayUrl),
                                                                                   Se.Set Beam.initTTLSec initTTLSec,
                                                                                   Se.Set Beam.paymentParamsJson paymentParamsJson,
                                                                                   Se.Set Beam.ppfEnabled ppfEnabled,
                                                                                   Se.Set Beam.ratingTTLSec ratingTTLSec,
                                                                                   Se.Set Beam.registryUrl (Kernel.Prelude.showBaseUrl registryUrl),
                                                                                   Se.Set Beam.searchTTLSec searchTTLSec,
                                                                                   Se.Set Beam.selectTTLSec selectTTLSec,
                                                                                   Se.Set Beam.settlementType settlementType,
                                                                                   Se.Set Beam.settlementWindow settlementWindow,
                                                                                   Se.Set Beam.staticTermsUrl ((Kernel.Prelude.fmap showBaseUrl) staticTermsUrl),
                                                                                   Se.Set Beam.statusTTLSec statusTTLSec,
                                                                                   Se.Set Beam.subscriberId subscriberId,
                                                                                   Se.Set Beam.subscriberUrl (Kernel.Prelude.showBaseUrl subscriberUrl),
                                                                                   Se.Set Beam.trackTTLSec trackTTLSec,
                                                                                   Se.Set Beam.uniqueKeyId uniqueKeyId,
                                                                                   Se.Set Beam.vehicleCategory vehicleCategory,
                                                                                   Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                                   Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
                                                                                   Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



