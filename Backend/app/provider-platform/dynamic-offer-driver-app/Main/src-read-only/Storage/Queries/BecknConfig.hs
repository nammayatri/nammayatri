{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BecknConfig (module Storage.Queries.BecknConfig, module ReExport) where

import qualified Domain.Types
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
import Storage.Queries.BecknConfigExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BecknConfig.BecknConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BecknConfig.BecknConfig] -> m ())
createMany = traverse_ create

findAllByMerchantId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> m ([Domain.Types.BecknConfig.BecknConfig]))
findAllByMerchantId merchantId = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId)]]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig -> m (Maybe Domain.Types.BecknConfig.BecknConfig))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByMerchantIdAndDomain ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Text -> m ([Domain.Types.BecknConfig.BecknConfig]))
findByMerchantIdAndDomain merchantId domain = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId), Se.Is Beam.domain $ Se.Eq domain]]

findByMerchantIdDomainAndBecknProtocol ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.BecknProtocol -> m (Maybe Domain.Types.BecknConfig.BecknConfig))
findByMerchantIdDomainAndBecknProtocol merchantId domain becknProtocol = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.becknProtocol $ Se.Eq becknProtocol
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig -> m (Maybe Domain.Types.BecknConfig.BecknConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BecknConfig.BecknConfig -> m ())
updateByPrimaryKey (Domain.Types.BecknConfig.BecknConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.becknProtocol becknProtocol,
      Se.Set Beam.buyerFinderFee buyerFinderFee,
      Se.Set Beam.collectedBy collectedBy,
      Se.Set Beam.domain domain,
      Se.Set Beam.gatewayUrl (Kernel.Prelude.showBaseUrl gatewayUrl),
      Se.Set Beam.multimodalOnSearchTTLSec multimodalOnSearchTTLSec,
      Se.Set Beam.networkId networkId,
      Se.Set Beam.offlineContract offlineContract,
      Se.Set Beam.onCancelTTLSec onCancelTTLSec,
      Se.Set Beam.onConfirmTTLSec onConfirmTTLSec,
      Se.Set Beam.onInitTTLSec onInitTTLSec,
      Se.Set Beam.onSearchTTLSec onSearchTTLSec,
      Se.Set Beam.onSelectTTLSec onSelectTTLSec,
      Se.Set Beam.onStatusTTLSec onStatusTTLSec,
      Se.Set Beam.onSupportTTLSec onSupportTTLSec,
      Se.Set Beam.onTrackTTLSec onTrackTTLSec,
      Se.Set Beam.onUpdateTTLSec onUpdateTTLSec,
      Se.Set Beam.paymentParamsJson paymentParamsJson,
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
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
