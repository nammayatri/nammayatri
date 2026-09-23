{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Merchant (module Storage.Queries.Merchant, module ReExport) where

import qualified Domain.Types
import qualified Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.Registry
import qualified Kernel.Types.Version
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant as Beam
import Storage.Queries.MerchantExtra as ReExport
import Storage.Queries.Transformers.Merchant

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Merchant.Merchant -> m ())
create = createWithKV

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.Merchant.Merchant))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByShortId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.Merchant.Merchant))
findByShortId shortId = do findOneWithKV [Se.Is Beam.shortId $ Se.Eq (Kernel.Types.Id.getShortId shortId)]

findBySubscriberId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.ShortId Kernel.Types.Registry.Subscriber -> m (Maybe Domain.Types.Merchant.Merchant))
findBySubscriberId subscriberId = do findOneWithKV [Se.Is Beam.subscriberId $ Se.Eq (Kernel.Types.Id.getShortId subscriberId)]

updateCloudConfig ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Version.CloudType -> Kernel.Prelude.Maybe Kernel.Types.Common.BaseUrl -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m ())
updateCloudConfig cloudType cloudBaseUrl id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.cloudType (Kernel.Prelude.fmap Kernel.Prelude.show cloudType),
      Se.Set Beam.cloudBaseUrl (Kernel.Prelude.fmap Kernel.Prelude.showBaseUrl cloudBaseUrl),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateGatewayAndRegistryPriorityList :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.GatewayAndRegistryService] -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m ())
updateGatewayAndRegistryPriorityList gatewayAndRegistryPriorityList id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.gatewayAndRegistryPriorityList (Kernel.Prelude.Just gatewayAndRegistryPriorityList), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
