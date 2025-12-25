{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SubscriptionTransaction (module Storage.Queries.SubscriptionTransaction, module ReExport) where

import qualified Domain.Types.SubscriptionTransaction
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SubscriptionTransaction as Beam
import Storage.Queries.SubscriptionTransactionExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SubscriptionTransaction.SubscriptionTransaction -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SubscriptionTransaction.SubscriptionTransaction] -> m ())
createMany = traverse_ create

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.SubscriptionTransaction.SubscriptionTransaction -> m (Maybe Domain.Types.SubscriptionTransaction.SubscriptionTransaction))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SubscriptionTransaction.SubscriptionTransaction -> m ())
updateByPrimaryKey (Domain.Types.SubscriptionTransaction.SubscriptionTransaction {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.entityId entityId,
      Se.Set Beam.fleetOwnerId (Kernel.Types.Id.getId <$> fleetOwnerId),
      Se.Set Beam.fromLocationId (Kernel.Types.Id.getId <$> fromLocationId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.runningBalance runningBalance,
      Se.Set Beam.status status,
      Se.Set Beam.toLocationId (Kernel.Types.Id.getId <$> toLocationId),
      Se.Set Beam.transactionType transactionType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
