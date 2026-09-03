{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CustomerBlockTransactions where

import qualified Domain.Types.CustomerBlockTransactions
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CustomerBlockTransactions as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CustomerBlockTransactions.CustomerBlockTransactions -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CustomerBlockTransactions.CustomerBlockTransactions] -> m ())
createMany = traverse_ create

blockCountByCustomerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Domain.Types.CustomerBlockTransactions.ActionType -> m ([Domain.Types.CustomerBlockTransactions.CustomerBlockTransactions]))
blockCountByCustomerId customerId actionType = do findAllWithKV [Se.And [Se.Is Beam.customerId $ Se.Eq (Kernel.Types.Id.getId customerId), Se.Is Beam.actionType $ Se.Eq actionType]]

findByCustomerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.CustomerBlockTransactions.CustomerBlockTransactions]))
findByCustomerId customerId = do findAllWithKV [Se.Is Beam.customerId $ Se.Eq (Kernel.Types.Id.getId customerId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CustomerBlockTransactions.CustomerBlockTransactions -> m (Maybe Domain.Types.CustomerBlockTransactions.CustomerBlockTransactions))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CustomerBlockTransactions.CustomerBlockTransactions -> m ())
updateByPrimaryKey (Domain.Types.CustomerBlockTransactions.CustomerBlockTransactions {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actionType actionType,
      Se.Set Beam.blockLiftTime blockLiftTime,
      Se.Set Beam.blockReason blockReason,
      Se.Set Beam.blockTimeInHours blockTimeInHours,
      Se.Set Beam.blockedBy blockedBy,
      Se.Set Beam.customerId (Kernel.Types.Id.getId customerId),
      Se.Set Beam.reasonCode reasonCode,
      Se.Set Beam.reportedAt reportedAt,
      Se.Set Beam.requestorId requestorId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.CustomerBlockTransactions Domain.Types.CustomerBlockTransactions.CustomerBlockTransactions where
  fromTType' (Beam.CustomerBlockTransactionsT {..}) = do
    pure $
      Just
        Domain.Types.CustomerBlockTransactions.CustomerBlockTransactions
          { actionType = actionType,
            blockLiftTime = blockLiftTime,
            blockReason = blockReason,
            blockTimeInHours = blockTimeInHours,
            blockedBy = blockedBy,
            customerId = Kernel.Types.Id.Id customerId,
            id = Kernel.Types.Id.Id id,
            reasonCode = reasonCode,
            reportedAt = reportedAt,
            requestorId = requestorId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CustomerBlockTransactions Domain.Types.CustomerBlockTransactions.CustomerBlockTransactions where
  toTType' (Domain.Types.CustomerBlockTransactions.CustomerBlockTransactions {..}) = do
    Beam.CustomerBlockTransactionsT
      { Beam.actionType = actionType,
        Beam.blockLiftTime = blockLiftTime,
        Beam.blockReason = blockReason,
        Beam.blockTimeInHours = blockTimeInHours,
        Beam.blockedBy = blockedBy,
        Beam.customerId = Kernel.Types.Id.getId customerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.reasonCode = reasonCode,
        Beam.reportedAt = reportedAt,
        Beam.requestorId = requestorId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
