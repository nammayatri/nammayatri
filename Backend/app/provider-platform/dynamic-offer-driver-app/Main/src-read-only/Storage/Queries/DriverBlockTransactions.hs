{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverBlockTransactions where

import qualified Domain.Types.DriverBlockTransactions
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverBlockTransactions as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverBlockTransactions.DriverBlockTransactions -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverBlockTransactions.DriverBlockTransactions] -> m ())
createMany = traverse_ create

blockCountByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Domain.Types.DriverBlockTransactions.ActionType -> m [Domain.Types.DriverBlockTransactions.DriverBlockTransactions])
blockCountByDriverId driverId actionType = do findAllWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.actionType $ Se.Eq actionType]]

findByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.DriverBlockTransactions.DriverBlockTransactions])
findByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DriverBlockTransactions.DriverBlockTransactions -> m (Maybe Domain.Types.DriverBlockTransactions.DriverBlockTransactions))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverBlockTransactions.DriverBlockTransactions -> m ())
updateByPrimaryKey (Domain.Types.DriverBlockTransactions.DriverBlockTransactions {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actionType actionType,
      Se.Set Beam.blockLiftTime blockLiftTime,
      Se.Set Beam.blockReason blockReason,
      Se.Set Beam.blockReasonFlag blockReasonFlag,
      Se.Set Beam.blockTimeInHours blockTimeInHours,
      Se.Set Beam.blockedBy blockedBy,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.reasonCode reasonCode,
      Se.Set Beam.reportedAt reportedAt,
      Se.Set Beam.requestorId requestorId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DriverBlockTransactions Domain.Types.DriverBlockTransactions.DriverBlockTransactions where
  fromTType' (Beam.DriverBlockTransactionsT {..}) = do
    pure $
      Just
        Domain.Types.DriverBlockTransactions.DriverBlockTransactions
          { actionType = actionType,
            blockLiftTime = blockLiftTime,
            blockReason = blockReason,
            blockReasonFlag = blockReasonFlag,
            blockTimeInHours = blockTimeInHours,
            blockedBy = blockedBy,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            reasonCode = reasonCode,
            reportedAt = reportedAt,
            requestorId = requestorId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverBlockTransactions Domain.Types.DriverBlockTransactions.DriverBlockTransactions where
  toTType' (Domain.Types.DriverBlockTransactions.DriverBlockTransactions {..}) = do
    Beam.DriverBlockTransactionsT
      { Beam.actionType = actionType,
        Beam.blockLiftTime = blockLiftTime,
        Beam.blockReason = blockReason,
        Beam.blockReasonFlag = blockReasonFlag,
        Beam.blockTimeInHours = blockTimeInHours,
        Beam.blockedBy = blockedBy,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.reasonCode = reasonCode,
        Beam.reportedAt = reportedAt,
        Beam.requestorId = requestorId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
