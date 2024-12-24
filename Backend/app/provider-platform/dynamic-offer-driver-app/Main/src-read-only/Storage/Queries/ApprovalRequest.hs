{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ApprovalRequest where

import qualified Domain.Types.ApprovalRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ApprovalRequest as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ApprovalRequest.ApprovalRequest -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ApprovalRequest.ApprovalRequest] -> m ())
createMany = traverse_ create

findByTripReqAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction -> Domain.Types.DriverRequest.RequestType -> Kernel.Prelude.Maybe Domain.Types.DriverRequest.RequestStatus -> m (Maybe Domain.Types.DriverRequest.DriverRequest))
findByTripReqAndStatus tripTransactionId requestType status = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.tripTransactionId $ Se.Eq (Kernel.Types.Id.getId tripTransactionId),
          Se.Is Beam.requestType $ Se.Eq requestType,
          Se.Is Beam.status $ Se.Eq status
        ]
    ]

updateStatusWithReason ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.DriverRequest.RequestStatus -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Types.Id.Id Domain.Types.DriverRequest.DriverRequest -> m ())
updateStatusWithReason status reason id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.reason reason, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.ApprovalRequest.ApprovalRequest -> m (Maybe Domain.Types.ApprovalRequest.ApprovalRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ApprovalRequest.ApprovalRequest -> m ())
updateByPrimaryKey (Domain.Types.ApprovalRequest.ApprovalRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.body body,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.reason reason,
      Se.Set Beam.requestType requestType,
      Se.Set Beam.requesteeId (Kernel.Types.Id.getId requesteeId),
      Se.Set Beam.requestorId (Kernel.Types.Id.getId requestorId),
      Se.Set Beam.status status,
      Se.Set Beam.title title,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.ApprovalRequest Domain.Types.ApprovalRequest.ApprovalRequest where
  fromTType' (Beam.ApprovalRequestT {..}) = do
    pure $
      Just
        Domain.Types.ApprovalRequest.ApprovalRequest
          { body = body,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            reason = reason,
            requestType = requestType,
            status = status,
            title = title,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.ApprovalRequest Domain.Types.ApprovalRequest.ApprovalRequest where
  toTType' (Domain.Types.ApprovalRequest.ApprovalRequest {..}) = do
    Beam.ApprovalRequestT
      { Beam.body = body,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.reason = reason,
        Beam.requestType = requestType,
        Beam.status = status,
        Beam.title = title,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
