{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ApprovalRequest (module Storage.Queries.ApprovalRequest, module ReExport) where

import qualified Data.Text
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
import Storage.Queries.ApprovalRequestExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ApprovalRequest.ApprovalRequest -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ApprovalRequest.ApprovalRequest] -> m ())
createMany = traverse_ create

updateStatusWithReason ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.ApprovalRequest.RequestStatus -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Types.Id.Id Domain.Types.ApprovalRequest.ApprovalRequest -> m ())
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
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.reason reason,
      Se.Set Beam.requestData requestData,
      Se.Set Beam.requesteeId (Kernel.Types.Id.getId requesteeId),
      Se.Set Beam.requestorId (Kernel.Types.Id.getId requestorId),
      Se.Set Beam.status status,
      Se.Set Beam.title title,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
