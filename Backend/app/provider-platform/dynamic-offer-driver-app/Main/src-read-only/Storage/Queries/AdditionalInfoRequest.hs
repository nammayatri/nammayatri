{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AdditionalInfoRequest (module Storage.Queries.AdditionalInfoRequest, module ReExport) where

import qualified Domain.Types.AdditionalInfoRequest
import qualified Domain.Types.OperationHubRequests
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.AdditionalInfoRequest as Beam
import Storage.Queries.AdditionalInfoRequestExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AdditionalInfoRequest.AdditionalInfoRequest -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AdditionalInfoRequest.AdditionalInfoRequest] -> m ())
createMany = traverse_ create

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.AdditionalInfoRequest.AdditionalInfoRequest -> m (Maybe Domain.Types.AdditionalInfoRequest.AdditionalInfoRequest))
findById id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

findAllByOperationHubRequestId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.OperationHubRequests.OperationHubRequests -> m [Domain.Types.AdditionalInfoRequest.AdditionalInfoRequest])
findAllByOperationHubRequestId operationHubRequestId = do
  findAllWithKV
    [ Se.Is Beam.operationHubRequestId $ Se.Eq (Kernel.Types.Id.getId operationHubRequestId)
    ]

updateStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.AdditionalInfoRequest.AdditionalInfoStatus -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.AdditionalInfoRequest.AdditionalInfoRequest -> m ())
updateStatus status updatedAt id = do
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateResponse ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.AdditionalInfoRequest.AdditionalInfoStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe [Kernel.Prelude.Text] -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.AdditionalInfoRequest.AdditionalInfoRequest -> m ())
updateResponse status responseRemarks responseDocumentIds updatedAt id = do
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.responseRemarks responseRemarks,
      Se.Set Beam.responseDocumentIds responseDocumentIds,
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.AdditionalInfoRequest.AdditionalInfoRequest -> m (Maybe Domain.Types.AdditionalInfoRequest.AdditionalInfoRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
