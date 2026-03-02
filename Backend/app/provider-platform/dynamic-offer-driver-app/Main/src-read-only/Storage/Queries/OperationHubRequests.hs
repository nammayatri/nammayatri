{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OperationHubRequests (module Storage.Queries.OperationHubRequests, module ReExport) where

import qualified Domain.Types.OperationHubRequests
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.OperationHubRequests as Beam
import Storage.Queries.OperationHubRequestsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.OperationHubRequests.OperationHubRequests -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.OperationHubRequests.OperationHubRequests] -> m ())
createMany = traverse_ create

findByCreatorStatusAndType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.OperationHubRequests.RequestStatus -> Domain.Types.OperationHubRequests.RequestType -> m [Domain.Types.OperationHubRequests.OperationHubRequests])
findByCreatorStatusAndType creatorId requestStatus requestType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.creatorId $ Se.Eq (Kernel.Types.Id.getId creatorId),
          Se.Is Beam.requestStatus $ Se.Eq requestStatus,
          Se.Is Beam.requestType $ Se.Eq requestType
        ]
    ]

findOneByRequestStatusAndDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.OperationHubRequests.RequestStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> m (Maybe Domain.Types.OperationHubRequests.OperationHubRequests))
findOneByRequestStatusAndDriverId requestStatus driverId = do findOneWithKV [Se.And [Se.Is Beam.requestStatus $ Se.Eq requestStatus, Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId <$> driverId)]]

findOneByRequestStatusAndRegistrationNo ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.OperationHubRequests.RequestStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.OperationHubRequests.OperationHubRequests))
findOneByRequestStatusAndRegistrationNo requestStatus registrationNo = do findOneWithKV [Se.And [Se.Is Beam.requestStatus $ Se.Eq requestStatus, Se.Is Beam.registrationNo $ Se.Eq registrationNo]]

updateStatusWithDetails ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.OperationHubRequests.RequestStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Types.Id.Id Domain.Types.OperationHubRequests.OperationHubRequests -> m ())
updateStatusWithDetails requestStatus remarks fulfilledAt operatorId id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.requestStatus requestStatus,
      Se.Set Beam.remarks remarks,
      Se.Set Beam.fulfilledAt fulfilledAt,
      Se.Set Beam.operatorId (Kernel.Types.Id.getId <$> operatorId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.OperationHubRequests.OperationHubRequests -> m (Maybe Domain.Types.OperationHubRequests.OperationHubRequests))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.OperationHubRequests.OperationHubRequests -> m ())
updateByPrimaryKey (Domain.Types.OperationHubRequests.OperationHubRequests {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.creatorId (Kernel.Types.Id.getId creatorId),
      Se.Set Beam.driverId (Kernel.Types.Id.getId <$> driverId),
      Se.Set Beam.fulfilledAt fulfilledAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.operationHubId (Kernel.Types.Id.getId operationHubId),
      Se.Set Beam.operatorId (Kernel.Types.Id.getId <$> operatorId),
      Se.Set Beam.registrationNo registrationNo,
      Se.Set Beam.remarks remarks,
      Se.Set Beam.requestStatus requestStatus,
      Se.Set Beam.requestType requestType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
