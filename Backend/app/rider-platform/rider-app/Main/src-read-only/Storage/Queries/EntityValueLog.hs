{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.EntityValueLog (module Storage.Queries.EntityValueLog, module ReExport) where

import qualified Domain.Types.EntityValueLog
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.EntityValueLog as Beam
import Storage.Queries.EntityValueLogExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.EntityValueLog.EntityValueLog -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.EntityValueLog.EntityValueLog] -> m ()
createMany = traverse_ create

findAllByEntityTypeAndEntityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Prelude.Text -> (Kernel.Prelude.Text -> m [Domain.Types.EntityValueLog.EntityValueLog])
findAllByEntityTypeAndEntityId entityType entityId = do findAllWithKV [Se.And [Se.Is Beam.entityType $ Se.Eq entityType, Se.Is Beam.entityId $ Se.Eq entityId]]

findAllByEntityTypeAndEntityIdAndOperationId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Text ->
  (Kernel.Prelude.Text -> (Kernel.Prelude.Text -> m [Domain.Types.EntityValueLog.EntityValueLog]))
findAllByEntityTypeAndEntityIdAndOperationId entityType entityId operationId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.entityType $ Se.Eq entityType,
          Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.operationId $ Se.Eq operationId
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.EntityValueLog.EntityValueLog -> m (Maybe Domain.Types.EntityValueLog.EntityValueLog)
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.EntityValueLog.EntityValueLog -> m ()
updateByPrimaryKey (Domain.Types.EntityValueLog.EntityValueLog {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actorId actorId,
      Se.Set Beam.actorType actorType,
      Se.Set Beam.delta delta,
      Se.Set Beam.entityId entityId,
      Se.Set Beam.entityType entityType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.operationId operationId,
      Se.Set Beam.operationType operationType,
      Se.Set Beam.reason reason,
      Se.Set Beam.status status,
      Se.Set Beam.valueAfter valueAfter,
      Se.Set Beam.valueBefore valueBefore,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
