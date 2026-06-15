{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AdminRequest (module Storage.Queries.AdminRequest, module ReExport) where

import qualified Domain.Types.AdminRequest
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.AdminRequest as Beam
import Storage.Queries.AdminRequestExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AdminRequest.AdminRequest -> m ())
create = createWithKV

findByReferenceIdAndStatuses ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> [Domain.Types.AdminRequest.AdminRequestStatus] -> m (Maybe Domain.Types.AdminRequest.AdminRequest))
findByReferenceIdAndStatuses referenceId status = do findOneWithKV [Se.And [Se.Is Beam.referenceId $ Se.Eq referenceId, Se.Is Beam.status $ Se.In status]]

updateStatusErrorMessageAndAdminChecker ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.AdminRequest.AdminRequestStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.AdminRequest.AdminRequest -> m ())
updateStatusErrorMessageAndAdminChecker status errorMessage adminCheckerId adminCheckerName id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.errorMessage errorMessage,
      Se.Set Beam.adminCheckerId (Kernel.Types.Id.getId <$> adminCheckerId),
      Se.Set Beam.adminCheckerName adminCheckerName,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.AdminRequest.AdminRequest -> m (Maybe Domain.Types.AdminRequest.AdminRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
