module Storage.Queries.MediaFileDocumentExtra where

import qualified Domain.Types.MediaFileDocument as DMFD
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MediaFileDocument as Beam
import Storage.Queries.OrphanInstances.MediaFileDocument ()

updateStatusAndResetUploadLink ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DMFD.MediaFileDocumentStatus ->
  Id DMFD.MediaFileDocument ->
  m ()
updateStatusAndResetUploadLink status id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.uploadLink Nothing,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
