{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ScheduledBookingOpsNote (module Storage.Queries.ScheduledBookingOpsNote, module ReExport) where

import qualified Domain.Types.OpsNote
import qualified Domain.Types.ScheduledBookingOpsNote
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ScheduledBookingOpsNote as Beam
import Storage.Queries.ScheduledBookingOpsNoteExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ScheduledBookingOpsNote.ScheduledBookingOpsNote -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ScheduledBookingOpsNote.ScheduledBookingOpsNote] -> m ())
createMany = traverse_ create

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.ScheduledBookingOpsNote.ScheduledBookingOpsNote -> m (Maybe Domain.Types.ScheduledBookingOpsNote.ScheduledBookingOpsNote))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByTransactionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.ScheduledBookingOpsNote.ScheduledBookingOpsNote]))
findByTransactionId transactionId = do findAllWithKV [Se.Is Beam.transactionId $ Se.Eq transactionId]

updateNote ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.OpsNote.OpsNoteStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.ScheduledBookingOpsNote.ScheduledBookingOpsNote -> m ())
updateNote status content id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.content content, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.ScheduledBookingOpsNote.ScheduledBookingOpsNote -> m (Maybe Domain.Types.ScheduledBookingOpsNote.ScheduledBookingOpsNote))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ScheduledBookingOpsNote.ScheduledBookingOpsNote -> m ())
updateByPrimaryKey (Domain.Types.ScheduledBookingOpsNote.ScheduledBookingOpsNote {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bookingId (Kernel.Types.Id.getId <$> bookingId),
      Se.Set Beam.content content,
      Se.Set Beam.createdByDashboardUserId createdByDashboardUserId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.noteType noteType,
      Se.Set Beam.status status,
      Se.Set Beam.transactionId transactionId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
