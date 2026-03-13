{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CorporateRoster (module Storage.Queries.CorporateRoster, module ReExport) where

import qualified Data.Time.Calendar
import qualified Domain.Types.Booking
import qualified Domain.Types.CorporateEmployee
import qualified Domain.Types.CorporateEntity
import qualified Domain.Types.CorporateRoster
import qualified Domain.Types.CorporateShift
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CorporateRoster as Beam
import Storage.Queries.CorporateRosterExtra as ReExport
import Storage.Queries.OrphanInstances.CorporateRoster ()

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CorporateRoster.CorporateRoster -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CorporateRoster.CorporateRoster] -> m ())
createMany = traverse_ create

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CorporateRoster.CorporateRoster -> m (Maybe Domain.Types.CorporateRoster.CorporateRoster))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByCorporateEntityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity -> m [Domain.Types.CorporateRoster.CorporateRoster])
findByCorporateEntityId corporateEntityId = do findAllWithKV [Se.Is Beam.corporateEntityId $ Se.Eq (Kernel.Types.Id.getId corporateEntityId)]

findByShiftIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift -> Data.Time.Calendar.Day -> m [Domain.Types.CorporateRoster.CorporateRoster])
findByShiftIdAndDate corporateShiftId rosterDate = do
  findAllWithKV [Se.And [Se.Is Beam.corporateShiftId $ Se.Eq (Kernel.Types.Id.getId corporateShiftId), Se.Is Beam.rosterDate $ Se.Eq rosterDate]]

findByEmployeeIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CorporateEmployee.CorporateEmployee -> Data.Time.Calendar.Day -> m (Maybe Domain.Types.CorporateRoster.CorporateRoster))
findByEmployeeIdAndDate corporateEmployeeId rosterDate = do
  findOneWithKV [Se.And [Se.Is Beam.corporateEmployeeId $ Se.Eq (Kernel.Types.Id.getId corporateEmployeeId), Se.Is Beam.rosterDate $ Se.Eq rosterDate]]

updateAttendanceStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.CorporateRoster.CorporateAttendanceStatus -> Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.CorporateRoster.CorporateRoster -> m ())
updateAttendanceStatus attendanceStatus confirmedAt updatedAt id = do
  updateWithKV
    [ Se.Set Beam.attendanceStatus (Kernel.Prelude.show attendanceStatus),
      Se.Set Beam.confirmedAt confirmedAt,
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateBookingId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe (Kernel.Types.Id.Id Domain.Types.Booking.Booking) -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.CorporateRoster.CorporateRoster -> m ())
updateBookingId bookingId updatedAt id = do
  updateWithKV
    [ Se.Set Beam.bookingId (Kernel.Types.Id.getId <$> bookingId),
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.CorporateRoster.CorporateRoster -> m (Maybe Domain.Types.CorporateRoster.CorporateRoster))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
