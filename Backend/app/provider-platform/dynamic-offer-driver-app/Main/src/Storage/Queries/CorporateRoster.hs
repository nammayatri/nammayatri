{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CorporateRoster where

import qualified Data.Time.Calendar
import qualified Domain.Types.CorporateRoster
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CorporateRoster as Beam
import Storage.Queries.OrphanInstances.CorporateRoster ()

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.CorporateRoster.CorporateRoster ->
  m (Maybe Domain.Types.CorporateRoster.CorporateRoster)
findById rosterId = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId rosterId)]

-- | Find roster entries assigned to a driver (via bookingId) for a given date.
-- The driver-side roster doesn't have a direct driverId field, so we look for
-- entries with a non-null bookingId that match the date and are in CONFIRMED status,
-- indicating they've been assigned.
findByDateAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Data.Time.Calendar.Day ->
  Domain.Types.CorporateRoster.CorporateAttendanceStatus ->
  m [Domain.Types.CorporateRoster.CorporateRoster]
findByDateAndStatus date status = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.rosterDate $ Se.Eq date,
          Se.Is Beam.attendanceStatus $ Se.Eq (Kernel.Prelude.show status)
        ]
    ]

-- | Find all CONFIRMED roster entries for a given date (for driver roster view)
findConfirmedByDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Data.Time.Calendar.Day ->
  m [Domain.Types.CorporateRoster.CorporateRoster]
findConfirmedByDate date =
  findByDateAndStatus date Domain.Types.CorporateRoster.CONFIRMED

-- | Find completed roster entries within a date range (for earnings calculation)
findCompletedInDateRange ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Data.Time.Calendar.Day ->
  Data.Time.Calendar.Day ->
  m [Domain.Types.CorporateRoster.CorporateRoster]
findCompletedInDateRange startDate endDate = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.rosterDate $ Se.GreaterThanOrEq startDate,
          Se.Is Beam.rosterDate $ Se.LessThanOrEq endDate,
          Se.Is Beam.attendanceStatus $ Se.Eq (Kernel.Prelude.show Domain.Types.CorporateRoster.COMPLETED)
        ]
    ]

updateAttendanceStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Domain.Types.CorporateRoster.CorporateAttendanceStatus ->
  Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.UTCTime ->
  Kernel.Types.Id.Id Domain.Types.CorporateRoster.CorporateRoster ->
  m ()
updateAttendanceStatus attendanceStatus confirmedAt updatedAt rosterId = do
  updateWithKV
    [ Se.Set Beam.attendanceStatus (Kernel.Prelude.show attendanceStatus),
      Se.Set Beam.confirmedAt confirmedAt,
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId rosterId)]

updateBookingId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Text ->
  Kernel.Prelude.UTCTime ->
  Kernel.Types.Id.Id Domain.Types.CorporateRoster.CorporateRoster ->
  m ()
updateBookingId bookingId updatedAt rosterId = do
  updateWithKV
    [ Se.Set Beam.bookingId bookingId,
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId rosterId)]
