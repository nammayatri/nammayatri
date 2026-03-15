module Storage.Queries.CorporateRosterExtra where

import Data.Time.Calendar (Day)
import qualified Domain.Types.CorporateEntity as DCEnt
import Domain.Types.CorporateRoster
import qualified Domain.Types.CorporateShift as DCS
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.CorporateRoster as Beam
import Storage.Queries.OrphanInstances.CorporateRoster ()

-- Extra code goes here --

-- | Paginated query for rosters by corporate entity ID
findByCorporateEntityIdPaginated ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DCEnt.CorporateEntity ->
  Int ->
  Int ->
  m [CorporateRoster]
findByCorporateEntityIdPaginated entityId limit offset =
  findAllWithOptionsKV
    [Se.Is Beam.corporateEntityId $ Se.Eq (getId entityId)]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

-- | Paginated query for rosters by shift ID and date
findByShiftIdAndDatePaginated ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DCS.CorporateShift ->
  Day ->
  Int ->
  Int ->
  m [CorporateRoster]
findByShiftIdAndDatePaginated shiftId date limit offset =
  findAllWithOptionsKV
    [Se.And [Se.Is Beam.corporateShiftId $ Se.Eq (getId shiftId), Se.Is Beam.rosterDate $ Se.Eq date]]
    (Se.Asc Beam.createdAt)
    (Just limit)
    (Just offset)

-- | Count completed roster entries for a corporate entity
findCompletedByEntityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DCEnt.CorporateEntity ->
  m [CorporateRoster]
findCompletedByEntityId entityId =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.corporateEntityId $ Se.Eq (getId entityId),
          Se.Is Beam.attendanceStatus $ Se.Eq (show COMPLETED)
        ]
    ]
