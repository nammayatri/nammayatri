module Storage.Queries.DriverGracePeriodTrackerExtra where

import qualified Domain.Types.DriverGracePeriodTracker
import qualified Domain.Types.PenaltyRule
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverGracePeriodTracker as Beam

-- Extra code goes here --

-- | Atomically increment offense count and update timestamp for a tracker.
-- Uses updateWithKV which provides atomic DB-level update.
incrementOffenseCountByDriverIdAndRuleId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Types.Id.Id Domain.Types.PenaltyRule.PenaltyRule ->
  Int ->
  UTCTime ->
  m ()
incrementOffenseCountByDriverIdAndRuleId driverId ruleId newCount now = do
  updateWithKV
    [ Se.Set Beam.offenseCount newCount,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.ruleId $ Se.Eq (Kernel.Types.Id.getId ruleId)
        ]
    ]

-- | Atomically reset the grace window and set offense count to 1 (new window).
resetWindowByDriverIdAndRuleId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Types.Id.Id Domain.Types.PenaltyRule.PenaltyRule ->
  Int ->
  UTCTime ->
  UTCTime ->
  UTCTime ->
  m ()
resetWindowByDriverIdAndRuleId driverId ruleId offenseCount windowStart windowEnd now = do
  updateWithKV
    [ Se.Set Beam.offenseCount offenseCount,
      Se.Set Beam.windowStartTime windowStart,
      Se.Set Beam.windowEndTime windowEnd,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.ruleId $ Se.Eq (Kernel.Types.Id.getId ruleId)
        ]
    ]

deleteByDriverIdAndRuleId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Types.Id.Id Domain.Types.PenaltyRule.PenaltyRule ->
  m ()
deleteByDriverIdAndRuleId driverId ruleId = do
  deleteWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.ruleId $ Se.Eq (Kernel.Types.Id.getId ruleId)
        ]
    ]
