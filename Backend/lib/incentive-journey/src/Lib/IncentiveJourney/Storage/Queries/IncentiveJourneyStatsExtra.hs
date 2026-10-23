{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra where

import Data.Time (Day, UTCTime (UTCTime), addDays, addUTCTime)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (Seconds)
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime, secondsToNominalDiffTime)
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Beam.IncentiveJourneyStats as Beam
import Lib.IncentiveJourney.Storage.Queries.OrphanInstances.IncentiveJourneyStats ()
import qualified Sequelize as Se

-- | Upsert by (personId, cohortJourneyMappingId, milestoneId, periodKey).
upsertJourneyStats ::
  (BeamFlow m r) =>
  DIJS.IncentiveJourneyStats ->
  m DIJS.IncentiveJourneyStats
upsertJourneyStats stats = do
  mbExisting <-
    findOneWithKV
      [ Se.And
          [ Se.Is Beam.personId $ Se.Eq (getId stats.personId),
            Se.Is Beam.cohortJourneyMappingId $ Se.Eq (getId <$> stats.cohortJourneyMappingId),
            Se.Is Beam.milestoneId $ Se.Eq (getId stats.milestoneId),
            Se.Is Beam.periodKey $ Se.Eq stats.periodKey
          ]
      ]
  case mbExisting of
    Nothing -> do
      createWithKV stats
      pure stats
    Just existing -> do
      now <- getCurrentTime
      let updated =
            existing
              { DIJS.currentValue = stats.currentValue,
                DIJS.status = stats.status,
                DIJS.rewardValue = maybe existing.rewardValue Just stats.rewardValue,
                DIJS.conditionType = stats.conditionType,
                DIJS.conditionOperator = stats.conditionOperator,
                DIJS.conditionValue = stats.conditionValue,
                DIJS.rewardType = stats.rewardType,
                DIJS.updatedAt = now
              }
      updateWithKV
        [ Se.Set Beam.currentValue updated.currentValue,
          Se.Set Beam.status updated.status,
          Se.Set Beam.rewardValue updated.rewardValue,
          Se.Set Beam.conditionType updated.conditionType,
          Se.Set Beam.conditionOperator updated.conditionOperator,
          Se.Set Beam.conditionValue updated.conditionValue,
          Se.Set Beam.rewardType updated.rewardType,
          Se.Set Beam.updatedAt now
        ]
        [Se.Is Beam.id $ Se.Eq (getId existing.id)]
      pure updated

-- | History for a person on a calendar day.
-- dayStart / dayEnd should already be UTC bounds for that local day.
findHistoryByPersonIdAndCreatedAtRange ::
  (BeamFlow m r) =>
  Id Common.Person ->
  UTCTime ->
  UTCTime ->
  Maybe Int ->
  Maybe Int ->
  m [DIJS.IncentiveJourneyStats]
findHistoryByPersonIdAndCreatedAtRange personId dayStart dayEnd mbLimit mbOffset =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (getId personId),
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq dayStart,
          Se.Is Beam.createdAt $ Se.LessThan dayEnd
        ]
    ]
    (Se.Desc Beam.createdAt)
    mbLimit
    (Just $ fromMaybe 0 mbOffset)

findByPersonIdAndPeriodKey ::
  (BeamFlow m r) =>
  Id Common.Person ->
  Text ->
  m [DIJS.IncentiveJourneyStats]
findByPersonIdAndPeriodKey personId periodKey =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (getId personId),
          Se.Is Beam.periodKey $ Se.Eq periodKey
        ]
    ]

-- | Convenience: Day -> [local midnight, next midnight) in the given UTC offset.
mkLocalDayUtcBounds :: Day -> Seconds -> (UTCTime, UTCTime)
mkLocalDayUtcBounds day timeDiffFromUtc =
  let offset = negate (secondsToNominalDiffTime timeDiffFromUtc)
      dayStartLocal = UTCTime day 0
      dayEndLocal = UTCTime (addDays 1 day) 0
   in ( addUTCTime offset dayStartLocal,
        addUTCTime offset dayEndLocal
      )

findStatsByPersonCjmAndPeriod ::
  (BeamFlow m r) =>
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text ->
  m [DIJS.IncentiveJourneyStats]
findStatsByPersonCjmAndPeriod personId cohortJourneyMappingId periodKey =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (getId personId),
          Se.Is Beam.cohortJourneyMappingId $ Se.Eq (Just (getId cohortJourneyMappingId)),
          Se.Is Beam.periodKey $ Se.Eq periodKey
        ]
    ]

-- | Scoped fetch for streak-end eligibility — only this campaign's period keys.
findStatsByPersonCjmAndPeriodKeys ::
  (BeamFlow m r) =>
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  [Text] ->
  m [DIJS.IncentiveJourneyStats]
findStatsByPersonCjmAndPeriodKeys _personId _cohortJourneyMappingId [] = pure []
findStatsByPersonCjmAndPeriodKeys personId cohortJourneyMappingId periodKeys =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (getId personId),
          Se.Is Beam.cohortJourneyMappingId $ Se.Eq (Just (getId cohortJourneyMappingId)),
          Se.Is Beam.periodKey $ Se.In periodKeys
        ]
    ]

findStatsByPersonCjmAndMilestonePeriod ::
  (BeamFlow m r) =>
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  m (Maybe DIJS.IncentiveJourneyStats)
findStatsByPersonCjmAndMilestonePeriod personId cohortJourneyMappingId milestoneId periodKey =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (getId personId),
          Se.Is Beam.cohortJourneyMappingId $ Se.Eq (Just (getId cohortJourneyMappingId)),
          Se.Is Beam.milestoneId $ Se.Eq (getId milestoneId),
          Se.Is Beam.periodKey $ Se.Eq periodKey
        ]
    ]
