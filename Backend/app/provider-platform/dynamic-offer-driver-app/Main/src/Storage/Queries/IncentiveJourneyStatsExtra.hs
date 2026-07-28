{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.IncentiveJourneyStatsExtra where

import Data.Time (Day, UTCTime (UTCTime), addDays, addUTCTime)
import qualified Domain.Types.IncentiveJourney as DIJ
import qualified Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Domain.Types.IncentiveJourneyStats as DIJS
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (Seconds)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime, secondsToNominalDiffTime)
import qualified Sequelize as Se
import qualified Storage.Beam.IncentiveJourneyStats as Beam
import Storage.Queries.OrphanInstances.IncentiveJourneyStats ()

-- | Upsert by (driverId, journeyId, milestoneId, periodKey).
-- Updates currentValue / status / rewardValue when a row already exists.
upsertJourneyStats ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  DIJS.IncentiveJourneyStats ->
  m DIJS.IncentiveJourneyStats
upsertJourneyStats stats = do
  mbExisting <-
    findOneWithKV
      [ Se.And
          [ Se.Is Beam.driverId $ Se.Eq (getId stats.driverId),
            Se.Is Beam.journeyId $ Se.Eq (getId stats.journeyId),
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

-- | History for a driver on a calendar day.
-- dayStart / dayEnd should already be UTC bounds for that local day.
-- Pass Nothing for mbLimit to fetch the full day (needed when callers expand
-- milestones and paginate the expanded list themselves).
findHistoryByDriverIdAndCreatedAtRange ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  UTCTime ->
  UTCTime ->
  Maybe Int ->
  Maybe Int ->
  m [DIJS.IncentiveJourneyStats]
findHistoryByDriverIdAndCreatedAtRange driverId dayStart dayEnd mbLimit mbOffset =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (getId driverId),
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq dayStart,
          Se.Is Beam.createdAt $ Se.LessThan dayEnd
        ]
    ]
    (Se.Desc Beam.createdAt)
    mbLimit
    (Just $ fromMaybe 0 mbOffset)

findByDriverIdAndPeriodKey ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Text ->
  m [DIJS.IncentiveJourneyStats]
findByDriverIdAndPeriodKey driverId periodKey =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (getId driverId),
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

findStatsByDriverJourneyAndPeriod ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Id DIJ.IncentiveJourney ->
  Text ->
  m [DIJS.IncentiveJourneyStats]
findStatsByDriverJourneyAndPeriod driverId journeyId periodKey =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (getId driverId),
          Se.Is Beam.journeyId $ Se.Eq (getId journeyId),
          Se.Is Beam.periodKey $ Se.Eq periodKey
        ]
    ]

findStatsByDriverAndMilestonePeriod ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Id DIJ.IncentiveJourney ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  m (Maybe DIJS.IncentiveJourneyStats)
findStatsByDriverAndMilestonePeriod driverId journeyId milestoneId periodKey =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (getId driverId),
          Se.Is Beam.journeyId $ Se.Eq (getId journeyId),
          Se.Is Beam.milestoneId $ Se.Eq (getId milestoneId),
          Se.Is Beam.periodKey $ Se.Eq periodKey
        ]
    ]
