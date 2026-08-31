{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.IncentiveJourney.Storage.Queries.CohortJourneyMappingExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (generateGUID, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.CohortDetails as DCD
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Beam.CohortJourneyMapping as Beam
import Lib.IncentiveJourney.Storage.Queries.OrphanInstances.CohortJourneyMapping ()
import qualified Lib.IncentiveJourney.Types as IJ
import qualified Sequelize as Se

createCohortJourneyMapping ::
  (BeamFlow m r) =>
  Id DCD.CohortDetails ->
  Id DIJ.IncentiveJourney ->
  UTCTime ->
  Int ->
  Maybe Common.MilestoneRewardType ->
  Maybe Int ->
  Maybe Int ->
  Maybe IJ.SubscriptionWaiveOffSpec ->
  m DCJM.CohortJourneyMapping
createCohortJourneyMapping cohortId journeyId startDate streakRange streakEndRewardType streakEndRewardValue streakEndRewardExpirationAt mbWaiveOff = do
  now <- getCurrentTime
  mappingId <- generateGUID
  let row =
        DCJM.CohortJourneyMapping
          { id = mappingId,
            cohortId = cohortId,
            journeyId = journeyId,
            startDate = startDate,
            streakRange = streakRange,
            streakEndRewardType = streakEndRewardType,
            streakEndRewardValue = streakEndRewardValue,
            streakEndRewardExpirationAt = streakEndRewardExpirationAt,
            streakEndSubscriptionWaiveOffPercentage = (.percentage) <$> mbWaiveOff,
            streakEndSubscriptionWaiveOffDaysValidFor = (.daysValidFor) <$> mbWaiveOff,
            streakEndSubscriptionWaiveOffServiceName = (.serviceName) <$> mbWaiveOff,
            streakEndSubscriptionWaiveOffMode = (.waiveOffMode) <$> mbWaiveOff,
            createdAt = now,
            updatedAt = now
          }
  createWithKV row
  pure row

updateCohortJourneyMappingFields ::
  (BeamFlow m r) =>
  DCJM.CohortJourneyMapping ->
  m DCJM.CohortJourneyMapping
updateCohortJourneyMappingFields updated = do
  now <- getCurrentTime
  let row = updated {DCJM.updatedAt = now}
  updateWithKV
    [ Se.Set Beam.startDate row.startDate,
      Se.Set Beam.streakRange row.streakRange,
      Se.Set Beam.streakEndRewardType row.streakEndRewardType,
      Se.Set Beam.streakEndRewardValue row.streakEndRewardValue,
      Se.Set Beam.streakEndRewardExpirationAt row.streakEndRewardExpirationAt,
      Se.Set Beam.streakEndSubscriptionWaiveOffPercentage row.streakEndSubscriptionWaiveOffPercentage,
      Se.Set Beam.streakEndSubscriptionWaiveOffDaysValidFor row.streakEndSubscriptionWaiveOffDaysValidFor,
      Se.Set Beam.streakEndSubscriptionWaiveOffServiceName row.streakEndSubscriptionWaiveOffServiceName,
      Se.Set Beam.streakEndSubscriptionWaiveOffMode row.streakEndSubscriptionWaiveOffMode,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId row.id)]
  pure row

-- | Create or update window + streak-end reward for (cohortId, journeyId).
upsertCohortJourneyMapping ::
  (BeamFlow m r) =>
  Id DCD.CohortDetails ->
  Id DIJ.IncentiveJourney ->
  UTCTime ->
  Int ->
  Maybe Common.MilestoneRewardType ->
  Maybe Int ->
  Maybe Int ->
  Maybe IJ.SubscriptionWaiveOffSpec ->
  m DCJM.CohortJourneyMapping
upsertCohortJourneyMapping cohortId journeyId startDate streakRange streakEndRewardType streakEndRewardValue streakEndRewardExpirationAt mbWaiveOff = do
  mbExisting <-
    findOneWithKV
      [ Se.And
          [ Se.Is Beam.cohortId $ Se.Eq (getId cohortId),
            Se.Is Beam.journeyId $ Se.Eq (getId journeyId)
          ]
      ]
  case mbExisting of
    Nothing ->
      createCohortJourneyMapping cohortId journeyId startDate streakRange streakEndRewardType streakEndRewardValue streakEndRewardExpirationAt mbWaiveOff
    Just existing ->
      updateCohortJourneyMappingFields
        existing
          { DCJM.startDate = startDate,
            DCJM.streakRange = streakRange,
            DCJM.streakEndRewardType = streakEndRewardType,
            DCJM.streakEndRewardValue = streakEndRewardValue,
            DCJM.streakEndRewardExpirationAt = streakEndRewardExpirationAt,
            DCJM.streakEndSubscriptionWaiveOffPercentage = maybe existing.streakEndSubscriptionWaiveOffPercentage (Just . (.percentage)) mbWaiveOff,
            DCJM.streakEndSubscriptionWaiveOffDaysValidFor = maybe existing.streakEndSubscriptionWaiveOffDaysValidFor (Just . (.daysValidFor)) mbWaiveOff,
            DCJM.streakEndSubscriptionWaiveOffServiceName = maybe existing.streakEndSubscriptionWaiveOffServiceName (Just . (.serviceName)) mbWaiveOff,
            DCJM.streakEndSubscriptionWaiveOffMode = maybe existing.streakEndSubscriptionWaiveOffMode (Just . (.waiveOffMode)) mbWaiveOff
          }

findByIds :: (BeamFlow m r) => [Id DCJM.CohortJourneyMapping] -> m [DCJM.CohortJourneyMapping]
findByIds ids
  | null ids = pure []
  | otherwise =
    findAllWithKV
      [Se.Is Beam.id $ Se.In (map getId ids)]

findCohortJourneyMappingById :: (BeamFlow m r) => Id DCJM.CohortJourneyMapping -> m (Maybe DCJM.CohortJourneyMapping)
findCohortJourneyMappingById mappingId =
  findOneWithKV [Se.Is Beam.id $ Se.Eq (getId mappingId)]
