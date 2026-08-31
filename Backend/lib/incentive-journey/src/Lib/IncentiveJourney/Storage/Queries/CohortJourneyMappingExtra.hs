{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.IncentiveJourney.Storage.Queries.CohortJourneyMappingExtra where

import Data.Aeson (Value)
import qualified Data.Text as T
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
import qualified Sequelize as Se

createCohortJourneyMapping ::
  (BeamFlow m r) =>
  Id Common.Merchant ->
  Id DCD.CohortDetails ->
  Id DIJ.IncentiveJourney ->
  UTCTime ->
  Int ->
  Maybe Common.MilestoneRewardType ->
  Maybe Int ->
  Maybe Int ->
  Maybe Value ->
  m DCJM.CohortJourneyMapping
createCohortJourneyMapping merchantId cohortId journeyId startDate streakRange streakEndRewardType streakEndRewardValue streakEndRewardExpirationAt streakEndRewardMetadata = do
  now <- getCurrentTime
  mappingId <- generateGUID
  let row =
        DCJM.CohortJourneyMapping
          { id = mappingId,
            merchantId = merchantId,
            cohortId = cohortId,
            journeyId = journeyId,
            startDate = startDate,
            streakRange = streakRange,
            streakEndRewardType = streakEndRewardType,
            streakEndRewardValue = streakEndRewardValue,
            streakEndRewardExpirationAt = streakEndRewardExpirationAt,
            streakEndRewardMetadata = streakEndRewardMetadata,
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
      Se.Set Beam.streakEndRewardMetadata row.streakEndRewardMetadata,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId row.id)]
  pure row

-- | Create or update window + streak-end reward for (cohortId, journeyId).
upsertCohortJourneyMapping ::
  (BeamFlow m r) =>
  Id Common.Merchant ->
  Id DCD.CohortDetails ->
  Id DIJ.IncentiveJourney ->
  UTCTime ->
  Int ->
  Maybe Common.MilestoneRewardType ->
  Maybe Int ->
  Maybe Int ->
  Maybe Value ->
  m DCJM.CohortJourneyMapping
upsertCohortJourneyMapping merchantId cohortId journeyId startDate streakRange streakEndRewardType streakEndRewardValue streakEndRewardExpirationAt streakEndRewardMetadata = do
  mbExisting <-
    findOneWithKV
      [ Se.And
          [ Se.Is Beam.cohortId $ Se.Eq (getId cohortId),
            Se.Is Beam.journeyId $ Se.Eq (getId journeyId)
          ]
      ]
  case mbExisting of
    Nothing ->
      createCohortJourneyMapping merchantId cohortId journeyId startDate streakRange streakEndRewardType streakEndRewardValue streakEndRewardExpirationAt streakEndRewardMetadata
    Just existing ->
      updateCohortJourneyMappingFields
        existing
          { DCJM.merchantId = merchantId,
            DCJM.startDate = startDate,
            DCJM.streakRange = streakRange,
            DCJM.streakEndRewardType = streakEndRewardType,
            DCJM.streakEndRewardValue = streakEndRewardValue,
            DCJM.streakEndRewardExpirationAt = streakEndRewardExpirationAt,
            DCJM.streakEndRewardMetadata = streakEndRewardMetadata
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

findByMerchantIdWithOptionalCohortName ::
  (BeamFlow m r) =>
  Maybe Int ->
  Maybe Int ->
  Id Common.Merchant ->
  Maybe Text ->
  m [DCJM.CohortJourneyMapping]
findByMerchantIdWithOptionalCohortName limit offset merchantId mbCohortName = do
  let merchantClause = [Se.Is Beam.merchantId $ Se.Eq (getId merchantId)]
      hasNameFilter =
        case T.strip <$> mbCohortName of
          Just name | not (T.null name) -> True
          _ -> False
  if hasNameFilter
    then findAllWithKV merchantClause
    else findAllWithOptionsKV merchantClause (Se.Desc Beam.createdAt) limit offset
