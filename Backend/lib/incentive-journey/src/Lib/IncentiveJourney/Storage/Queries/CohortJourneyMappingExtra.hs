{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.IncentiveJourney.Storage.Queries.CohortJourneyMappingExtra where

import Data.Aeson (Value)
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
  Id Common.MerchantOperatingCity ->
  Id DCD.CohortDetails ->
  Id DIJ.IncentiveJourney ->
  UTCTime ->
  Int ->
  Bool ->
  Maybe Int ->
  Maybe Common.MilestoneRewardType ->
  Maybe Int ->
  Maybe Int ->
  Maybe Value ->
  m DCJM.CohortJourneyMapping
createCohortJourneyMapping merchantId merchantOperatingCityId cohortId journeyId startDate streakRange enabled maxWaiveOffCount streakEndRewardType streakEndRewardValue streakEndRewardExpirationAt streakEndRewardMetadata = do
  now <- getCurrentTime
  mappingId <- generateGUID
  let row =
        DCJM.CohortJourneyMapping
          { id = mappingId,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            cohortId = cohortId,
            journeyId = journeyId,
            startDate = startDate,
            streakRange = streakRange,
            enabled = enabled,
            maxWaiveOffCount = maxWaiveOffCount,
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
      Se.Set Beam.enabled row.enabled,
      Se.Set Beam.maxWaiveOffCount row.maxWaiveOffCount,
      Se.Set Beam.streakEndRewardType row.streakEndRewardType,
      Se.Set Beam.streakEndRewardValue row.streakEndRewardValue,
      Se.Set Beam.streakEndRewardExpirationAt row.streakEndRewardExpirationAt,
      Se.Set Beam.streakEndRewardMetadata row.streakEndRewardMetadata,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId row.id)]
  pure row

findByMerchantOperatingCityIdAndCohortIdAndJourneyId ::
  (BeamFlow m r) =>
  Id Common.MerchantOperatingCity ->
  Id DCD.CohortDetails ->
  Id DIJ.IncentiveJourney ->
  m (Maybe DCJM.CohortJourneyMapping)
findByMerchantOperatingCityIdAndCohortIdAndJourneyId merchantOperatingCityId cohortId journeyId =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
          Se.Is Beam.cohortId $ Se.Eq (getId cohortId),
          Se.Is Beam.journeyId $ Se.Eq (getId journeyId)
        ]
    ]

-- | Create or update window + streak-end reward for (city, cohortId, journeyId).
upsertCohortJourneyMapping ::
  (BeamFlow m r) =>
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  Id DCD.CohortDetails ->
  Id DIJ.IncentiveJourney ->
  UTCTime ->
  Int ->
  Bool ->
  Maybe Int ->
  Maybe Common.MilestoneRewardType ->
  Maybe Int ->
  Maybe Int ->
  Maybe Value ->
  m DCJM.CohortJourneyMapping
upsertCohortJourneyMapping merchantId merchantOperatingCityId cohortId journeyId startDate streakRange enabled maxWaiveOffCount streakEndRewardType streakEndRewardValue streakEndRewardExpirationAt streakEndRewardMetadata = do
  mbExisting <- findByMerchantOperatingCityIdAndCohortIdAndJourneyId merchantOperatingCityId cohortId journeyId
  case mbExisting of
    Nothing ->
      createCohortJourneyMapping merchantId merchantOperatingCityId cohortId journeyId startDate streakRange enabled maxWaiveOffCount streakEndRewardType streakEndRewardValue streakEndRewardExpirationAt streakEndRewardMetadata
    Just existing ->
      updateCohortJourneyMappingFields
        existing
          { DCJM.merchantId = merchantId,
            DCJM.merchantOperatingCityId = merchantOperatingCityId,
            DCJM.startDate = startDate,
            DCJM.streakRange = streakRange,
            DCJM.enabled = enabled,
            DCJM.maxWaiveOffCount = maxWaiveOffCount,
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

findByMerchantIdAndCohortId ::
  (BeamFlow m r) =>
  Maybe Int ->
  Maybe Int ->
  Id Common.Merchant ->
  Id DCD.CohortDetails ->
  m [DCJM.CohortJourneyMapping]
findByMerchantIdAndCohortId limit offset merchantId cohortId =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (getId merchantId),
          Se.Is Beam.cohortId $ Se.Eq (getId cohortId)
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findByMerchantId ::
  (BeamFlow m r) =>
  Maybe Int ->
  Maybe Int ->
  Id Common.Merchant ->
  m [DCJM.CohortJourneyMapping]
findByMerchantId limit offset merchantId =
  findAllWithOptionsKV
    [Se.Is Beam.merchantId $ Se.Eq (getId merchantId)]
    (Se.Desc Beam.createdAt)
    limit
    offset

findByMerchantOperatingCityIdWithOptionalFilters ::
  (BeamFlow m r) =>
  Maybe Int ->
  Maybe Int ->
  Id Common.MerchantOperatingCity ->
  Maybe [Id DCD.CohortDetails] ->
  Maybe [Id DIJ.IncentiveJourney] ->
  Maybe Bool ->
  m [DCJM.CohortJourneyMapping]
findByMerchantOperatingCityIdWithOptionalFilters limit offset merchantOpCityId mbCohortIds mbJourneyIds mbEnabled =
  case (mbCohortIds, mbJourneyIds) of
    (Just [], _) -> pure []
    (_, Just []) -> pure []
    _ ->
      let cityClause = Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOpCityId)
          cohortClause = case mbCohortIds of
            Just ids -> Just (Se.Is Beam.cohortId $ Se.In (map getId ids))
            Nothing -> Nothing
          journeyClause = case mbJourneyIds of
            Just ids -> Just (Se.Is Beam.journeyId $ Se.In (map getId ids))
            Nothing -> Nothing
          enabledClause = case mbEnabled of
            Just en -> Just (Se.Is Beam.enabled $ Se.Eq en)
            Nothing -> Nothing
          clauses = cityClause : catMaybes [cohortClause, journeyClause, enabledClause]
          whereClause = case clauses of
            [c] -> [c]
            cs -> [Se.And cs]
       in findAllWithOptionsKV whereClause (Se.Desc Beam.createdAt) limit offset
