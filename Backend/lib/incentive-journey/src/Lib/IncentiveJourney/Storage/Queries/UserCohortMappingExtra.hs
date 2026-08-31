{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.IncentiveJourney.Storage.Queries.UserCohortMappingExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (generateGUID, getCurrentTime, logWarning)
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.UserCohortMapping as DUCM
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Beam.UserCohortMapping as Beam
import Lib.IncentiveJourney.Storage.Queries.OrphanInstances.UserCohortMapping ()
import qualified Sequelize as Se

-- | Create or update isTestGroup / validTill for (userId, cohortJourneyMappingId).
upsertUserCohortMapping ::
  (BeamFlow m r) =>
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Bool ->
  UTCTime ->
  m DUCM.UserCohortMapping
upsertUserCohortMapping userId cohortMappingId isTestGroup validTill = do
  now <- getCurrentTime
  mbExisting <-
    findOneWithKV
      [ Se.And
          [ Se.Is Beam.userId $ Se.Eq (getId userId),
            Se.Is Beam.cohortMappingId $ Se.Eq (getId cohortMappingId)
          ]
      ]
  case mbExisting of
    Nothing -> do
      rowId <- generateGUID
      let row =
            DUCM.UserCohortMapping
              { id = rowId,
                userId = userId,
                cohortMappingId = cohortMappingId,
                isTestGroup = Just isTestGroup,
                validTill = validTill,
                createdAt = now,
                updatedAt = now
              }
      createWithKV row
      pure row
    Just existing -> do
      let updated =
            existing
              { DUCM.isTestGroup = Just isTestGroup,
                DUCM.validTill = validTill,
                DUCM.updatedAt = now
              }
      updateWithKV
        [ Se.Set Beam.isTestGroup updated.isTestGroup,
          Se.Set Beam.validTill updated.validTill,
          Se.Set Beam.updatedAt now
        ]
        [Se.Is Beam.id $ Se.Eq (getId existing.id)]
      pure updated

findActiveByUserId ::
  (BeamFlow m r) =>
  Id Common.Person ->
  m [DUCM.UserCohortMapping]
findActiveByUserId userId = do
  now <- getCurrentTime
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.userId $ Se.Eq (getId userId),
          Se.Is Beam.validTill $ Se.GreaterThanOrEq now
        ]
    ]

insertUserCohortMappingIgnoringConflict ::
  (BeamFlow m r) =>
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Bool ->
  UTCTime ->
  m (Maybe (Id Common.Person))
insertUserCohortMappingIgnoringConflict userId cohortMappingId isTestGroup validTill = do
  now <- getCurrentTime
  rowId <- generateGUID
  let row =
        DUCM.UserCohortMapping
          { id = rowId,
            userId = userId,
            cohortMappingId = cohortMappingId,
            isTestGroup = Just isTestGroup,
            validTill = validTill,
            createdAt = now,
            updatedAt = now
          }
  result <- try @_ @SomeException $ createWithKV row
  case result of
    Right _ -> pure (Just userId)
    Left err -> do
      logWarning $
        "user_cohort_mapping insert skipped userId="
          <> getId userId
          <> " cohortMappingId="
          <> getId cohortMappingId
          <> " err="
          <> show err
      pure Nothing

isTestGroupOrFalse :: DUCM.UserCohortMapping -> Bool
isTestGroupOrFalse mapping = fromMaybe False mapping.isTestGroup

deleteUserCohortMapping ::
  (BeamFlow m r) =>
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  m ()
deleteUserCohortMapping userId cohortMappingId =
  deleteWithKV
    [ Se.And
        [ Se.Is Beam.userId $ Se.Eq (getId userId),
          Se.Is Beam.cohortMappingId $ Se.Eq (getId cohortMappingId)
        ]
    ]
