{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.IncentiveJourney.Storage.Queries.UserCohortMappingExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, logWarning)
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.UserCohortMapping as DUCM
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Beam.UserCohortMapping as Beam
import Lib.IncentiveJourney.Storage.Queries.OrphanInstances.UserCohortMapping ()
import qualified Sequelize as Se

-- | Create or update enabled / validTill for (userId, cohortJourneyMappingId).
-- Opt-in fields are left unchanged on update; new rows default to not opted in
-- (Assigned cohorts ignore optedInAt; OptIn cohorts require a later optIn write).
upsertUserCohortMapping ::
  (BeamFlow m r) =>
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Bool ->
  UTCTime ->
  m DUCM.UserCohortMapping
upsertUserCohortMapping userId cohortMappingId enabled validTill = do
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
                enabled = enabled,
                validTill = validTill,
                optInPeriod = Nothing,
                optedInAt = Nothing,
                optedInBy = Nothing,
                termsAcceptedAt = Nothing,
                createdAt = now,
                updatedAt = now
              }
      createWithKV row
      pure row
    Just existing -> do
      let updated =
            existing
              { DUCM.enabled = enabled,
                DUCM.validTill = validTill,
                DUCM.updatedAt = now
              }
      updateWithKV
        [ Se.Set Beam.enabled updated.enabled,
          Se.Set Beam.validTill updated.validTill,
          Se.Set Beam.updatedAt now
        ]
        [Se.Is Beam.id $ Se.Eq (getId existing.id)]
      pure updated

-- | Mark an invite row as opted in for the current period.
-- membershipValidTill / optedInAt / termsAcceptedAt must be city-local wall-clock (same as UCM.validTill).
-- Do not shrink invite validTill below the existing invite end — period activity uses optedInAt + optInPeriod.
markUserCohortMappingOptedIn ::
  (BeamFlow m r) =>
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  DCJM.OptInPeriod ->
  UTCTime -> -- req / invite validTill (membership stays max with existing)
  UTCTime -> -- city-local optedInAt
  Text ->
  m DUCM.UserCohortMapping
markUserCohortMappingOptedIn userId cohortMappingId optInPeriod reqValidTill localOptedInAt optedInBy = do
  now <- getCurrentTime
  existing <-
    findOneWithKV
      [ Se.And
          [ Se.Is Beam.userId $ Se.Eq (getId userId),
            Se.Is Beam.cohortMappingId $ Se.Eq (getId cohortMappingId)
          ]
      ]
      >>= fromMaybeM (InvalidRequest "User cohort mapping invite not found; assign/invite first")
  let membershipValidTill = max existing.validTill reqValidTill
      updated =
        existing
          { DUCM.enabled = True,
            DUCM.validTill = membershipValidTill,
            DUCM.optInPeriod = Just optInPeriod,
            DUCM.optedInAt = Just localOptedInAt,
            DUCM.optedInBy = Just optedInBy,
            DUCM.termsAcceptedAt = Just localOptedInAt,
            DUCM.updatedAt = now
          }
  updateWithKV
    [ Se.Set Beam.enabled True,
      Se.Set Beam.validTill membershipValidTill,
      Se.Set Beam.optInPeriod (Just optInPeriod),
      Se.Set Beam.optedInAt (Just localOptedInAt),
      Se.Set Beam.optedInBy (Just optedInBy),
      Se.Set Beam.termsAcceptedAt (Just localOptedInAt),
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId existing.id)]
  pure updated

-- | Active UCMs: enabled and validTill >= now.
-- Pass city-local now (getLocalCurrentTime); validTill is stored as local wall-clock UTCTime.
findActiveByUserId ::
  (BeamFlow m r) =>
  Id Common.Person ->
  UTCTime ->
  m [DUCM.UserCohortMapping]
findActiveByUserId userId now =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.userId $ Se.Eq (getId userId),
          Se.Is Beam.enabled $ Se.Eq True,
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
insertUserCohortMappingIgnoringConflict userId cohortMappingId enabled validTill = do
  now <- getCurrentTime
  rowId <- generateGUID
  let row =
        DUCM.UserCohortMapping
          { id = rowId,
            userId = userId,
            cohortMappingId = cohortMappingId,
            enabled = enabled,
            validTill = validTill,
            optInPeriod = Nothing,
            optedInAt = Nothing,
            optedInBy = Nothing,
            termsAcceptedAt = Nothing,
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

-- | Unassign: disable invite and clear opt-in so re-invite is Pending (fresh opt-in required).
disableUserCohortMapping ::
  (BeamFlow m r) =>
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  m ()
disableUserCohortMapping userId cohortMappingId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.enabled False,
      Se.Set Beam.optInPeriod Nothing,
      Se.Set Beam.optedInAt Nothing,
      Se.Set Beam.optedInBy Nothing,
      Se.Set Beam.termsAcceptedAt Nothing,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.userId $ Se.Eq (getId userId),
          Se.Is Beam.cohortMappingId $ Se.Eq (getId cohortMappingId)
        ]
    ]

-- | OptIn campaigns: clear opt-in for this period but keep the invite row enabled
-- so the driver can opt in again next period. Assignment filter requires period-active optedInAt.
clearOptInUserCohortMapping ::
  (BeamFlow m r) =>
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  m ()
clearOptInUserCohortMapping userId cohortMappingId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.optInPeriod Nothing,
      Se.Set Beam.optedInAt Nothing,
      Se.Set Beam.optedInBy Nothing,
      Se.Set Beam.termsAcceptedAt Nothing,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.userId $ Se.Eq (getId userId),
          Se.Is Beam.cohortMappingId $ Se.Eq (getId cohortMappingId)
        ]
    ]

-- | Clear opt-in only if stored optedInAt still equals the stamp classified as stale.
-- Returns True when no live opt-in remains after the attempt (cleared here, or already
-- cleared). Returns False when a newer opt-in won the race — caller must not drop membership.
clearOptInUserCohortMappingIfOptedInAt ::
  (BeamFlow m r) =>
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  UTCTime ->
  m Bool
clearOptInUserCohortMappingIfOptedInAt userId cohortMappingId expectedOptedInAt = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.optInPeriod Nothing,
      Se.Set Beam.optedInAt Nothing,
      Se.Set Beam.optedInBy Nothing,
      Se.Set Beam.termsAcceptedAt Nothing,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.userId $ Se.Eq (getId userId),
          Se.Is Beam.cohortMappingId $ Se.Eq (getId cohortMappingId),
          Se.Is Beam.optedInAt $ Se.Eq (Just expectedOptedInAt)
        ]
    ]
  mbUcm <-
    findOneWithKV
      [ Se.And
          [ Se.Is Beam.userId $ Se.Eq (getId userId),
            Se.Is Beam.cohortMappingId $ Se.Eq (getId cohortMappingId)
          ]
      ]
  pure $ maybe True (isNothing . (.optedInAt)) mbUcm
