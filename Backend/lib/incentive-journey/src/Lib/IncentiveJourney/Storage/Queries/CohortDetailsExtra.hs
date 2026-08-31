{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.IncentiveJourney.Storage.Queries.CohortDetailsExtra where

import Data.Aeson (Value)
import qualified Data.Text as T
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (generateGUID, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.CohortDetails as DCD
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Beam.CohortDetails as Beam
import Lib.IncentiveJourney.Storage.Queries.OrphanInstances.CohortDetails ()
import qualified Sequelize as Se

-- | Create a cohort row. Uniqueness of (category, name) is enforced in DB
-- (@extraIndexes@) and checked here for a clearer API error.
createCohortDetails ::
  (BeamFlow m r) =>
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Value ->
  m DCD.CohortDetails
createCohortDetails name mbCategory mbDescription mbCohortRule = do
  now <- getCurrentTime
  cohortId <- generateGUID
  let row =
        DCD.CohortDetails
          { id = cohortId,
            name = name,
            category = mbCategory,
            description = mbDescription,
            cohortRule = mbCohortRule,
            createdAt = now,
            updatedAt = now
          }
  createWithKV row
  pure row

findCohortDetailsById :: (BeamFlow m r) => Id DCD.CohortDetails -> m (Maybe DCD.CohortDetails)
findCohortDetailsById cohortId =
  findOneWithKV [Se.Is Beam.id $ Se.Eq (getId cohortId)]

findByNameAndCategory ::
  (BeamFlow m r) =>
  Text ->
  Maybe Text ->
  m (Maybe DCD.CohortDetails)
findByNameAndCategory name mbCategory =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.name $ Se.Eq name,
          Se.Is Beam.category $ Se.Eq mbCategory
        ]
    ]

findByIds :: (BeamFlow m r) => [Id DCD.CohortDetails] -> m [DCD.CohortDetails]
findByIds ids
  | null ids = pure []
  | otherwise =
    findAllWithKV
      [Se.Is Beam.id $ Se.In (map getId ids)]

escapeLikeLiteral :: Text -> Text
escapeLikeLiteral = T.concatMap $ \c -> case c of
  '%' -> "\\%"
  '_' -> "\\_"
  '\\' -> "\\\\"
  _ -> T.singleton c

-- | List cohorts with optional name substring (SQL LIKE) and/or exact category (AND when both set).
listWithOptionalNameAndCategory ::
  (BeamFlow m r) =>
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  m [DCD.CohortDetails]
listWithOptionalNameAndCategory mbLimit mbOffset mbCohortName mbCategory =
  findByOptionalNameAndCategory (Just $ fromMaybe 20 mbLimit) (Just $ fromMaybe 0 mbOffset) mbCohortName mbCategory

-- | Unpaginated match for use as a foreign-key filter (name LIKE and/or category).
findMatchingByOptionalNameAndCategory ::
  (BeamFlow m r) =>
  Maybe Text ->
  Maybe Text ->
  m [DCD.CohortDetails]
findMatchingByOptionalNameAndCategory =
  findByOptionalNameAndCategory Nothing Nothing

findByOptionalNameAndCategory ::
  (BeamFlow m r) =>
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  m [DCD.CohortDetails]
findByOptionalNameAndCategory mbLimit mbOffset mbCohortName mbCategory = do
  let nameClause = case T.strip <$> mbCohortName of
        Just name
          | not (T.null name) -> Just (Se.Is Beam.name $ Se.Like ("%" <> escapeLikeLiteral name <> "%"))
        _ -> Nothing
      categoryClause = case T.strip <$> mbCategory of
        Just cat
          | not (T.null cat) -> Just (Se.Is Beam.category $ Se.Eq (Just cat))
        _ -> Nothing
      clauses = catMaybes [nameClause, categoryClause]
      whereClause = case clauses of
        [] -> [Se.Is Beam.id $ Se.Not $ Se.Eq ""]
        [c] -> [c]
        cs -> [Se.And cs]
  case nameClause of
    Just _ -> findAllWithOptionsDb whereClause (Se.Desc Beam.createdAt) mbLimit mbOffset
    Nothing -> findAllWithOptionsKV whereClause (Se.Desc Beam.createdAt) mbLimit mbOffset
