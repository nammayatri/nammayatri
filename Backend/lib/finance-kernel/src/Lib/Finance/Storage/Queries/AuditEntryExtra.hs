{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.AuditEntryExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Lib.Finance.Domain.Types.AuditEntry as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.AuditEntry as Beam
import Lib.Finance.Storage.Queries.AuditEntry ()
import qualified Sequelize as Se

-- | Paginated audit entries with optional filters
findAllWithFilters ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Maybe Text -> -- entityType filter
  Maybe Domain.AuditAction -> -- action filter
  Maybe Text -> -- actorType filter
  Maybe UTCTime -> -- dateFrom
  Maybe UTCTime -> -- dateTo
  Int -> -- limit
  Int -> -- offset
  m [Domain.AuditEntry]
findAllWithFilters merchantId mbEntityType mbAction mbActorType mbDateFrom mbDateTo limit offset =
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is Beam.merchantId $ Se.Eq merchantId]
          <> maybe [] (\et -> [Se.Is Beam.entityType $ Se.Eq et]) mbEntityType
          <> maybe [] (\a -> [Se.Is Beam.action $ Se.Eq a]) mbAction
          <> maybe [] (\at -> [Se.Is Beam.actorType $ Se.Eq at]) mbActorType
          <> maybe [] (\df -> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq df]) mbDateFrom
          <> maybe [] (\dt -> [Se.Is Beam.createdAt $ Se.LessThanOrEq dt]) mbDateTo
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

-- | Count audit entries with optional filters
countWithFilters ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Maybe Text -> -- entityType filter
  Maybe Domain.AuditAction -> -- action filter
  Maybe Text -> -- actorType filter
  Maybe UTCTime -> -- dateFrom
  Maybe UTCTime -> -- dateTo
  m Int
countWithFilters merchantId mbEntityType mbAction mbActorType mbDateFrom mbDateTo = do
  entries <-
    findAllWithKV
      [ Se.And $
          [Se.Is Beam.merchantId $ Se.Eq merchantId]
            <> maybe [] (\et -> [Se.Is Beam.entityType $ Se.Eq et]) mbEntityType
            <> maybe [] (\a -> [Se.Is Beam.action $ Se.Eq a]) mbAction
            <> maybe [] (\at -> [Se.Is Beam.actorType $ Se.Eq at]) mbActorType
            <> maybe [] (\df -> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq df]) mbDateFrom
            <> maybe [] (\dt -> [Se.Is Beam.createdAt $ Se.LessThanOrEq dt]) mbDateTo
      ]
  pure $ length (entries :: [Domain.AuditEntry])

-- | Find admin-specific audit entries (actorType = "ADMIN")
findAdminActions ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Maybe UTCTime -> -- dateFrom
  Maybe UTCTime -> -- dateTo
  Int -> -- limit
  Int -> -- offset
  m [Domain.AuditEntry]
findAdminActions merchantId mbDateFrom mbDateTo limit offset =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.actorType $ Se.Eq ("ADMIN" :: Text)
        ]
          <> maybe [] (\df -> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq df]) mbDateFrom
          <> maybe [] (\dt -> [Se.Is Beam.createdAt $ Se.LessThanOrEq dt]) mbDateTo
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

-- | Find audit entries by entity ID with text search
findByEntityIdSearch ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Text -> -- entityId (exact match)
  m [Domain.AuditEntry]
findByEntityIdSearch merchantId entityId =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.entityId $ Se.Eq entityId
        ]
    ]

-- | Find audit entries within a date range for a merchant
findByDateRange ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  UTCTime -> -- from
  UTCTime -> -- to
  m [Domain.AuditEntry]
findByDateRange merchantId dateFrom dateTo =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq dateFrom,
          Se.Is Beam.createdAt $ Se.LessThanOrEq dateTo
        ]
    ]
