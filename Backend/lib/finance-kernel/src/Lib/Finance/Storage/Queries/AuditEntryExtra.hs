{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Finance.Storage.Queries.AuditEntryExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Finance.Audit as AuditTypes
import qualified Lib.Finance.Domain.Types.AuditEntry as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.AuditEntry as Beam
import Lib.Finance.Storage.Queries.AuditEntry ()
import qualified Sequelize as Se

-- | Paginated audit entries with optional filters
findAllWithFilters ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Maybe AuditTypes.AuditEntityType -> -- entityType filter
  Maybe AuditTypes.AuditAction -> -- action filter
  Maybe AuditTypes.AuditActorType -> -- actorType filter
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

-- | Count audit entries with optional filters.
--   Uses findAllWithOptionsKV with a large limit to avoid unbounded row loading,
--   since the KV connector does not expose a native SQL COUNT.
--   TODO: Replace with a proper SQL COUNT aggregation when countWithKV is available.
countWithFilters ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Maybe AuditTypes.AuditEntityType -> -- entityType filter
  Maybe AuditTypes.AuditAction -> -- action filter
  Maybe AuditTypes.AuditActorType -> -- actorType filter
  Maybe UTCTime -> -- dateFrom
  Maybe UTCTime -> -- dateTo
  m Int
countWithFilters merchantId mbEntityType mbAction mbActorType mbDateFrom mbDateTo = do
  let maxCountLimit = 10000 -- Cap to prevent OOM; callers should use pagination
  entries <-
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
      (Just maxCountLimit)
      Nothing
  pure $ length (entries :: [Domain.AuditEntry])

-- | Find admin-specific audit entries (actorType = AdminUser)
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
          Se.Is Beam.actorType $ Se.Eq AuditTypes.AdminUser
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

-- | Find audit entries within a date range for a merchant (with pagination).
--   Uses a default limit of 1000 and offset of 0 to prevent unbounded result sets.
findByDateRange ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  UTCTime -> -- from
  UTCTime -> -- to
  Int -> -- limit
  Int -> -- offset
  m [Domain.AuditEntry]
findByDateRange merchantId dateFrom dateTo limit offset =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq dateFrom,
          Se.Is Beam.createdAt $ Se.LessThanOrEq dateTo
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

-- | Find the latest audit entry for an entity (used for hash chain computation)
findLatestForEntity ::
  (BeamFlow m r) =>
  AuditTypes.AuditEntityType -> -- entityType
  Text -> -- entityId
  m (Maybe Domain.AuditEntry)
findLatestForEntity entityType entityId = do
  entries <-
    findAllWithOptionsKV
      [ Se.And
          [ Se.Is Beam.entityType $ Se.Eq entityType,
            Se.Is Beam.entityId $ Se.Eq entityId
          ]
      ]
      (Se.Desc Beam.createdAt)
      (Just 1)
      Nothing
  pure $ listToMaybe entries
