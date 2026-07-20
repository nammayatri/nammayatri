module Lib.Finance.Storage.Queries.ReconciliationEntryExtra
  ( findBySummaryIdWithPagination,
    findBySpecAndSourceIds,
    findByNaturalKeys,
    findSourceDerivedByMatchKeys,
    findOpenBySpecScope,
    upsertReconEntries,
  )
where

import qualified Data.HashMap.Strict as HM
import Data.List (partition)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, generateGUID, getCurrentTime)
import qualified Lib.Finance.Domain.Types.ReconciliationEntry as Domain
import qualified Lib.Finance.Domain.Types.ReconciliationSummary as DomainSummary
import qualified Lib.Finance.Reconciliation.Types as ReconT
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.ReconciliationEntry as Beam
import Lib.Finance.Storage.Queries.ReconciliationEntry ()
import qualified Lib.Finance.Storage.Queries.ReconciliationEntry as QReconEntry
import qualified Sequelize as Se

findBySummaryIdWithPagination ::
  (BeamFlow m r) =>
  Id DomainSummary.ReconciliationSummary ->
  Int -> -- limit
  Int -> -- offset
  m [Domain.ReconciliationEntry]
findBySummaryIdWithPagination summaryId limit offset =
  findAllWithOptionsKV
    [Se.Is Beam.summaryId $ Se.Eq (getId summaryId)]
    (Se.Asc Beam.createdAt)
    (Just limit)
    (Just offset)

-- | Bulk fetch entries for a (spec, sourceRecordId set). Used by the
--   settlement-list dashboard endpoint to attach per-settlement recon
--   status / date / variance without an N-fetch loop.
--
--   Callers typically pick the latest entry per source id application-side
--   (order by reconciliationDate DESC, then updatedAt DESC) — no ORDER BY
--   here so the query stays index-friendly.
findBySpecAndSourceIds ::
  (BeamFlow m r) =>
  ReconT.Domain ->
  ReconT.DataSource ->
  ReconT.DataSource ->
  [Text] -> -- source record ids
  m [Domain.ReconciliationEntry]
findBySpecAndSourceIds _ _ _ [] = pure []
findBySpecAndSourceIds domain source target srcIds =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.source $ Se.Eq source,
          Se.Is Beam.target $ Se.Eq target,
          Se.Is Beam.sourceRecordId $ Se.In (map Just srcIds)
        ]
    ]

-- | Bulk fetch existing entries by the natural key
--   @(domain, source, target, merchantId, cityId, entryKey)@. Used by the
--   upsert path (see 'upsertReconEntries') and by the sweep to hydrate
--   OPEN entries before re-classification.
findByNaturalKeys ::
  (BeamFlow m r) =>
  ReconT.Domain ->
  ReconT.DataSource ->
  ReconT.DataSource ->
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  [Text] -> -- entryKeys
  m [Domain.ReconciliationEntry]
findByNaturalKeys _ _ _ _ _ [] = pure []
findByNaturalKeys domain source target merchantId opCityId keys =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.source $ Se.Eq source,
          Se.Is Beam.target $ Se.Eq target,
          Se.Is Beam.merchantId $ Se.Eq (Just merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just opCityId),
          Se.Is Beam.entryKey $ Se.In keys
        ]
    ]

-- | Source-derived entries (sourceRecordId IS NOT NULL) whose
--   @groupTargetKey@ falls in the given set. The runner populates
--   'groupTargetKey' with @srcMatchKey@ uniformly (Individual and
--   GroupByTargetKey), so this is how the B2 sweep looks up whether a
--   source-side twin exists for an orphan target — matching on match key,
--   not on entry key, because for recipes where @srcId /= srcMatchKey@
--   (e.g. postpaid @driver_fee.id@ vs @payment_order.id@) the entry key
--   wouldn't help.
findSourceDerivedByMatchKeys ::
  (BeamFlow m r) =>
  ReconT.Domain ->
  ReconT.DataSource ->
  ReconT.DataSource ->
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  [Text] -> -- match keys (groupTargetKey values)
  m [Domain.ReconciliationEntry]
findSourceDerivedByMatchKeys _ _ _ _ _ [] = pure []
findSourceDerivedByMatchKeys domain source target merchantId opCityId keys =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.source $ Se.Eq source,
          Se.Is Beam.target $ Se.Eq target,
          Se.Is Beam.merchantId $ Se.Eq (Just merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just opCityId),
          Se.Is Beam.groupTargetKey $ Se.In (map Just keys),
          -- Filter to source-derived so orphans (sourceRecordId IS NULL,
          -- also stored with groupTargetKey = tgtMatchKey) don't match
          -- themselves and their siblings as "twins."
          Se.Is Beam.sourceRecordId $ Se.Not $ Se.Eq Nothing
        ]
    ]

-- | The B2 sweep's working set: OPEN entries for a given (spec, scope),
--   oldest first. Served by the partial index on @open@ so the sweep
--   read cost stays proportional to outstanding work.
findOpenBySpecScope ::
  (BeamFlow m r) =>
  ReconT.Domain ->
  ReconT.DataSource ->
  ReconT.DataSource ->
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  Int -> -- limit
  m [Domain.ReconciliationEntry]
findOpenBySpecScope domain source target merchantId opCityId limit =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.source $ Se.Eq source,
          Se.Is Beam.target $ Se.Eq target,
          Se.Is Beam.merchantId $ Se.Eq (Just merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just opCityId),
          Se.Is Beam.open $ Se.Eq True
        ]
    ]
    (Se.Asc Beam.firstSeenAt)
    (Just limit)
    Nothing

-- | Insert-or-update on the natural key. The runner supplies fresh
--   'ReconciliationEntry' values populated by 'mkEntry'\/'mkOrphanEntry'
--   with @id = ""@ as a placeholder; this helper looks each one up by
--   @entryKey@ (grouped per spec\/scope for a single indexed query), then:
--
--     * generates a fresh UUID for @id@ on insert; preserves @old.id@ on
--       update, so external references (audit rows, dashboard links) stay
--       stable across sweep re-classifications
--     * preserves @firstSeenAt@ from the existing row on update
--     * sets @resolvedAt@ to now on the OPEN→CLOSED transition, preserves
--       it when both sides are already CLOSED, clears when going back OPEN
--     * always refreshes @updatedAt@
--
--   Idempotent by construction — safe to replay after a crash.
upsertReconEntries ::
  (BeamFlow m r, MonadFlow m) =>
  [Domain.ReconciliationEntry] ->
  m ()
upsertReconEntries [] = pure ()
upsertReconEntries newEntries = do
  now <- getCurrentTime
  -- Assume all entries in one call share the same (spec, scope) — the
  -- runner always passes a single chunk's worth. Cheap to enforce.
  let e0 = head newEntries
      merchantId = fromMaybe "" e0.merchantId
      opCityId = fromMaybe "" e0.merchantOperatingCityId
      keys = map (.entryKey) newEntries
  existing <-
    findByNaturalKeys
      e0.domain
      e0.source
      e0.target
      merchantId
      opCityId
      keys
  let existingByKey :: HM.HashMap Text Domain.ReconciliationEntry
      existingByKey = HM.fromList [(e.entryKey, e) | e <- existing]

  -- Monadic because inserts need a fresh UUID from generateGUID. Runs
  -- once per entry, bounded by chunk / sweep-batch size — no round-trip
  -- amplification.
  merged <- forM newEntries $ \new -> case HM.lookup new.entryKey existingByKey of
    Nothing -> do
      freshId <- generateGUID
      pure
        new{Domain.id = Id freshId,
            Domain.resolvedAt = if new.open then Nothing else Just now,
            Domain.updatedAt = now
           }
    Just old ->
      pure
        new{Domain.id = old.id,
            Domain.firstSeenAt = old.firstSeenAt,
            Domain.resolvedAt = case (old.open, new.open) of
              (True, False) -> Just now -- transitioning OPEN→CLOSED
              (False, False) -> old.resolvedAt -- both closed, preserve
              _ -> Nothing, -- OPEN now: clear any prior resolution
            Domain.updatedAt = now
           }

  let (toUpdate, toInsert) = partition (\e -> HM.member e.entryKey existingByKey) merged

  -- Two bulk operations per call. In steady state most calls land
  -- almost entirely in one bucket or the other, so the other stays a
  -- no-op.
  mapM_ upsertOne toUpdate
  unless (null toInsert) $ QReconEntry.createMany toInsert
  where
    -- Row-level update by PK. Beam-KV bulk update by predicate isn't
    -- expressive enough here (per-row divergent field values), so we take
    -- the per-row cost — the batch is bounded by the chunk size.
    upsertOne e =
      updateWithKV
        [ Se.Set Beam.summaryId (getId e.summaryId),
          Se.Set Beam.reconciliationDate e.reconciliationDate,
          Se.Set Beam.expectedAmount e.expectedAmount,
          Se.Set Beam.actualAmount e.actualAmount,
          Se.Set Beam.variance e.variance,
          Se.Set Beam.reconStatus e.reconStatus,
          Se.Set Beam.mismatchReason e.mismatchReason,
          Se.Set Beam.entityId e.entityId,
          Se.Set Beam.partyId e.partyId,
          Se.Set Beam.sourceRecordId e.sourceRecordId,
          Se.Set Beam.targetRecordId e.targetRecordId,
          Se.Set Beam.component e.component,
          Se.Set Beam.entityMeta e.entityMeta,
          Se.Set Beam.settlementId e.settlementId,
          Se.Set Beam.settlementDate e.settlementDate,
          Se.Set Beam.settlementMode e.settlementMode,
          Se.Set Beam.transactionDate e.transactionDate,
          Se.Set Beam.rrn e.rrn,
          Se.Set Beam.groupTargetKey e.groupTargetKey,
          Se.Set Beam.groupSourceTotal e.groupSourceTotal,
          Se.Set Beam.groupTargetAmount e.groupTargetAmount,
          Se.Set Beam.sourceLifecycle e.sourceLifecycle,
          Se.Set Beam.open e.open,
          Se.Set Beam.resolvedAt e.resolvedAt,
          Se.Set Beam.closeReason e.closeReason,
          Se.Set Beam.updatedAt e.updatedAt
        ]
        [Se.Is Beam.id $ Se.Eq (getId e.id)]
