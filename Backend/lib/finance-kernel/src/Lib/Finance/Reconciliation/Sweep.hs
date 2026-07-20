{-
  Lib.Finance.Reconciliation.Sweep

  The B2 sweep: re-checks OPEN entries so a reconciliation result stops
  being permanent. When a source record changes after we reconciled it,
  the stored verdict is corrected rather than left stale.

  Three passes per fire, all against the OPEN pool loaded via
  'findOpenBySpecScope':

    1. Orphan resolution — for entries with @sourceRecordId = Nothing@,
       look up whether a source-side twin (an entry keyed by the same
       underlying entity id) now exists. If yes the orphan is redundant:
       close it with @closeReason = "RESOLVED_BY_SOURCE"@. Runs regardless
       of age so an old orphan doesn't get force-closed as AGED_OUT when a
       cleaner resolution is available.

    2. Force-close on age — anything not resolved above and older than
       'maxOpenAge' is force-closed with @closeReason = "AGED_OUT"@. The
       classifier's original verdict in @reconStatus@ is preserved intact
       so the mismatch diagnosis stays visible.

    3. Re-classify source-derived — the remaining fresh source-derived
       entries have their sources re-fetched (via 'fetchSourcesByIds'),
       their targets re-fetched (via 'fetchTargetsById') and are re-run
       through 'recipe.classify'.

  All writes go through the same 'upsertReconEntries' path the chunk
  runner uses, so identity and 'firstSeenAt' survive the re-check.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Reconciliation.Sweep
  ( SweepOutcome (..),
    runSweep,
    sweepBatchLimit,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (partition)
import Data.Time (diffUTCTime)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Common as KTC
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common (getCurrentTime, logInfo, logWarning)
import qualified Lib.Finance.Domain.Types.ReconciliationEntry as DRE
import Lib.Finance.Reconciliation.Recipe
import Lib.Finance.Reconciliation.Types
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.ReconciliationEntryExtra as QReconEntryExtra

-- ─── Outcome sent back to the app-side glue ────────────────────────────────

data SweepOutcome = SweepOutcome
  { -- | Total OPEN rows the sweep loaded this pass.
    sweepLoaded :: Int,
    -- | Orphan rows closed because a source-side twin has since been
    --   persisted. @closeReason = "RESOLVED_BY_SOURCE"@; the classifier's
    --   MISSING_IN_SOURCE verdict is preserved.
    sweepOrphanResolved :: Int,
    -- | Rows aged past 'maxOpenAge' and force-closed with
    --   @closeReason = "AGED_OUT"@. Their original mismatch verdict in
    --   @reconStatus@ is preserved.
    sweepForceClosed :: Int,
    -- | Rows re-classified. Some flip to MATCHED (in-flight source settled
    --   cleanly), others stay OPEN (still in-flight or still mismatched).
    sweepReclassified :: Int
  }
  deriving (Show)

-- | Cap on how many OPEN rows the sweep loads per pass. Bounds the memory
--   working set and DB round-trips per fire — anything not touched this
--   pass comes back around on the next @sweepInterval@ tick.
sweepBatchLimit :: Int
sweepBatchLimit = 500

-- ─── Main entry point ──────────────────────────────────────────────────────

-- | One sweep pass for a (spec, scope). Wraps the DB work in a Redis lock
--   keyed on the same (spec, scope) so two concurrent sweep workers can't
--   race on the same OPEN pool.
runSweep ::
  ( BeamFlow.BeamFlow m r,
    Hedis.HedisFlow m r
  ) =>
  Recipe m ->
  MerchantScope ->
  m SweepOutcome
runSweep recipe scope = do
  let lockKey = sweepLockKey recipe.spec scope
      lockTtlSeconds = 1800 :: Int
  lockResult <-
    Hedis.whenWithLockRedisAndReturnValue lockKey lockTtlSeconds $
      sweepBody recipe scope
  case lockResult of
    Left () -> do
      logWarning $
        "Sweep " <> specKey recipe.spec
          <> " for merchant "
          <> scope.merchantId
          <> " already running; skipping this pass."
      pure $ SweepOutcome 0 0 0 0
    Right outcome -> pure outcome
  where
    sweepLockKey spec s =
      "ReconSweepLock:"
        <> specKey spec
        <> "|"
        <> s.merchantId
        <> "|"
        <> s.merchantOperatingCityId

sweepBody ::
  (BeamFlow.BeamFlow m r) =>
  Recipe m ->
  MerchantScope ->
  m SweepOutcome
sweepBody recipe scope = do
  now <- getCurrentTime
  entries <-
    QReconEntryExtra.findOpenBySpecScope
      recipe.spec.domain
      recipe.spec.source
      recipe.spec.target
      scope.merchantId
      scope.merchantOperatingCityId
      sweepBatchLimit
  if null entries
    then pure $ SweepOutcome 0 0 0 0
    else do
      let (orphans, sourceDerived) = partition (isNothing . (.sourceRecordId)) entries

      -- Pass 1: orphans whose source-side twin now exists. Runs before the
      -- age check so a long-standing orphan closes as RESOLVED_BY_SOURCE
      -- (informative) rather than AGED_OUT (less informative) when the
      -- twin has since been persisted.
      orphanResolved <- resolveOrphans recipe scope now orphans
      let resolvedOrphanIds :: HS.HashSet Text
          resolvedOrphanIds = HS.fromList (map (KTI.getId . DRE.id) orphanResolved)
          unresolvedOrphans =
            filter (\e -> not (HS.member (KTI.getId e.id) resolvedOrphanIds)) orphans

      -- Pass 2: age-out on everything not resolved above. Orphans without
      -- twins share the same maxOpenAge as source-derived entries.
      let remaining = sourceDerived <> unresolvedOrphans
          (tooOld, freshEnough) = partition (isTooOld recipe.maxOpenAge now) remaining
          forceClosed = map (forceCloseEntry now) tooOld

      -- Pass 3: fresh source-derived get re-classified. Fresh orphans
      -- without twins stay OPEN — the next sweep will re-check for twins,
      -- giving late-arriving source-side entries a chance to resolve them.
      let freshSourceDerived = filter (isJust . (.sourceRecordId)) freshEnough
      reclassified <- reclassifySourceDerived recipe scope now freshSourceDerived

      let toWrite = orphanResolved <> forceClosed <> reclassified
      unless (null toWrite) $ QReconEntryExtra.upsertReconEntries toWrite
      logInfo $
        "Sweep " <> specKey recipe.spec
          <> " for merchant "
          <> scope.merchantId
          <> ": loaded="
          <> show (length entries)
          <> " orphanResolved="
          <> show (length orphanResolved)
          <> " forceClosed="
          <> show (length forceClosed)
          <> " reclassified="
          <> show (length reclassified)
      pure $
        SweepOutcome
          { sweepLoaded = length entries,
            sweepOrphanResolved = length orphanResolved,
            sweepForceClosed = length forceClosed,
            sweepReclassified = length reclassified
          }

-- ─── Orphan resolution (pass 1) ────────────────────────────────────────────

-- | Close orphan entries whose source-side twin has since been persisted.
--
--   The orphan's 'groupTargetKey' holds @tgtMatchKey@; a source-side twin
--   (from the same or another chunk) is any source-derived entry whose
--   'groupTargetKey' matches — which the runner populates with
--   @srcMatchKey@ uniformly.
--
--   We look up twins via 'findSourceDerivedByMatchKeys' rather than
--   'findByNaturalKeys': for recipes where @srcId /= srcMatchKey@ (the
--   postpaid many-driver-fees → one-payment-order case) the entry_key
--   would never match the orphan's match key. This lookup joins on the
--   right column.
--
--   The classifier's MISSING_IN_SOURCE verdict is preserved on the orphan;
--   @closeReason = "RESOLVED_BY_SOURCE"@ is the only signal that a twin
--   was discovered. Dashboards can distinguish "this orphan is redundant
--   because we've reconciled it from the source side" from "we gave up on
--   this orphan after maxOpenAge."
resolveOrphans ::
  (BeamFlow.BeamFlow m r) =>
  Recipe m ->
  MerchantScope ->
  UTCTime ->
  [DRE.ReconciliationEntry] ->
  m [DRE.ReconciliationEntry]
resolveOrphans _ _ _ [] = pure []
resolveOrphans recipe scope now orphans = do
  let candidateKeys = mapMaybe (.groupTargetKey) orphans
  if null candidateKeys
    then pure []
    else do
      twins <-
        QReconEntryExtra.findSourceDerivedByMatchKeys
          recipe.spec.domain
          recipe.spec.source
          recipe.spec.target
          scope.merchantId
          scope.merchantOperatingCityId
          candidateKeys
      let twinMatchKeys :: HS.HashSet Text
          twinMatchKeys = HS.fromList (mapMaybe (.groupTargetKey) twins)
      pure
        [ orphan
            { DRE.open = False,
              DRE.resolvedAt = Just now,
              DRE.closeReason = Just "RESOLVED_BY_SOURCE",
              DRE.updatedAt = now
            }
          | orphan <- orphans,
            Just key <- [orphan.groupTargetKey],
            HS.member key twinMatchKeys
        ]

-- ─── Force-close (pass 2) ──────────────────────────────────────────────────

isTooOld :: NominalDiffTime -> UTCTime -> DRE.ReconciliationEntry -> Bool
isTooOld maxAge now e = diffUTCTime now e.firstSeenAt > maxAge

-- | Force-close preserves the classifier's original verdict — 'reconStatus'
--   and 'mismatchReason' are the diagnosis of what was wrong, which stays
--   useful for triage long after the sweep gives up. Only 'closeReason'
--   marks that automation surrendered.
forceCloseEntry :: UTCTime -> DRE.ReconciliationEntry -> DRE.ReconciliationEntry
forceCloseEntry now e =
  e
    { DRE.open = False,
      DRE.resolvedAt = Just now,
      DRE.closeReason = Just "AGED_OUT",
      DRE.updatedAt = now
    }

-- ─── Re-classification of fresh source-derived entries (pass 3) ────────────

reclassifySourceDerived ::
  (BeamFlow.BeamFlow m r) =>
  Recipe m ->
  MerchantScope ->
  UTCTime ->
  [DRE.ReconciliationEntry] ->
  m [DRE.ReconciliationEntry]
reclassifySourceDerived _ _ _ [] = pure []
reclassifySourceDerived recipe scope now entries = do
  let srcIds = mapMaybe (.sourceRecordId) entries

  sources <-
    if null srcIds
      then pure []
      else recipe.fetchSourcesByIds scope srcIds

  let sourceById :: HM.HashMap Text SourceRecord
      sourceById = HM.fromList [(s.srcId, s) | s <- sources]
      matchKeys = HS.fromList $ mapMaybe (.srcMatchKey) sources

  targets <-
    if HS.null matchKeys
      then pure []
      else recipe.fetchTargetsById scope matchKeys

  let targetsByKey :: HM.HashMap Text [TargetRecord]
      targetsByKey = HM.fromListWith (<>) [(t.tgtMatchKey, [t]) | t <- targets]

  -- Branch on grouping: GroupByTargetKey needs the whole set of sources
  -- sharing a target to compute the sum comparison correctly, so we can't
  -- re-classify one entry at a time. Individual mode does one-source-at-
  -- a-time re-classification unchanged.
  case recipe.grouping of
    Individual ->
      pure $ mapMaybe (reclassifyIndividual recipe sourceById targetsByKey now) entries
    GroupByTargetKey ->
      pure $ reclassifyGrouped recipe sourceById targetsByKey now entries

-- | Individual re-classification: one entry → one source → its targets.
--   Returns Nothing when the source has vanished (deleted since chunk
--   time); those stay OPEN and eventually age out — deleting the source
--   silently isn't behaviour the sweep should paper over.
reclassifyIndividual ::
  Recipe m ->
  HM.HashMap Text SourceRecord ->
  HM.HashMap Text [TargetRecord] ->
  UTCTime ->
  DRE.ReconciliationEntry ->
  Maybe DRE.ReconciliationEntry
reclassifyIndividual recipe sourceById targetsByKey now e = do
  srcId <- e.sourceRecordId
  s <- HM.lookup srcId sourceById
  let tgts = maybe [] (\k -> HM.lookupDefault [] k targetsByKey) s.srcMatchKey
      result = recipe.classify [s] tgts
      totalTgt = sum (map (.tgtAmount) tgts)
      mbTgt = listToMaybe tgts
      isOpen = s.srcLifecycle == InFlight || result.reconStatus /= MATCHED
  pure $ applyReclassification e s totalTgt mbTgt result isOpen now

-- | GroupByTargetKey re-classification: gather every entry sharing a
--   match key (via its re-fetched source's @srcMatchKey@), classify the
--   whole group at once, then apply the shared verdict to each entry.
--   Mirrors the chunk runner's 'buildGroupedEntries' shape so the sum-
--   check semantics match end to end.
reclassifyGrouped ::
  Recipe m ->
  HM.HashMap Text SourceRecord ->
  HM.HashMap Text [TargetRecord] ->
  UTCTime ->
  [DRE.ReconciliationEntry] ->
  [DRE.ReconciliationEntry]
reclassifyGrouped recipe sourceById targetsByKey now entries =
  -- Build (matchKey, [(entry, source)]) buckets. Entries whose source
  -- vanished (Nothing lookup) are dropped, matching the individual path.
  let paired :: [(Text, (DRE.ReconciliationEntry, SourceRecord))]
      paired =
        [ (mk, (e, s))
          | e <- entries,
            Just srcId <- [e.sourceRecordId],
            Just s <- [HM.lookup srcId sourceById],
            Just mk <- [s.srcMatchKey]
        ]
      groups :: HM.HashMap Text [(DRE.ReconciliationEntry, SourceRecord)]
      groups = HM.fromListWith (<>) [(mk, [pair]) | (mk, pair) <- paired]
   in concatMap (reclassifyOneGroup recipe targetsByKey now) (HM.toList groups)

reclassifyOneGroup ::
  Recipe m ->
  HM.HashMap Text [TargetRecord] ->
  UTCTime ->
  (Text, [(DRE.ReconciliationEntry, SourceRecord)]) ->
  [DRE.ReconciliationEntry]
reclassifyOneGroup recipe targetsByKey now (mk, pairs) =
  let groupSources = map snd pairs
      tgts = HM.lookupDefault [] mk targetsByKey
      result = recipe.classify groupSources tgts
      totalTgt = sum (map (.tgtAmount) tgts)
      mbTgt = listToMaybe tgts
      -- Same open rule as Individual, but "in flight" is a group-level
      -- property: if any source in the group is still InFlight the whole
      -- group waits. Otherwise the classifier's verdict decides.
      groupInFlight = any ((== InFlight) . srcLifecycle) groupSources
      isOpen = groupInFlight || result.reconStatus /= MATCHED
   in [applyReclassification e s totalTgt mbTgt result isOpen now | (e, s) <- pairs]

-- | Common projection of a re-classification result onto an entry —
--   shared between Individual and Grouped paths so they can't drift.
applyReclassification ::
  DRE.ReconciliationEntry ->
  SourceRecord ->
  KTC.HighPrecMoney ->
  Maybe TargetRecord ->
  ReconResult ->
  Bool -> -- isOpen
  UTCTime ->
  DRE.ReconciliationEntry
applyReclassification e s totalTgt mbTgt result isOpen now =
  e
    { DRE.expectedAmount = s.srcAmount,
      DRE.actualAmount = totalTgt,
      DRE.variance = s.srcAmount - totalTgt,
      DRE.reconStatus = result.reconStatus,
      DRE.mismatchReason = result.mismatchReason,
      DRE.targetRecordId = tgtId <$> mbTgt,
      DRE.settlementId = mbTgt >>= (.tgtSettlementId),
      DRE.settlementDate = mbTgt >>= (.tgtSettlementDate),
      DRE.settlementMode = mbTgt >>= (.tgtSettlementMode),
      DRE.transactionDate = mbTgt >>= (.tgtTransactionDate),
      DRE.rrn = mbTgt >>= (.tgtRrn),
      DRE.sourceLifecycle = s.srcLifecycle,
      DRE.open = isOpen,
      -- resolvedAt / firstSeenAt are computed inside upsertReconEntries
      -- based on the pre-existing row state.
      DRE.updatedAt = now,
      DRE.timestamp = now
    }
