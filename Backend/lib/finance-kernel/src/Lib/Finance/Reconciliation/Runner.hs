{-
  Lib.Finance.Reconciliation.Runner

  The generic per-chunk runner. Given a Recipe m and the current job's
  RecipeJobInput, this fetches sources, fetches targets, matches them
  in-application (no JOINs), and atomically writes the chunk's summary
  + entries via delete-then-write.

  It is deliberately scheduler-agnostic. The runner returns a 'RunOutcome'
  and it is the app-side glue's job to translate that into
  'Lib.Scheduler.Types.ExecutionResult' + 'JC.createJobIn' calls. That
  keeps the framework in finance-kernel where both apps can consume it,
  without dragging AllocatorJobType or other app-specific typeclasses in.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Reconciliation.Runner
  ( -- * Run one chunk
    RunOutcome (..),
    runNextChunk,

    -- * Chunk planning & safety window (exported for tests)
    planChunks,
    chunkIsSafe,
    latestClosedChunkEnd,
    retryPadSeconds,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (partition)
import Data.Time (addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Common as KTC
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common (generateGUID, getCurrentTime, logInfo, logWarning)
import Kernel.Utils.Text (encodeToText)
import qualified Lib.Finance.Domain.Types.ReconciliationEntry as DRE
import qualified Lib.Finance.Domain.Types.ReconciliationSummary as DRS
import Lib.Finance.Reconciliation.Job (RecipeJobInput (..))
import Lib.Finance.Reconciliation.Recipe
import Lib.Finance.Reconciliation.Types
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.ReconciliationEntryExtra as QReconEntryExtra
import qualified Lib.Finance.Storage.Queries.ReconciliationSummary as QReconSummary

-- ─── Outcome sent back to app-side glue ────────────────────────────────────

data RunOutcome
  = -- | The chunk was processed (or was already locked by another worker).
    --   No follow-up job is enqueued: the scheduler drives the next chunk.
    Finished
  | -- | The chunk has not yet cleared its recipe's settlement buffer.
    --   Re-run at the given wall-clock time (@chunk.to + buffer + pad@).
    AwaitingSafeWindow UTCTime
  deriving (Show)

-- ─── Chunk planning ────────────────────────────────────────────────────────

-- | Slice a range into non-overlapping, gap-free half-open windows sized by
--   the plan. The last chunk is clamped at 'range.to' so ranges not evenly
--   divisible by the interval still cover their tail without spilling.
--
--   Exported for tests and for the immediate-mode dashboard trigger, which
--   enqueues one job per chunk for a whole-day recon.
planChunks :: ChunkPlan -> DateRange -> [DateRange]
planChunks plan r = go r.from
  where
    delta = chunkDuration plan
    go t
      | t >= r.to = []
      | otherwise =
        let t' = addUTCTime delta t
         in DateRange t (min t' r.to) : go t'

-- | End of the latest fully-closed chunk under @plan@ that ends on or
--   before @cutoff@. Aligns to the plan's natural grid from the Unix epoch
--   so chunk starts don't drift with scheduler jitter — successive fires
--   pick up exactly where the last one left off.
latestClosedChunkEnd :: ChunkPlan -> UTCTime -> UTCTime
latestClosedChunkEnd plan cutoff =
  let delta = chunkDuration plan
      epoch = posixSecondsToUTCTime 0
      elapsed = diffUTCTime cutoff epoch
      nChunks = floor (elapsed / delta) :: Integer
   in addUTCTime (fromIntegral nChunks * delta) epoch

-- ─── Safety window ─────────────────────────────────────────────────────────

-- | Fixed clock-skew pad on the retry time. A second configurable knob
--   here would drift out of sync with 'settlementBuffer' — this exists
--   only to keep runner\/scheduler clock jitter from causing an immediate
--   re-AwaitingSafeWindow flap at the boundary.
retryPadSeconds :: NominalDiffTime
retryPadSeconds = 60

-- | A chunk is safe to process iff its exclusive end is at or before
--   @now - settlementBuffer@. Time-relative and per-recipe: fast-settling
--   recons can use a minute-scale buffer, PG-settlement recons use days.
chunkIsSafe :: NominalDiffTime -> UTCTime -> DateRange -> Bool
chunkIsSafe buffer now chunk = chunk.to <= addUTCTime (negate buffer) now

-- ─── Main entry point ──────────────────────────────────────────────────────

runNextChunk ::
  ( BeamFlow.BeamFlow m r,
    Hedis.HedisFlow m r
  ) =>
  Recipe m ->
  RecipeJobInput ->
  m RunOutcome
runNextChunk recipe input = do
  now <- getCurrentTime
  let chunk = input.range
  if not (chunkIsSafe recipe.settlementBuffer now chunk)
    then do
      let nextCheck = addUTCTime (recipe.settlementBuffer + retryPadSeconds) chunk.to
      logInfo $
        "Recon " <> specKey recipe.spec
          <> ": chunk "
          <> show chunk
          <> " not yet safe (buffer="
          <> show recipe.settlementBuffer
          <> "); will retry at "
          <> show nextCheck
      pure $ AwaitingSafeWindow nextCheck
    else do
      -- Redis lock per (spec, merchant, city, chunk) prevents two
      -- concurrent runs from double-writing a summary row for the same
      -- window. TTL is generous enough that any single legitimate chunk
      -- finishes well inside it; a dead lock expires and the next
      -- attempt takes it.
      let lockKey = chunkLockKey recipe.spec input.scope chunk
          lockTtlSeconds = 1800 :: Int
      lockResult <-
        Hedis.whenWithLockRedisAndReturnValue lockKey lockTtlSeconds $
          processChunk recipe input.scope chunk
      case lockResult of
        Left () -> do
          -- Lock held by another runner. The scheduler will drive the next
          -- chunk on its next fire; treat this one as done for the job.
          logWarning $
            "Recon " <> specKey recipe.spec
              <> ": chunk "
              <> show chunk
              <> " locked; skipping."
          pure Finished
        Right () -> pure Finished
  where
    chunkLockKey spec scope chunk =
      "ReconChunkLock:"
        <> specKey spec
        <> "|"
        <> scope.merchantId
        <> "|"
        <> scope.merchantOperatingCityId
        <> "|"
        <> show chunk.from
        <> "|"
        <> show chunk.to

-- ─── Per-chunk processing ──────────────────────────────────────────────────

processChunk ::
  (BeamFlow.BeamFlow m r) =>
  Recipe m ->
  MerchantScope ->
  DateRange ->
  m ()
processChunk recipe scope chunk = do
  sources <- recipe.fetchSourceChunk scope chunk
  let matchKeys = HS.fromList $ mapMaybe (.srcMatchKey) sources
  targets <-
    if HS.null matchKeys
      then pure []
      else recipe.fetchTargetsById scope matchKeys
  -- Index on tgtMatchKey (NOT tgtId): matching goes through the FK the
  -- source referenced, while tgtId is preserved for persistence as
  -- targetRecordId on each entry. A list per key because a single source
  -- may have multiple corresponding targets (e.g. a subscription's
  -- SubscriptionCredit ledger entries, or a charge + later refund pair).
  let targetIx :: HM.HashMap Text [TargetRecord]
      targetIx = HM.fromListWith (<>) [(t.tgtMatchKey, [t]) | t <- targets]

  now <- getCurrentTime
  summaryId <- KTI.Id <$> generateGUID

  let sourceEntries = case recipe.grouping of
        Individual -> buildIndividualEntries recipe sourceRecordCtx sources targetIx
        GroupByTargetKey -> buildGroupedEntries recipe sourceRecordCtx sources targetIx
      sourceRecordCtx = EntryCtx recipe.spec scope summaryId chunk now

  -- Optional orphan-target sweep: recipes that opt in return targets in the
  -- chunk whose tgtMatchKey is NOT in the already-seen source-side set.
  -- Each such target becomes a MISSING_IN_SOURCE entry (or whatever the
  -- recipe's classify decides for @[] [t]@).
  orphanEntries <- case recipe.fetchOrphanTargets of
    Nothing -> pure []
    Just fetchOrphans -> do
      orphans <- fetchOrphans scope chunk matchKeys
      pure $ map (mkOrphanEntry recipe sourceRecordCtx) orphans

  let entries = sourceEntries <> orphanEntries
      summary = buildSummary recipe.spec scope summaryId chunk entries now
  -- Order matters: upsert entries first (idempotent by natural key), then
  -- write the summary. A crash between the two is recoverable — the next
  -- run's upsert is a no-op and the summary write proceeds. The reverse
  -- order would leave a summary claiming N entries with zero rows behind
  -- it after a crash.
  QReconEntryExtra.upsertReconEntries entries
  QReconSummary.create summary

  -- Post-persist: sync each source-driven entry's status back to the source
  -- entity's own reconciliationStatus JSON map. Entries are matched to their
  -- sources by sourceRecordId (== srcId) — safer than positional zipping
  -- because GroupByTargetKey reorders sources across groups. Orphan entries
  -- have no source and are naturally skipped (sourceRecordId is Nothing).
  case recipe.syncSourceStatus of
    Nothing -> pure ()
    Just syncFn -> do
      let sourceById = HM.fromList [(s.srcId, s) | s <- sources]
          pairs =
            [ (src, e.reconStatus)
              | e <- sourceEntries,
                Just srcId <- [e.sourceRecordId],
                Just src <- [HM.lookup srcId sourceById]
            ]
      forM_ pairs $ \(src, st) -> syncFn src st

-- ─── Entry construction ────────────────────────────────────────────────────

-- | Invariant context passed through every entry builder — kept local so
--   changes to the entry shape don't ripple through five-argument-long
--   builder signatures.
data EntryCtx = EntryCtx
  { ctxSpec :: ReconciliationSpec,
    ctxScope :: MerchantScope,
    ctxSummaryId :: KTI.Id DRS.ReconciliationSummary,
    ctxChunk :: DateRange,
    ctxNow :: UTCTime
  }

buildIndividualEntries ::
  Recipe m ->
  EntryCtx ->
  [SourceRecord] ->
  HM.HashMap Text [TargetRecord] ->
  [DRE.ReconciliationEntry]
buildIndividualEntries recipe ctx sources targetIx =
  -- Individual entries carry their srcMatchKey in the same 'groupTargetKey'
  -- column that GroupByTargetKey uses. This makes the column serve as a
  -- uniform "matchKey": both source-derived (srcMatchKey) and orphan
  -- (tgtMatchKey) entries populate it, so the B2 sweep can resolve
  -- orphans by looking up source-side twins whose matchKey matches the
  -- orphan's — required for recipes where srcId /= srcMatchKey
  -- (e.g. driver_fee.id vs payment_order.id in the postpaid recon).
  [ mkEntry ctx s tgts s.srcMatchKey Nothing Nothing (recipe.classify [s] tgts)
    | s <- sources,
      let tgts = maybe [] (\k -> HM.lookupDefault [] k targetIx) s.srcMatchKey
  ]

buildGroupedEntries ::
  Recipe m ->
  EntryCtx ->
  [SourceRecord] ->
  HM.HashMap Text [TargetRecord] ->
  [DRE.ReconciliationEntry]
buildGroupedEntries recipe ctx sources targetIx =
  let (keyed, unkeyed) = partition (isJust . (.srcMatchKey)) sources
      groups :: HM.HashMap Text [SourceRecord]
      groups = HM.fromListWith (<>) [(tk, [s]) | s <- keyed, Just tk <- [s.srcMatchKey]]

      groupEntries = concatMap emitGroup (HM.toList groups)
      missingEntries =
        [ mkEntry ctx s [] Nothing Nothing Nothing (recipe.classify [s] [])
          | s <- unkeyed
        ]
   in groupEntries <> missingEntries
  where
    emitGroup (tk, group) =
      let tgts = HM.lookupDefault [] tk targetIx
          result = recipe.classify group tgts
          totalSrc = sum (map (.srcAmount) group)
          totalTgt = sum (map (.tgtAmount) tgts)
          -- Group columns stay Nothing when no target matched, mirroring the
          -- prior contract (Nothing => "no group total was computable").
          mbSrcTot = if null tgts then Nothing else Just totalSrc
          mbTgtTot = if null tgts then Nothing else Just totalTgt
       in [ mkEntry ctx s tgts (Just tk) mbSrcTot mbTgtTot result
            | s <- group
          ]

-- | Builder for source-driven entries. Takes the list of targets matched to
--   this source (Individual) or its group (GroupByTargetKey); column-fill
--   fields (@targetRecordId@, @settlementId@, etc.) pull from the first
--   target when any exist. Group-total columns are populated by the caller.
mkEntry ::
  EntryCtx ->
  SourceRecord ->
  [TargetRecord] ->
  Maybe Text ->
  Maybe KTC.HighPrecMoney ->
  Maybe KTC.HighPrecMoney ->
  ReconResult ->
  DRE.ReconciliationEntry
mkEntry ctx s tgts groupKey groupSrcTotal groupTgtAmount result =
  let mbTgt = listToMaybe tgts
      totalTgt = sum (map (.tgtAmount) tgts)
      -- OPEN when the source can still change or the result is a
      -- mismatch: those are the entries the B2 sweep re-checks.
      isOpen = s.srcLifecycle == InFlight || result.reconStatus /= MATCHED
   in DRE.ReconciliationEntry
        { id = KTI.Id "", -- filled by generateGUID inside createMany
          summaryId = ctx.ctxSummaryId,
          domain = ctx.ctxSpec.domain,
          source = ctx.ctxSpec.source,
          target = ctx.ctxSpec.target,
          reconciliationDate = ctx.ctxChunk.from,
          expectedAmount = s.srcAmount,
          actualAmount = totalTgt,
          variance = s.srcAmount - totalTgt,
          reconStatus = result.reconStatus,
          mismatchReason = result.mismatchReason,
          entityId = s.srcEntityId,
          partyId = s.srcPartyId,
          sourceRecordId = Just s.srcId,
          targetRecordId = tgtId <$> mbTgt,
          component = s.srcComponent,
          entityMeta = encodeToText <$> s.srcMeta,
          settlementId = mbTgt >>= (.tgtSettlementId),
          settlementDate = mbTgt >>= (.tgtSettlementDate),
          settlementMode = mbTgt >>= (.tgtSettlementMode),
          transactionDate = mbTgt >>= (.tgtTransactionDate),
          rrn = mbTgt >>= (.tgtRrn),
          groupTargetKey = groupKey,
          groupSourceTotal = groupSrcTotal,
          groupTargetAmount = groupTgtAmount,
          merchantId = Just ctx.ctxScope.merchantId,
          merchantOperatingCityId = Just ctx.ctxScope.merchantOperatingCityId,
          timestamp = ctx.ctxNow,
          createdAt = ctx.ctxNow,
          updatedAt = ctx.ctxNow,
          entryKey = s.srcId, -- source-derived: natural key is the source pk
          firstSeenAt = ctx.ctxNow, -- overwritten by upsert on conflict
          sourceLifecycle = s.srcLifecycle,
          resolvedAt = Nothing, -- computed on the upsert boundary
          open = isOpen,
          -- Nothing here means "closed naturally": either MATCHED at chunk
          -- time or reached MATCHED via a later sweep pass. Only the sweep
          -- ever writes 'closeReason', and only when it force-closes on age.
          closeReason = Nothing
        }

-- | Build an entry for an orphan target (a target row with no matching source).
--   The status comes from @recipe.classify [] [t]@ so recipes that want a
--   different label than the default MISSING_IN_SOURCE can override.
mkOrphanEntry ::
  Recipe m ->
  EntryCtx ->
  TargetRecord ->
  DRE.ReconciliationEntry
mkOrphanEntry recipe ctx t =
  let result = recipe.classify [] [t]
      -- Orphan entries have no source lifecycle to track; they open when
      -- the status is a mismatch and close otherwise. The classifier
      -- already returned MISSING_IN_SOURCE (or whatever the recipe
      -- overrides), so 'MATCHED' here would be an unusual case where a
      -- recipe deliberately swallows orphans as fine.
      isOpen = result.reconStatus /= MATCHED
   in DRE.ReconciliationEntry
        { id = KTI.Id "", -- filled by generateGUID inside upsertReconEntries on insert
          summaryId = ctx.ctxSummaryId,
          domain = ctx.ctxSpec.domain,
          source = ctx.ctxSpec.source,
          target = ctx.ctxSpec.target,
          reconciliationDate = ctx.ctxChunk.from,
          expectedAmount = 0,
          actualAmount = t.tgtAmount,
          variance = negate t.tgtAmount,
          reconStatus = result.reconStatus,
          mismatchReason = result.mismatchReason,
          entityId = Just t.tgtMatchKey,
          partyId = Nothing,
          sourceRecordId = Nothing,
          targetRecordId = Just t.tgtId,
          component = Nothing,
          entityMeta = encodeToText <$> t.tgtMeta,
          settlementId = t.tgtSettlementId,
          settlementDate = t.tgtSettlementDate,
          settlementMode = t.tgtSettlementMode,
          transactionDate = t.tgtTransactionDate,
          rrn = t.tgtRrn,
          -- Same as source-derived entries: 'groupTargetKey' carries the
          -- underlying match key uniformly. For an orphan that's the
          -- target's 'tgtMatchKey', which by construction equals the
          -- 'srcMatchKey' any source-side twin would use. This is what
          -- the sweep's orphan-resolution query joins on.
          groupTargetKey = Just t.tgtMatchKey,
          groupSourceTotal = Nothing,
          groupTargetAmount = Nothing,
          merchantId = Just ctx.ctxScope.merchantId,
          merchantOperatingCityId = Just ctx.ctxScope.merchantOperatingCityId,
          timestamp = ctx.ctxNow,
          createdAt = ctx.ctxNow,
          updatedAt = ctx.ctxNow,
          -- Orphan natural key prefix disambiguates from source-derived
          -- keys sharing the same underlying id. In Postgres NULL never
          -- equals NULL for uniqueness, so an explicit non-null key is
          -- the only way to dedupe orphans across sweeps.
          entryKey = "tgt:" <> t.tgtId,
          firstSeenAt = ctx.ctxNow,
          sourceLifecycle = Settled, -- no source; carry a stable terminal value
          resolvedAt = Nothing,
          open = isOpen,
          closeReason = Nothing
        }

buildSummary ::
  ReconciliationSpec ->
  MerchantScope ->
  KTI.Id DRS.ReconciliationSummary ->
  DateRange ->
  [DRE.ReconciliationEntry] ->
  UTCTime ->
  DRS.ReconciliationSummary
buildSummary spec scope summaryId chunk entries now =
  let total = length entries
      matched = length [e | e <- entries, e.reconStatus == MATCHED]
      -- AWAITING_SETTLEMENT is neither matched nor a discrepancy — it's
      -- a "come back later" verdict. It drops out of both the break count
      -- and the match-rate denominator so short-lived in-flight rows don't
      -- deflate the ratio the ops dashboard shows.
      awaiting = length [e | e <- entries, e.reconStatus == AWAITING_SETTLEMENT]
      terminal = total - awaiting
      discrep = terminal - matched
      sourceTotal = sum (map (.expectedAmount) entries)
      targetTotal = sum (map (.actualAmount) entries)
      variance = sourceTotal - targetTotal
      -- Dispute amount excludes in-flight entries: their variance is
      -- expected to resolve on the sweep, not to represent a real gap.
      disputeTotal = sum [abs e.variance | e <- entries, e.reconStatus /= AWAITING_SETTLEMENT]
      matchRate =
        if terminal > 0
          then show ((fromIntegral matched * 100 / fromIntegral terminal) :: Double) <> "%"
          else "0%"
   in DRS.ReconciliationSummary
        { id = summaryId,
          domain = spec.domain,
          source = spec.source,
          target = spec.target,
          reconciliationDate = chunk.from,
          merchantId = scope.merchantId,
          merchantOperatingCityId = scope.merchantOperatingCityId,
          totalRecords = total,
          matchedRecords = matched,
          totalDiscrepancies = discrep,
          sourceTotal = sourceTotal,
          targetTotal = targetTotal,
          varianceAmount = variance,
          disputeAmountTotal = disputeTotal,
          matchRate = matchRate,
          status = DRS.COMPLETED,
          errorMessage = Nothing,
          createdAt = now,
          updatedAt = now
        }
