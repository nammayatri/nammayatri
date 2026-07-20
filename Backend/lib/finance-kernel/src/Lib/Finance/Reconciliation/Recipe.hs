{-
  Lib.Finance.Reconciliation.Recipe

  A recipe encapsulates everything that varies between recons: which two
  data sources are involved, how to fetch each side, how to compare a
  matched pair, and whether the check is per-row or group-level.

  The framework runner is a single generic loop over Recipe m; adding a new
  recon is a matter of instantiating one of these and dropping it into an
  app-side registry.
-}
module Lib.Finance.Reconciliation.Recipe
  ( Recipe (..),
    defaultClassify,
  )
where

import qualified Data.HashSet as HS
import Kernel.Prelude
import Lib.Finance.Reconciliation.Types

-- | The set of hooks a recon must provide. Everything the runner does is
--   parameterised on these — the runner never queries a specific table
--   directly, and never inspects the meaning of a match key.
data Recipe m = Recipe
  { -- | The (domain, source, target) triple this recipe answers to. Used
    --   by the app-side registry as the dispatch key and stamped into every
    --   entry it produces.
    spec :: ReconciliationSpec,
    -- | How the runner slices the incoming date range. Individual chunks
    --   are processed in isolation and persisted atomically; the safety
    --   window (see Runner.chunkIsSafe) ensures we never touch a chunk that
    --   hasn't fully closed yet.
    chunkPlan :: ChunkPlan,
    -- | How long after a chunk closes before its data is considered
    --   complete enough to read. Detection latency is
    --   @chunkDuration + settlementBuffer@ — internal-only recons can set
    --   this to minutes; recons whose target arrives via a settlement file
    --   need days.
    --
    --   Required with no default. Every recipe declares its own so the
    --   compiler enforces the choice on new recipes and there is no hidden
    --   fallback to reason about when debugging.
    settlementBuffer :: NominalDiffTime,
    -- | Row-level (1:1) vs group-level (n:1) reconciliation. When
    --   'GroupByTargetKey' is chosen the runner sums source amounts sharing
    --   a target and applies the resulting recon status to every source in
    --   the group.
    grouping :: GroupingStrategy,
    -- | Fetch every source row whose chunk timestamp falls in the given
    --   range. Convention: use a single indexed query, then bulk-load any
    --   intermediate tables the recipe needs and merge in-application —
    --   never write a multi-table JOIN here (the framework depends on the
    --   planner not surprising us).
    fetchSourceChunk :: MerchantScope -> DateRange -> m [SourceRecord],
    -- | Bulk-fetch target rows keyed by the given set of match keys. When
    --   the set is empty the runner short-circuits and this is never called.
    fetchTargetsById :: MerchantScope -> HS.HashSet Text -> m [TargetRecord],
    -- | Bulk-fetch source rows by their primary key (== 'srcId'). Used
    --   only by the B2 sweep to rebuild fresh 'SourceRecord's from the
    --   stored @sourceRecordId@ on OPEN entries. The chunk runner never
    --   calls this — it walks fresh sources via 'fetchSourceChunk'.
    fetchSourcesByIds :: MerchantScope -> [Text] -> m [SourceRecord],
    -- | How often the sweep fires for this recipe. Cadence should track
    --   how fast the source's state actually turns over — roughly hourly
    --   for internal-only recipes, every few hours for settlement-file
    --   recipes. Sub-hourly is waste; the pool doesn't turn over faster.
    sweepInterval :: NominalDiffTime,
    -- | Age at which an entry that's still OPEN is force-closed
    --   (@closeReason = "AGED_OUT"@; the classifier's verdict in
    --   @reconStatus@ is preserved). Bounds the sweep's working set:
    --   without this the OPEN pool grows without limit for entries that
    --   never resolve. Set generously — a premature aging-out is worse
    --   than a slow sweep.
    maxOpenAge :: NominalDiffTime,
    -- | Optional orphan-target detection. When 'Just', the runner fetches
    --   targets in the chunk range whose 'tgtMatchKey' is NOT in the given
    --   already-matched set, and emits one MISSING_IN_SOURCE entry per row.
    --   Skip ('Nothing') when the source side is authoritative — e.g. a
    --   settlement-driven recon where a target without a source is expected
    --   noise, not a discrepancy.
    fetchOrphanTargets :: Maybe (MerchantScope -> DateRange -> HS.HashSet Text -> m [TargetRecord]),
    -- | The decision function. Called uniformly across paths:
    --     * 'Individual' passes @[source]@ and every target matching the
    --       source's @srcMatchKey@ (0, 1, or many).
    --     * 'GroupByTargetKey' passes the whole source group and every
    --       target matching their shared key.
    --     * An unmatched source passes @[source]@ and @[]@.
    --     * An orphan target (from 'fetchOrphanTargets') passes @[]@ and
    --       @[target]@.
    --   Recipes almost always reuse 'defaultClassify', which handles the
    --   in-flight short-circuit, the empty-side cases, and the sum
    --   comparison. Override when the recon needs per-component checks
    --   (see PrepaidDsrVsLedger) or a custom tolerance.
    classify :: [SourceRecord] -> [TargetRecord] -> ReconResult,
    -- | Optional per-entry write-back to the source entity's own
    --   @reconciliationStatus@ column (JSON status map keyed by 'specKey').
    --   Runner calls this after 'processChunk' persists, one call per
    --   emitted entry. Recipes typically implement it as:
    --      * find the source entity by 'srcEntityId'
    --      * merge (spec -> status) into its 'reconciliationStatus' map
    --      * write back
    --   See 'Lib.Finance.Reconciliation.StatusMap' for the helpers.
    --   Skip ('Nothing') when the source table has no
    --   @reconciliationStatus@ column, or when downstream doesn't care.
    syncSourceStatus :: Maybe (SourceRecord -> ReconciliationStatus -> m ())
  }

-- | Baseline comparator. Handles four shapes uniformly:
--     * any in-flight source → 'AWAITING_SETTLEMENT'
--     * @[source]@ + @[]@   → 'MISSING_IN_TARGET'
--     * @[]@   + @[target]@ → 'MISSING_IN_SOURCE'
--     * otherwise: sum comparison with the total-based status.
--   Recipes override when they need per-component checks or domain-specific
--   tolerance (paise rounding, sub-paise noise on GST math).
defaultClassify :: [SourceRecord] -> [TargetRecord] -> ReconResult
defaultClassify srcs tgts
  | any ((== InFlight) . srcLifecycle) srcs =
    ReconResult AWAITING_SETTLEMENT Nothing
  | null srcs && null tgts =
    ReconResult MATCHED Nothing -- unreachable from the runner
  | null srcs =
    ReconResult MISSING_IN_SOURCE (Just "Source record missing")
  | null tgts =
    ReconResult MISSING_IN_TARGET (Just "Target record missing")
  | totalSrc == totalTgt =
    ReconResult MATCHED Nothing
  | totalTgt > totalSrc =
    ReconResult HIGHER_IN_TARGET (Just amountMismatch)
  | otherwise =
    ReconResult LOWER_IN_TARGET (Just amountMismatch)
  where
    totalSrc = sum (map (.srcAmount) srcs)
    totalTgt = sum (map (.tgtAmount) tgts)

amountMismatch :: Text
amountMismatch = "Amount mismatch"
