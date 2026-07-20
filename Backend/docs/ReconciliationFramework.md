# Reconciliation Framework

A recipe-driven framework in `finance-kernel` for cross-checking financial data across sources (DSR, PG settlements, ledger, subscriptions, driver_fees, payment_orders, etc.). Each recon is a single declarative `Recipe` value; the shared runner handles chunking, locking, persistence, and — via the sweep — re-checking previously-broken entries.

## Building blocks

- **`ReconciliationSpec`** — identity triple `(Domain, DataSource, DataSource)`, e.g. `(PREPAID_SUBSCRIPTION, DSR, LEDGER)`.
- **`SourceRecord`** — normalised row from the source side. Carries `srcId` (PK), optional `srcMatchKey` (join key into targets), `srcAmount`, `srcMeta`, and — critically — **`srcLifecycle :: Lifecycle`** where `Lifecycle = InFlight | Settled | Cancelled`. Recipes map their table's status column into this.
- **`TargetRecord`** — target-side row. `tgtId` (PK) and `tgtMatchKey` are split so the framework can match on a foreign key while persisting the target row's actual identity.
- **`ReconciliationStatus`** — `MATCHED | HIGHER_IN_TARGET | LOWER_IN_TARGET | MISSING_IN_TARGET | MISSING_IN_SOURCE | AWAITING_SETTLEMENT`. `AWAITING_SETTLEMENT` is the short-circuit when any source in a comparison is `InFlight`; it's excluded from mismatch counts on the dashboard.
- **`Recipe`** — declares `spec`, `chunkPlan` (`ByHour` or `ByDay`), `grouping` (`Individual` or `GroupByTargetKey`), bulk fetchers, `classify`, and lifecycle knobs — see the field list below.

## The `Recipe` shape

```haskell
data Recipe m = Recipe
  { spec               :: ReconciliationSpec
  , chunkPlan          :: ChunkPlan              -- ByHour | ByDay
  , settlementBuffer   :: NominalDiffTime        -- required; no default
  , grouping           :: GroupingStrategy       -- Individual | GroupByTargetKey
  , fetchSourceChunk   :: MerchantScope -> DateRange -> m [SourceRecord]
  , fetchTargetsById   :: MerchantScope -> HashSet Text -> m [TargetRecord]
  , fetchSourcesByIds  :: MerchantScope -> [Text] -> m [SourceRecord]   -- sweep only
  , sweepInterval      :: NominalDiffTime        -- how often the sweep fires
  , maxOpenAge         :: NominalDiffTime        -- age at which an OPEN entry is force-closed
  , fetchOrphanTargets :: Maybe (MerchantScope -> DateRange -> HashSet Text -> m [TargetRecord])
  , classify           :: [SourceRecord] -> [TargetRecord] -> ReconResult
  , syncSourceStatus   :: Maybe (SourceRecord -> ReconciliationStatus -> m ())
  }
```

- `settlementBuffer` — how long after a chunk closes before its data is considered complete enough to read. Detection latency ≈ `chunkDuration + settlementBuffer`. Required with no default so every recipe declares its own choice.
- `classify :: [SourceRecord] -> [TargetRecord] -> ReconResult` — uniform signature across `Individual` (`[s]` and matching targets), `GroupByTargetKey` (whole group and its targets), and orphans (`[]` and `[t]`). `defaultClassify` handles the in-flight short-circuit, empty-side cases, and sum comparison. Recipes override only for per-component checks (`PrepaidDsrVsLedger`).
- `fetchSourcesByIds` — only the sweep calls this, to rebuild fresh `SourceRecord`s from stored `sourceRecordId`s on OPEN entries.

## The two runners

There are two separate runners, dispatched by two job types plus one scheduler heartbeat.

### `runNextChunk` (chunk runner, `Reconciliation` job)

1. Read the chunk range from `RecipeJobInput`. **The runner is stateless** — no `resumeFrom` chain; the scheduler passes a single chunk per fire.
2. `chunkIsSafe recipe.settlementBuffer now chunk` — bail with `AwaitingSafeWindow (chunk.to + buffer + 60s pad)` if the chunk hasn't cleared its buffer yet.
3. Wrap the rest in a Redis lock keyed on `(specKey, merchantId, cityId, chunkStart, chunkEnd)` with 1800 s TTL.
4. `fetchSourceChunk` → derive match keys → `fetchTargetsById` (bulk, no JOINs).
5. Target index is `HM.HashMap Text [TargetRecord]` — a source can have many targets (e.g. multiple SubscriptionCredit ledger entries per subscription).
6. Classify per source (Individual) or per group (GroupByTargetKey). All paths call `recipe.classify`; there's no hard-wired sum comparison.
7. Optionally run `fetchOrphanTargets` to emit entries via `recipe.classify [] [t]`.
8. **`QReconEntryExtra.upsertReconEntries entries`** — natural-key upsert; preserves `firstSeenAt`, computes `resolvedAt` on OPEN→CLOSED transitions.
9. Write the summary row.
10. Post-persist hook: call `syncSourceStatus` per source-derived entry to write back per-entity recon status (booking / subscription).

Return values: `Finished` (chunk done, scheduler drives the next one) or `AwaitingSafeWindow retryAt`.

### `runSweep` (sweep runner, `ReconciliationSweep` job)

One pass per fire, wrapped in a Redis lock keyed on `(spec, scope)`:

1. `findOpenBySpecScope` — bulk fetch up to `sweepBatchLimit` (500) OPEN entries, ordered by `first_seen_at`. Uses the partial `WHERE open` index.
2. **Pass 1 — orphan resolution.** For entries with `sourceRecordId IS NULL`, one `findByNaturalKeys` call to check whether a source-side twin (an entry keyed by the same underlying entity id, stored on the orphan as `entityId`) now exists. If yes → close with `open=false, closeReason="RESOLVED_BY_SOURCE"`. Runs *before* the age check so a long-standing orphan whose twin has since arrived gets the informative close reason rather than `AGED_OUT`.
3. **Pass 2 — force-close on age.** Anything older than `recipe.maxOpenAge` and not resolved above → `open=false, closeReason="AGED_OUT"`. **The classifier's original `reconStatus` is preserved intact** — dashboards see e.g. `reconStatus=MISSING_IN_TARGET, closeReason="AGED_OUT"` and know both what was wrong and that we gave up.
4. **Pass 3 — re-classify source-derived.** For fresh source-derived entries: `fetchSourcesByIds` → rebuild source-side view → `fetchTargetsById` with no date range → `recipe.classify` → upsert. This is what catches chunk-boundary late arrivals (target arrives in a later chunk than its source).

All writes go through the same `upsertReconEntries` path the chunk runner uses.

### `ReconciliationScheduler` (chunk heartbeat job)

One long-running scheduler job per `(spec, scope)`. On each fire:

1. Read `recipe.settlementBuffer` and `recipe.chunkPlan` from `reconciliationRegistry`.
2. Compute the latest closed chunk: `latestClosedChunkEnd plan (now - buffer)` — aligned to the plan's grid from the Unix epoch so chunk starts don't drift.
3. Enqueue **one** `Reconciliation` job for that single chunk.
4. Re-enqueue itself at `now + chunkDuration`.

The chunk runner never enqueues successors — the scheduler is what keeps the cadence going. A failed chunk affects only itself; the scheduler still fires an hour later.

### The immediate-mode dashboard trigger

`postFinanceManagementReconciliationTrigger` with a `day` param fans out `planChunks (midnight_D, midnight_D+1)` = 24 `Reconciliation` jobs at once. This is the backfill path — the only place where "24 jobs at once" happens.

## Entry lifecycle & `closeReason`

Every entry has `open :: Bool` and `closeReason :: Maybe Text` in addition to `reconStatus`. The three values `closeReason` can take:

| `closeReason` | Set by | Meaning |
|---|---|---|
| `NULL` | Runner or sweep on a natural MATCHED close | Clean resolution. |
| `"RESOLVED_BY_SOURCE"` | Sweep pass 1 | Orphan closed because a source-side entry for the same entity now exists. `reconStatus` stays `MISSING_IN_SOURCE`. |
| `"AGED_OUT"` | Sweep pass 2 | Sweep gave up. `reconStatus` preserved for triage. |

`reconStatus` is never overwritten by the sweep after the classifier's initial verdict — it's a permanent record of what was wrong.

## Storage — B1 identity + upsert

`finance_reconciliation_entry` carries a stable natural-key identity so entries survive across sweep re-checks:

| Column | Purpose |
|---|---|
| `id` | UUID PK (fresh per row insert). |
| `entry_key` | Natural key: `srcId` for source-derived, `"tgt:" <> tgtId` for orphans. Non-null; sidesteps Postgres NULL-in-UNIQUE inequality. |
| `first_seen_at` | First insertion time. Preserved across upserts; `updatedAt` tracks re-writes. |
| `source_lifecycle` | `InFlight` / `Settled` / `Cancelled` at the time of last classification. |
| `open` | `sourceLifecycle == InFlight OR reconStatus /= MATCHED`. The sweep only reads OPEN rows. |
| `resolved_at` | Set on OPEN→CLOSED transitions; cleared on the reverse. Computed inside `upsertReconEntries`. |
| `close_reason` | Distinguishes natural close (`NULL`) from `"RESOLVED_BY_SOURCE"` / `"AGED_OUT"`. |

Constraints and indices (see `dev/ddl-migrations/*/*-finance-reconciliation-entry-b1-b2-columns-and-indexes.sql`):

- **UNIQUE** `(domain, source, target, merchant_id, merchant_operating_city_id, entry_key)` — the upsert path depends on this.
- **Partial index** `WHERE open` on `(domain, source, target, merchant_id, merchant_operating_city_id, first_seen_at)` — the sweep hot path; working set stays proportional to outstanding work.
- Plain btree indices on `summary_id` and `(domain, source, target, source_record_id)` for the dashboard and settlement-list reads.

The chunk runner reordered its writes: **entries upsert first (idempotent by natural key), summary second.** A crash between the two leaves recoverable state — the next run's upsert is a no-op and the summary write proceeds.

## Job glue

`SharedLogic.Allocator.Jobs.Reconciliation.Reconciliation` exports the `reconciliationRegistry :: RecipeRegistry m` (static `spec → Recipe` map) and `runReconciliationJob`. Sibling modules `ReconciliationScheduler.hs` and `ReconciliationSweep.hs` import the registry.

All three job handlers check `transporterConfig.reconciliationJobsEnabled` as the operational kill switch — disabling a merchant pauses chunks and sweeps together.

## Recipes shipped

| Recipe | Spec | Grouping | `settlementBuffer` | `sweepInterval` / `maxOpenAge` |
|---|---|---|---|---|
| `PrepaidDsrVsLedger` | `PREPAID, DSR, LEDGER` | Individual (custom `classifyMultiComponent`) | 1 day | 4d / 30d |
| `PrepaidDsrVsSubscription` | `PREPAID, DSR, SUBSCRIPTION_PURCHASE` | Individual | 1 day | 4d / 30d |
| `PrepaidDssrVsSubscription` | `PREPAID, DSSR, SUBSCRIPTION_PURCHASE` | Individual | 1 day | 4d / 30d |
| `PrepaidPgPaymentVsSubscription` | `PREPAID, PG_PAYMENT_SETTLEMENT, SUBSCRIPTION_PURCHASE` | Individual | 2 days | 1d / 60d |
| `PrepaidPgPayoutVsPayoutRequest` | `PREPAID, PG_PAYOUT_SETTLEMENT, PAYOUT_REQUEST` | Individual | 2 days | 1d / 60d |
| `PrepaidSubscriptionPurchaseVsTransaction` | `PREPAID, SUBSCRIPTION_PURCHASE, LEDGER` | Individual (composite target, orphan sweep) | 1 day | 4d / 90d |
| `PostpaidDriverFeeVsPaymentOrder` | `POSTPAID, DRIVER_FEE, PAYMENT_ORDER` | GroupByTargetKey | 1 day | 4d / 30d |

Notes on the individual PG recipes:

- Both drop their old CREDITED / same-day guards and instead map their target's status to `srcLifecycle`. In-flight payouts (INITIATED / PROCESSING / RETRYING) and pending subscriptions classify as `AWAITING_SETTLEMENT` — excluded from break counts, revisited by the sweep once they settle.
- **`fetchSourcesByIds` is implemented** for both, delegating to a shared `buildSourceRecords` helper that both the chunk (`fetchSources`) and sweep re-fetch paths use. This is what actually flips an `AWAITING_SETTLEMENT` entry to `MATCHED` once the underlying subscription / payout_request reaches a terminal status.

Notes on `PrepaidSubscriptionPurchaseVsTransaction` (consumption-side recon, ported from PR #15484):

- **Expected** (source): `subscription.planRideCredit` — money credit granted at purchase.
- **Actual** (target): one composite `TargetRecord` per sub summing `sum(settled RideSubscriptionDebit ledger entries for the sub's rides)` + `sum(settled ExpiryCreditTransfer ledger entries for the sub)`. Only entries with `EntryStatus = SETTLED` are counted.
- Status mapping is finer here: `PENDING` / **`ACTIVE`** → `InFlight` (`AWAITING_SETTLEMENT` — consumption is still in progress). `EXPIRED` / `EXHAUSTED` → `Settled` (leftover credit must have landed in the expiry bucket). `FAILED` → `Cancelled` (`srcAmount = 0`; any consumption on a failed sub is unexpected).
- `maxOpenAge = 90 days` — longer than the internal 30-day default because an `ACTIVE` sub can sit `InFlight` for the full plan duration before there's anything for the sweep to close it against.
- **Ride fetch is per-sub via `findAllBySubscriptionPurchaseId`** in a `forM` loop; the `subscriptionPurchaseIds` column on `Ride` is a Postgres array so a proper bulk fetch needs an array-intersection query that Beam doesn't expose ergonomically. TODO comment inline. Fine at current chunk volumes.
- **Orphan check** is scoped to `ExpiryCreditTransfer` ledger entries whose referenced subscription doesn't exist anywhere in `subscription_purchase`. Chunk-boundary artifacts (sub was purchased in a prior chunk, still exists) are filtered out via a bulk `findByIds` lookup so only **phantom-subscription** cases (real data quality failures) surface as `MISSING_IN_SOURCE`. Ride-debit orphans aren't covered — `reference_id` points at a booking, not a sub, so that would need an extra ride→sub join; follow-up if needed.

## Design invariants

- **No JOINs** — separate indexed `WHERE id = ANY(?)` queries per table, grouped in-app.
- **Bulk-only fetchers** — every recipe issues a bounded number of queries per chunk regardless of chunk size.
- **Idempotent chunks** — upsert on natural key; safe to replay after a crash.
- **Per-recipe settlement buffer** — every recipe declares its own; no hidden global fallback.
- **Chunk runner is stateless** — one chunk in, one chunk out. No chain, no successor enqueue.
- **Scheduler as heartbeat** — the recurring `ReconciliationScheduler` is what advances time; the runner never does.
- **`reconStatus` is a permanent verdict** — the sweep never overwrites it. `open` and `closeReason` are the only signals the sweep touches.

## Frontend integration

- `services/finance.ts` sends `{ domain, source, target }` on the recon API.
- `hooks/useFinanceApi.ts::RECON_SPEC_BY_SLUG` maps a UI slug to the triple — one line per new recon.
- Settlement list uses backend bulk `findBySpecAndSourceIds` for per-row status enrichment.

## Adding a new recon

1. Write a module exporting `recipe :: Recipe m` under `SharedLogic/Finance/Reconciliation/Recipes/`. All fields required — the compiler enforces `settlementBuffer`, `sweepInterval`, `maxOpenAge`, `fetchSourcesByIds`.
2. Add the module to `reconciliationRegistry` in `Reconciliation.hs`.
3. Add the slug → spec entry in `RECON_SPEC_BY_SLUG` on the frontend.

No runner, scheduler, storage, or migration changes required for a pure recipe addition.

## Migrations

The B1/B2 columns and indices land via a single ddl-migration per app:

- `dev/ddl-migrations/dynamic-offer-driver-app/0846-finance-reconciliation-entry-b1-b2-columns-and-indexes.sql`
- `dev/ddl-migrations/rider-app/1555-finance-reconciliation-entry-b1-b2-columns-and-indexes.sql`

Both use `IF NOT EXISTS` guards so a re-run is a no-op. Column definitions also flow through the `ReconciliationEntry.yaml` Namma-DSL spec so codegen keeps read-only migrations in sync for fresh provisions.
