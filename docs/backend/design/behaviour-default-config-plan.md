# Behaviour engine: default rulebooks + one-call city enablement — plan

Status: PROPOSED (2026-10-06). Rev 2: canonical rulebooks live in the DB (no repo JSON,
no config-sync applier) — per review feedback.

## Problem

Enabling a behaviour (cancellation-rate, rating, issue-breach, …) in a city today requires:
authoring JsonLogic elements via the verify API, creating a rollout row, seeding
`merchant_overlay` PN keys, flipping `transporter_config` prerequisites, and sometimes
seeding `driver_block_reason`. Nobody does all of it: the production audit (2026-10-06)
found 10 of 11 behaviour domains with zero rules anywhere, and the one configured domain
(GPS-TOLL, Bangalore) is **inert** because its `enable_gps_toll_behavior` flag was never
set — rolled out but dead. The per-city JsonLogic authoring cost is the stated blocker.

## What already exists (load-bearing discoveries)

1. **Rule elements are global, not per-city.** `app_dynamic_logic_element` is keyed by
   (domain, version) — `DALE.findByDomainAndVersion` ignores city/merchant. Only the
   *rollout* row (`app_dynamic_logic_rollout`) is city-scoped. So "enabling in a city"
   never needed logic duplication — it's one rollout row pointing at a shared version.
2. **A reserved `"default"` city fallback is already implemented** in
   `lib/utils/src/Tools/DynamicLogic.hs` — `selectAppDynamicLogicVersion` (:221) and
   `selectVersionForConfigs` (:172): if a city has no rollouts for a domain, rollouts of
   `merchant_operating_city_id = 'default'` are used. **Unused in prod (0 rows).**
   Caveat: default-city rollouts only match `Unbounded` time bounds (:173 comment).
3. Dashboard APIs already exist for both halves: `POST /appDynamicLogic/verify` (author
   elements/version) and `POST /appDynamicLogic/upsertLogicRollout` (per-city rollout);
   `BulkLogicRolloutReq` types exist in `Lib.Yudhishthira.Types`.
4. The control-centre UI already has the advanced editing surface (see UI section).

So the plan is mostly **packaging**, not new machinery.

## Design

### Concept: BehaviourPack (per-domain enablement bundle)

A behaviour is enabled by applying a *pack*, not by authoring config. Registry in code
(driver app, e.g. `SharedLogic/BehaviourManagement/Packs.hs`):

```haskell
data BehaviourPack = BehaviourPack
  { domain :: LYT.LogicDomain,
    requiredOverlayKeys :: [Text],        -- e.g. ["LOW_RATING_NUDGE", "LOW_RATING_WARN"]
    transporterConfigPrereqs :: [ToggleSpec], -- e.g. enable_gps_toll_behavior := true
    blockReasons :: [BlockReasonSeed]     -- e.g. LOW_RATING / 168h
  }
-- The canonical rulebook itself is NOT in the pack: it lives in the DB as the
-- (domain, version) referenced by the "default"-city 0% base rollout.
```

### Phase 1 — canonical rulebooks live in the DB, authored via the existing verify flow

No repo JSON assets, no config-sync applier. The DB already provides what we need:
elements are append-only and versioned per (domain, version), and dashboard audit rows
record who changed what.

- Author each behaviour's rulebook ONCE through the existing verify flow (elements →
  new version), then **mark it canonical**: a base rollout row under city id `"default"`
  with `percentage_rollout = 0`.
  - 0% means the existing fallback never *selects* it (`findLogic` requires
    `randVal <= cumPercent`, randVal ∈ [1,100] — a 0% row never matches), so nothing turns
    on implicitly. The row is purely the machine-readable marker of "the canonical version
    for this domain". Enforcement behaviours must be opt-in per city; a 100% default row
    would silently enable in all 124 cities.
  - Small backend affordance: `POST /nammaTag/behavior/{domain}/markCanonical {version}`
    (or let upsertLogicRollout target the reserved `"default"` city) — enforces at most
    one canonical base row per domain. Gated by its own capability; canonical changes can
    later adopt the control-centre maker-checker pattern if stronger governance is wanted.
  - Overlay templates follow the same principle: seed `merchant_overlay` rows once under
    the `"default"` city (runtime never reads them — only the enable API copies them into
    target cities). Alternative if preferred later: embed default title/body per language
    in the BehaviourPack registry.
  - Dev/local environments: a one-time seed in dev migrations if canonical rows are needed
    outside prod.
- Guardrail (small code change, recommended): in `selectAppDynamicLogicVersion`, skip the
  `"default"`-city fallback for `*_BEHAVIOR` domains entirely, so a mis-seeded non-zero
  default row can never mass-enable an enforcement behaviour. CONFIG/UI domains keep
  today's semantics.

### Phase 2 — one-call enable/disable API (the actual win)

New dashboard endpoints (NammaTag spec, `ApiAuthV2`, `helperApiExtra` requestor injection,
new capabilities `city-operations.behaviour.read/.write`):

- `POST /nammaTag/behavior/{domain}/enable` `{percentageRollout :: Maybe Int (default 100), overrideVersion :: Maybe Int}`
  1. Resolve canonical version: base rollout of city `"default"` for the domain
     (or `overrideVersion` for cities with custom rulebooks). Error if no canonical exists.
  2. Upsert the city rollout row (city, domain, version, percentage, Unbounded) through
     the existing upsertLogicRollout internals.
  3. Seed `merchant_overlay` rows for `requiredOverlayKeys` — copied from the
     `"default"`-city template rows; skip keys the city already has (never overwrite city
     customisations).
  4. Apply `transporterConfigPrereqs` (e.g. GPS-toll's enable flag) + clear config cache.
  5. Seed `blockReasons` into `driver_block_reason` if missing.
  6. Audit row (requestor name/id via helperApiExtra) + clear dynamic-logic caches.
- `POST /nammaTag/behavior/{domain}/disable` — set city rollout to 0% (keep history),
  revert prereq toggles, leave overlays in place.
- Percentage parameter gives gradual rollout for free (existing rollout machinery).

Enabling the rating behaviour in a new city becomes exactly one API call.

### Phase 3 — status endpoint (kills the "rolled out but inert" class)

`GET /nammaTag/behavior/status` (per city) returning, for every behaviour domain:
- enabled? (city rollout %, version; canonical vs custom vs stale-canonical)
- prerequisites green? (each transporter_config toggle, each overlay key present per
  language, block reasons seeded)
- activity last 30d: consequence counts from the `bt:` visibility layer +
  `driver_block_transactions` by `block_reason_flag`.

This check would have flagged GPS-toll's dead flag on day one.

### Phase 4 — version lifecycle

- Canonical rulebook updated (new version under `"default"` via "Save as canonical"):
  cities pointing at the old canonical version do NOT auto-bump (enforcement rules must
  change deliberately). Add `POST /nammaTag/behavior/{domain}/bumpToCanonical` (single
  city); the status endpoint marks cities on stale versions.
- City wants custom thresholds: author a city-specific version via the existing verify
  flow; `enable` with `overrideVersion`. Status shows "custom (vN; canonical vM)".

## Control-centre (UI) changes

The control-center repo (`~/workspace/control-center`, React/TS + tanstack-query) already
has the advanced editing surface: `src/modules/config/DynamicLogicPage.tsx` with
`VerifyCreateForm` (author elements), `VersionsSection`, `RolloutSection`,
`TimeBoundsSection`, and `bulkUpsertLogicRollout` (multi-city rollout) in
`src/services/dynamicLogic.ts`. What's missing is the *operator-grade* surface: nobody
should need the JsonLogic editor just to turn a behaviour on.

### New: Behaviours switchboard page

`src/modules/config/BehavioursPage.tsx` (nav: Config → Behaviours), driven entirely by the
new backend endpoints:

- **Table: one row per behaviour domain** for the selected merchant+city (from
  `useDashboardContext`), columns:
  - Behaviour name + description (static copy per domain)
  - Status pill: `Enabled (100%)` / `Partial (25%)` / `Disabled` / **`Inert`** (enabled
    but a prerequisite is red — the GPS-toll failure mode, surfaced loudly)
  - Version badge: `canonical vN` / `custom vX (canonical vN)` / `stale vM → vN available`
  - Prerequisite checklist popover: each overlay key (per language), each
    transporter_config toggle, block-reason seeds — green/red per item
  - 30-day activity: nudges / warns / soft blocks / hard blocks from the status endpoint
  - Actions: **Enable** (dialog: percentage slider + confirm summary of everything the
    pack will apply), **Disable**, **Bump to canonical** (when stale), **Advanced →**
    deep-link into DynamicLogicPage pre-filtered to the domain (existing `?tag=` support)
- Enable/disable call the new one-call APIs — the UI never composes rollout + overlay +
  toggle writes itself, so UI and API can't drift.

### Changes to existing UI pieces

| Piece | Change |
|---|---|
| `src/services/dynamicLogic.ts` | add `getBehaviorStatus`, `enableBehavior`, `disableBehavior`, `bumpBehaviorToCanonical`, `markCanonical` client fns + types |
| `src/hooks/useDynamicLogic.ts` | add `useBehaviorStatus` query + mutation hooks (invalidate `['dynamicLogic']` on enable/disable, same pattern as the side-switch) |
| `VersionsSection.tsx` | "canonical" badge on the version referenced by the `default`-city base rollout; warn badge on versions no city uses |
| `RolloutSection.tsx` | render the 0% `default`-city marker row read-only with an explainer ("canonical marker — not active anywhere by itself"); for `*_BEHAVIOR` domains, banner linking to the Behaviours page as the preferred path |
| `VerifyCreateForm.tsx` | capability-gated "Save as canonical" option = save elements + call `markCanonical` (normal saves unchanged) |
| Nav/routes/i18n | route in `App.tsx`, nav label in `lib/translations/{en,hi,kn,fi}.ts` + `nav-labels.ts`, capability gating via the existing `access` module (new `city-operations.behaviour.read/.write`) |
| `driver-cancellation` module (`RulesSection.tsx`, `RepeatOffendersTab.tsx`) | status banner sourced from `getBehaviorStatus` once CANCELLATION_RATE_BEHAVIOR migrates off the legacy slab config, so ops see which engine governs the city |

Other dynamic-logic consumers (ConfigPilotPage, ManageSurge, FarePolicyStudio) use CONFIG/
POOLING domains whose fallback semantics are untouched — no changes needed there.

## Execution order & effort

| Step | Effort | Depends on |
|---|---|---|
| 1. BehaviourPack registry (overlay keys / toggles / block reasons per domain) | 0.5–1 d | — |
| 2. markCanonical affordance; author canonical rulebooks via verify flow (Rating + Cancellation-rate); seed default-city overlay templates | 1 d | 1 |
| 3. enable/disable/status endpoints (spec + handler + capabilities) | 2–3 d | 1 |
| 4. Fallback guardrail for `*_BEHAVIOR` domains | 0.5 d | — |
| 5. Control-centre: service fns + hooks + Behaviours switchboard page | 2–3 d | 3 |
| 6. Control-centre: canonical badges + "Save as canonical" in existing sections | 1 d | 2, 3 |
| 7. bumpToCanonical (API + UI) | 1 d | 3, 5 |

## Risks / notes

- `chooseLogic` with a single 0% rollout: verified never selected — safe "marker" state.
- Set `is_base_version`/`experiment_status` correctly on default rows so experiment
  tooling (`isExperimentRunning`, version stickiness) isn't confused.
- Elements table carries `merchant_id` but lookups ignore it — canonical saves should use
  a sentinel merchant id consistently.
- Multi-merchant cities (e.g. Delhi NY vs Sahakar): enablement is per
  merchant_operating_city_id, which already disambiguates.
- Don't change fallback semantics for existing CONFIG/POOLING domains — additive only.
- UI must treat the status endpoint as the single source of truth (no client-side
  recomputation of prerequisites), so backend pack changes reflect without UI releases.
- Governance trade-off of DB-first canonicals: no git review of rule changes. Compensated
  by element versioning + dashboard audit + capability-gated markCanonical; maker-checker
  can be added later if needed.
