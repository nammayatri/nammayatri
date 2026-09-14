# Ride search latency: findings and plan

Status: on hold after PR #16670 (2026-09-10). Resume from section 8.

Companion pages (claude.ai artifacts, private): ranked audit
`https://claude.ai/code/artifact/180cd185-09f6-40f9-a1b7-9c2246fd7257`, flow diagrams
`https://claude.ai/code/artifact/292916ea-cf33-4b20-b17e-23f880c685fa`.

## 1. What the Grafana metric measures

`beckn_search_round_trip` (rider-app) starts at `Domain/Action/UI/Search.hs` after maps,
geocoding and passetto work, just before the Beckn fork, and stops in
`Domain/Action/Beckn/OnSearch.hs` when on_search has been persisted. It excludes the
`/rideSearch` pre-work and the results poll. Buckets are 0.5 s wide, so a TP50 under 1 s
is interpolated. The panel query was already TP99; only the legend said TP50.

Per-stage averages exist through `withTimeAPI`, exported as
`datastore_operation_duration{datastore, operation}`:

| datastore     | what it covers                                   |
|---------------|--------------------------------------------------|
| `search`      | driver-app estimate build (`handler` and stages) |
| `farePolicy`  | fare pipeline inside `getAllFarePolicies`        |
| `rideSearch`  | rider-app `/rideSearch`                          |
| `syncSearch`  | driver-app internal sync endpoint                |

Panel query for an average per stage:

```
sum(rate(datastore_operation_duration_sum[5m])) by (operation)
  / sum(rate(datastore_operation_duration_count[5m])) by (operation)
```

with legend `{{operation}}`.

## 2. Measurements (Bangalore, NAMMA_YATRI, 2026-09-08)

Driver-app estimate build, averages in seconds:

| Stage                     | Off-peak 12-14h | Morning peak 10-11h | Evening 16-20h |
|---------------------------|-----------------|---------------------|----------------|
| handler (whole build)     | 0.45            | 1.0-1.35            | 0.6-1.1        |
| getAllFarePolicies        | 0.25            | 0.6-0.85            | 0.35-0.75      |
| per trip category         | 0.14            | 0.3-0.47            | 0.2-0.45       |
| buildEstimatesAndQuotes   | 0.07            | 0.1-0.2             | 0.1-0.2        |
| createEstimates           | 0.07            | 0.1-0.2             | 0.1-0.2        |
| createSearchRequest       | 0.03            | 0.25                | 0.1-0.2        |
| everything else           | 0.03            |                     |                |

Inside the fare pipeline: `getFullFarePolicies` 0.05 off-peak rising to 0.25 at peak,
`buildDynamicPricingInputs` (QAR and friends) flat at about 0.06, `getAllFareProducts`
0.03 with cache-refill spikes, `getBaseVariantFarePolicy` 0.02.

Round-trip TP50 was 0.6-0.8 s off-peak, of which `handler` is about 0.45. The rest is
three signed hops, the gateway registry lookup and the rider-app on_search pre-ack and
persist.

## 3. Why the 09-11h and 16-20h plateau

- Bangalore pricing time bounds (`time_bound_config`, DYNAMIC-PRICING-UNIFIED):
  MorningPeakHours 08-09 and 10-11, MorningSuperPeakHours 09-10, PeakHours 16-17 and
  20-21, SuperPeakHours 17-20, NightHours 22-05.
- Inside a bound the per-tier dynamic pricing logic runs in `getFullFarePolicy`: the logic
  is refetched per tier and the full logic AST is info-logged per tier. That stage is 4-5x
  slower in-window and is the plateau. QAR inputs are not peak-sensitive.
- Pricing rollout edits during peak coincided with every rise. Rollout rows carry up to
  62 KB of embedded logic and the whole domain list is decoded on a sticky-version miss.
- Search volume does not explain it: peak volume was 09:00-09:50, the latency plateau
  10:00-11:00 with a sharp edge at 11:00.
- ML pricing is off for Bangalore and there are no time-bounded fare products there.

## 4. Sync search shadow (`sync_search_shadow:total`)

This is the "walk and save" shadow search. When `/rideSearch` finds a pickup or drop
point on the resolved route that removes a detour, the rider-app creates a second search
request with its own transaction id and a pointer to the parent, and sends it straight to
the driver-app internal `sync_search` endpoint, in parallel with the real search
(`API/UI/Search.hs`, `SharedLogic/BetterRoutePointSearch.hs`). The driver-app tags the
span `sync_search_shadow:total`; it wraps validation, the full `handler` build, the
value-add check and the on_search payload (`SharedLogic/SearchRequestProcessing.hs`).

Why it runs 0.3-0.4 s above `handler`: the shadow is priced against the parent's
dynamic-pricing inputs so it never gets a different congestion charge. It polls Redis for
the inputs the parent publishes, up to 10 tries at 100 ms (`SharedLogic/FarePolicy.hs`,
`waitForPublishedDpInputs`). Both searches are dispatched at the same moment, so the
shadow usually arrives before the parent has published and spends a few hundred
milliseconds waiting. Shadows are a small share of traffic, so their average is noisy;
isolated spikes to several seconds need the request count before being read as a
systematic issue. `handler` already includes shadow searches, so every customer search
with a suggestion doubles the driver-app estimate work.

Levers, in order: publish the inputs before dispatching the shadow, or subscribe instead
of polling; give shadows a fares-only path that skips estimate and quote persistence.

## 5. Full backlog

Phase 0, measure: named stage panels (done), results-poll timer around `getQuotes'` in
`API/UI/Quote.hs`, external-host panel.

Phase 1, driver-app parallelism: concurrent reverse geocodes in `Search.hs`, concurrent
LTS per tier, concurrent estimate loop, batched estimate and quote inserts, concurrent
per-category QAR fetch.

Phase 2, driver-app dedupe: fare pipeline once per search (item 1, done in PR #16670),
cache resolved QAR 30-60 s, congestion MGET, city in QAR GEO keys, settlement type once,
transporter config once in `FareProduct.hs`, logic once per search with quiet logs,
ambulance double calculation.

Phase 3, rider-app on_search and poll: ack earlier in `API/Beckn/OnSearch.hs`, cache the
whitelist count in `WhiteListOrgExtra.hs`, scope and back off the estimate-build lock
shared by on_search and the quotes poll, insurance config once, bulk inserts, skip
journey-leg update on taxi, fork auto-select, lighten the poll.

Phase 4, rider-app before the metric starts: bound the maps call, take passetto decrypts
off the path, concurrent enrichment, trim `createDSReq` (item 6, done in PR #16670),
rider config once, index `cached_route_response`, remove serial 5 s awaits.

Phase 5, transport: cache the gateway BPP lookup, floor the registry TTL, per-action
timeouts with sub-second backoff instead of 4 s and 8 s sleeps, `managerConnCount`,
replace the 45 s metrics sleeper, sync path for on-us searches.

Phase 6, process: no pricing rollout edits in peak, slim rollout rows, confirm prod flags.

## 6. What PR #16670 changes

Branch `dynamic-offer-driver-app/perf/search-fare-pipeline-write-chain`, rebased on
main at `d24459f886`.

### Item 6: `createDSReqFresh` (`Storage/Queries/SearchRequestExtra.hs`)

The generic `createDSReq` is written for entities that may already have location
mappings. For each mapping it first demotes the previous version at that order, for the
drop it reads the current max order, and each location insert is guarded by a
`findById`. For a search request created in the search handler all five reads are
guaranteed misses, and a KV miss falls through to Postgres. With the five sequential
writes that was about ten sequential DB operations per search, and the stage went from
0.03 s off-peak to 0.25 s under load.

`createDSReqFresh` builds the mappings in memory with orders known up front (pickup 0,
stops 1..n, drop n+1, version LATEST), issues no lookups, and runs the location, mapping
and search-request writes concurrently through `runWritesConcurrently`, which rethrows
the first failed write instead of dropping it. The handler calls it; `createDSReq` stays
for callers that remap existing entities.

### Item 1: `getAllFarePoliciesProducts` (`SharedLogic/FarePolicy.hs`)

A search prices several trip categories at once (a plain one-way search prices both
`OneWay RideOtp` and `OneWay OnDemandDynamicOffer`). The handler ran the whole fare
pipeline once per category. Only fare products and full fare policies are category
specific; transporter config, special-location resolution and the dynamic-pricing Redis
inputs were fetched once per category, and the wrapper span (0.25 s) was well above the
per-category span (0.14 s), so the two runs did not even overlap well.

The new function takes every category of the search and works in four steps:
per-category fare products and tier resolution (cached reads), dynamic-pricing inputs
built once for the union of vehicle categories, base CAR fare per category, then one
concurrent `getFullFarePolicy` batch across all products of all categories. The result
is combined the way the handler's removed `combineFarePoliciesProducts` did.
`getAllFarePoliciesProduct` is kept as a single-category wrapper for the other callers.

Behaviour change to flag to the pricing owner: the random toss used for percentage
rollouts is now drawn once per search rather than once per category, so all categories
of a search land in the same rollout arm.

Panel note: the per-category span `withTimeAPI "search" "getAllFarePoliciesProduct"` no
longer exists. `getAllFarePolicies` and the `farePolicy` spans remain.

## 7. Verification state

- Before the rebase, all three modules compiled under `-Wall -Werror` in a full
  `cabal build` pass and loaded clean in `cabal repl`.
- After the rebase, the local machine cannot build the driver-app library at all: GHC
  9.2.7 panics at the start of the library build (`lookupModuleWithSuggestions` on
  `Domain.Types.Ride`), identically on untouched main, after clearing the package build
  directory, its in-place registration and the cabal plan cache. The rebased code has not
  been compiled yet. Build on the dev box (`, run-cabal-build-devbox`).
- Not yet run: before/after estimate comparison for a fixed search in dev.

## 8. Resume checklist

1. Dev-box build of the PR branch; fix anything the upstream fare-policy revamp changed
   around `getFullFarePolicy` arguments if the build complains.
2. Fixed test search in dev, compare estimate count and fares before and after.
3. Tell the pricing owner about the shared toss.
4. Release, then read the `datastore="search"` panel: `createSearchRequest` should sit
   at or under 0.01 s and stop rising with load, `getAllFarePolicies` should approach
   the old per-category value or below, `handler` down by the sum. Expected roughly
   -0.1 s off-peak and -0.3 s at peak on `handler`.
5. Fix the round-trip panel legend (TP50 vs TP99 per query) and remove any panel query
   that still references the `getAllFarePoliciesProduct` span.
6. Pick the next items: the shadow-search wait (section 4), then Phase 1 parallelism.

## 9. Open questions

- What was the 0.1 s non-overlap between `getAllFarePolicies` and its per-category span?
  Answerable from the panels after release.
- Which stage carried the 14:15 and 14:35 spikes on 2026-09-08 (no config change then)?
- Poll endpoint `/rideSearch/{id}/results` latency is unmeasured until the timer lands.
