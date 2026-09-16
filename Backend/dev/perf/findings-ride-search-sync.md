# rideSearch (sync) — measured findings, 2026-09-16

Environment: local stack, NAMMA_YATRI Bangalore, Koramangala → Indiranagar, `?enableSyncSearch=true`,
64 seeded riders (one per concurrent user), rider-app and driver-app built `-O1 -eventlog` and run under
`, prof run <exe> rts`. Logging at INFO (`enableAPILatencyLogging` on), so every number below is the
apps' own `withTimeAPI` step timing, not an external probe.

## Baseline (before any change)

30 sequential searches, 7 estimates each, 30/30 inline results:

| | ms |
|---|---|
| p50 | 958 |
| p90 | 990 |
| p99 | 1009 |

Where it went (steps reconcile exactly):

| Step | p50 ms | share |
|---|---|---|
| `rideSearch:total` | 958 | 100% |
| ├─ `domainSearch` → `getRouteDetails` → `getRoutes` (OSRM) | 267 | 28% |
| ├─ `dispatchSearchToBpp` → `awaitSyncSearch` | 673 | 70% |
| │  ├─ `bppSyncSearch` (entire driver-app round trip) | 66 | 7% |
| │  ├─ `processOnSearchInline` | 16 | 2% |
| │  └─ `getQuotesFromInMemory` | 587 | 61% |
| └─ `createSearchRequest` | 6 | <1% |

## Finding 1 — driver-app is not the bottleneck

`syncSearch:total` = 64 ms at c=1 (94 ms p50 at 16 VUs). Inside it nothing exceeds 23 ms
(`getAllFarePolicies`), and `createQuotes` / `addNearestDriverInfo` are ~0. Bangalore prices 41 enabled
fare products for this trip. So the per-policy fan-out, the sequential `foldrM processPolicy`, and the
one-insert-per-quote `for_ quotes QQuote.create` are all real code smells but cost nothing measurable
here; optimising them would not have moved the request.

## Finding 2 (fixed) — an offers round trip for an empty basket

`buildGetQuotesRes` calls `getOffers … quoteList` and then `getEstimates … estimateList`
unconditionally. On an on-demand search the data is in `estimateList` and `quoteList` is empty, so the
quotes path called `offerListWithBasket` with `products = []` — a full provider round trip whose answer
can only be empty. Two `offers/list` calls were observed per search (256 ms + 252 ms, same request id).

Fix: guard both call sites with `not (null products)` (`Domain/Action/UI/Quote.hs`). The response is
consumed only via `Map.fromList productOffers` lookups keyed by product id, so with no products there
are no lookups — behaviour is identical.

Measured, same method as the baseline:

| | before | after | Δ |
|---|---|---|---|
| p50 | 958 ms | **664 ms** | −294 ms (−30.7%) |
| p99 | 1009 ms | 704 ms | −305 ms |
| `getQuotesFromInMemory` p50 | 587 ms | 289 ms | −298 ms |
| estimates per inline response | 7 | 7 | unchanged |
| throughput @16 VUs | 15.1 rps | **19.7 rps** | +30% |
| p50 @16 VUs | 1.03 s | 789 ms | −24% |

`getRoutes`, `bppSyncSearch` and `processOnSearchInline` were unchanged, confirming the delta is exactly
the removed call.

## Finding 3 — the gateway copy races the sync call (pre-existing)

In sync mode the search is *also* forked to the gateway (`fork "search cabs" $ dispatch`). Both entry
points take the same dedupe key (`setNxExpire (searchTxnDedupKey txnId merchantId) 60`). When the gateway
copy lands first the BPP rejects the sync call with "Search already processed by beckn", the rider logs
`INTERNAL_SYNC_SEARCH_FAILED` and returns `results = null`, so the client falls back to polling.

Observed: 22 driver-side rejections / 11 rider fallbacks during a ramp; at c=1, 2/30 (both matched to
driver-side rejections by txn id). Not caused by, and not affected by, Finding 2. Worth deciding whether
sync mode should dispatch to the gateway at all, or claim the key first.

## Finding 4 — flat ~250 ms per outbound HTTP call (not business logic)

Every outbound call from rider-app costs ~250 ms regardless of host, method or payload size, while the
same endpoints answer in 1–6 ms when called directly:

| call | in-app span | direct |
|---|---|---|
| OSRM `route/v1/...` | 256 ms | 1.2 ms |
| `offers/list/` | 256 ms | 1.5–6 ms |
| `offers/list/` (2nd) | 252 ms | — |
| Beckn gateway `search/` | **15 ms** | — |

Excluded by measurement: server latency, DNS (`localhost` resolves in 11 µs), connection churn, retries
(one "Ok response" per call, no retry lines), `timeoutMs` (200 s), the kernel's `callAPI'` wrapper (a log
line, a request-id header, `L.callAPI'` under latency measurement), and euler-hs manager resolution
(`addAuthManagersToFlowRt` does `HMS.unions` over maps that each insert the `"default"` key; the
interpreter receives an already-resolved manager). The 15 ms gateway call through identical machinery is
the counter-example that says this is not universal.

This dominates the request — ~500 of the original 958 ms — so if it reproduces off this laptop it matters
more than any code change here. **Next step: re-measure on a Linux dev box**, where `/metrics` also works
and `external_request_duration` gives per-host attribution directly.

## Not pursued, and why

- **Per-estimate config-pilot lookups**: `mkCumulativeOfferResp` (dynamic logic) and two
  `translateServiceTierText` per estimate ≈ 21 lookups per request. All in-memory cache hits here because
  this DB has no `app_dynamic_logic_rollout` experiments; with an experiment live, `selectActiveElementVersions`
  does a Redis GET **per call**, so this becomes a real N+1 in production. Unmeasurable locally.
- **Synchronous `OnSearchEvent.create`** before parsing in `buildOnSearchReqV2`, and the `createFares`
  JourneyLeg update on every non-multimodal search: real, but `processOnSearchInline` is only 15 ms.
- **Driver-side N+1s** (Finding 1): not worth the risk at 64 ms.

## Environment caveats

- `/metrics` returns HTTP 500 on macOS for both apps: `Kernel.Tools.Metrics.Init.serve` registers
  `procMetrics`, which reads Linux-only `/proc/<pid>/stat`, and one failing collector fails the whole
  scrape. Fix belongs in shared-kernel (register it only when `/proc/self/stat` exists).
- A 26 s outlier appeared once at VUS=8 on a cold app and did not reproduce warm (two clean VUS=8 runs).
  It was an orphaned fork: `awaitSyncSearch` times out at 5 s, the response returns `results = null`, and
  the abandoned fork keeps running — which is why inner steps logged 25 s while `rideSearch:total` maxed
  at 1.65 s.
