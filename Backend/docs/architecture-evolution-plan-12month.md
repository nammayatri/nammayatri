# Namma Yatri: 12-Month Architecture Evolution Plan

**Date**: March 2026
**Scope**: Backend platform (48 Haskell packages, ~40K+ LOC)
**Framework**: Venkatesan's Yin/Yang — balancing Critical Path hardening (Yin) with Abundance (Yang)

---

## Current State Assessment

### Architecture Profile

| Dimension | Current State | Target State (12mo) |
|-----------|--------------|---------------------|
| **Services** | 48 packages, monorepo, single-process allocator | Sharded allocator, horizontally scalable services |
| **Database** | PostgreSQL (10-25 conn pool), no query-level metrics | Connection pooling via PgBouncer, query latency SLOs |
| **Cache** | Redis single + cluster, 50 max connections | Redis Sentinel, tiered caching, cache-aside patterns |
| **Observability** | Prometheus counters, basic logging | Distributed tracing, APM, SLO dashboards |
| **Resilience** | Basic retries (3x exponential), no circuit breakers | Circuit breakers, bulkheads, chaos-tested |
| **Throughput** | Allocator: ~300-500 jobs/sec peak | Allocator: ~2000+ jobs/sec with sharding |
| **Search Latency** | ~500ms+ (maps API bound) | <200ms (cached routes, parallel aggregation) |

### Critical Path (Yin Analysis)

The ride flow is the revenue-generating backbone: **Search -> Quote -> Confirm -> Allocate -> Track -> Complete -> Pay**. Every millisecond of latency and every point of failure here directly impacts revenue.

```
SINGLE POINTS OF FAILURE IDENTIFIED:
  [CRITICAL] Allocator (port 9996) — single process, all driver assignment serialized
  [CRITICAL] Redis cluster — location cache, scheduler queue, driver pool (no fallback)
  [HIGH]     OSRM dependency — blocking in snap-to-road, fare calculation
  [HIGH]     Juspay gateway — no circuit breaker, payment blocking
  [HIGH]     Maps API chain — search blocked on external geocoding
  [MEDIUM]   Kafka producer — event publishing in hot path
  [MEDIUM]   PostgreSQL — 10-25 conn pool, no PgBouncer, no read routing
```

### Abundance Assessment (Yang Analysis)

| Capability | Current | Gap |
|------------|---------|-----|
| Geographic redundancy | Single region | No multi-region failover |
| Service redundancy | Single instance per service (dev) | No auto-scaling |
| Data redundancy | PostgreSQL replicas exist | No automatic failover routing |
| Maps provider diversity | 3 providers (NextBillion, Google, MMI) | Fallback chain exists but sequential |
| Payment diversity | Juspay only | Single provider, no fallback |

---

## Yin/Yang Framework Application

### Yin (Critical Path — Centralize, Simplify, Strengthen)

The ride flow must be **simple, fast, and unbreakable**. Yin work reduces moving parts on the critical path, hardens what remains, and makes failures impossible or instantly recoverable.

**Principle**: Fewer dependencies, stronger guarantees, faster recovery.

### Yang (Abundance — Distribute, Diversify, Multiply)

Everything supporting the critical path should be **redundant, distributed, and self-healing**. Yang work adds capacity, alternatives, and resilience layers around the core.

**Principle**: More instances, more providers, more fallback paths.

---

## Quarter 1: STABILIZE (April - June 2026)

**Theme**: Fix critical reliability gaps. Make the system observable. Harden the ride critical path.

### Milestone 1.1: Observability Foundation (Weeks 1-4)

**Yin** — You cannot strengthen what you cannot see.

| Task | Description | Effort |
|------|-------------|--------|
| Distributed tracing | Instrument rider-app, driver-app, allocator with OpenTelemetry. Propagate trace IDs across BECKN calls and Kafka messages. | 3 engineers, 3 weeks |
| Query latency tracking | Add Prometheus histograms for PostgreSQL query latency per table (ride_booking, ride, search_request, payment_order). | 1 engineer, 2 weeks |
| Redis operation metrics | Track cache hit/miss ratios, operation latency, key cardinality for driver location and scheduler queues. | 1 engineer, 1 week |
| SLO dashboard | Define and instrument: Search P99 < 500ms, Allocation P99 < 2s, Payment P99 < 3s. Alert on SLO burn rate. | 1 engineer, 2 weeks |
| External dependency health | Health check endpoints for OSRM, Juspay, Maps APIs. Track availability and latency as Prometheus gauges. | 1 engineer, 1 week |

**Deliverable**: Grafana dashboards showing end-to-end ride flow latency, database query performance, cache efficiency, and external dependency health.

### Milestone 1.2: Critical Path Hardening (Weeks 3-8)

**Yin** — Reduce failure modes on the ride flow.

| Task | Description | Effort |
|------|-------------|--------|
| Circuit breakers for Juspay | Implement circuit breaker pattern (half-open/open/closed) around all Juspay API calls. Fallback: queue payment for retry. | 2 engineers, 2 weeks |
| Circuit breakers for Maps APIs | Wrap NextBillion/Google/MMI calls with circuit breakers. Parallelize fallback chain (fire all 3, take fastest). | 2 engineers, 2 weeks |
| OSRM local caching | Cache OSRM snap-to-road results in Redis (key: route hash, TTL: 1 hour). Reduces repeat calls for common routes. | 1 engineer, 2 weeks |
| Redis failover | Configure Redis Sentinel for automatic failover. Add read-from-replica for non-critical reads (cached queries). | 1 engineer, 2 weeks |
| Graceful degradation for search | If maps API is down, return approximate results using cached route data + straight-line distance. | 2 engineers, 2 weeks |

**Deliverable**: Zero single-external-dependency failures can take down the ride flow. Circuit breakers prevent cascade failures.

### Milestone 1.3: Allocator Reliability (Weeks 5-12)

**Yin** — Strengthen the single most critical bottleneck.

| Task | Description | Effort |
|------|-------------|--------|
| Allocator health monitoring | Deep health checks: job queue depth, processing latency, thread utilization. Alert when queue exceeds 1000 pending jobs. | 1 engineer, 1 week |
| Allocator graceful recovery | On crash recovery, resume from Redis queue state (currently possible but untested). Add integration test for crash-recovery. | 1 engineer, 2 weeks |
| Job priority queues | Separate high-priority (SendSearchRequestToDriver) from batch jobs (DailyUpdateTag, BadDebtCalculation). High-priority jobs get 70% of threads. | 2 engineers, 3 weeks |
| Dead letter queue | Jobs that fail 3x go to dead letter queue with alerting, instead of infinite retry. | 1 engineer, 1 week |

**Deliverable**: Allocator has priority queues, monitored health, and crash recovery guarantees.

### Q1 Resource Requirements

| Role | Count | Duration |
|------|-------|----------|
| Backend engineers (Haskell) | 4 | Full quarter |
| SRE / Infrastructure | 2 | Full quarter |
| Total engineer-months | **18** | |

### Q1 Expected Outcomes

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Mean time to detect (MTTD) | 15-30 min (manual) | <2 min (automated) | **10x** |
| Search availability during maps outage | 0% | ~80% (degraded) | **Critical** |
| Payment availability during Juspay outage | 0% | ~95% (queued) | **Critical** |
| Allocator crash recovery time | Unknown (manual restart) | <30s (automatic) | **Critical** |
| Observability coverage | ~20% of critical path | ~95% of critical path | **4.7x** |

---

## Quarter 2: OPTIMIZE (July - September 2026)

**Theme**: Database optimization. Caching strategy. Latency reduction across the board.

### Milestone 2.1: Database Optimization (Weeks 1-6)

**Yin** — Make the data layer fast and predictable.

| Task | Description | Effort |
|------|-------------|--------|
| PgBouncer deployment | Connection pooler in front of PostgreSQL. Move from 10-25 direct connections to 200+ pooled connections. Transaction-level pooling. | 1 SRE, 2 weeks |
| Query analysis and indexing | Analyze slow query log. Add composite indexes for: `(merchantId, status, createdAt)` on ride_booking, `(driverId, status)` on ride. Partial indexes for active rides. | 2 engineers, 3 weeks |
| Read replica routing | Route all `findAll*`, `findBy*` analytics queries to read replicas. Keep writes and critical-path reads on primary. Implement in Beam connection layer. | 2 engineers, 3 weeks |
| Table partitioning | Partition `search_request` by `createdAt` (daily). Partition `ride` by `status` (active vs completed). Auto-drop old search_request partitions (>24h). | 1 engineer, 3 weeks |
| Connection pool tuning | Profile connection usage. Set pool sizes based on actual P99: rider-app 50, driver-app 100, allocator 75. Add connection timeout of 5s. | 1 engineer, 1 week |

**Deliverable**: 3-5x more database throughput. Sub-millisecond simple queries. No connection exhaustion under load.

### Milestone 2.2: Tiered Caching Strategy (Weeks 3-8)

**Yang** — Multiple cache layers reduce load on every downstream system.

| Layer | Implementation | TTL | Hit Rate Target |
|-------|---------------|-----|-----------------|
| **L1: In-process** | Bounded LRU cache (already 100MB configured). Expand to merchant config, fare rules, service area polygons. | 60s | 95%+ for config |
| **L2: Redis** | Current Redis cache. Add: route cache (hash → distance/duration), driver pool snapshots, fare estimate cache. | 5min-1hr | 80%+ for routes |
| **L3: PostgreSQL** | Materialized views for: driver availability by zone (refresh 30s), active ride counts by city. | 30s refresh | N/A (source) |

| Task | Description | Effort |
|------|-------------|--------|
| Route result caching | Cache OSRM/Maps distance+duration results. Key: `(origin_geohash_7, dest_geohash_7, provider)`. Dramatically reduces maps API calls. | 2 engineers, 3 weeks |
| Fare estimate cache | Pre-compute and cache fare estimates for popular origin-destination pairs (top 1000 routes per city). | 1 engineer, 2 weeks |
| Driver pool snapshot | Instead of querying Redis per-search, maintain a materialized driver availability snapshot per zone (geohash-5). Refresh every 5s. | 2 engineers, 3 weeks |
| Cache warming on deploy | Pre-warm L1 caches on service startup. Load merchant configs, fare rules, and service areas before accepting traffic. | 1 engineer, 1 week |

**Deliverable**: 60-70% reduction in external API calls. Consistent sub-100ms for cached route lookups.

### Milestone 2.3: Search Latency Reduction (Weeks 5-12)

**Yin** — Make the highest-frequency user action blazing fast.

| Task | Description | Effort |
|------|-------------|--------|
| Parallel maps provider calls | Fire all 3 maps providers simultaneously. Take the first successful response. Cancel others. Current: sequential fallback. | 2 engineers, 2 weeks |
| Search request batching | Batch multiple search requests to the same BPP within a 50ms window. Reduces per-request overhead. | 2 engineers, 3 weeks |
| Pre-computed serviceability | Replace runtime geofence checks with pre-computed serviceability grid (geohash-6 resolution). Update hourly. | 1 engineer, 2 weeks |
| Quote aggregation streaming | Replace polling-based quote aggregation with Kafka streaming. Return partial results as they arrive (progressive loading). | 2 engineers, 4 weeks |

**Deliverable**: Search P99 from ~500ms to <200ms. 3x reduction in maps API costs.

### Q2 Resource Requirements

| Role | Count | Duration |
|------|-------|----------|
| Backend engineers (Haskell) | 5 | Full quarter |
| SRE / Infrastructure | 2 | Full quarter |
| Database engineer | 1 | Full quarter |
| Total engineer-months | **24** | |

### Q2 Expected Outcomes

| Metric | Before (post-Q1) | After | Improvement |
|--------|-------------------|-------|-------------|
| Search P99 latency | ~500ms | <200ms | **2.5x** |
| Database query P99 | ~50ms | <10ms | **5x** |
| Maps API calls per search | 3-5 | 1-2 (cached) | **60-70% reduction** |
| PostgreSQL connection utilization | 80%+ at peak | <40% with PgBouncer | **2x headroom** |
| OSRM calls per ride (location tracking) | ~50-100 | ~15-30 (cached) | **3x reduction** |
| Monthly maps API cost | Baseline | -60% | **Estimated 60% reduction (exact savings depend on current spend)** |

---

## Quarter 3: MIGRATE (October - December 2026)

**Theme**: Begin Rust migration of highest-impact services. Start with the allocator and location pipeline.

### Migration Strategy: Strangler Fig Pattern

We do NOT rewrite services wholesale. Instead:
1. Build Rust service alongside Haskell service
2. Route increasing traffic percentage to Rust via feature flag
3. Validate correctness with shadow mode (run both, compare results)
4. Cut over when Rust service matches or exceeds Haskell on all metrics
5. Decommission Haskell service

### Milestone 3.1: Rust Allocator (Weeks 1-8)

**Yang** — The allocator is the single biggest bottleneck. Rust enables horizontal scaling.

**Why Rust for the Allocator**:
- Current: Single Haskell process, 10 threads, ~300-500 jobs/sec
- Rust: Multi-threaded async (tokio), zero-cost abstractions, predictable latency (no GC pauses)
- Target: 2000+ jobs/sec per instance, horizontally shardable by city

| Task | Description | Effort |
|------|-------------|--------|
| Rust allocator core | Implement job dequeue, driver pool lookup, assignment logic in Rust. Use tokio for async. Redis client: `fred` or `redis-rs`. | 3 engineers, 4 weeks |
| City-based sharding | Partition allocator by `operatingCity`. Each shard handles allocation for one city. Redis key prefix by city. | 2 engineers, 2 weeks |
| Protocol compatibility | Rust allocator reads from same Redis queues, writes to same PostgreSQL tables. Zero change to Haskell services. | 1 engineer, 2 weeks |
| Shadow mode | Run Rust allocator in parallel. Compare assignment decisions with Haskell allocator. Log discrepancies. | 1 engineer, 2 weeks |
| Gradual rollout | Route 10% -> 25% -> 50% -> 100% of traffic per city, starting with lowest-traffic city. | 1 engineer, 2 weeks (ongoing) |

**Architecture**:
```
                    ┌─────────────────┐
                    │  Redis Queues    │
                    │  (per city)      │
                    └──────┬──────────┘
                           │
              ┌────────────┼────────────┐
              ▼            ▼            ▼
    ┌─────────────┐ ┌──────────┐ ┌──────────┐
    │ Allocator   │ │Allocator │ │Allocator │
    │ Bengaluru   │ │ Delhi    │ │ Chennai  │
    │ (Rust)      │ │ (Rust)   │ │ (Rust)   │
    └─────────────┘ └──────────┘ └──────────┘
         │              │            │
         ▼              ▼            ▼
    ┌─────────────────────────────────────┐
    │         PostgreSQL (shared)          │
    └─────────────────────────────────────┘
```

### Milestone 3.2: Rust Location Pipeline (Weeks 5-12)

**Yin** — Location processing is the highest-frequency operation. Rust eliminates GC pauses in the hot path.

**Why Rust for Location Updates**:
- Current: ~100 location updates/sec/driver, waypoint batching, OSRM calls
- Rust: SIMD-capable distance calculations, zero-copy buffer management, predictable sub-ms processing
- Target: 2-3x throughput per instance, <1ms processing latency (excluding OSRM)

| Task | Description | Effort |
|------|-------------|--------|
| Rust location processor | Implement waypoint ingestion, distance calculation, batch management. Use `geo` crate for haversine/vincenty. | 2 engineers, 4 weeks |
| OSRM client with connection pooling | Rust HTTP client (reqwest) with connection pooling to local OSRM. Batch snap-to-road requests. | 1 engineer, 2 weeks |
| Redis pipeline integration | Batch Redis operations using pipelining (not individual commands). 10x fewer round trips. | 1 engineer, 2 weeks |
| Toll detection optimization | Port toll geofence detection to Rust. Use R-tree spatial index for O(log n) lookups instead of linear scan. | 1 engineer, 2 weeks |
| Shadow mode and cutover | Same pattern as allocator: shadow, validate, gradual rollout. | 1 engineer, 2 weeks |

### Milestone 3.3: Rust FFI Bridge (Weeks 1-4)

**Yin** — For components not worth full migration, expose Rust functions to Haskell via FFI.

| Task | Description | Effort |
|------|-------------|--------|
| FFI bridge library | Create `nammayatri-rust-core` crate exposing: distance calculations, geohash operations, route matching. Haskell FFI bindings. | 2 engineers, 3 weeks |
| Replace hot-path pure functions | Swap Haskell distance/bearing calculations with Rust FFI calls in location-updates lib. | 1 engineer, 1 week |

### Q3 Resource Requirements

| Role | Count | Duration |
|------|-------|----------|
| Rust engineers | 4 | Full quarter |
| Backend engineers (Haskell, for integration) | 2 | Full quarter |
| SRE / Infrastructure | 2 | Full quarter |
| Total engineer-months | **24** | |

### Q3 Expected Outcomes

| Metric | Before (post-Q2) | After | Improvement |
|--------|-------------------|-------|-------------|
| Allocator throughput | ~500 jobs/sec (single) | ~2000+ jobs/sec (per shard) | **4x+** |
| Allocator P99 latency | ~200ms | <50ms | **4x** |
| Location processing latency | ~5ms | <1ms (excluding OSRM) | **5x** |
| Location pipeline memory | ~2GB (Haskell RTS) | ~200MB (Rust) | **10x** |
| GC pause impact on allocation | 10-50ms pauses | 0ms (no GC) | **Eliminated** |
| Horizontal scaling | 1 allocator instance | N instances (per city) | **Linear scaling** |

---

## Quarter 4: SCALE (January - March 2027)

**Theme**: Auto-scaling, chaos testing, cost optimization. Make the system self-healing.

### Milestone 4.1: Auto-Scaling Infrastructure (Weeks 1-6)

**Yang** — The system should grow and shrink with demand.

| Task | Description | Effort |
|------|-------------|--------|
| Kubernetes HPA for all services | Horizontal Pod Autoscaler based on: CPU (70%), memory (80%), custom metrics (request rate, queue depth). | 2 SRE, 3 weeks |
| Allocator auto-scaling | Scale Rust allocator shards based on pending job queue depth. Add shard when queue > 500 for > 30s. Remove when < 100 for > 5min. | 1 SRE + 1 engineer, 2 weeks |
| Database connection auto-scaling | PgBouncer pool sizing based on active connections. Scale read replicas based on query load. | 1 SRE, 2 weeks |
| Redis cluster auto-scaling | Add Redis cluster nodes based on memory utilization and operation latency. | 1 SRE, 2 weeks |
| Predictive scaling | Use historical ride demand patterns (morning rush, evening rush, weekend) to pre-scale 15 minutes before predicted surge. | 2 engineers, 3 weeks |

### Milestone 4.2: Chaos Testing (Weeks 3-8)

**Yin** — Prove resilience through controlled failure injection.

| Test Scenario | Method | Success Criteria |
|--------------|--------|-----------------|
| Redis cluster node failure | Kill 1 of 3 Redis cluster nodes | Zero dropped rides, <5s recovery |
| PostgreSQL primary failover | Promote replica to primary | <30s failover, zero data loss |
| Allocator instance crash | Kill one city allocator shard | Other shards unaffected, killed shard recovers in <10s |
| Maps API total outage | Block all maps API endpoints | Search still works with cached/approximate results |
| Juspay payment outage | Block Juspay endpoints | Rides complete, payments queued for retry |
| Kafka broker failure | Kill 1 of 3 Kafka brokers | Event publishing continues, <1% message loss |
| Network partition | Introduce 50% packet loss between services | Circuit breakers trigger, graceful degradation |
| CPU exhaustion | Limit service to 10% CPU | Requests slow but don't error, back-pressure applied |

| Task | Description | Effort |
|------|-------------|--------|
| Chaos testing framework | Integrate Litmus/Chaos Mesh with CI/CD. Define game days schedule (monthly). | 2 SRE, 3 weeks |
| Automated chaos in staging | Run chaos scenarios nightly in staging. Alert on any scenario that doesn't meet success criteria. | 1 SRE, 2 weeks |
| Runbook generation | For each failure mode, document: detection, impact, automated recovery, manual recovery. | 1 SRE + 1 engineer, 2 weeks |

### Milestone 4.3: Cost Optimization (Weeks 5-12)

**Yang** — Abundance doesn't mean waste. Right-size everything.

| Task | Description | Effort |
|------|-------------|--------|
| Spot instance strategy | Run non-critical services (dashboards, analytics, batch jobs) on spot instances. Save 60-70% on compute. | 1 SRE, 2 weeks |
| ClickHouse cold storage | Move ride data older than 90 days to S3-backed cold storage. Keep hot data in ClickHouse. | 1 engineer, 2 weeks |
| Redis memory optimization | Audit key patterns. Compress location data with MessagePack (currently JSON). Reduce TTLs where possible. | 1 engineer, 2 weeks |
| Right-size PostgreSQL | Analyze actual storage growth. Implement aggressive archival for search_request (>24h), expired quotes (>1h). | 1 engineer, 2 weeks |
| Maps API cost reduction | With route caching (Q2) + Rust efficiency (Q3), negotiate volume discounts with maps providers. Target 70% reduction from baseline. | 1 engineer, 1 week |
| Compute right-sizing | Profile actual CPU/memory usage per service post-Rust migration. Right-size Kubernetes resource requests/limits. | 1 SRE, 2 weeks |

### Milestone 4.4: Performance Validation (Weeks 9-12)

**Yin** — Validate the full stack under production-like load.

| Task | Description | Effort |
|------|-------------|--------|
| Load test at 10x current peak | Extend Locust suite to simulate 10x current peak ride demand. Validate all SLOs hold. | 2 engineers, 3 weeks |
| Soak test (72 hours) | Run system at 2x peak for 72 continuous hours. Monitor for memory leaks, connection leaks, queue growth. | 1 engineer, 1 week |
| Failover drill | Full production failover drill: primary region failure, recover in secondary. Target: <5min RTO, zero data loss. | 2 SRE, 1 week |

### Q4 Resource Requirements

| Role | Count | Duration |
|------|-------|----------|
| SRE / Infrastructure | 4 | Full quarter |
| Backend engineers (Rust + Haskell) | 3 | Full quarter |
| Total engineer-months | **21** | |

### Q4 Expected Outcomes

| Metric | Before (post-Q3) | After | Improvement |
|--------|-------------------|-------|-------------|
| Peak ride capacity | ~10K concurrent | ~100K concurrent | **10x** |
| Auto-scale response time | Manual (minutes-hours) | Automatic (<60s) | **Eliminated manual** |
| Monthly infrastructure cost | Baseline | -35% (right-sizing + spot) | **Significant** |
| Chaos test pass rate | 0% (no testing) | 95%+ (automated) | **New capability** |
| RTO (Recovery Time Objective) | Unknown | <5 min | **Defined and tested** |
| RPO (Recovery Point Objective) | Unknown | 0 (zero data loss) | **Defined and tested** |

---

## Yin/Yang Balance Summary

| Quarter | Yin (Harden) | Yang (Distribute) | Balance |
|---------|-------------|-------------------|---------|
| Q1 | Circuit breakers, allocator reliability, SLOs | Redis Sentinel, observability coverage | **70% Yin / 30% Yang** — Fix first |
| Q2 | Query optimization, search latency, connection tuning | Tiered caching, parallel providers, pre-computation | **50% Yin / 50% Yang** — Balanced |
| Q3 | Rust core (predictable latency, no GC), FFI bridge | City-sharded allocator, horizontal location pipeline | **40% Yin / 60% Yang** — Distribute |
| Q4 | Chaos testing, soak testing, failover drills | Auto-scaling, spot instances, cold storage | **30% Yin / 70% Yang** — Scale out |

The progression is deliberate: **strengthen the core first** (Q1), then **balance optimization with distribution** (Q2), then **migrate to enable scaling** (Q3), then **scale and prove resilience** (Q4).

---

## Cost-Benefit Analysis

### Total Investment

| Category | Q1 | Q2 | Q3 | Q4 | Total |
|----------|-----|-----|-----|-----|-------|
| **Engineering headcount** | 6 | 8 | 8 | 7 | — |
| **Engineer-months** | 18 | 24 | 24 | 21 | **87** |
| **Estimated cost (@ $15K/eng-month)** | $270K | $360K | $360K | $315K | **$1.305M** |
| **Infrastructure (new tooling)** | $20K | $30K | $25K | $35K | **$110K** |
| **Training (Rust upskilling)** | — | $15K | $10K | — | **$25K** |
| **Chaos/load testing infra** | — | — | — | $20K | **$20K** |
| **Total per quarter** | **$290K** | **$405K** | **$395K** | **$370K** | **$1.46M** |

### Expected Returns (Annual, post-implementation)

| Category | Estimated Annual Savings | Basis |
|----------|-------------------------|-------|
| **Maps API cost reduction** | $180K-300K | 70% reduction from route caching + parallel providers |
| **Compute cost reduction** | $150K-250K | Rust efficiency (10x memory, 4x throughput) + spot instances + right-sizing |
| **Database cost reduction** | $50K-100K | Partitioning + archival + PgBouncer (fewer/smaller instances) |
| **Redis cost reduction** | $30K-60K | MessagePack compression + TTL optimization (smaller cluster) |
| **Incident cost avoidance** | $200K-400K | Reduced MTTR, fewer outages, chaos-tested resilience |
| **Revenue from improved reliability** | $500K-1M+ | Higher completion rates, lower rider churn from faster search |
| **Total annual return** | **$1.11M - $2.11M** | |

### Payback Analysis

| Scenario | Total Investment | Annual Return | Payback Period |
|----------|-----------------|---------------|----------------|
| Conservative | $1.46M | $1.11M | **16 months** |
| Expected | $1.46M | $1.56M | **11 months** |
| Optimistic | $1.46M | $2.11M | **8 months** |

### Reliability Improvement (Non-Financial)

| Metric | Current | Post-12mo | Business Impact |
|--------|---------|-----------|-----------------|
| **Availability** | ~99.5% (estimated) | 99.95% | 4.3 fewer hours of downtime/year |
| **Search P99** | ~500ms | <100ms | Higher conversion, lower rider drop-off |
| **Ride completion rate** | Baseline | +2-5% | Direct revenue increase |
| **MTTR** | 15-30 min | <5 min | Reduced incident severity |
| **Blast radius of failure** | Entire system | Single city/service | Contained impact |

---

## Risk Register

| Risk | Probability | Impact | Mitigation |
|------|------------|--------|------------|
| Rust hiring difficulty | Medium | High | Start Rust training in Q1 for existing Haskell engineers. Haskell->Rust transition is natural. |
| Allocator migration data inconsistency | Medium | Critical | Shadow mode validation for 4+ weeks before any cutover. |
| PgBouncer introduces latency | Low | Medium | Benchmark in staging first. Transaction-level pooling minimizes overhead. |
| Scope creep in Rust migration | High | High | Strict strangler fig: only migrate what's defined. No scope expansion without re-evaluation. |
| Redis Sentinel split-brain | Low | High | Use odd number of sentinels (3+). Test failover monthly. |
| Maps provider contract changes | Low | Medium | 3-provider diversity already exists. Add OpenStreetMap as 4th fallback. |

---

## Success Criteria (12-Month Review)

The plan succeeds if, at month 12:

1. **Search P99 < 200ms** (from ~500ms) — measurable via Prometheus
2. **System availability > 99.95%** — measurable via uptime monitoring
3. **Allocator handles 10x current peak** — validated via load testing
4. **All chaos test scenarios pass** — automated monthly
5. **Infrastructure costs reduced 30%+** — measurable via cloud billing
6. **MTTR < 5 minutes** — measurable via incident tracking
7. **Zero single-point-of-failure in ride critical path** — validated via architecture review
8. **Rust services in production** for allocator + location pipeline — validated via deployment

---

## Quarterly Review Checkpoints

| Checkpoint | Date | Key Question |
|-----------|------|-------------|
| Q1 Review | July 1, 2026 | Can we detect and recover from any critical-path failure in <5 min? |
| Q2 Review | Oct 1, 2026 | Is search P99 < 200ms and database headroom > 50%? |
| Q3 Review | Jan 1, 2027 | Are Rust allocator and location pipeline handling production traffic? |
| Q4 Review | April 1, 2027 | Does the system survive chaos tests and handle 10x peak? |

At each review, re-evaluate priorities. If a quarter's outcomes weren't met, extend that quarter's work before moving to the next phase. The plan is sequential for a reason — each quarter builds on the previous.
