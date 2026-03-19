# Incident Response Playbooks

> "RCA/CoE process, done with rigour and made sacred." — Venkatesan

These playbooks cover the 10 most critical failure scenarios for the Namma Yatri backend platform. Each follows a structured protocol: Detection, Impact Assessment, Immediate Mitigation, Root Cause Investigation, Resolution, and Post-Incident Review.

**Severity Levels:**
- **P0 (Critical):** Revenue-impacting, all rides affected, immediate war room
- **P1 (High):** Major feature degraded, subset of rides affected
- **P2 (Medium):** Partial degradation, workarounds available
- **P3 (Low):** Minor impact, no revenue loss

**General Principles:**
1. Acknowledge the incident within 5 minutes of detection
2. Assign an Incident Commander (IC) immediately
3. Communicate status every 15 minutes until resolved
4. Never make changes without rollback plan
5. Document every action taken during the incident

---

## Playbook 1: PostgreSQL Primary Database Failure

**Severity: P0**

### Detection

| Signal | Source | Threshold |
|--------|--------|-----------|
| Connection pool exhaustion | Prometheus metrics (port 9999/9997) | Pool usage > 90% for 2 min |
| Query timeouts | Application logs | > 50 timeouts/min |
| Replication lag | `pg_stat_replication` | Lag > 30 seconds |
| Service 5XX spike | Load balancer metrics | > 5% error rate |
| PostgreSQL process down | Host monitoring / PG health check | Process absent or unresponsive |

**Automated alerts:** Connection refused errors from `esqDBEnv` in rider-app (port 8013) or driver-app (port 8016).

### Impact Assessment

| Component | Impact |
|-----------|--------|
| rider-app (8013) | Cannot create bookings, process searches, or update ride state |
| driver-app (8016) | Cannot accept rides, update location in DB, process payments |
| allocator (9996) | Cannot assign drivers to rides — all 33 job types fail |
| scheduler (8058) | Background jobs (payment intents, notifications, fees) halt |
| dashboards | Operations team loses visibility |

**Schema mapping:**
- `atlas_app` — all rider-side data (bookings, searches, payments)
- `atlas_driver_offer_bpp` — all driver-side data (rides, allocations, earnings)

**Revenue impact:** Complete halt of new ride creation and in-progress ride completion. Estimated revenue loss scales linearly with outage duration.

### Immediate Mitigation

```bash
1. VERIFY the failure:
   $ psql -h <primary_host> -p 5434 -U <user> -d atlas_dev -c "SELECT 1;"
   $ psql -h <replica_host> -p 5434 -U <user> -d atlas_dev -c "SELECT 1;"

2. If primary is down but replica is up:
   - Promote read replica to primary (database-specific procedure)
   - Update connection config to point esqDBCfg to the promoted replica
   - Pool sizes: rider-app=10, driver-app=25 (from dhall config)
   - Restart affected services with updated config

3. If both primary and replica are down:
   - Check disk space, OOM, and OS-level issues on DB hosts
   - Attempt PostgreSQL restart
   - If data corruption suspected, restore from latest backup — DO NOT attempt repair on live data

4. Communicate to operations team immediately:
   - "Database primary failure. New rides cannot be created.
      In-progress rides on cached state. ETA for resolution: [X] minutes."
```

### Root Cause Investigation

```bash
1. Check PostgreSQL logs:
   $ tail -1000 /var/log/postgresql/postgresql-*.log

2. Check disk and memory:
   $ df -h /var/lib/postgresql/
   $ free -m
   $ dmesg | grep -i "oom\|kill"

3. Check connection state:
   $ psql -c "SELECT count(*), state FROM pg_stat_activity GROUP BY state;"
   $ psql -c "SELECT * FROM pg_stat_replication;"

4. Check for long-running queries / locks:
   $ psql -c "SELECT pid, now() - query_start AS duration, query
              FROM pg_stat_activity WHERE state = 'active'
              ORDER BY duration DESC LIMIT 20;"

5. Check for recent migrations:
   $ ls -lt Backend/dev/migrations/rider-app/ | head -10
   $ ls -lt Backend/dev/migrations/dynamic-offer-driver-app/ | head -10

Common root causes:
- Connection pool exhaustion (max 10/25 connections per service)
- Disk full on DB host
- OOM kill by OS
- Long-running transaction holding locks
- Bad migration / schema change
- Network partition between app and DB hosts
```

### Resolution

1. Restore database connectivity (restart, failover, or restore from backup)
2. Verify replication is healthy: `SELECT * FROM pg_stat_replication;`
3. Verify application connectivity: check logs for successful DB queries
4. Verify ride flow end-to-end: test search → select → init → confirm
5. Monitor for 30 minutes before declaring resolved

### Post-Incident Review (CoE)

- [ ] Timeline of events (minute-by-minute)
- [ ] Was detection automated or manual? If manual, what alert should exist?
- [ ] Was the connection pool size adequate? (rider: 10, driver: 25)
- [ ] Was replica promotion tested before this incident?
- [ ] Action items: backup verification schedule, failover drill cadence
- [ ] Update runbook with any new findings

---

## Playbook 2: Redis Cluster Failure

**Severity: P0**

### Detection

| Signal | Source | Threshold |
|--------|--------|-----------|
| Redis connection errors | Application logs (`hedisClusterEnv`) | > 10 errors/min |
| Cache miss rate spike | Prometheus metrics | Miss rate > 80% |
| Scheduler jobs stalling | `Scheduled_Jobs` / `Scheduled_Jobs_Rider` streams | No jobs processed for > 60s |
| Location tracking failures | LTS Redis (`ltsHedisEnv`) errors | > 5% error rate |
| CLUSTER INFO state | Redis CLI | `cluster_state:fail` |

**Redis topology:**
- Single instance: port 6379 (general caching, 50 max connections)
- Primary cluster: port 30001 (job queues, distributed locks, critical cache)
- Secondary cluster: port 30002 (failover)
- LTS Redis: dedicated instance for location tracking

### Impact Assessment

| Redis Component | Failure Impact |
|-----------------|---------------|
| Single instance (6379) | Cached queries fail, config cache stale (kvConfigUpdateFrequency=10s), rate limiting disabled |
| Primary cluster (30001) | Scheduler halts (all 67 job types across rider+driver), distributed locks fail, PT circuit breaker state lost |
| Secondary cluster (30002) | Reduced redundancy, non-critical operations degrade |
| LTS Redis | Real-time driver location data unavailable, nearBy search fails |

**Critical Redis keys at risk:**
- `Scheduled_Jobs` / `Available_Jobs` — driver allocator job streams
- `Scheduled_Jobs_Rider` / `Available_Jobs_Rider` — rider scheduler streams
- `RunnningJob:{jobId}` — distributed execution locks
- `pt:failures:{mocId}:{mode}:{apiType}` — circuit breaker state
- `pt:fareCache:*` — cached public transport fares

### Immediate Mitigation

```bash
1. IDENTIFY which Redis tier is affected:
   $ redis-cli -p 6379 PING          # Single instance
   $ redis-cli -p 30001 CLUSTER INFO  # Primary cluster
   $ redis-cli -p 30002 CLUSTER INFO  # Secondary cluster

2. If primary cluster (30001) is down:
   a. Check if secondary cluster (30002) is healthy
   b. Toggle cutOffHedisCluster=True in config to fall back to single instance
   c. Restart services — 90-second grace period applies (graceTerminationPeriod)

3. If single instance (6379) is down:
   a. Attempt Redis restart
   b. Services will fall through to DB for cache misses — monitor DB load
   c. Rate limiting will be disabled — enable external rate limiting if available

4. If LTS Redis is down:
   a. Location tracking degrades — drivers visible but stale positions
   b. nearBy driver search will use secondary cloud (if configured)
   c. Multi-cloud fallback: primary call errors propagate, secondary returns []

5. Monitor connection pool usage (50 max connections per Redis tier):
   $ redis-cli -p <port> INFO clients
```

### Root Cause Investigation

```bash
1. Check Redis logs and memory:
   $ redis-cli -p <port> INFO memory
   $ redis-cli -p <port> INFO persistence
   $ redis-cli -p <port> SLOWLOG GET 20

2. Check cluster health:
   $ redis-cli -p 30001 CLUSTER NODES
   $ redis-cli -p 30001 CLUSTER SLOTS

3. Check for memory pressure:
   $ redis-cli -p <port> INFO stats | grep evicted_keys
   $ redis-cli -p <port> DBSIZE

4. Check for hot keys / large keys:
   $ redis-cli -p <port> --hotkeys
   $ redis-cli -p <port> --bigkeys

Common root causes:
- Memory exhaustion (maxmemory reached, eviction policy triggered)
- Cluster node failure / split brain
- Network partition between cluster nodes
- Slow commands blocking event loop (KEYS *, large SORT)
- RDB/AOF persistence stalling on disk I/O
```

### Resolution

1. Restore Redis cluster to healthy state
2. Verify cluster topology: `CLUSTER NODES` shows all nodes as connected
3. Verify scheduler is processing jobs: check `Available_Jobs` stream length decreasing
4. Verify distributed locks are working: allocator assigns rides
5. Re-enable `cutOffHedisCluster=False` if it was toggled
6. Monitor cache hit rates for 30 minutes

### Post-Incident Review (CoE)

- [ ] Was the `cutOffHedisCluster` / `cutOffNonCriticalHedisCluster` failover tested?
- [ ] Is the 50 max connection limit adequate under peak load?
- [ ] Should LTS Redis be on a separate physical host from general cache?
- [ ] Were scheduler jobs recovered or lost? What is the job replay strategy?
- [ ] Memory capacity planning: current usage vs allocated

---

## Playbook 3: Kafka Broker Failure

**Severity: P1**

### Detection

| Signal | Source | Threshold |
|--------|--------|-----------|
| Producer send failures | Application logs (`KafkaProducerTools`) | > 10 failures/min |
| Consumer lag | Kafka consumer group metrics | Lag > 10,000 messages |
| Location updates stale | Location tracking service | No updates > 30 seconds |
| Event stream gaps | ClickHouse `atlas_kafka` database | Missing events for > 2 min |
| Broker unreachable | Kafka health check | Broker at 29092 not responding |

**Kafka topology:**
- Broker: `localhost:29092`
- Compression: LZ4
- Primary + secondary producer for failover

### Impact Assessment

| Topic | Impact When Unavailable |
|-------|------------------------|
| `location-updates` | Driver positions not updated — nearBy search shows stale data, distance calculation stops |
| `rider-app-events-updates` | Ride events not propagated — analytics gap, no ClickHouse data |
| `dynamic-offer-driver-events-updates` | Driver events lost — allocator may not see driver state changes |
| `broadcast-messages` | Driver notifications delayed or lost |
| `AutoCompleteData` | Location autocomplete analytics missing |
| `EventTracker` | Event tracking data lost (non-critical) |

**Key distinction:** Kafka failure does NOT block the ride flow directly (HTTP-based). It affects:
- Real-time location tracking (driver positions stale)
- Analytics and event propagation
- Asynchronous notifications
- ClickHouse data pipeline

### Immediate Mitigation

```bash
1. VERIFY broker status:
   $ kafka-broker-api-versions --bootstrap-server localhost:29092
   $ kafka-topics --bootstrap-server localhost:29092 --list

2. Check producer health:
   - Look for "Failed to produce" in service logs
   - Secondary Kafka producer should auto-failover if configured

3. If broker is down:
   a. Attempt broker restart
   b. If multi-broker setup: verify remaining brokers handle partition leaders
   c. Services continue operating (HTTP-based ride flow unaffected)
   d. Location data will be stale — inform operations team

4. Check consumer groups:
   $ kafka-consumer-groups --bootstrap-server localhost:29092 --describe --all-groups

5. Priority: restore `location-updates` topic first (most time-sensitive)

6. Communicate:
   - "Kafka broker failure. Rides continue operating.
      Driver locations may be stale. Analytics pipeline paused."
```

### Root Cause Investigation

```bash
1. Check Kafka logs:
   $ tail -1000 /var/log/kafka/server.log

2. Check disk space (Kafka is disk-intensive):
   $ df -h /var/lib/kafka/

3. Check JVM heap:
   $ jstat -gcutil <kafka_pid> 1000

4. Check ZooKeeper connectivity (if applicable):
   $ echo ruok | nc localhost 2181

5. Check ISR (In-Sync Replicas):
   $ kafka-topics --bootstrap-server localhost:29092 \
     --describe --under-replicated-partitions

Common root causes:
- Disk full (log retention not configured properly)
- JVM OOM / GC pauses
- ZooKeeper session timeout
- Network partition
- Too many partitions per broker
```

### Resolution

1. Restore broker and verify topic availability
2. Check consumer group offsets — consumers will auto-resume from last committed offset
3. Verify location-updates topic is flowing: driver positions refresh in < 5s
4. Check ClickHouse `atlas_kafka` for event gap — may need backfill
5. Verify secondary producer failback if it was activated
6. Monitor consumer lag for 30 minutes

### Post-Incident Review (CoE)

- [ ] Did the secondary Kafka producer failover activate? Did it work?
- [ ] What was the data loss window? Can events be replayed?
- [ ] Is log retention configured to prevent disk exhaustion?
- [ ] Should location-updates have a dedicated Kafka cluster?
- [ ] Consumer auto-recovery: did all consumer groups resume cleanly?

---

## Playbook 4: Google Maps API Outage

**Severity: P1**

### Detection

| Signal | Source | Threshold |
|--------|--------|-----------|
| Maps API HTTP errors | Application logs (Google Maps calls) | > 5% error rate |
| Maps API latency | Response time metrics | P99 > 5 seconds |
| Google status page | https://status.cloud.google.com/ | Maps API incident reported |
| Search failures | rider-app search flow | Distance/duration calculations failing |
| Autocomplete failures | Place autocomplete API | Suggestions not returning |

**Google Maps endpoints used:**
- `https://maps.googleapis.com/maps/api/` — Directions, Distance Matrix
- `https://roads.googleapis.com/` — Snap to road
- `https://places.googleapis.com/v1/` — Place autocomplete (new API)

### Impact Assessment

| Feature | Impact |
|---------|--------|
| Ride search | Distance and duration estimates fail — fare calculation impossible |
| Route planning | Multimodal journey planning (`getMultimodalWalkDistance`) degrades |
| Place autocomplete | Users cannot search for destinations |
| Snap-to-road | Driver route tracking loses accuracy |
| ETA calculation | Arrival time estimates unavailable |

**Built-in fallbacks:**
- OSRM is configured as alternative routing engine via `MerchantServiceUsageConfig`
- If OSRM also fails, proportional duration calculation kicks in
- `withShortRetry` (3 retries, 2x backoff) wraps maps API calls

### Immediate Mitigation

```bash
1. VERIFY Google Maps is the issue (not our config/quota):
   $ curl "https://maps.googleapis.com/maps/api/directions/json?\
     origin=12.9716,77.5946&destination=12.9352,77.6245&key=<API_KEY>"

2. Check API quota and billing:
   - Google Cloud Console → APIs & Services → Maps API → Quotas
   - Look for "OVER_QUERY_LIMIT" or "REQUEST_DENIED" errors

3. If Google Maps is genuinely down:
   a. Switch to OSRM routing via MerchantServiceUsageConfig
   b. OSRM handles: distance calculation, route generation, snap-to-road
   c. Place autocomplete has NO automatic fallback — rides can still be booked
      with lat/lng directly from the app's map picker

4. If OSRM is also degraded:
   a. Proportional duration fallback activates automatically
   b. Fare estimates will be approximate but rides still bookable

5. Communicate:
   - "Google Maps API outage. Search using OSRM fallback.
      Place autocomplete may be unavailable. Fare estimates approximate."
```

### Root Cause Investigation

```bash
1. Confirm outage scope:
   - Check https://status.cloud.google.com/
   - Check if issue is regional or global
   - Check if specific API (Directions vs Places) is affected

2. Check our API key and quota:
   - API key not expired or restricted?
   - Billing account active?
   - Daily quota exceeded?

3. Check network path:
   $ traceroute maps.googleapis.com
   $ curl -v "https://maps.googleapis.com/maps/api/"

4. Check error details in application logs:
   - HTTP status codes (403 = auth, 429 = rate limit, 5xx = Google outage)
   - Error response bodies

Common root causes:
- Google-side outage (nothing we can do but wait + use fallbacks)
- API key quota exceeded (increase quota or add secondary key)
- Billing issue (payment method expired)
- IP restriction misconfigured on API key
```

### Resolution

1. Confirm Google Maps API is restored (test all three endpoints)
2. Switch back from OSRM to Google Maps in MerchantServiceUsageConfig
3. Verify place autocomplete works end-to-end
4. Verify fare estimates match expected values (not using proportional fallback)
5. Monitor error rates for 30 minutes

### Post-Incident Review (CoE)

- [ ] How quickly did OSRM fallback activate? Was it automatic or manual?
- [ ] What was the fare accuracy impact during OSRM/proportional fallback?
- [ ] Should we run OSRM as permanent primary to reduce Google dependency?
- [ ] Is there a place autocomplete fallback we should build?
- [ ] API key rotation and quota monitoring: are alerts in place?

---

## Playbook 5: Payment Gateway (Juspay) Outage

**Severity: P0**

### Detection

| Signal | Source | Threshold |
|--------|--------|-----------|
| Payment API errors | `lib/payment` logs | > 5% error rate on createOrder |
| Webhook delivery failures | `WebhookHandler` retry count | > 3 retries per webhook |
| Payment status stuck | Orders stuck in PENDING | > 50 orders pending > 5 min |
| Payout failures | Payout state machine | INITIATED → not progressing |
| Juspay status page | Juspay merchant dashboard | Incident reported |

**Payment integration points:**
- `createOrderService()` — order creation
- `orderStatusService()` — status polling
- `juspayWebhookService()` — asynchronous payment confirmations
- `createRefundService()` — refund processing
- `createPaymentIntentService()` / `chargePaymentIntentService()` — payment intents

### Impact Assessment

| Flow | Impact |
|------|--------|
| Ride payment | Riders cannot pay for completed rides |
| Driver payouts | INITIATED → SUBMITTED → EXECUTED flow halts |
| Subscription mandates | `MandateExecution` scheduler jobs fail |
| Refunds | Pending refunds not processed |
| Auto-pay | `ExecutePaymentIntent` scheduler jobs fail |

**Critical:** Rides CAN still be completed. Payment is asynchronous. But:
- Cash rides unaffected
- Digital payment rides complete but payment hangs
- Driver earnings not settled

**Webhook retry strategy:** Exponential backoff: `rescheduleTimeThreshold * (2 ^ retryCount)`, modes: BATCHING or REAL_TIME.

### Immediate Mitigation

```bash
1. VERIFY Juspay is the issue:
   - Check Juspay merchant dashboard for incident status
   - Test API endpoint directly with a test order
   - Check for HTTP 5xx vs 4xx (our issue vs their issue)

2. If Juspay is down:
   a. Rides continue — payment is async and will settle when Juspay recovers
   b. Webhook retries will queue up automatically (exponential backoff)
   c. DO NOT retry aggressively — respect backoff to avoid overwhelming recovery
   d. Consider enabling Stripe as alternative if configured:
      - `stripeWebhookService()` exists as alternative handler

3. For driver payouts:
   a. Payouts in INITIATED state are safe — idempotency check via `isPayoutExecutable()`
   b. DO NOT manually advance payout state machine
   c. Payouts will resume when Juspay recovers

4. For mandate executions:
   a. `MandateExecution` scheduler jobs will retry on next loop (5-second interval)
   b. Monitor `Scheduled_Jobs` stream for job accumulation

5. Communicate:
   - "Payment gateway outage. Rides continue operating.
      Payments will settle automatically when gateway recovers.
      Cash rides unaffected. Driver payouts delayed."
```

### Root Cause Investigation

```bash
1. Check Juspay response codes and bodies:
   - 503 Service Unavailable → Juspay outage
   - 429 Too Many Requests → Rate limiting (our side)
   - 401/403 → API key / merchant config issue

2. Check webhook delivery status:
   - Query WebhookHandler state: PENDING vs FAILED vs RETRIES_ENDED
   - Count of webhooks in FAILED state

3. If our side:
   - API key rotation? Check merchant credentials
   - Network connectivity to Juspay endpoints
   - TLS certificate issues

4. Check payment order table for stuck orders:
   - Orders in PENDING > 5 minutes
   - Last successful webhook timestamp

Common root causes:
- Juspay platform outage (most common)
- API credential expiry
- Network/DNS issues reaching Juspay
- Rate limiting from too aggressive retries
- Webhook endpoint unreachable from Juspay's side
```

### Resolution

1. Confirm Juspay API is healthy (test createOrder + orderStatus)
2. Check webhook backlog is draining (retries succeeding)
3. Verify stuck payments are resolving: PENDING → SUCCESS/FAILED
4. Verify driver payouts resume: INITIATED → SUBMITTED → EXECUTED
5. Reconcile any payments that fell into RETRIES_ENDED state — manual review
6. Monitor for 1 hour (payment settlement is slower than ride operations)

### Post-Incident Review (CoE)

- [ ] How many payments were affected? Revenue impact?
- [ ] Did webhook retry logic work correctly? Any RETRIES_ENDED that need manual fix?
- [ ] Is Stripe failover viable as automatic fallback?
- [ ] Should we implement a payment queue that buffers during outages?
- [ ] Payout state machine: did idempotency hold? Any duplicate payouts?
- [ ] SLA review: what is our contractual uptime with Juspay?

---

## Playbook 6: Complete rider-app Service Failure

**Severity: P0**

### Detection

| Signal | Source | Threshold |
|--------|--------|-----------|
| Health check failures | Load balancer health checks | 3 consecutive failures |
| Port 8013 unreachable | TCP probe | Connection refused |
| Metrics endpoint down | Prometheus scrape (port 9999) | Scrape failures > 2 min |
| Customer app errors | Mobile app error reporting | API calls returning network errors |
| Zero searches | Business metrics | Search count = 0 for > 2 min |

### Impact Assessment

**rider-app (port 8013) handles ALL customer-facing APIs:**

| Feature | Status |
|---------|--------|
| Ride search | DOWN — no new ride requests |
| Booking | DOWN — no new bookings |
| Ride tracking | DOWN — customers cannot see ride status |
| Payment | DOWN — customer payment APIs unavailable |
| BECKN callbacks | DOWN — on_search, on_select, on_confirm not received |
| Estimated fares | DOWN — no fare estimates |
| Place autocomplete | DOWN — no destination search |
| Customer auth | DOWN — no login/registration |

**What still works:**
- driver-app (8016) — drivers can continue ongoing rides
- allocator (9996) — can still process existing allocations
- scheduler (8058) — background jobs continue independently
- Kafka consumers — continue processing events

**BECKN impact:** BAP (Beckn Application Platform) is down. No new searches enter the BECKN network. In-progress rides on driver side continue but rider cannot see updates.

### Immediate Mitigation

```bash
1. CHECK process status:
   $ ps aux | grep rider-app
   $ curl -s http://localhost:8013/health  # If health endpoint exists
   $ curl -s http://localhost:9999/metrics  # Prometheus metrics endpoint

2. CHECK logs for crash reason:
   $ journalctl -u rider-app --since "5 minutes ago" -n 500
   OR
   $ tail -500 /var/log/rider-app/rider-app.log

3. Common crash causes and immediate fixes:
   a. OOM Kill:
      $ dmesg | grep -i "oom.*rider"
      → Restart with increased memory limits

   b. Port conflict:
      $ lsof -i :8013
      → Kill conflicting process, restart

   c. DB connection failure:
      → Check PostgreSQL (see Playbook 1)

   d. Redis connection failure:
      → Check Redis (see Playbook 2)

   e. Config error (bad dhall):
      → Check recent config changes, rollback if needed

4. RESTART the service:
   - Ensure graceful shutdown completes (90-second grace period)
   - Verify startup sequence: DB connect → Redis connect → Kafka → HTTP server

5. If restart fails repeatedly:
   a. Rollback to last known good deployment
   b. Check for recent code or config changes
   c. Verify all dependencies are reachable from rider-app host

6. Communicate:
   - "Rider app service is down. Customers cannot book new rides.
      In-progress rides on driver side continue. Restarting service."
```

### Root Cause Investigation

```bash
1. Check startup initialization order (from Environment.hs):
   - Dhall config loading
   - Metrics server init
   - AppEnv build
   - PostgreSQL connection (primary + replica, pool size 10)
   - Redis connections (primary, cluster, non-critical, LTS)
   - Kafka producer init
   - ClickHouse connections
   - HTTP server start (Warp)

2. Check for resource exhaustion:
   $ ulimit -a                    # Check open file limits
   $ ls /proc/<pid>/fd | wc -l   # File descriptor count
   $ netstat -an | grep 8013 | wc -l  # Connection count

3. Check for thread/memory leaks:
   - GHC RTS stats if available
   - Heap profile if -hT enabled

4. Check recent deployments:
   $ git log --oneline -10
   - Any recent changes to rider-app?

5. Check graceful shutdown state:
   - Was isShuttingDown TMVar triggered?
   - Did releaseAppEnv complete? (Kafka, logging, Redis disconnect)

Common root causes:
- Memory leak in long-running service
- Dependency failure cascade (DB/Redis/Kafka)
- Bad deployment (code bug, config error)
- Resource limits (file descriptors, threads)
- Segfault in GHC runtime (check: ulimit -s 9999)
```

### Resolution

1. Service is up and responding on port 8013
2. Verify BECKN flow: search → on_search → select → on_select → init → confirm
3. Verify authentication works (PASETO token validation)
4. Verify a test ride can be booked end-to-end
5. Check that queued BECKN callbacks are processing
6. Monitor error rates and latency for 30 minutes

### Post-Incident Review (CoE)

- [ ] Root cause identified and fix deployed?
- [ ] Was the 90-second graceful shutdown period adequate?
- [ ] Should rider-app have multiple instances behind a load balancer?
- [ ] Are startup dependency checks (DB, Redis, Kafka) failing fast enough?
- [ ] GHC RTS flags: is `-with-rtsopts` configured for production monitoring?
- [ ] Segfault history: is `ulimit -s 9999` set in production?

---

## Playbook 7: Allocation Service (driver-offer-allocator) Failure

**Severity: P0**

### Detection

| Signal | Source | Threshold |
|--------|--------|-----------|
| Allocator port 9996 down | TCP probe | Connection refused |
| Job queue growing | Redis `Available_Jobs` stream | Length increasing for > 30s |
| No driver assignments | Business metrics | 0 assignments for > 1 min |
| SendSearchRequestToDriver jobs stuck | Scheduler metrics | Job count > 100 unprocessed |
| Customer wait times spiking | Ride lifecycle metrics | No rides entering ASSIGNED state |

### Impact Assessment

**The allocator is the single most critical service for ride completion.**

| Job Type | Impact When Halted |
|----------|-------------------|
| `SendSearchRequestToDriver` | NO drivers receive ride requests — all new searches dead |
| `CalculateDriverFees` | Driver fee calculations stop |
| `BadDebtCalculation` | Bad debt accumulates untracked |
| `SendPDNNotificationToDriver` | Payment notifications to drivers stop |
| `OrderAndNotificationStatusUpdate` | Payment status sync stops |
| `ScheduledRideNotificationsToDriver` | Scheduled ride reminders fail |
| `MandateExecution` | Auto-pay mandate execution halts |
| `DriverReferralPayout` / `SpecialZonePayout` | Payout processing stops |
| `UnblockDriver` / `SoftBlockNotifyDriver` | Driver state management stops |

**Configuration:**
- `loopIntervalSec = 5` — checks for new jobs every 5 seconds
- `tasksPerIteration = 20` — processes 20 jobs per loop
- `schedulerSetName = "Scheduled_Jobs"`
- `streamName = "Available_Jobs"`

**The allocator shares the same database (atlas_driver_offer_bpp) and Redis as the driver-app but runs as a separate process.**

### Immediate Mitigation

```bash
1. CHECK process status:
   $ ps aux | grep allocator
   $ curl -s http://localhost:9996/health

2. CHECK job queue backlog:
   $ redis-cli -p 30001 XLEN Available_Jobs
   $ redis-cli -p 30001 XLEN Scheduled_Jobs

3. RESTART the allocator:
   - Grace period is SHORT (10 seconds, not 90 like other services)
   - Verify DB and Redis connectivity first
   - Distributed locks (RunnningJob:{jobId}) will expire — no manual cleanup needed

4. If restart fails:
   a. Check allocator-specific config: driver-offer-allocator.dhall
   b. Verify Redis stream exists and is readable
   c. Check for port conflict on 9996
   d. Rollback to last known good version

5. MONITOR recovery:
   - Watch Available_Jobs stream length decrease
   - Watch for SendSearchRequestToDriver jobs completing
   - First sign of recovery: drivers start receiving ride requests

6. Communicate:
   - "Allocation service is down. Drivers are not receiving ride requests.
      Existing in-progress rides continue. Restarting allocator."
```

### Root Cause Investigation

```bash
1. Check scheduler loop health:
   - Is the 5-second loop running?
   - Are jobs being picked up from Available_Jobs stream?
   - Are distributed locks (RunnningJob:*) orphaned?

2. Check Redis stream state:
   $ redis-cli -p 30001 XINFO STREAM Available_Jobs
   $ redis-cli -p 30001 XPENDING Available_Jobs <group>

3. Check for job processing errors:
   - Are specific job types failing?
   - Timeout on job execution?
   - DB query failures within job handlers?

4. Check resource usage:
   - maxThreads = 10 — are all threads blocked?
   - DB connection pool (25 connections) — exhausted?
   - Memory / CPU usage

Common root causes:
- Redis stream corruption or unavailability
- DB connection exhaustion (shared pool with driver-app)
- Specific job type causing crash loop (bad data → handler error → restart)
- Memory leak in long-running scheduler loop
- Distributed lock contention (RunnningJob not expiring, expirationTime=60s)
```

### Resolution

1. Allocator is running and processing jobs from `Available_Jobs` stream
2. Job queue backlog is draining (stream length decreasing)
3. Drivers are receiving and accepting ride requests
4. Verify all 33 job types are functioning (spot-check critical ones)
5. Check for any orphaned distributed locks
6. Monitor for 30 minutes — watch for job accumulation

### Post-Incident Review (CoE)

- [ ] How many ride requests were lost during the outage?
- [ ] Can the allocator run as multiple instances for HA?
- [ ] Is the 10-second grace period sufficient for clean shutdown?
- [ ] Should critical job types (SendSearchRequestToDriver) have a separate queue?
- [ ] Was the 60-second lock expiration (expirationTime) appropriate?
- [ ] Job queue monitoring: are alerts on Available_Jobs length in place?

---

## Playbook 8: Location Tracking Service Failure

**Severity: P1**

### Detection

| Signal | Source | Threshold |
|--------|--------|-----------|
| LTS port 8081 unreachable | TCP probe | Connection refused |
| Driver positions stale | nearBy API results | Positions older than 30 seconds |
| Kafka consumer lag on `location-updates` | Consumer group metrics | Lag > 5,000 messages |
| Stop detection callbacks failing | driver-app logs (port 8016) | HTTP errors to 127.0.0.1:8016/internal/stopDetection |
| Route deviation alerts missing | Violation detection logs | No violation events for > 5 min |

### Impact Assessment

| Feature | Impact |
|---------|--------|
| nearBy driver search | Stale positions → wrong drivers shown to riders, poor matching |
| ETA updates | In-ride ETA not updating for customers |
| Distance-based fare calculation | Odometer-style distance calculation stops |
| Stop detection (multi-stop rides) | Stop arrival callbacks to driver-app fail |
| Violation detection | Route deviation, overspeeding, wrong direction — all undetected |
| Safety features | Safety check integration loses real-time data |
| Toll detection | Toll crossing notifications stop |
| Snap-to-road | Driver route loses accuracy |

**Multi-cloud architecture:**
- Primary cloud: MUST succeed (errors propagate)
- Secondary cloud: OPTIONAL (errors return `[]` fallback)
- Both run concurrently via `awaitableFork`
- Results combined from both clouds when available

**Kafka consumer: `location-updates` topic (LZ4 compressed)**
- Batch processing with metric reporting
- Consumer group manages offsets

### Immediate Mitigation

```bash
1. CHECK LTS status:
   $ curl -s http://localhost:8081/health
   $ ps aux | grep location-tracking

2. CHECK Kafka consumer:
   $ kafka-consumer-groups --bootstrap-server localhost:29092 \
     --describe --group <lts-consumer-group>

3. CHECK LTS Redis:
   $ redis-cli -p 6379 PING  # LTS uses dedicated Redis connection (ltsRedis)

4. If LTS is down:
   a. Rides in progress continue — driver-app still has last known position
   b. New searches use stale driver positions — match quality degrades
   c. Fare calculation falls back to route-based estimate (not odometer)

5. RESTART LTS:
   - Check OSRM connectivity (used for snap-to-road, interpolation)
   - Verify Kafka consumer group can resume from last offset
   - Verify detection callback URLs:
     - Stop detection: http://127.0.0.1:8016/internal/stopDetection
     - Violation detection: http://127.0.0.1:8016/internal/violationDetection

6. If Kafka consumer cannot catch up:
   a. Location data during outage is lost (real-time stream)
   b. Reset consumer offset to latest if lag is too large:
      - This skips stale location data (acceptable — we want current positions)
   c. Monitor consumer lag decreasing after restart

7. Communicate:
   - "Location tracking service degraded. Driver positions may be stale.
      Rides continue but ETA and fare may be approximate."
```

### Root Cause Investigation

```bash
1. Check LTS-specific dependencies:
   - OSRM service reachable?
   - LTS Redis (ltsHedisEnv) healthy?
   - Kafka location-updates topic available?

2. Check processing pipeline:
   - Kafka consume → Redis store → OSRM snap → Callback to driver-app
   - Which stage is failing?

3. Check detection thresholds (from location_tracking_service.dhall):
   - Speed limit: 60mph
   - Distance thresholds: 25-50m
   - Sample sizes for detection algorithms

4. Check for location data volume spikes:
   - Rush hour = higher location update rate
   - Was the consumer overwhelmed?

Common root causes:
- OSRM service down (snap-to-road, interpolation fails)
- LTS Redis memory full (location data is high-volume)
- Kafka consumer group rebalance (new consumer joining/leaving)
- Network issue between LTS and driver-app (callbacks fail)
```

### Resolution

1. LTS is processing location updates in real-time (< 5s latency)
2. nearBy search returns fresh driver positions
3. Stop detection and violation detection callbacks working
4. Kafka consumer lag is stable (not growing)
5. OSRM snap-to-road returning accurate positions
6. Monitor for 30 minutes

### Post-Incident Review (CoE)

- [ ] Was the multi-cloud fallback (primary + secondary) effective?
- [ ] How much distance/fare data was lost during the outage?
- [ ] Should LTS have its own dedicated Kafka cluster?
- [ ] OSRM dependency: is it a single point of failure?
- [ ] Consumer group auto-recovery: did it work or need manual reset?
- [ ] Location data volume planning: are we sized for peak load?

---

## Playbook 9: Spike in 5XX Errors

**Severity: P1 (escalate to P0 if > 20% error rate)**

### Detection

| Signal | Source | Threshold |
|--------|--------|-----------|
| 5XX error rate | Load balancer / Prometheus | > 1% for 2 min (warn), > 5% for 1 min (critical) |
| Exception rate | Application error logs | > 50 exceptions/min |
| Specific endpoint errors | Per-endpoint metrics | Any endpoint > 10% error rate |
| Customer/driver app errors | Mobile error reporting | Spike in API errors |
| `fromMaybeM` errors | Application logs | Entity not found errors spiking |

### Impact Assessment

**Determine which service(s) are affected:**

| Service | 5XX Source | Likely Impact |
|---------|-----------|---------------|
| rider-app (8013) | Customer-facing API errors | Search, booking, payment failures |
| driver-app (8016) | Driver-facing API errors | Ride acceptance, location update failures |
| allocator (9996) | Internal job processing errors | Driver assignment failures |
| BECKN gateway (8015) | Protocol routing errors | Inter-platform communication failure |

**Error classification:**
- `500 Internal Server Error` — unhandled exception, bug
- `502 Bad Gateway` — upstream service unreachable
- `503 Service Unavailable` — service overloaded or shutting down
- `504 Gateway Timeout` — upstream timeout (HTTP timeout: 200s default)

### Immediate Mitigation

```bash
1. IDENTIFY the affected service and endpoint:
   - Which service port is returning 5XX?
   - Which specific API endpoint(s)?
   - Is it all requests or a specific pattern (e.g., specific merchant, city)?

2. CHECK recent deployments:
   $ git log --oneline --since="1 hour ago"
   - Was there a deployment in the last hour?
   - If yes: ROLLBACK immediately, investigate later

3. CHECK dependencies:
   a. PostgreSQL: Can the service query the DB?
   b. Redis: Is caching working?
   c. Kafka: Are producers/consumers healthy?
   d. External APIs: Google Maps, Juspay, OSRM responding?

4. CHECK for resource exhaustion:
   - CPU/Memory on service hosts
   - DB connection pool usage (rider: 10, driver: 25)
   - Redis connection pool (50 max per tier)
   - Open file descriptors

5. If a specific endpoint is the source:
   a. Check for bad data causing handler exceptions
   b. Check for missing entities (fromMaybeM → PersonNotFound, etc.)
   c. Check for race conditions in concurrent requests

6. If widespread across all endpoints:
   a. Likely a dependency failure — check DB, Redis, Kafka
   b. Or a bad deployment — rollback
   c. Or resource exhaustion — scale up or restart

7. RATE LIMITING:
   - If caused by traffic spike, enable/tighten rate limits
   - Check existing rate limiting (Redis-based) is functioning

8. Communicate based on severity:
   - < 5%: "Elevated error rates on [service]. Investigating."
   - > 5%: "Service degradation. [X]% of requests failing. Active mitigation."
   - > 20%: Escalate to P0, open war room
```

### Root Cause Investigation

```bash
1. Correlate error timing with events:
   - Deployment? Config change? Traffic spike? Dependency failure?

2. Sample error responses:
   - Grep application logs for stack traces
   - Look at error response bodies (not just status codes)

3. Check Haskell-specific issues:
   - GHC -Werror means all warnings are errors at compile time,
     but runtime exceptions are a separate class
   - Common: IOException, HttpException, SqlError, RedisError
   - Pattern: exception in `fromMaybeM` = data consistency issue

4. Check for slow queries:
   $ psql -c "SELECT query, calls, mean_exec_time
              FROM pg_stat_statements ORDER BY mean_exec_time DESC LIMIT 20;"

5. Check for thread starvation:
   - Warp thread pool exhausted?
   - Long-running handlers blocking threads?

6. Check ClickHouse for historical patterns:
   - Query atlas_kafka for event volume anomalies
   - Compare with previous days/weeks

Common root causes:
- Bad deployment (most common for sudden spike)
- Database slow queries (gradual degradation)
- Dependency timeout cascade
- Data migration causing entity-not-found errors
- Traffic spike exceeding capacity
```

### Resolution

1. Error rate returns to baseline (< 0.1%)
2. All endpoints responding within SLA
3. No ongoing dependency issues
4. If rollback was performed: identify the bad change, fix, and redeploy
5. Monitor for 1 hour

### Post-Incident Review (CoE)

- [ ] Was detection automated? What was time-to-detect?
- [ ] Was the root cause a deployment, dependency, or data issue?
- [ ] Could canary deployment have caught this before full rollout?
- [ ] Are per-endpoint error rate alerts in place?
- [ ] Is there a rollback runbook / one-click rollback procedure?
- [ ] `-Werror` catches compile-time issues — what catches runtime regressions?

---

## Playbook 10: Spike in Latency

**Severity: P2 (escalate to P1 if P99 > 10s, P0 if timeouts cascade)**

### Detection

| Signal | Source | Threshold |
|--------|--------|-----------|
| P50 latency increase | Prometheus | > 2x baseline for 5 min |
| P99 latency increase | Prometheus | > 5x baseline for 2 min |
| Request timeouts | Application logs | Timeout count > 10/min |
| Customer app slowness | Mobile performance metrics | API calls > 3 seconds |
| Warp thread pool saturation | Service metrics | Active threads near maximum |

**Baseline latencies (approximate):**
- Search API: < 500ms
- Booking API: < 300ms
- Status check: < 100ms
- nearBy drivers: < 200ms
- Payment initiation: < 1s

### Impact Assessment

| Latency Range | User Experience |
|---------------|----------------|
| 1-3 seconds | Noticeable delay, users wait |
| 3-10 seconds | Poor UX, some users abandon |
| 10-30 seconds | Severe, most users abandon or retry (making it worse) |
| > 30 seconds | Effective outage, retry storms |
| > 200 seconds | HTTP client timeout (configured `timeoutMs = 200000`) |

**Cascade risk:** High latency → thread pool exhaustion → request queuing → more latency → timeouts → 5XX errors. This is the most dangerous escalation pattern.

### Immediate Mitigation

```bash
1. IDENTIFY the bottleneck:
   a. Which service has high latency? (rider-app, driver-app, allocator)
   b. Which specific endpoints are slow?
   c. Is latency uniform or intermittent?

2. CHECK database performance:
   $ psql -c "SELECT count(*), state FROM pg_stat_activity GROUP BY state;"
   $ psql -c "SELECT pid, now() - query_start AS duration, query
              FROM pg_stat_activity WHERE state = 'active' AND query_start < now() - interval '5 seconds'
              ORDER BY duration DESC LIMIT 20;"

   → If long-running queries: identify and kill if safe
   $ psql -c "SELECT pg_cancel_backend(<pid>);"

3. CHECK Redis performance:
   $ redis-cli -p 6379 SLOWLOG GET 10
   $ redis-cli -p 30001 SLOWLOG GET 10
   $ redis-cli -p <port> INFO commandstats

   → If slow commands: identify pattern (KEYS, large SORT, big hashes)

4. CHECK external API latency:
   - Google Maps API response times
   - OSRM response times
   - Juspay response times
   - BECKN protocol callbacks (are BPP responses slow?)

5. CHECK for GC pressure (Haskell/GHC specific):
   - GHC RTS stats if available
   - Major GC pauses can cause latency spikes
   - Heap size growing unbounded → memory pressure

6. If database is the bottleneck:
   a. Check for missing indexes on hot queries
   b. Check for table bloat (VACUUM needed?)
   c. Consider temporarily directing reads to replica

7. If external API is the bottleneck:
   a. Short retry (3 retries, 2x backoff) may be making it worse
   b. Consider temporarily increasing timeouts or disabling retries
   c. Switch to fallback (e.g., Google Maps → OSRM)

8. If thread pool is saturated:
   a. Reduce incoming traffic (enable stricter rate limiting)
   b. Identify and shed low-priority requests
   c. Scale up if possible

9. Communicate:
   - "Elevated latency on [service]. [X]ms P50, [Y]ms P99.
      Investigating bottleneck."
```

### Root Cause Investigation

```bash
1. Database analysis:
   - pg_stat_statements: query execution time trends
   - Missing indexes: seq scans on large tables
   - Lock contention: pg_locks with waiting queries
   - Connection pool exhaustion: pool size (10/25) vs active connections

2. Redis analysis:
   - Memory fragmentation ratio
   - Keyspace hit/miss ratio
   - Connected clients vs maxclients
   - Network bandwidth to Redis

3. Application-level analysis:
   - Profile handler execution time
   - Identify N+1 query patterns
   - Check for unnecessary serialization/deserialization
   - BECKN protocol roundtrip time (BAP ↔ BPP)

4. Infrastructure analysis:
   - CPU steal time (noisy neighbor on shared infra)
   - Network latency between services
   - DNS resolution latency
   - Disk I/O on database hosts

5. Traffic analysis:
   - Request volume vs capacity
   - Geographic distribution (city-specific issues?)
   - New feature rollout driving higher API call volume?

Common root causes:
- Missing database index (most common for gradual degradation)
- Database VACUUM not running (table bloat)
- N+1 query pattern in new code
- External API degradation (Google Maps, Juspay)
- GHC garbage collection pressure (heap too large)
- Traffic spike beyond capacity
- Redis slow command blocking event loop
```

### Resolution

1. Latency returns to baseline (P50, P95, P99 all normal)
2. No request timeouts
3. Database connections not saturated
4. External API response times normal
5. If a specific fix was applied (index, query optimization), verify it holds under load
6. Monitor for 1 hour — latency issues often recur

### Post-Incident Review (CoE)

- [ ] Root cause: database, application, external, or infrastructure?
- [ ] Was the issue caught before users noticed?
- [ ] Are latency SLOs defined? Were they breached?
- [ ] Should we add query execution time alerts?
- [ ] Database maintenance: is VACUUM/ANALYZE running on schedule?
- [ ] Load testing: does our staging environment replicate production load?
- [ ] GHC RTS tuning: are heap/GC parameters optimized for our workload?

---

## Post-Incident Review (CoE) Template

> This template should be used for every P0 and P1 incident. Completed within 48 hours.

### Incident Summary
| Field | Value |
|-------|-------|
| **Incident ID** | INC-YYYY-NNN |
| **Date** | YYYY-MM-DD |
| **Duration** | Start time → End time (total minutes) |
| **Severity** | P0 / P1 / P2 |
| **Services affected** | rider-app, driver-app, allocator, etc. |
| **Customers impacted** | Number of affected users / rides |
| **Revenue impact** | Estimated lost rides × average fare |

### Timeline
| Time (IST) | Event | Actor |
|-------------|-------|-------|
| HH:MM | Alert triggered / User report | System / User |
| HH:MM | Incident acknowledged | On-call engineer |
| HH:MM | IC assigned | Engineering manager |
| HH:MM | Root cause identified | IC |
| HH:MM | Mitigation applied | Engineer |
| HH:MM | Service restored | Engineer |
| HH:MM | Monitoring confirmed stable | IC |

### Root Cause Analysis
1. **What happened?** (factual description)
2. **Why did it happen?** (5 Whys analysis)
3. **Why wasn't it caught earlier?** (detection gap)
4. **Why was the blast radius what it was?** (isolation gap)

### Action Items
| # | Action | Owner | Due Date | Priority |
|---|--------|-------|----------|----------|
| 1 | | | | |
| 2 | | | | |
| 3 | | | | |

### Lessons Learned
- What went well during the response?
- What could have gone better?
- What surprised us?

### Recurrence Prevention
- [ ] Monitoring/alerting gap closed
- [ ] Runbook updated with findings
- [ ] Failover tested
- [ ] Capacity reviewed

---

## Escalation Matrix

| Severity | Response Time | IC Level | Communication |
|----------|--------------|----------|---------------|
| P0 | 5 min | Senior engineer + engineering manager | Every 15 min to stakeholders |
| P1 | 15 min | Senior engineer | Every 30 min to team |
| P2 | 1 hour | On-call engineer | Status update when resolved |
| P3 | Next business day | Assigned engineer | Ticket update |

## Quick Reference: Service Ports

| Service | Port | Metrics Port | Schema |
|---------|------|-------------|--------|
| rider-app | 8013 | 9999 | atlas_app |
| beckn-gateway | 8015 | — | — |
| driver-app | 8016 | 9997 | atlas_driver_offer_bpp |
| mock-registry | 8020 | — | — |
| rider-scheduler | 8058 | — | atlas_app |
| location-tracking | 8081 | — | — |
| allocator | 9996 | — | atlas_driver_offer_bpp |
| Redis (single) | 6379 | — | — |
| Redis (cluster) | 30001 | — | — |
| Redis (secondary) | 30002 | — | — |
| PostgreSQL | 5434 | — | atlas_dev |
| ClickHouse | 8123 | — | atlas_kafka, atlas_app |
| Kafka | 29092 | — | — |

## Quick Reference: Key Redis Keys

| Key Pattern | Purpose | Service |
|-------------|---------|---------|
| `Scheduled_Jobs` | Driver allocator job set | allocator |
| `Available_Jobs` | Driver allocator job stream | allocator |
| `Scheduled_Jobs_Rider` | Rider scheduler job set | rider-scheduler |
| `Available_Jobs_Rider` | Rider scheduler job stream | rider-scheduler |
| `RunnningJob:{jobId}` | Distributed execution lock (TTL 60s) | scheduler |
| `pt:failures:{mocId}:{mode}:{apiType}` | Circuit breaker failure tracking (TTL 10 min) | rider-app |
| `pt:canary:{mocId}:{mode}:{apiType}` | Circuit breaker canary counter | rider-app |
| `pt:fareCache:*` | Public transport fare cache | rider-app |

## Quick Reference: Configuration Files

| Config | Path |
|--------|------|
| Rider-app | `dhall-configs/dev/rider-app.dhall` |
| Driver-app | `dhall-configs/dev/dynamic-offer-driver-app.dhall` |
| Allocator | `dhall-configs/dev/driver-offer-allocator.dhall` |
| Rider scheduler | `dhall-configs/dev/rider-app-scheduler.dhall` |
| Common settings | `dhall-configs/generic/common.dhall` |
| LTS | `dhall-configs/dev/location_tracking_service.dhall` |
| Retry config | `common.dhall` → `shortDurationRetryCfg`, `longDurationRetryCfg` |
