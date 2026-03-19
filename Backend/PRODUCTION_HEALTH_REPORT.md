# Namma Yatri Production Health Report

**Date**: March 15, 2026
**Reporting Period**: March 14, 2026 (full day, UTC)
**Cluster**: eks-kurukshetra (AWS ap-south-1)
**Data Sources**: OpenSearch (istio-proxy, beckn-lp-logs, mettle, juspay-recon, istio, beckn-settlement-cron)
**Note**: VictoriaMetrics was unavailable for this report; CPU/memory/disk metrics are not included.

---

## Executive Summary

The Namma Yatri platform processed **509.6 million service mesh requests** and generated **51.6 million application log entries** on March 14. Core ride-hailing services (rider-app, driver-app) remained operational but exhibited several issues requiring attention. **Three critical issues** demand immediate action: a persistent database authentication failure in Mettle, Redis cache schema mismatches causing decode failures, and multiple observability pipelines that have silently stopped ingesting data.

**Overall Health: DEGRADED**

---

## 1. Service Availability

| Service | Status | Evidence |
|---------|--------|----------|
| **Rider App** (beckn-app-backend, port 8013) | DEGRADED | Serving traffic but producing 10K+ HTTP 500s, 1,878 HTTP 503s, and 378 gateway timeouts (504s) |
| **Driver App** (beckn-driver-offer-bpp, port 8016) | DEGRADED | Serving traffic but producing 6,537 HTTP 500s, 1,468 HTTP 503s, 8,401 ConnectionErrors |
| **Driver Allocator** (beckn-driver-offer-allocator) | DEGRADED | Functional but 873 ClickHouse query failures (missing column) |
| **OSRM Routing** (beckn-osrm) | DEGRADED | 245 HTTP 502s (upstream premature disconnects on route/table APIs) |
| **Notification Service** (beckn-notification-service) | HEALTHY | 0 HTTP 500s, long-lived gRPC streams functioning normally |
| **Location Tracking** (beckn-location-tracking-service) | HEALTHY | 2 HTTP 500s (negligible) |
| **Mettle Backend** | DOWN | 1,205 persistent database authentication failures; unable to connect to RDS |
| **Safety Dashboard** (beckn-safety-dashboard) | UNKNOWN | 10K+ HTTP 503s on /metrics endpoint (Prometheus scrape failures) |
| **BPP Dashboard External** | DEGRADED | Redis decode failures on authTokenCacheKey (tuple length mismatch) |
| **URL Shortener** (nammayatri-url-shortner) | HEALTHY | Logging normally, no errors observed |
| **Settlement Automation Cron** | DEGRADED | 7,316 warnings - missing CMRL payout orders (HTTP 404s from Juspay) |
| **Juspay Recon** | DEGRADED | 32 "API is expired" crypto errors in evening hours |
| **GPS Server** | UNKNOWN | **Stopped logging to OpenSearch after March 11** |
| **GPS Processor** | UNKNOWN | **Stopped logging to OpenSearch after Feb 12** |
| **Atlas SDK** | UNKNOWN | **Stopped logging to OpenSearch after March 12** |

### Uptime Assessment
- **Core services (rider-app, driver-app)**: Operational but with elevated error rates
- **3 observability pipelines dark**: GPS Server (4 days), Atlas SDK (2 days), GPS Processor (31 days)
- **1 service fully down**: Mettle backend (persistent DB auth failure)

---

## 2. Performance (Latency)

> Note: Precise latency percentiles require VictoriaMetrics (unavailable). The following is derived from istio-proxy access log duration fields.

| Metric | Value | Source |
|--------|-------|--------|
| **Gateway timeouts (504s)** | 378 requests at ~40,000ms | istio-ingressgateway -> rider-app |
| **OSRM routing failures (502s)** | 245 requests, upstream premature disconnect | beckn-osrm |
| **Normal request range** | 0-300ms for typical API calls | istio-proxy sample logs |
| **gRPC notification streams** | ~300,000ms (expected for long-lived streams) | beckn-notification-service |

### Timeout-Affected Endpoints (rider-app, 504s)

| Endpoint | Impact |
|----------|--------|
| `/v2/nearbyDrivers` | Driver discovery for riders |
| `/v2/rideBooking` | Ride booking flow |
| `/v2/serviceability` | Service area checks |
| `/v2/pickup/route` | Pickup route calculation |
| `/v2/maps/getPlaceName` | Place name resolution |

**Assessment**: Gateway timeouts are concentrated on the rider-app, suggesting intermittent resource exhaustion or slow downstream dependencies (OSRM, database queries). The 40-second timeout threshold is being hit on several user-critical endpoints.

---

## 3. Error Rates (by Service, by Type)

### HTTP Error Distribution (March 14, from istio-proxy)

| Status | Volume | Primary Services Affected |
|--------|--------|--------------------------|
| **400 Bad Request** | 10,000+ | Rider-app, Driver-app |
| **401 Unauthorized** | 10,000+ | Rider-app (INVALID_TOKEN) |
| **403 Forbidden** | 10,000+ | Multiple services |
| **404 Not Found** | 10,000+ | Multiple services |
| **422 Unprocessable** | 10,000+ | Multiple services |
| **429 Too Many Requests** | 10,000+ | Rider-app (IP rate limiting) |
| **500 Internal Server Error** | 10,000+ | Rider-app (10K+), Driver-app (6,537), Ingress (5,813), Mettle-Slack (22) |
| **502 Bad Gateway** | 245 | OSRM routing (100%) |
| **503 Service Unavailable** | 10,000+ | Safety Dashboard, LTS Healthcheck, Rider Person Stat (mostly Prometheus scrape failures) |
| **504 Gateway Timeout** | 378 | Ingress -> Rider-app (100%) |

### Application-Level Errors (March 14, from beckn-lp-logs)

| Error Type | Count | Service | Severity |
|-----------|-------|---------|----------|
| **E401 INVALID_TOKEN** | 10,000+ | Rider-app | Medium - Expected for expired tokens |
| **E429 IP_HITS_LIMIT_EXCEED** | 10,000+ | Rider-app | Medium - Rate limiting working as designed |
| **E500 GUPSHUP_UNKNOWN_ERROR** | 10,000+ | Rider-app | High - WhatsApp notification failures |
| **HttpManagerNotFound / ConnectionError** | 8,401 | Driver-app | High - Cannot reach http-signature-api |
| **Redis Decode Failure (PayoutConfig)** | 10,000+ | Rider-app | High - Null value for expected Object |
| **Redis Decode Failure (authTokenCacheKey)** | Present | BPP Dashboard | High - Array/tuple length mismatch |
| **ClickHouse Missing Column** | 873 | Driver Allocator | Medium - vehicle_service_tier vs vehicle_service_tier_name |
| **SECONDARY_CLUSTER Failover** | 10,000+ | Rider-app, Driver-app | Medium - Primary DB returning Nothing |
| **Mettle DB Auth Failure** | 1,205 | Mettle Backend | CRITICAL - Service effectively down |
| **Juspay API Expired** | 32 | Juspay Recon | Low - Crypto key expiration |
| **Settlement Order 404** | 7,316 | Settlement Cron | Low - Historical orders not found |

---

## 4. Resource Utilization

> **Data gap**: VictoriaMetrics was unavailable. CPU, memory, and disk metrics could not be collected.

### OpenSearch Storage (observed)

| Index Category | Daily Volume (March 14) | Daily Storage |
|---------------|------------------------|---------------|
| istio-proxy | 509.6M docs | 301.8 GB |
| istio (control plane) | 171.9M docs | 152.9 GB |
| beckn-lp-logs | 51.6M docs | 59.9 GB |
| beckn-settlement-cron | 792K docs | 239.4 MB |
| mettle | 289K docs | 169.2 MB |
| juspay-recon | 14.7K docs | 24.7 MB |
| **Daily Total** | **~733.8M docs** | **~515 GB** |

**Cluster health**: All 304 indices report **green** status with proper replication (5 primary + 1 replica per index).

### Database (inferred from logs)
- **atlas-driver-v1-cluster** (RDS ap-south-1): Reachable by main services but **rejecting mettle_rw user** (password auth failure)
- **SECONDARY_CLUSTER failover**: 10K+ events where primary DB returned Nothing, triggering secondary reads - suggests primary DB read pressure or replication lag
- **ClickHouse**: v23.8.9.54 operational but schema drift detected (missing `vehicle_service_tier` column)

---

## 5. Dependency Health

| Dependency | Status | Evidence |
|-----------|--------|----------|
| **PostgreSQL (atlas_app, atlas_driver_offer_bpp)** | DEGRADED | No direct PostgreSQL errors, but 10K+ SECONDARY_CLUSTER failovers indicate primary read issues |
| **PostgreSQL (mettle)** | FAILED | mettle_rw password authentication failed continuously against atlas-driver-v1-cluster RDS |
| **Redis** | DEGRADED | Decode failures on PayoutConfig (null where Object expected) and authTokenCacheKey (3-element array vs 2-element tuple) - indicates schema mismatch after deployment |
| **ClickHouse** | DEGRADED | Missing column `vehicle_service_tier` in allocator queries (873 errors); should be `vehicle_service_tier_name` |
| **Kafka** | HEALTHY | No Kafka infrastructure errors detected (138 matches were false positives - customer name) |
| **OSRM (Routing)** | DEGRADED | 245 upstream premature disconnects on /route and /table APIs |
| **Gupshup (WhatsApp)** | FAILING | 10K+ E500 GUPSHUP_UNKNOWN_ERROR - WhatsApp opt-in API calls failing |
| **Juspay (Payments)** | DEGRADED | 32 expired API crypto errors; 7,316 missing settlement orders (404s for CMRL) |
| **http-signature-api (BECKN signing)** | FAILING | 8,401 ConnectionErrors (HttpManagerNotFound) from driver-app |
| **Istio Service Mesh** | HEALTHY | Normal xdsproxy connection resets during control plane updates |

---

## 6. Capacity Headroom

> **Limited data available** - VictoriaMetrics metrics (CPU, memory, network) were unavailable. Assessment based on traffic patterns and error signals.

| Dimension | Assessment | Evidence |
|-----------|-----------|----------|
| **OpenSearch Storage** | MONITOR | 515 GB/day across all indices; 304 indices in cluster. At this rate, ~15 TB/month of log storage |
| **Rider-App Capacity** | AT RISK | 378 gateway timeouts + elevated 500s suggest resource constraints under peak load |
| **OSRM Routing** | AT RISK | 245 upstream disconnects suggest OSRM instances may be under-provisioned or overloaded |
| **Database Connections** | UNKNOWN | SECONDARY_CLUSTER failovers suggest primary may be hitting connection or query limits |
| **Rate Limiting** | WORKING | 10K+ 429 responses indicate IP rate limiting is actively protecting services |

---

## 7. Cost Implications

| Area | Observation | Cost Impact |
|------|------------|-------------|
| **OpenSearch** | ~515 GB/day log ingestion, 304 indices, all replicated | High - consider reducing istio-proxy log verbosity (301.8 GB/day alone) |
| **SECONDARY_CLUSTER reads** | 10K+ failover events hitting secondary DB | Increased read replica load and potential for elevated RDS costs |
| **Gupshup API failures** | 10K+ failed WhatsApp calls being retried | Wasted API call costs + customer notification gaps |
| **OSRM compute** | 245 failed routing requests | Re-computation costs + degraded user experience |
| **Dead observability pipelines** | GPS Server, GPS Processor, Atlas SDK not logging | Potential hidden issues going undetected, increasing incident resolution time |

---

## 8. Top 10 Issues Requiring Immediate Attention

| Priority | Issue | Impact | Service |
|----------|-------|--------|---------|
| **P0** | **Mettle DB authentication failure** - mettle_rw user cannot connect to atlas-driver-v1-cluster RDS. 1,205 continuous failures. Service is effectively DOWN. | Mettle backend completely non-functional | Mettle |
| **P0** | **Redis PayoutConfig decode failure** - Cached value is `null` but code expects an Object. 10K+ errors/day. | Payout functionality likely broken for affected merchants | Rider-app |
| **P0** | **http-signature-api unreachable** - 8,401 ConnectionErrors (HttpManagerNotFound). Driver-app cannot reach BECKN signing service. | BECKN protocol communication failures | Driver-app |
| **P1** | **3 observability pipelines silently dead** - GPS Server (since Mar 11), Atlas SDK (since Mar 12), GPS Processor (since Feb 12). | Blind spots in monitoring; incidents in these systems will go undetected | GPS, Atlas SDK |
| **P1** | **Gupshup WhatsApp API failures** - 10K+ E500 errors on WhatsApp opt-in API. | Customers not receiving WhatsApp notifications | Rider-app |
| **P1** | **Gateway timeouts on critical rider endpoints** - 378 504s targeting /nearbyDrivers, /rideBooking, /serviceability. | Direct user-facing impact: riders unable to find drivers or book rides | Rider-app |
| **P1** | **Redis authTokenCacheKey schema mismatch** - Array of 3 elements but code expects tuple of 2. | Dashboard authentication failures for some users | BPP Dashboard |
| **P2** | **ClickHouse schema drift** - 873 errors for missing column `vehicle_service_tier` (should be `vehicle_service_tier_name`). | Allocator analytics queries failing, potentially affecting driver allocation decisions | Driver Allocator |
| **P2** | **OSRM upstream disconnects** - 245 502s on routing and distance matrix APIs. | Degraded route calculations; fallback or retry overhead | OSRM |
| **P2** | **Security probe detected** - BeanShell injection attempt (POST /T49aBuvHSqxq.jsp) from 162.240.151.141 against logs.beckn.juspay.in. One request returned HTTP 200. | Potential unauthorized access; WAF rule gap | Istio Ingress |

---

## 9. Top 10 Optimization Opportunities

| # | Opportunity | Expected Impact | Effort |
|---|------------|-----------------|--------|
| 1 | **Reduce istio-proxy log volume** - 301.8 GB/day (59% of all logs). Filter out health checks, Prometheus scrapes, and low-value 200 responses at the Envoy access log level. | 50-70% reduction in OpenSearch storage costs (~$X/month savings) | Medium |
| 2 | **Fix SECONDARY_CLUSTER failover pattern** - 10K+ primary DB miss events triggering secondary reads. Investigate why primary returns Nothing: replication lag, connection pool exhaustion, or query timeout. | Reduced DB load, faster response times, lower RDS costs | Medium |
| 3 | **Consolidate Redis cache schema management** - PayoutConfig null values and authTokenCacheKey tuple mismatch indicate no schema versioning for cached objects. Implement cache versioning or invalidation on deploy. | Eliminate entire class of deploy-related cache errors | Medium |
| 4 | **Fix ClickHouse column name** - Simple rename of `vehicle_service_tier` to `vehicle_service_tier_name` in the allocator query or add a column alias in ClickHouse. | Eliminate 873 errors/day | Low |
| 5 | **Implement circuit breaker for Gupshup API** - 10K+ retries against a failing external API wastes resources. Add circuit breaker with exponential backoff. | Reduced load on rider-app, faster failure responses, lower API costs | Low |
| 6 | **Restore observability pipelines** - GPS Server, GPS Processor, and Atlas SDK logging needs investigation and restoration. | Eliminate monitoring blind spots | Medium |
| 7 | **OSRM horizontal scaling or connection pooling** - 245 upstream premature disconnects suggest resource pressure. Add instances or improve connection management. | More reliable routing, fewer retries | Medium |
| 8 | **Tune rider-app timeout thresholds** - 40-second gateway timeout is very long. Consider per-endpoint timeouts: /nearbyDrivers (5s), /rideBooking (10s), /serviceability (3s). | Faster failure detection, better resource reclamation | Low |
| 9 | **Clean up CMRL settlement orders** - 7,316 warnings for orders that no longer exist in Juspay. Mark these orders as reconciliation-failed and stop retrying. | Reduced log noise, cleaner settlement pipeline | Low |
| 10 | **Add WAF rule for JSP injection probes** - Block requests matching `*.jsp*` patterns at the ingress level. The BeanShell probe returned HTTP 200, indicating the request was served. | Improved security posture | Low |

---

## Appendix: Data Collection Methodology

- **istio-proxy-2026.03.14**: 509,633,023 docs / 301.8 GB - Envoy access logs from service mesh
- **beckn-lp-logs-2026-03-14**: 51,637,135 docs / 59.9 GB - Application logs (JSON in message field)
- **istio-2026.03.14**: 171,889,737 docs / 152.9 GB - Istio control plane logs
- **mettle-2026.03.14**: 289,366 docs / 169.2 MB - Mettle backend logs
- **juspay-recon-2026.03.14**: 14,769 docs / 24.7 MB - Payment reconciliation logs
- **beckn-settlement-cron-2026.03.14**: 792,345 docs / 239.4 MB - Settlement cron logs

**Limitations**: OpenSearch's default `track_total_hits` caps exact counts at 10,000. Where counts show "10,000+", the actual number is at or above this threshold. VictoriaMetrics infrastructure metrics (CPU, memory, disk, network) were unavailable during data collection.

---

*Report generated on March 15, 2026 from OpenSearch production indices.*
