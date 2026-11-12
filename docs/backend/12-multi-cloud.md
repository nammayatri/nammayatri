# Multi-Cloud Architecture

## Overview

Namma Yatri operates across AWS, GCP and NAMMA clouds. Understanding data flow between clouds is critical for debugging.

## Cloud Model

| Component | AWS | GCP |
|-----------|-----|-----|
| BAP DB | `atlas_app_v2` | `atlas_app_v1` |
| BPP DB | `atlas_driver_offer_bpp_v2` | `atlas_driver_offer_bpp_v1` |
| Redis | Independent instance | Independent instance |

## Key Architectural Rules

1. **Drivers poll the cloud they're registered in** — AWS driver always hits AWS BPP
2. **BAP makes cross-cloud calls to BPP** using BPP URLs (rider in AWS can book driver in GCP)
3. **Updates only happen in the Redis where data is found** — no cross-Redis sync on writes
4. **Other clouds' Redis are consulted only on a MISS (when fallback is enabled)** — stale cache hits in the local Redis bypass the other clouds! This is a common source of bugs
5. **DB replicates AWS→GCP via logical replication** — Redis does NOT replicate

## Data Replication

| Data Store | Replication | Direction |
|-----------|-------------|-----------|
| PostgreSQL | Logical replication | AWS → GCP |
| Redis | None | Independent per cloud |
| ClickHouse | — | Per cloud |

## KV Connector

The KV connector layer abstracts DB and Redis operations. It's implemented in the `euler-hs` package.

### Read Logic
1. Check the Redis of the cloud named by the id's cloud tag (the local cloud when there is no tag)
2. On MISS → if `KV_CLOUD_FALLBACK_ENABLED` is set, check every other cloud's Redis in parallel
3. On MISS → query PostgreSQL
4. Cache result in the Redis where it was found (the local cloud's Redis if new)

### Write Logic
1. Write to PostgreSQL
2. Write to the Redis where data was found (the local cloud's Redis if new)
3. **Never writes to more than one cloud's KV Redis**

### Key Functions

Core KV functions are in:
- `shared-kernel`: `lib/mobility-core/src/Kernel/Beam/Functions.hs`
- `euler-hs`: `src/EulerHS/KVConnector/Flow.hs`

| Function | Purpose |
|----------|---------|
| `findOneWithKV` | Find single record (Redis → DB fallback) |
| `findAllWithKV` | Find multiple records |
| `findAllWithOptionsKV` | Find with sort/pagination |
| `updateWithKV` | Update record (DB + Redis) |
| `deleteWithKV` | Delete record |
| `createWithKV` | Create record (DB + Redis) |

## N-Cloud Redis (AWS / GCP / NAMMA)

There is no primary or secondary Redis. Each cloud has one entry in a list, and the local cloud is the entry matching `CLOUD_TYPE`.

| Piece | Where |
|-------|-------|
| `CloudType` (`AWS`, `GCP`, `NAMMA`, `UNAVAILABLE`) and codes 0/1/2 | `Kernel.Types.Version` (`cloudTypeCode`) |
| Cloud tag on ids | `Kernel.Types.CloudTag` (`tagLocalGuid`), `EulerHS.KVConnector.CloudTag` |
| KV / Hedis config | `hedisClusterCfgs :: [HedisCloudCfg]` in the app config |
| LTS Redis config | `ltsRedisCfgs :: [HedisCloudCfg]` in the app config |
| Location Tracking Service | `ltsCfg.clouds :: [{ cloud, url }]` |
| Envs | `hedisClusterEnv` (local) and `peerHedisClusterEnvs :: [(CloudType, HedisEnv)]`; `ltsHedisEnv` and `peerLTSHedisEnvs` |
| KV probing | `EulerHS.KVConnector.Flow` / `Utils` (`meshPeerConns`, `findOneFromRedisAcrossClouds`) |

### Cloud-tagged ids
`generateGUID` (FlowR) replaces the last character of the UUID with the decimal digit of the generating pod's cloud (`AWS` 0, `GCP` 1, `NAMMA` 2, from `CLOUD_TYPE`). Ids that are not UUIDs, or have no digit tag, simply have no hint. The table is fixed because stored ids depend on it.

### Read and write behaviour
1. `findOne`: the cloud named by the tag on the id is probed first. Only when `KV_CLOUD_FALLBACK_ENABLED` is set are the other clouds probed (in parallel) on a miss. Without a tag the local cloud goes first.
2. `findAll` / `findAllWithOptions` / update-all / delete-all: when `KV_MULTI_CLOUD_READ_ENABLED` is set all clouds are read in parallel (plus DB where applicable) and merged by primary key; the local row wins.
3. Updates and deletes are applied in the cloud where the row was found. Creates go to the local cloud.
4. The tag is only a hint. A wrong or missing tag falls back as the flag allows, so correctness never depends on it.
5. The raw Redis wrappers (`runInMultiCloudRedis*`) read the local cloud first and, on a miss, read every peer in parallel. Writes go to the local cloud and every peer in parallel. The LTS list readers behave the same way.
6. Driver pool data is read from the local LTS replica and every peer LTS (`runOnAllPeerLTSRedis`), and written to the LTS of the driver's own cloud (`withPeerLTSRedis`).
7. `nearBy` and the other Location Tracking Service calls go to every cloud in `ltsCfg.clouds` in parallel. The local call must succeed; peer calls are best-effort.

### Environment variables
| Variable | Meaning |
|----------|---------|
| `CLOUD_TYPE` | The pod's cloud (`AWS`, `GCP`, `NAMMA`). Selects the local entry in every cloud list and the cloud tag on new ids |
| `KV_MULTI_CLOUD_READ_ENABLED` | Enables cross-cloud KV reads for all tables, `findAll` included |
| `KV_CLOUD_FALLBACK_ENABLED` | `findOne` falls back to the other clouds on a miss |
| `MASTER_CLOUD_TYPE` | Master cloud for `runInMasterCloudRedisCell` and `runInMasterLTSRedisCell` (no default) |
| `RUN_IN_MASTER_REDIS_CELL` / `RUN_IN_MASTER_LTS_REDIS_CELL` | Route those helpers to the master cloud |

### Config
- Every entry is `{ hedisCloud, hedisCloudCfg }`. See `dhall-configs/generic/common.dhall` (`HedisCloudCfg`, `CloudType`).
- The local entry connects as `KVRedis`; the others connect as `KVRedis<Cloud>`. If `CLOUD_TYPE` is set but missing from a list with more than one entry, startup fails.
- Peer KV connections must succeed at startup. Peer Hedis and LTS Redis connections that fail are logged and skipped.
- The `Tables` flags `enableSecondaryCloudRead`, `tablesForSecondaryCloudRead`, `enableAllTablesForSecondaryCloudRead` and `enableFindAllForMultiCloud` are ignored; the environment variables above decide.

## Multi-Cloud Callback Routing

For FRFS (public transport), `mkCloudBapUri` in `Beckn/ACL/FRFS/Utils.hs` replaces the host of `subscriberUrl` with runtime `nwAddress` for correct multi-cloud callback routing.

## Shared Redis Keys

`lib/utils/src/Tools/SharedRedisKeys.hs` — utilities for managing Redis keys that need to be consistent across services.

## Common Multi-Cloud Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Stale data after update | Cache hit in wrong Redis (rule #4) | Check which Redis the data is cached in |
| Driver not seeing update | Driver polling wrong cloud | Verify driver's registration cloud |
| Cross-cloud booking fails | BPP URL incorrect | Check BAP→BPP URL configuration |
| Data missing after failover | Redis doesn't replicate | Data only in one Redis; DB has it via replication |

## Related Docs

- Database patterns: `08-database-patterns.md`
- Architecture overview: `01-architecture-overview.md`
- FRFS multi-cloud: `10-frfs-public-transport.md`
