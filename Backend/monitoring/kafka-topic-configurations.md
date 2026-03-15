# Kafka Topic Configuration Reference

Recommended partition counts, retention, and compression settings for all
Namma Yatri Kafka topics. These are enforced in the dev environment via
`nix/services/nammayatri.nix` init script and should be mirrored in
production Kafka cluster configuration.

## Broker-Level Defaults (dev)

| Setting                              | Value      | Rationale                                      |
|--------------------------------------|------------|-------------------------------------------------|
| `num.partitions`                     | 6          | Sane default; high-throughput topics override   |
| `compression.type`                   | `producer` | Preserves client-side LZ4 end-to-end           |
| `log.retention.hours`                | 72         | 3 days; overridden per-topic where needed       |
| `log.segment.bytes`                  | 256 MB     | Granular cleanup without excessive small files  |
| `offsets.topic.replication.factor`   | 1          | Dev only (single broker); prod should be >= 3   |

## Topic Configurations

### High Throughput

| Topic                | Partitions | Retention | Compression | Consumer Groups                                          | Notes                                           |
|----------------------|------------|-----------|-------------|----------------------------------------------------------|-------------------------------------------------|
| `location-updates`   | 12         | 24h       | lz4         | `location-update-consumer`, `driver-availability-consumer` | Continuous driver location pings; highest volume |

### Medium Throughput

| Topic                                   | Partitions | Retention | Compression | Consumer Groups             | Notes                                     |
|-----------------------------------------|------------|-----------|-------------|-----------------------------|-------------------------------------------|
| `dynamic-offer-driver-events-updates`   | 6          | 72h       | lz4         | (ClickHouse sink)           | 10 event types: ride/booking lifecycle     |
| `rider-app-events-updates`              | 6          | 72h       | lz4         | `person-stats-compute`      | 10 event types: ride/booking lifecycle     |
| `broadcast-messages`                    | 3          | 48h       | lz4         | `broadcast-messages-compute` | Driver broadcast notifications             |
| `rider-sdk-events`                      | 3          | 48h       | lz4         | (SDK event pipeline)        | Mobile SDK telemetry                       |
| `driver-sdk-events`                     | 3          | 48h       | lz4         | (SDK event pipeline)        | Mobile SDK telemetry                       |

### Low Throughput

| Topic                          | Partitions | Retention      | Compression | Notes                          |
|--------------------------------|------------|----------------|-------------|--------------------------------|
| `ExophoneData`                 | 1          | 72h (default)  | lz4         | Call tracking events           |
| `AutoCompleteData`             | 1          | 72h (default)  | lz4         | Search autocomplete analytics  |
| `RouteCollection`              | 1          | 72h (default)  | lz4         | Route data events              |
| `EventTracker`                 | 1          | 72h (default)  | lz4         | Driver event tracking metrics  |
| `MarketingParamsData`          | 1          | 72h (default)  | lz4         | Marketing attribution          |
| `MarketingParamsPreLoginData`  | 1          | 72h (default)  | lz4         | Pre-login marketing params     |
| `metro-webview-events`         | 1          | 72h (default)  | lz4         | Metro webview telemetry        |

## Consumer Group Reference

| Consumer Group                  | Topic(s)                          | Consumer Type       | Config File                           |
|---------------------------------|-----------------------------------|---------------------|---------------------------------------|
| `location-update-consumer`      | `location-updates`                | `LOCATION_UPDATE`   | `dhall-configs/dev/location-update.dhall`            |
| `driver-availability-consumer`  | `location-updates`                | `AVAILABILITY_TIME` | `dhall-configs/dev/driver-availability-calculator.dhall` |
| `broadcast-messages-compute`    | `broadcast-messages`              | `BROADCAST_MESSAGE` | `dhall-configs/dev/broadcast-message.dhall`          |
| `person-stats-compute`          | `rider-app-events-updates`        | `PERSON_STATS`      | `dhall-configs/dev/person-stats.dhall`               |

## Production Scaling Notes

- **Partition count rule of thumb**: `max(consumer_instances * 2, expected_MB_per_sec * 2)`.
  For `location-updates` in prod with ~50k concurrent drivers at 1 update/3s, target 24-48 partitions.
- **Replication factor**: All topics should use RF=3 in production.
- **Retention**: Production retention may differ based on compliance requirements.
  ClickHouse-sunk topics can use shorter retention (24-48h) since data is durably stored downstream.
- **Consumer lag monitoring**: Set up alerts on consumer group lag via `kafka_consumergroup_lag`
  Prometheus metric. Threshold: > 10k messages for high-throughput topics, > 1k for others.
