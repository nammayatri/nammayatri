# Ride-Events Consumer — Observability

Operational reference for the `RIDE_EVENTS_CONSUMER` consumer in `kafka-consumers`.

## Metric sources

All consumer emissions today are **structured log lines** from
`Consumer.RedisStream.Metrics` and `Consumer.RedisStream.Reader`. Ingest via
Loki/equivalent and extract counters with log-based metrics.

| Log key prefix | Source | Meaning |
| --- | --- | --- |
| `ride-events.processed handler=<H> entry=<id>` | `recordProcessed` | One handler invocation succeeded |
| `ride-events.failed handler=<H> entry=<id> attempt=<N> err=<...>` | `recordFailed` | One handler invocation failed |
| `ride-events.dlq_push shard=<S> source_stream=<T> entry=<id>` | `recordDlqPush` | Entry pushed to DLQ after exhausting retries |
| `ride-events.idempotency-skip handler=<H> rideId=<id>` | `withIdempotency` | Re-delivery skipped by SETNX guard |
| `redis-stream consumer starting: group=<G> instance=<I> shards=<N>` | `Reader.runReader` | Process boot |

Stream-level state (XLEN, XPENDING summary) is **not yet exported as Prometheus
gauges** — shared-kernel `Kernel.Storage.Hedis` does not expose `xLen`/`xPending`
wrappers. See [follow-up](#phase-2-follow-ups).

## Publisher-side counters

The publisher in `EndRide/Internal.hs` emits these log lines:

| Log key | Meaning |
| --- | --- |
| `ride-events.published rideId=<R> stream=<S>` | xAdd succeeded |
| `ride-events.publish-failed rideId=<R> err=<...>` | xAdd failed (event lost — direct-publish risk) |
| `ride-events.kafka-published rideId=<R> topic=<T>` | Kafka produce succeeded |
| `ride-events.kafka-publish-failed rideId=<R> err=<...>` | Kafka produce failed (event lost on that sink) |

Metrics: `ride_events_published`, `ride_events_publish_failed` (Redis) and
`ride_events_kafka_published`, `ride_events_kafka_publish_failed` (Kafka).

## Publisher / consumer configuration

`rideEventsPublisherCfg` in the driver-app dhall has two independent, optional sinks:

```dhall
Some { redisConfig = Some { streamPrefix = "ride_events_stream_", shardCount = +10 }
     , kafkaConfig = Some { kafkaTopic = "ride-events" } }
```

`None` on a sink disables it. Dev toggles: `RIDE_EVENTS_REDIS_PUBLISH_ENABLED` (default True),
`RIDE_EVENTS_KAFKA_PUBLISH_ENABLED` (default False), and on the consumer
`RIDE_EVENTS_CONSUMER_USE_KAFKA` (default False). Kafka messages are keyed by `rideId`.

Redis to Kafka rollout: enable both publisher sinks, run a Kafka-transport consumer
(group `ride-events-consumers`) alongside the Redis one (handler idempotency keys prevent
double side-effects), then drain the Redis streams, disable `redisConfig` and stop the
Redis consumer. Roll back by re-enabling `redisConfig`.

## Recommended dashboards

| Panel | Metric query (LogQL example) |
| --- | --- |
| Events published / s | `sum(rate({app="dynamic-offer-driver-app"} \|= "ride-events.published" [1m]))` |
| Events processed / s per handler | `sum by (handler) (rate({app="kafka-consumers"} \|= "ride-events.processed" [1m]))` |
| Published vs processed (sanity) | overlay above two; ratio should ≈ 1.0 once forks are flipped to `skip=True` |
| Failures / s per handler | `sum by (handler) (rate({app="kafka-consumers"} \|= "ride-events.failed" [1m]))` |
| DLQ pushes / s | `sum(rate({app="kafka-consumers"} \|= "ride-events.dlq_push" [5m]))` |
| Idempotency hits / s | `sum(rate({app="kafka-consumers"} \|= "ride-events.idempotency-skip" [5m]))` |
| Publish failures / s | `sum(rate({app="dynamic-offer-driver-app"} \|= "ride-events.publish-failed" [5m]))` |

## Recommended alerts

| Alert | Condition | Severity |
| --- | --- | --- |
| DLQ growth | DLQ pushes > 0 sustained for 5 min | warn |
| High publish-failure rate | `rate(publish-failed) > 0.01/s` for 5 min | page |
| Handler failure rate | `rate(failed{handler="X"}) / rate(processed{handler="X"}) > 0.05` | warn |
| Idempotency hit rate spike | `rate(idempotency-skip) > 10× baseline` | warn (indicates excessive re-delivery / PEL accumulation) |
| Consumer not making progress | No `ride-events.processed` in last 5 min while publisher emits | page |

## Manual stream inspection

Until xLen/xPending are wrapped in shared-kernel, ops can inspect directly:

```sh
# Stream lengths
for n in 0 1 2 3 4 5 6 7; do redis-cli XLEN "ride.events.shard${n}"; done

# Pending entries (uncommitted from PEL)
for n in 0 1 2 3 4 5 6 7; do redis-cli XPENDING "ride.events.shard${n}" ride-events-cg; done

# DLQ
redis-cli XLEN ride.events.dlq

# Killswitch (set to pause all shards, del to resume)
redis-cli SET consumer:paused:ride-events "1"
redis-cli DEL consumer:paused:ride-events
```

## Phase-2 follow-ups

1. **Wrap XLEN, XPENDING, XAUTOCLAIM, XCLAIM in `Kernel.Storage.Hedis`** so we can
   export Prometheus gauges and recover entries from permanently-dead consumer
   instances.
2. **Promote log-based metrics to Prometheus counters** registered through
   `CoreMetrics.CoreMetricsContainer`. The `Consumer.RedisStream.Metrics` API
   stays the same; only its body changes.
3. **Reconciler** — periodically scan recently-completed rides whose
   `ride:processed:<handler>:<rideId>` keys are missing across all 8 handlers
   and republish their `RideEndedEvent`. Closes the direct-xAdd loss window.
