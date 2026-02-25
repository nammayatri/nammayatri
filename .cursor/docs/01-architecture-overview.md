# Architecture Overview

## Project Structure

Namma Yatri is an open-source mobility platform (ride-hailing + delivery + public transport) built on the BECKN/ONDC protocol. Monorepo with Haskell backend, PureScript frontend, native Android.

The backend is a multi-package Cabal project (~48 packages) following microservices architecture.

## Core Services

### Rider Platform (BAP — Beckn Application Platform = customer side)

| Package | Port | Path | Purpose |
|---------|------|------|---------|
| rider-app | 8013 | `app/rider-platform/rider-app/` | Main customer APIs: search, booking, payment |
| rider-app-scheduler | — | `app/rider-platform/rider-app/Scheduler/` | Background jobs for rider platform |
| public-transport-rider-platform | — | `app/rider-platform/public-transport-rider-platform/` | FRFS bus/metro services |

### Provider Platform (BPP — Beckn Provider Platform = driver side)

| Package | Port | Path | Purpose |
|---------|------|------|---------|
| dynamic-offer-driver-app | 8016 | `app/provider-platform/dynamic-offer-driver-app/` | Main driver APIs: ride management |
| driver-offer-allocator | 9996 | `app/provider-platform/dynamic-offer-driver-app/Allocator/` | Core driver allocation engine |

### Dashboards

| Package | Path | Purpose |
|---------|------|---------|
| rider-dashboard | `app/dashboard/rider-dashboard/` | Rider operations dashboard |
| provider-dashboard | `app/dashboard/provider-dashboard/` | Provider operations dashboard |
| safety-dashboard | `app/safety-dashboard/` | Safety operations |
| unified-dashboard | `app/unified-dashboard/` | Unified dashboard APIs |
| CommonAPIs | `app/dashboard/CommonAPIs/` | Shared dashboard API types |
| Lib | `app/dashboard/Lib/` | Dashboard shared library |

### Other Services

| Package | Path | Purpose |
|---------|------|---------|
| kafka-consumers | `app/kafka-consumers/` | Kafka event consumers |
| sdk-event-pipeline | `app/sdk-event-pipeline/` | SDK event processing |
| special-zone | `app/special-zone/` | Special zone management service |
| mocks | `app/mocks/` | Mock services (google, idfy, fcm, sms) |

## Shared Libraries (`lib/`)

| Library | Path | Purpose |
|---------|------|---------|
| beckn-spec | `lib/beckn-spec/` | BECKN protocol types and API definitions |
| beckn-services | `lib/beckn-services/` | BECKN service utilities |
| payment | `lib/payment/` | Juspay payment gateway integration |
| scheduler | `lib/scheduler/` | Redis-based job scheduling |
| location-updates | `lib/location-updates/` | Real-time tracking with OSRM |
| yudhishthira | `lib/yudhishthira/` | Business rule/decision engine |
| finance-kernel | `lib/finance-kernel/` | Financial primitives |
| shared-services | `lib/shared-services/` | Cross-cutting concerns (issue management, etc.) |
| special-zone | `lib/special-zone/` | Special zone logic |
| webhook | `lib/webhook/` | Generic webhook handling |
| external | `lib/external/` | External service abstractions |
| producer | `lib/producer/` | Event/message producer |
| utils | `lib/utils/` | General utilities |
| dashcam | `lib/dashcam/` | Dashcam functionality |
| sessionizer-metrics | `lib/sessionizer-metrics/` | Session tracking |

## Databases

| Database | Rider Schema | Driver Schema | Purpose |
|----------|-------------|---------------|---------|
| PostgreSQL | `atlas_app` | `atlas_driver_offer_bpp` | Primary OLTP |
| Redis | Single 6379 / Cluster 30001 | Same | Caching, location, job queues |
| ClickHouse | — | — | Analytics and event tracking |
| Kafka | — | — | Event streaming between services |

## BAP / BPP Split

- **BAP (rider-app)**: Customer-facing. Initiates protocol calls (search, select, init, confirm). Receives callbacks (on_search, on_select, on_init, on_confirm).
- **BPP (driver-app)**: Provider-facing. Receives protocol calls. Sends callbacks back to BAP.
- **ACL modules** translate between BECKN protocol types and internal domain types.

## Key Configuration

- **Dhall configs**: `dhall-configs/dev/` — typed configuration files
- **Paseto**: Token-based API authentication
- **Nix**: Build system and dev environment

## Related Docs

- Service deep dives: `03-rider-app.md`, `04-driver-app.md`
- Protocol flow: `05-beckn-protocol-flow.md`
- Libraries detail: `11-libraries.md`
