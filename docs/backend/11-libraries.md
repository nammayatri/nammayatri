# Shared Libraries (`lib/`)

All shared libraries live under `Backend/lib/`. They provide cross-cutting concerns used by multiple services.

## Library Catalog

### beckn-spec
**Path**: `lib/beckn-spec/`

BECKN protocol type definitions (V1 and V2):
- Protocol message types for all Beckn steps
- Domain types shared across BAP and BPP: `BookingStatus`, `RideStatus`, `FRFSTicketBookingStatus`
- Tag definitions: `src/BecknV2/OnDemand/Tags.hs`
- Enums: `src/BecknV2/OnDemand/Enums.hs`
- V2 types: `src/BecknV2/` (Context, Order, Item, Fulfillment, etc.)

Key files:
| File | Content |
|------|---------|
| `src/Domain/Types/BookingStatus.hs` | BookingStatus enum |
| `src/Domain/Types/RideStatus.hs` | RideStatus enum |
| `src/Domain/Types/FRFSTicketBookingStatus.hs` | FRFSTicketBookingStatus enum |
| `src/BecknV2/OnDemand/Tags.hs` | All BECKN tags (must define here before using) |

### beckn-services
**Path**: `lib/beckn-services/`

BECKN service utilities — HTTP clients for making Beckn protocol calls.

### payment
**Path**: `lib/payment/`

Juspay payment gateway integration:
| Module | Purpose |
|--------|---------|
| `src/Lib/Payment/API.hs` | Payment API definitions |
| `src/Lib/Payment/Domain/Action.hs` | Core payment actions (createOrder, webhookService, refund, payout) |
| `src/Lib/Payment/Domain/Types/PaymentTransaction.hs` | Payment transaction types |
| `src/Lib/Payment/Domain/Types/PaymentOrder.hs` | Payment order types |
| `src/Lib/Payment/Payout/` | Payout handling (History, Status, Request, Registration, Items) |
| `src-read-only/` | Generated types and queries |

Key functions in `Domain/Action.hs`:
- `juspayWebhookService` — Webhook handler
- `createOrderService` — Payment order creation
- `orderStatusService` — Payment status checks
- `createRefundService` — Refund processing
- `createPayoutService` — Payout creation

### scheduler
**Path**: `lib/scheduler/`

Redis-based job scheduling framework:
| Module | Purpose |
|--------|---------|
| `src/Lib/Scheduler.hs` | Main scheduler module |
| `src/Lib/Scheduler/App.hs` | App initialization |
| `src/Lib/Scheduler/Handler.hs` | Request handlers |
| `src/Lib/Scheduler/ScheduleJob.hs` | Job scheduling logic |
| `src/Lib/Scheduler/JobHandler.hs` | Job execution |
| `src/Lib/Scheduler/Types.hs` | Type definitions |
| `src/Lib/Scheduler/JobStorageType/Redis/Queries.hs` | Redis storage |
| `src/Lib/Scheduler/JobStorageType/DB/Queries.hs` | Database storage |

### location-updates
**Path**: `lib/location-updates/`

Real-time location tracking with OSRM:
- Snap-to-road functionality
- Distance calculations
- Route matching
- Has its own test suite

### yudhishthira
**Path**: `lib/yudhishthira/`

Business rule/decision engine:
- Dynamic rule evaluation
- Configurable business logic
- Has its own test suite

### finance-kernel
**Path**: `lib/finance-kernel/`

Financial primitives:
- `HighPrecMoney` type for precise monetary calculations
- Currency handling
- Financial operations
- Has generated types in `src-read-only/`

### shared-services
**Path**: `lib/shared-services/`

Cross-cutting utilities:
| Module | Purpose |
|--------|---------|
| `src/IssueManagement/` | IGM (Issue & Grievance Management) |
| `src/IssueManagement/Beckn/ACL/` | IGM Beckn ACL (Issue, IssueStatus, OnIssue, OnIssueStatus) |

### webhook
**Path**: `lib/webhook/`

Generic webhook handling framework. Has generated code in `src-read-only/`.

### external
**Path**: `lib/external/`

External service abstractions — interfaces for third-party services.

### special-zone
**Path**: `lib/special-zone/`

Special zone logic — geofenced areas with special pricing/rules.

### utils
**Path**: `lib/utils/`

General utilities:
| Module | Purpose |
|--------|---------|
| `src/Tools/SharedRedisKeys.hs` | Multi-cloud shared Redis key utilities |

### producer
**Path**: `lib/producer/`

Event/message producer — Kafka event production utilities.

### sessionizer-metrics
**Path**: `lib/sessionizer-metrics/`

Session tracking and metrics collection.

### dashcam
**Path**: `lib/dashcam/`

Dashcam-related functionality.

## Related Docs

- Architecture: `01-architecture-overview.md`
- Payment integration: `13-external-integrations.md`
- Multi-cloud / Redis: `12-multi-cloud.md`
