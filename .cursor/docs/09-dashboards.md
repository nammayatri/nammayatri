# Dashboard Services

## Overview

Namma Yatri has multiple dashboard services for operations management.

## Dashboard Packages

| Package | Path | Purpose |
|---------|------|---------|
| rider-dashboard | `app/dashboard/rider-dashboard/` | Rider operations (bookings, customers, support) |
| provider-dashboard | `app/dashboard/provider-dashboard/` | Provider operations (drivers, fleet, revenue) |
| CommonAPIs | `app/dashboard/CommonAPIs/` | Shared API type definitions |
| Lib | `app/dashboard/Lib/` | Shared dashboard library |
| safety-dashboard | `app/safety-dashboard/` | Safety-related operations |
| unified-dashboard | `app/unified-dashboard/` | Unified dashboard entry point |

## Directory Structure

```
app/dashboard/
├── CommonAPIs/           # Shared API types across dashboards
│   └── src-read-only/    # Generated API types
├── Lib/                  # Shared dashboard library
│   └── src/              # Auth, handlers, shared logic
├── rider-dashboard/      # Rider-side operations
│   ├── spec/API/         # YAML API specs
│   ├── src/              # Business logic
│   └── src-read-only/    # Generated code
└── provider-dashboard/   # Provider-side operations
    ├── spec/API/         # YAML API specs
    ├── src/              # Business logic
    └── src-read-only/    # Generated code
```

## Authentication

Dashboard endpoints use `DashboardAuth` authentication type in YAML specs:

```yaml
apis:
  - GET:
      endpoint: /dashboard/booking/{bookingId}
      auth: DashboardAuth
      response:
        type: BookingDetails
```

## Internal service-token APIs (provider-dashboard)

For cluster-internal automation (e.g. stuck-booking cron jobs) there is a
service-token-only API under `/bpp/driver-offer/internal/` and the same for
the BAP tree under `/bap/internal/`:

- `internal/auth` — api-key + person token exchange (fleet helper, BPP only).
- `bpp/driver-offer/internal/admin/:merchantId/:city/...` — api-key-ONLY admin
  operations: `ride/end`, `ride/cancel`, `ride/sync`, `booking/sync`,
  `booking/cancel/allStuck`.
- `bap/internal/admin/:merchantId/:city/...` — api-key-ONLY admin operations:
  `ride/sync`, `booking/sync`.

Both are mounted before the merchant-capturing route trees (the BAP one lives
in provider-dashboard's `API.hs`, not in `API.RiderPlatform`, so the
rider-dashboard module mirror stays byte-identical).

Related marker: `booking.is_stucked` (BPP + rider DBs) is a cron/bookkeeping
boolean, set to `True` in batch (`updateIsStucked [Id Booking]`) by the
booking sync / stuck-cancel flows above; new bookings seed it `False`.

Auth: header `api-key` must equal the `internalAuthAPIKey` config value (in
`app/dashboard/Lib/src/Environment.hs`, set per environment via dhall/secrets).
There is NO RBAC/capability check — the token authorizes the operation for ANY
merchant, so these routes must only ever be reachable via in-cluster DNS
(e.g. `beckn-provider-dashboard-svc`), never a public ingress. The request
bodies are the same `MultipleRide*Req`/`MultipleBookingSyncReq` types the RBAC
routes take, and the same domain implementations are executed; calls are
audited in the dashboard `transaction` table with requestor
`INTERNAL_ADMIN_API`.

## Migrations

| Dashboard | Migration Path |
|-----------|---------------|
| rider-dashboard | `dev/migrations/rider-dashboard/` |
| provider-dashboard | `dev/migrations/provider-dashboard/` |
| safety-dashboard | `dev/migrations/safety-dashboard/` |

## Related Docs

- Architecture overview: `01-architecture-overview.md`
- API spec format: `07-namma-dsl.md`
