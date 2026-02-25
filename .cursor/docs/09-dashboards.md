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

## Migrations

| Dashboard | Migration Path |
|-----------|---------------|
| rider-dashboard | `dev/migrations/rider-dashboard/` |
| provider-dashboard | `dev/migrations/provider-dashboard/` |
| safety-dashboard | `dev/migrations/safety-dashboard/` |

## Related Docs

- Architecture overview: `01-architecture-overview.md`
- API spec format: `07-namma-dsl.md`
