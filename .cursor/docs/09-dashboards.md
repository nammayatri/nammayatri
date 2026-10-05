# Dashboard Serving

## Overview

Dashboard APIs are served **directly by the application servers** (rider-app
and dynamic-offer-driver-app). The old standalone proxy services
(rider-dashboard, provider-dashboard, safety-dashboard) have been removed from
the codebase; see `Backend/dev/docs/dashboard-direct-serving.md` for the full
design.

## Packages

| Package | Path | Purpose |
|---------|------|---------|
| Lib (lib-dashboard) | `app/dashboard/Lib/` | Shared dashboard library: login/session/role/2FA (`API.DashboardLogin`), merchant + person types, audit transactions. Used by both app servers. |
| rider-app | `app/rider-platform/rider-app/` | Serves rider-side dashboard routes under `/direct-dashboard/` (`API.DirectDashboard`, `API/Action/DashboardAuth/*`) |
| dynamic-offer-driver-app | `app/provider-platform/dynamic-offer-driver-app/` | Serves provider-side dashboard routes under `/direct-dashboard/`, plus the login/user-admin tree |

## Routing (dev nginx, `Backend/dev/nginx/nginx.conf`)

| Public prefix | Target |
|---|---|
| `/bpp/driver-offer/<merchantId>/<city>/...` | driver-app `/direct-dashboard/<merchantId>/<city>/...` |
| `/bap/<merchantId>/<city>/...` | rider-app `/direct-dashboard/<merchantId>/<city>/...` |
| `/user/*`, `/admin/*`, `/listTransactions`, `/specialZone/*` | driver-app `/direct-dashboard/...` (rider-app does NOT mount the login tree) |

## Authentication

Dashboard endpoints use `ApiAuthV2` / `DashboardUserAuth` in YAML specs; the
app server verifies the operator session and endpoint capability against the
unified dashboard database (`atlas_dashboard`, accessed via lib-dashboard).

## Database & Migrations

One unified schema: `atlas_dashboard`.

| Content | Path |
|-----------|---------------|
| DSL-generated (transactions, capability endpoints) | `dev/migrations-read-only/dashboard/` |
| Hand-written DDL | `dev/ddl-migrations/dashboard/` |
| Seeds (roles, capabilities) | `dev/seed-migrations/dashboard/` |
| Dev schema seed | `dev/sql-seed/dashboard-seed.sql` |
| Local testing data | `dev/local-testing-data/dashboard.sql`, `unified-dashboard-merchants.sql` |

In dev these are applied at startup by dynamic-offer-driver-app (see
`migrationPath` in `Backend/dhall-configs/dev/dynamic-offer-driver-app.dhall`).

## Related Docs

- Architecture overview: `01-architecture-overview.md`
- API spec format: `07-namma-dsl.md`
- Direct serving design: `Backend/dev/docs/dashboard-direct-serving.md`
