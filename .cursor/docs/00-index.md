# Nammayatri Documentation Index

This directory contains chunked documentation for the Nammayatri backend. Each doc covers a specific topic with exact file paths relative to `Backend/`.

## How to Use

1. The root `.cursorrules` file is always loaded and contains critical rules + a routing table
2. Read only the doc(s) relevant to your current task — don't load everything
3. Each doc is self-contained with cross-references to related docs

## Document Catalog

| Doc | Topic | Read when... |
|-----|-------|-------------|
| `01-architecture-overview.md` | Service map, ports, databases, BAP/BPP split | Starting a new task, need orientation on which service to modify |
| `02-build-and-dev.md` | Build commands, nix, cabal, comma commands, code gen | Setting up dev environment, running builds, troubleshooting compilation |
| `03-rider-app.md` | rider-app deep dive (81 UI handlers, Beckn, SharedLogic) | Working on customer-facing features (search, booking, payment, FRFS) |
| `04-driver-app.md` | dynamic-offer-driver-app deep dive (71 UI handlers, Allocator) | Working on driver-facing features (ride management, onboarding, fees) |
| `05-beckn-protocol-flow.md` | Full BAP/BPP file path table for all protocol steps | Implementing or debugging BECKN protocol interactions |
| `06-ride-flow.md` | Complete 8-phase ride lifecycle with file paths | Understanding end-to-end ride flow from search to completion |
| `07-namma-dsl.md` | YAML spec syntax, generator commands, output locations | Creating/modifying APIs or database schemas via YAML |
| `08-database-patterns.md` | Queries, cached queries, migrations, Beam, KV functions | Writing database queries, caching, or migrations |
| `09-dashboards.md` | Dashboard services, auth, API categories | Working on operations dashboards |
| `10-frfs-public-transport.md` | FRFS architecture, ONDC vs Direct, ExternalBPP | Working on metro/bus/public transport features |
| `11-libraries.md` | All lib/ packages with descriptions | Using or modifying shared libraries |
| `12-multi-cloud.md` | AWS/GCP model, KV connector, Redis patterns | Debugging multi-cloud issues or understanding data replication |
| `13-external-integrations.md` | Juspay, OSRM, Idfy, SMS, FCM, WhatsApp | Integrating with external services |
| `14-testing-and-debugging.md` | Test infra, mock services, debugging patterns | Writing tests or debugging issues |
| `15-conventions.md` | Module organization, naming, imports, extensions | Following project coding standards |
| `16-status-definitions.md` | All status enums with state transition diagrams | Understanding booking/ride/ticket state machines |

## Cross-Reference Guide

- **Adding a new API endpoint**: Read `07-namma-dsl.md` → `15-conventions.md`
- **Adding a new DB table**: Read `07-namma-dsl.md` → `08-database-patterns.md`
- **Debugging a ride issue**: Read `06-ride-flow.md` → `05-beckn-protocol-flow.md` → `16-status-definitions.md`
- **Payment integration**: Read `13-external-integrations.md` → `11-libraries.md`
- **FRFS feature**: Read `10-frfs-public-transport.md` → `05-beckn-protocol-flow.md`
- **Multi-cloud bug**: Read `12-multi-cloud.md` → `08-database-patterns.md`
