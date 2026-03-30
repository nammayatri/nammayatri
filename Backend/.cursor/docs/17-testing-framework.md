# Testing Framework

Complete architecture of the NammaYatri local testing infrastructure: config sync, integration tests, mock servers, and test tools.

## Overview

```
Backend/dev/
├── config-sync/          # Export config from master DB → patch → import to local
├── integration-tests/    # Newman/Postman collections for E2E testing
├── mock-servers/         # Unified Python mock for all external services
└── test-tool/            # Dashboard UI + context API for manual testing
```

---

## 1. Config Sync (`dev/config-sync/`)

Syncs configuration tables from a master (staging/prod) database to local dev, with URL and credential patching.

### Flow

```
Master DB ──export──→ JSON files ──patch──→ Patched JSON ──import──→ Local DB
              │                      │                          │
         config.json            patches.json              passetto (encrypt)
         (table list)     (URL replacements,             post-import-setup.sql
                          dimension overrides,            (dashboard access)
                          ENCRYPT: re-encryption)
```

### Modes

| Mode | Storage | Config | Usage |
|------|---------|--------|-------|
| `DEV=true` | Local filesystem (`assets/data/`) | `assets/environments.json` | Local dev |
| `DEV=false` | S3 bucket | Environment variables | CI/CD |

### Commands

```bash
# Export from master to local files
python config_transfer.py export --from master --to local

# Import from local files to local DB
python config_transfer.py import --from master --to local

# Full sync (export + patch + import)
python config_transfer.py sync --from master --to local

# List tables being synced
python config_transfer.py list-tables --schema atlas_app

# Discover new config tables (compares CachedQueries vs config.json)
python config_transfer.py discover
```

### Key Files

| File | Purpose | Git tracked? |
|------|---------|-------------|
| `config.json` | Lists all config tables per schema | Yes |
| `environments.json` | DB connection strings per environment | **No** (has real hosts) |
| `environments.json.example` | Template for environments.json | Yes |
| `patches.json` | URL replacements, dimension overrides, ENCRYPT: rules | **No** (has ENCRYPT: values) |
| `patches.json.example` | Template for patches.json | Yes |
| `post-import-setup.sql` | Dashboard access setup (INSERT operations) | Yes |
| `assets/data/` | Exported/patched JSON data files | **No** (master config data) |

### Patching System (`patches.json`)

#### Global Replacements
String find/replace on raw JSON file content:
```json
{"find": "https://api.juspay.in", "replace": "http://localhost:8080/juspay"}
```

#### Dimension Overrides
Per-table, per-row field overrides applied at import time:
```json
{
  "atlas_app": {
    "merchant_service_config": [{
      "where": {"service_name": "Payment_Juspay"},
      "merge_json": {
        "config_json": {
          "url": "http://localhost:8080/juspay",
          "apiKey": "ENCRYPT:S\"mock-api-key\"",
          "password": "ENCRYPT:S\"mock-password\""
        }
      }
    }]
  }
}
```

#### ENCRYPT: Prefix
Values prefixed with `ENCRYPT:` are encrypted via local passetto at import time:
- Format: `ENCRYPT:S"plaintext"` (the `S""` wrapper is passetto's expected format)
- Ensures credentials work with the local encryption key, not master's

---

## 2. Integration Tests (`dev/integration-tests/`)

Newman/Postman collections for automated E2E testing of all booking flows.

### Structure

```
integration-tests/
├── run-tests.sh                      # Test runner
├── Rules.md                          # Guidelines for writing collections
├── collections/
│   ├── RideBookingFlow/              # Cash ride flows (4 cities × 6 suites)
│   │   ├── 01-AutoRideFlow.json
│   │   ├── 02-AutoUserCancellation.json
│   │   ├── 03-AutoDriverCancellation.json
│   │   ├── 04-CabRideFlow.json
│   │   ├── 05-CabUserCancellation.json
│   │   ├── 06-CabDriverCancellation.json
│   │   ├── Local_NY_Bangalore.postman_environment.json
│   │   ├── Local_YS_Kolkata.postman_environment.json
│   │   ├── Local_NY_Chennai.postman_environment.json
│   │   └── Local_BT_Delhi.postman_environment.json
│   ├── OnlineRideBookingFlow/        # Stripe payment rides
│   │   ├── 01-StripeRideFlow.json
│   │   └── Local_BF_Helsinki.postman_environment.json
│   ├── BusTicketBookingFlow/         # FRFS bus tickets
│   │   ├── 01-DirectBusBooking.json
│   │   ├── 02-AutoRefunded.json
│   │   ├── Local_FRFS_Chennai.postman_environment.json
│   │   └── Local_FRFS_Bhubaneshwar.postman_environment.json
│   ├── MetroTicketBookingFlow/       # FRFS metro tickets
│   │   ├── 01-DirectMetroBooking.json
│   │   ├── 02-TechnicalFailure.json
│   │   ├── 03-AutoRefunded.json
│   │   ├── Local_FRFS_Chennai.postman_environment.json
│   │   └── Local_FRFS_Bangalore.postman_environment.json
│   └── SubwayTicketBookingFlow/      # FRFS subway tickets
│       ├── 01-DirectSubwayBooking.json
│       ├── 02-TechnicalFailure.json
│       ├── 03-AutoRefunded.json
│       └── Local_FRFS_Chennai.postman_environment.json
```

### Running Tests

```bash
./run-tests.sh                          # All ride suites for all cities
./run-tests.sh rides                    # All ride suites
./run-tests.sh rides NY_Bangalore       # Rides for Bangalore only
./run-tests.sh online                   # Stripe payment rides
./run-tests.sh bus                      # All bus ticket flows
./run-tests.sh metro FRFS_Chennai       # Metro for Chennai only
./run-tests.sh subway                   # All subway flows
./run-tests.sh --list                   # List all suites
```

Or via nix:
```bash
, run-mobility-stack-test               # Full stack: start services → import config → run all tests → stop
, run-mobility-stack-test rides         # Only ride tests
```

### Collection Design Principles

See `Rules.md` for full guidelines. Key rules:

1. **All identifiers are random per run** — mobile numbers, vehicle registrations, etc. generated in collection prerequest
2. **City/merchant config comes from environment files** — never hardcoded in collections
3. **Collections are shared across cities** — one collection, multiple environment files
4. **Dashboard access via switchMerchantAndCity** — one admin token, switched dynamically per city

### Adding a New City

1. Create `Local_<PREFIX>_<City>.postman_environment.json` with city-specific values
2. No collection changes needed — just add the environment file

### Adding a New Flow

1. Create `<NN>-<FlowName>.json` in the appropriate directory
2. Add collection prerequest for random number generation
3. Use mock server status/override APIs to control external service behavior

---

## 3. Mock Servers (`dev/mock-servers/`)

Unified Python HTTP server that mocks all external service integrations.

### Architecture

```
server.py                    # Router + middleware (override system)
status_store.py              # In-memory store + override matching engine
services/
├── juspay.py               # Payment orders, offers, refunds
├── stripe.py               # Full Stripe v1 API (21 endpoints, stateful)
├── paytm.py                # PayTM payment
├── cmrl.py                 # Chennai Metro (AES-CBC encrypted, auth tokens)
├── cris.py                 # Delhi Subway (AES-ECB encrypted, agent key derivation)
├── ebix.py                 # Kolkata Bus (ticket QR)
├── exotel.py               # Calls
├── acko.py                 # Insurance
├── sos.py                  # SOS/ERSS
├── kapture.py              # Ticketing
├── whatsapp.py             # GupShup/Karix/Tata
├── mmi.py                  # MapMyIndia
├── nextbillion.py          # Maps
├── gridline.py             # Aadhaar verification
├── hyperverge.py           # Document verification
├── gullak.py               # Tokenization
├── transit.py              # Nandi/GTFS
└── openai.py               # LLM
```

### Running

```bash
python dev/mock-servers/server.py --port 8080
```

Automatically started by `run-mobility-stack-dev` via process-compose.

### Override System

Two mechanisms for test automation to control mock responses:

#### 1. Status Store (`POST /mock/status`)

Keyed by explicit ID (e.g., payment order ID). Set by test collection after extracting the ID from API responses.

```json
POST /mock/status
{
  "service": "juspay",
  "id": ["order-uuid-123", "short-id-abc"],
  "status": "CHARGED",
  "data": {
    "amount": 150.0,
    "refunds": [{"id": "rfnd-1", "amount": 50, "status": "REFUND_SUCCESS"}]
  }
}
```

- `id` can be string or array — stored under all IDs for lookup
- `data` fields are **deep-merged** into the handler's default response
- Entries auto-expire after 5 minutes
- `GET /mock/status` lists all active entries
- `DELETE /mock/status` clears all (use sparingly — not concurrent-safe)

#### 2. Override Rules (`POST /mock/override`)

Matches incoming requests by extracting a field value — for services where the test doesn't know the exact ID upfront.

```json
POST /mock/override
{
  "service": "cris",
  "extract": "body.mob",
  "value": "7461059507",
  "response": {"agentTicketData": 12345, "encrypted": 12345}
}
```

**Extract syntax:**
| Syntax | Source | Example |
|--------|--------|---------|
| `body.<path>` | JSON request body | `body.mob`, `body.data.customer.phone` |
| `path.<index>` | URL path segment (0-based) | `path.2` = 3rd segment |
| `query.<param>` | Query parameter | `query.vehicleType` |
| `header.<name>` | Request header | `header.authorization` |

**Encrypted body support:**
For services with encrypted request bodies (CRIS uses AES-ECB, CMRL uses AES-CBC), register a **body decoder**:
```python
# In services/cris.py
register_body_decoder("cris", _decode_cris_body)
```
The middleware calls the decoder before matching, so `body.mob` extracts from the decrypted inner payload.

**Delete specific override:**
```json
DELETE /mock/override
{"service": "cris", "extract": "body.mob", "value": "7461059507"}
```

### Handler Pattern

Every service handler follows the same pattern:
```python
def handle(handler, path, body):
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("service_name", *path_ids)
    # _get_override merges: status store (by ID) + middleware override (by request matching)

    base = {"default": "response"}
    if extra:
        base = deep_merge(base, extra)
    handler._json(base)
```

### Adding a New Service Mock

1. Create `services/<name>.py` with `handle(handler, path, body)` function
2. Add route to `ROUTES` in `server.py`: `("/prefix", module, "service_name")`
3. If the service has encrypted requests, register a body decoder via `register_body_decoder()`

---

## 4. Test Tools (`dev/test-tool/`)

### Dashboard (`dev/test-tool/dashboard/`)

React web UI for manually running test flows (ride booking, Stripe payments, etc.)

```bash
# Build (one-time)
cd dev/test-tool/dashboard && npm install && npm run build

# Start (auto-started by run-mobility-stack-dev)
# Or manually:
cd dev/test-tool && ./start.sh
```

- **Dashboard**: http://localhost:7070
- **Context API**: http://localhost:7082 (DB queries for test data)
- **Mock Stripe**: Now served by unified mock server at http://localhost:8080/stripe

### Context API (`dev/test-tool/context-api/`)

Python server that provides DB query access for the dashboard:
- Queries test entities (drivers, riders, bookings)
- Proxies requests to rider-app/driver-app

### Full Stack Test (`run-mobility-stack-test`)

Single command to start everything, import config, and run tests:

```bash
, run-mobility-stack-test               # All tests
, run-mobility-stack-test rides         # Specific flow
, run-mobility-stack-test bus FRFS_Chennai  # Specific city
```

Steps:
1. Check ports are free (waits up to 5 min)
2. Start `run-mobility-stack-dev` in background
3. Wait for rider-app (8013) and driver-app (8016) health
4. Import config from master (`config_transfer.py`)
5. Run post-import setup SQL
6. Flush Redis
7. Run integration tests
8. Cleanup (kill all services)

---

## 5. Service Port Reference

| Service | Port | Type |
|---------|------|------|
| rider-app | 8013 | Haskell |
| driver-app | 8016 | Haskell |
| beckn-gateway | 8015 | Haskell |
| provider-dashboard | 8018 | Haskell |
| mock-google | 8019 | Haskell |
| mock-registry | 8020 | Haskell |
| mock-sms | 4343 | Haskell |
| mock-fcm | 4545 | Haskell |
| mock-idfy | 6235 | Haskell |
| mock-server (unified) | 8080 | Python |
| location-tracking-service | 8081 | External |
| PostgreSQL | 5434 | Infrastructure |
| Redis | 6379 | Infrastructure |
| Redis Cluster | 30001-30006 | Infrastructure |
| Kafka | 29092 | Infrastructure |
| OSRM | 5001 | Infrastructure |
| Passetto | 8079 | Infrastructure |
| Test Dashboard | 7070 | Test tool |
| Context API | 7082 | Test tool |
