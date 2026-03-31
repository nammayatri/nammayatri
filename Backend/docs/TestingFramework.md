# NammaYatri Testing Framework

Complete architecture of the local testing infrastructure covering config sync, integration tests, mock servers, and test tools.

## Table of Contents

- [Overview](#overview)
- [Config Sync](#1-config-sync)
- [Integration Tests](#2-integration-tests)
- [Mock Servers](#3-mock-servers)
- [Test Tools](#4-test-tools)
- [Running Everything](#5-running-everything)
- [Extending the Framework](#6-extending-the-framework)
- [Port Reference](#7-port-reference)

---

## Overview

```
Backend/dev/
├── config-sync/          # Export config from master DB → patch → import to local
├── integration-tests/    # Newman/Postman collections for E2E testing
├── mock-servers/         # Unified Python mock for all external services
└── test-tool/            # Dashboard UI + context API for manual testing
```

The testing framework enables fully local end-to-end testing of the NammaYatri platform by:
1. **Syncing real production config** (fare policies, merchant settings, service configs) from master to local with URL/credential patching
2. **Mocking all external services** (payment gateways, metro APIs, SMS providers, etc.) with a single Python server that supports response overrides
3. **Running automated test suites** via Newman that cover ride booking, ticket booking, payments, cancellations, refunds, and failure scenarios
4. **Providing a dashboard UI** for manual testing and debugging

---

## 1. Config Sync

**Location:** `Backend/dev/config-sync/`

Syncs ~150 configuration tables across 7 schemas from a master (staging/prod) database to local dev, applying URL replacements and credential re-encryption.

### Architecture

```
                    ┌─────────────┐
                    │  Master DB  │  (staging RDS)
                    └──────┬──────┘
                           │ export (SELECT → JSON)
                           ▼
                    ┌─────────────┐
                    │  JSON Files │  assets/data/master/
                    └──────┬──────┘
                           │ patch (global_replacements + dimension_overrides)
                           ▼
                    ┌─────────────┐
                    │ Patched JSON│  assets/data/master_to_local/
                    └──────┬──────┘
                           │ import (INSERT/UPSERT + ENCRYPT: via passetto)
                           ▼
                    ┌─────────────┐
                    │  Local DB   │  localhost:5434/atlas_dev
                    └─────────────┘
```

### DEV vs S3 Mode

| | DEV=true (Local) | DEV=false (CI/S3) |
|---|---|---|
| **Config source** | `assets/environments.json` | Environment variables |
| **Data storage** | `assets/data/` (filesystem) | S3 bucket |
| **When to use** | Local development | CI/CD pipelines |

### Commands

```bash
cd Backend/dev/config-sync

# Export from master → local JSON files
python config_transfer.py export --from master --to local

# Patch exported files (URL replacements, overrides)
python config_transfer.py patch --from master --to local

# Import patched files to local DB
python config_transfer.py import --from master --to local

# Full pipeline: export + patch + import
python config_transfer.py sync --from master --to local

# Discover config tables not yet in config.json
python config_transfer.py discover

# Validate config tables exist in DB
python config_transfer.py validate --schema atlas_app
```

### Key Files

| File | Purpose | Git? |
|------|---------|------|
| `config.json` | Config table registry per schema (~150 tables) | ✅ |
| `environments.json` | DB connections (master host, local host) | ❌ |
| `environments.json.example` | Template with placeholder hosts | ✅ |
| `patches.json` | URL replacements, dimension overrides, ENCRYPT: rules | ❌ |
| `patches.json.example` | Template with example patches | ✅ |
| `../feature-migrations/` | Ordered SQL files run after import (e.g., dashboard access) | ✅ |
| `assets/data/` | Exported/patched JSON data | ❌ |

### Patching System

The `patches.json` file controls three types of transformations:

#### 1. Global Replacements

Simple string find/replace on raw JSON content. Applied during the **patch** step:

```json
{
  "global_replacements": [
    {"find": "https://api.juspay.in", "replace": "http://localhost:8080/juspay"},
    {"find": "https://maps.googleapis.com/maps/api/", "replace": "http://localhost:8019/"},
    {"find": "https://quickticketapi.chennaimetrorail.org/api", "replace": "http://localhost:8080/cmrl"},
    {"find": "/beckn/beckn/", "replace": "/beckn/"}
  ]
}
```

#### 2. Dimension Overrides

Per-table, per-row field overrides. Applied during the **import** step. Supports:
- `set`: direct column value replacement
- `merge_json`: deep-merge into a JSON column
- `where`: match condition (empty = all rows)
- `eval:{field}`: dynamic value from another column

```json
{
  "dimension_overrides": {
    "atlas_app": {
      "merchant": [{
        "where": {},
        "set": {
          "signing_public_key": "1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws=",
          "gateway_and_registry_priority_list": "{NY,ONDC}"
        }
      }],
      "beckn_config": [{
        "where": {},
        "set": {"unique_key_id": "eval:{merchant_id}"}
      }],
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
}
```

#### 3. ENCRYPT: Prefix

Values starting with `ENCRYPT:` are encrypted via local passetto during import:

```
ENCRYPT:S"mock-password"
         └─ passetto expects S"..." format
```

This ensures master-encrypted credentials (which use master's passetto key) are re-encrypted with the local key. Every table with encrypted fields needs corresponding `ENCRYPT:` overrides in patches.json.

**Tables with encrypted fields:**
- `merchant_service_config` — apiKey, password in config_json (~100 service types)
- `integrated_bpp_config` — CMRL password, CRIS keys, EBIX password in config_json.contents
- `subscription_config` — apiKey, password in ext_webhook_configs
- `place_based_service_config` — apiKey, password in config_value
- `safety_dashboard.merchant` — auth_token_encrypted (direct column)
- `bbps_config` — bbps_signature_key (direct column)
- `partner_organization` — api_key_encrypted (direct column)

---

## 2. Integration Tests

**Location:** `Backend/dev/integration-tests/`

Automated E2E test suites using Newman (Postman CLI). Each suite is a JSON collection with pre-request scripts, test assertions, and environment variables.

### Directory Structure

```
collections/
├── RideBookingFlow/              # Cash rides: auto, cab, cancellation, reallocation
│   ├── 01-AutoRideFlow.json      # Full auto ride: onboard → search → assign → start → end
│   ├── 02-AutoUserCancellation   # Rider cancels after assignment
│   ├── 03-AutoDriverCancellation # Driver cancels, D2 gets reallocation
│   ├── 04-CabRideFlow.json       # SEDAN/TAXI full ride
│   ├── 05-CabUserCancellation    # Cab rider cancellation
│   ├── 06-CabDriverCancellation  # Cab driver cancel + reallocation
│   └── Local_NY_Bangalore.postman_environment.json (+ Kolkata, Chennai, Delhi)
│
├── OnlineRideBookingFlow/        # Stripe payment rides
│   ├── 01-StripeRideFlow.json    # Full ride with card payment setup
│   └── Local_BF_Helsinki.postman_environment.json
│
├── BusTicketBookingFlow/         # FRFS bus (Direct mode)
│   ├── 01-DirectBusBooking.json  # Search → quote → confirm → pay → verify ticket
│   ├── 02-AutoRefunded.json      # Payment auto-refund flow
│   └── Local_FRFS_Chennai.postman_environment.json (+ Bhubaneshwar)
│
├── MetroTicketBookingFlow/       # FRFS metro (CMRL/ONDC)
│   ├── 01-DirectMetroBooking.json
│   ├── 02-TechnicalFailure.json  # Confirm fails → refund flow
│   ├── 03-AutoRefunded.json
│   └── Local_FRFS_Chennai.postman_environment.json (+ Bangalore)
│
└── SubwayTicketBookingFlow/      # FRFS subway (CRIS)
    ├── 01-DirectSubwayBooking.json
    ├── 02-TechnicalFailure.json
    ├── 03-AutoRefunded.json
    └── Local_FRFS_Chennai.postman_environment.json
```

### Running Tests

```bash
cd Backend/dev/integration-tests

./run-tests.sh                           # All ride suites for all cities
./run-tests.sh rides NY_Bangalore        # Rides for Bangalore only
./run-tests.sh rides NY_Bangalore 01-AutoRideFlow  # Specific suite
./run-tests.sh online                    # Stripe payment rides
./run-tests.sh bus                       # All bus ticket suites
./run-tests.sh bus FRFS_Chennai          # Bus for Chennai only
./run-tests.sh metro                     # All metro suites
./run-tests.sh subway                    # All subway suites
./run-tests.sh --list                    # List all available suites
./run-tests.sh --check                   # Check for stuck entities in DB
```

### Collection Design Rules

See `Rules.md` for full guidelines.

**Concurrency & Idempotency:**
- All dynamic identifiers (mobile numbers, vehicle registrations) are **random per run** — generated in collection prerequest
- Prefix generated variables with `_test_` to distinguish from environment config
- Never hardcode phone numbers or per-entity identifiers

**Environment vs Collection Variables:**
- **Environment files** (hardcoded per city): URLs, merchant IDs, coordinates, vehicle config, login OTP
- **Collection variables** (generated per run): phone numbers, reg numbers, auth tokens, booking/search IDs

**Dashboard Access:**
- One admin token (`local-admin-token-bangalore-namma-yatri`) stored in all environments
- Collections call `POST {{dashboard_base_url}}/user/switchMerchantAndCity` to get a city-specific token
- The returned `authToken` replaces `dashboard_token` for subsequent dashboard API calls

**Mock Server Integration:**
- `POST /mock/status` — set payment status by order ID (extracted from booking status response)
- `POST /mock/override` — break external APIs by request field matching (e.g., `body.mob`)
- `DELETE /mock/override` — always clean up specific overrides after use (concurrent safety)

### Typical Ride Booking Flow

```
 1. Driver Auth           POST /auth
 2. Driver OTP            POST /auth/{authId}/verify
 3. Switch City           POST /user/switchMerchantAndCity
 4. Add Vehicle           POST /{merchant}/{city}/driver/{id}/addVehicle
 5. Enable Driver         POST /{merchant}/{city}/driver/{id}/enable
 6. Driver Location       POST /driver/location (LTS)
 7. Driver Online         POST /driver/setActivity?active=true
 8. Rider Auth            POST /auth
 9. Rider OTP             POST /auth/{authId}/verify
10. Driver Location       POST /driver/location (refresh)
11. Ride Search           POST /rideSearch
12. Get Estimates         GET  /rideSearch/{searchId}/results
13. Select Estimate       POST /estimate/{estimateId}/select2
14. Nearby Requests       GET  /driver/nearbyRideRequest
15. Driver Accept         POST /driver/searchRequest/quote/respond
16. Get Booking           GET  /rideBooking/list
17. Get Ride              GET  /driver/ride/list
18. Start Ride            POST /driver/ride/{rideId}/start
19. End Ride              POST /driver/ride/{rideId}/end
```

### Typical FRFS Ticket Flow

```
 1. Auth + Verify
 2. Origin Serviceability    POST /serviceability/origin
 3. FRFS Config              GET  /frfs/config
 4. Autocomplete             GET  /frfs/autocomplete (select route)
 5. Get Stations             GET  /frfs/stations
 6. Search                   POST /frfs/search
 7. Get Quotes               GET  /frfs/search/{searchId}/quote
 8. Confirm Quote V2         POST /frfs/quote/v2/{quoteId}/confirm
 9. Get Payment Info         GET  /frfs/booking/{bookingId}/status
10. Update Mock → CHARGED    POST /mock/status
11. Booking Status (×2)      GET  /frfs/booking/{bookingId}/status
12. Get Booking List         GET  /frfs/booking/list
13. Verify Ticket            POST /frfs/ticket/verify
14. Verify Ticket Status     GET  /frfs/booking/{bookingId}/status (ticket USED)
```

### TechnicalFailure Flow (Metro/Subway)

Tests external BPP API failure → refund:

```
 1-8.  Normal flow up to Get Quotes
 9.    POST /mock/override → break CMRL/CRIS confirm (corrupt field type)
10.    Update Mock → CHARGED
11.    Booking Status → FAILED + REFUND_PENDING (5-10s wait for async confirm failure + refund)
12.    DELETE /mock/override → clean up
13.    Update Mock → REFUND_SUCCESS (with refunds block)
14.    Booking Status → FAILED + REFUNDED
```

The override uses `body.mob` (rider mobile) as the common identifier. For CRIS (encrypted requests), the mock's registered body decoder decrypts the AES-ECB payload to extract `mob` before matching.

---

## 3. Mock Servers

**Location:** `Backend/dev/mock-servers/`

Single Python HTTP server (port 8080) that mocks all external service integrations. Each service has its own module under `services/`.

### Architecture

```
                       ┌──────────────────────────────┐
                       │         server.py             │
                       │  ┌────────────────────────┐   │
   Incoming Request ──▶│  │   Middleware:           │   │
                       │  │   1. Parse body         │   │
                       │  │   2. check_overrides()  │   │──▶ status_store.py
                       │  │   3. Set _request_override│  │    (in-memory store +
                       │  └────────────────────────┘   │     body decoders)
                       │           │                   │
                       │           ▼                   │
                       │  ┌────────────────────────┐   │
                       │  │  Route to handler:      │   │
                       │  │  /juspay → juspay.py   │   │
                       │  │  /stripe → stripe.py   │   │
                       │  │  /cmrl   → cmrl.py     │   │
                       │  │  /cris   → cris.py     │   │
                       │  │  ...21 services        │   │
                       │  └────────────────────────┘   │
                       │           │                   │
                       │           ▼                   │
                       │  handler._get_override()      │
                       │  → merges: status_store +     │
                       │    _request_override           │
                       │  → deep_merge(base, extra)    │
                       │  → handler._json(response)    │
                       └──────────────────────────────┘
```

### Services

| Service | Route | Features |
|---------|-------|----------|
| **Juspay** | `/juspay` | Orders, offers, refunds. Auto-stores refund on POST /orders/{id}/refunds |
| **Stripe** | `/stripe` | Full v1 API (21 endpoints), stateful, form-urlencoded, test cards |
| **CMRL** | `/cmrl` | V1+V2 auth, fare, tickets. AES-256-CBC encrypted responses |
| **CRIS** | `/cris` | Auth, route fare, book journey. AES-256-ECB encrypted, agent key derivation |
| **EBIX** | `/ebix` | Auth, save ticket, create QR |
| **PayTM** | `/paytm` | Payment status |
| **Exotel** | `/exotel` | Call status |
| **Acko** | `/acko` | Insurance policy |
| **SOS** | `/sos`, `/erss` | Incident reports |
| **Kapture** | `/kapture` | Ticket support |
| **WhatsApp** | `/gupshup`, `/karix` | Message delivery (OptApiResp format) |
| **MMI** | `/mmi` | MapMyIndia token + geocoding |
| **NextBillion** | `/nextbillion` | Routes |
| **Gridline** | `/gridline` | Aadhaar verification |
| **HyperVerge** | `/hyperverge` | Document verification |
| **Gullak** | `/gullak` | Tokenization |
| **Transit** | `/nandi`, `/gtfs` | Public transport data |
| **OpenAI** | `/openai` | LLM mock |

### Override System

#### Mechanism 1: Status Store (`POST /mock/status`)

For overriding by **known identifier** (e.g., Juspay order ID extracted from booking status response):

```
Collection extracts payment_order_id from GET /frfs/booking/{id}/status
    → POST /mock/status {"service":"juspay", "id":["order-uuid","short-id"], "status":"CHARGED"}
        → Next GET /juspay/orders/{short-id} returns status=CHARGED
```

```json
POST /mock/status
{"service": "juspay", "id": ["order-uuid", "short-id"], "status": "CHARGED", "data": {"amount": 150}}

GET /mock/status
→ lists all active statuses + overrides

DELETE /mock/status
→ clears all (not concurrent-safe, prefer specific override delete)
```

#### Mechanism 2: Override Rules (`POST /mock/override`)

For overriding by **request field matching** — when the test knows the rider's mobile but not the exact API request IDs:

```
Collection knows rider mobile = 7461059507
    → POST /mock/override {"service":"cris", "extract":"body.mob", "value":"7461059507", "response":{...}}
        → When CRIS bookJrny receives mob=7461059507, response is merged with override
```

```json
POST /mock/override
{"service": "cris", "extract": "body.mob", "value": "7461059507",
 "response": {"agentTicketData": 12345}}

DELETE /mock/override
{"service": "cris", "extract": "body.mob", "value": "7461059507"}
```

**Extract syntax:**

| Syntax | Source | Example |
|--------|--------|---------|
| `body.<json_path>` | Request body (supports nested) | `body.mob`, `body.data.customer.phone` |
| `path.<index>` | URL path segment (0-based) | `path.2` → 3rd segment |
| `query.<param>` | Query parameter | `query.vehicleType` |
| `header.<name>` | Request header | `header.authorization` |

**Encrypted body support:**

CRIS and CMRL send AES-encrypted request bodies. The mock registers **body decoders** that decrypt before the middleware matches:

```python
# In services/cris.py — registered at module load
def _decode_cris_body(body_raw):
    """Decrypt CRIS AES-ECB encrypted {app, data_} → inner JSON with mob field"""
    req = json.loads(body_raw)
    encrypted = req.get("data_")
    if encrypted:
        decrypted = _aes_ecb_decrypt(encrypted, _MOCK_ENCRYPT_KEY)
        return json.loads(decrypted)  # Now body.mob is accessible
    return req

register_body_decoder("cris", _decode_cris_body)
```

The middleware flow:
1. Server receives `POST /cris/t/uts.cris.in/VBCU/1/bookJrny`
2. Middleware tries plain JSON parse → gets `{app, data_}` (outer envelope)
3. Body decoder registered for "cris" → decrypts `data_` → gets `{mob, source, destination, ...}`
4. Override rule matches `body.mob == "7461059507"` → merges response
5. CRIS handler sees override in `handler._get_override()` → returns corrupted response

### Handler Pattern

Every service handler follows the same structure:

```python
from status_store import extract_path_ids, deep_merge

def handle(handler, path, body):
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("service_name", *path_ids)
    # override_status: from status store (e.g., "CHARGED")
    # extra: merged dict from status store data + middleware request override

    base = {
        "default": "response",
        "status": override_status or "NEW",
    }
    if extra:
        base = deep_merge(base, extra)  # Override wins for scalars, dicts merge recursively
    handler._json(base)
```

### Encryption in Mocks

| Service | Algorithm | Key Source | Mock Key |
|---------|-----------|-----------|----------|
| CMRL V2 | AES-256-CBC, zero IV | Auth response `key` field | `MockAES256Key123456789012345ABCD` (32 bytes) |
| CRIS | AES-256-ECB | DB `encryptionKey`/`decryptionKey` | `CRISEncryptKey...` / `CRISDecryptKey...` (32 bytes each) |
| CRIS agent | AES-256-ECB | `last5(mob) + agentKey(22) + first5(mob)` | `CRISAgentKey1234567890` (22 bytes → 32 derived) |

Mock uses `openssl` CLI for encryption (no Python crypto library dependency).

---

## 4. Test Tools

### Dashboard (`dev/test-tool/dashboard/`)

React web UI for manually running test flows.

```bash
# Build (one-time)
cd Backend/dev/test-tool/dashboard
npm install && npm run build

# Start
cd Backend/dev/test-tool && ./start.sh
```

| Component | Port | Purpose |
|-----------|------|---------|
| Dashboard | 7070 | React UI for test flows |
| Context API | 7082 | DB queries + request proxy |
| Mock Stripe | 8080/stripe | Via unified mock server |

The dashboard defines test scenarios in `src/flows/stripePayment.ts`:
- Happy Path (no auth card) — full ride with Stripe capture
- Ride + Tip — complete ride then add tip
- Cancellation + Fee — void payment intent
- Refund — refund after completed ride
- Dues + Debt Settlement — failed payment → clearDues
- Card Declined — mock returns card_declined
- 3DS Auth — requires_action flow

### Context API (`dev/test-tool/context-api/`)

Python server providing DB access for the dashboard:
- Queries test entities (drivers, riders, bookings)
- Proxies requests to rider-app/driver-app via `/proxy/rider/`, `/proxy/driver/`

---

## 5. Running Everything

### Manual (local development)

```bash
# Terminal 1: Start all services
, run-mobility-stack-dev

# Terminal 2: Import config (first time or after config changes)
cd Backend/dev/config-sync
python config_transfer.py import --from master --to local
for f in $(ls ../../feature-migrations/*.sql 2>/dev/null | sort); do psql -h localhost -p 5434 -U atlas_superuser -d atlas_dev -f "$f"; done
redis-cli FLUSHALL

# Terminal 3: Run tests
cd Backend/dev/integration-tests
./run-tests.sh rides NY_Bangalore
```

### Automated (single command)

```bash
, run-mobility-stack-test                    # All tests
, run-mobility-stack-test rides              # Only ride flows
, run-mobility-stack-test bus FRFS_Chennai   # Specific city
```

This command:
1. Waits for ports to be free (up to 5 min, checks every 30s)
2. Starts `run-mobility-stack-dev` in background
3. Waits for rider-app and driver-app health (up to 10 min)
4. Runs `config_transfer.py import`
5. Runs `dev/feature-migrations/*.sql` (sorted)
6. Flushes Redis
7. Runs `./run-tests.sh` with passthrough args
8. Kills all services on exit

---

## 6. Extending the Framework

### Adding a New City

1. Create `Local_<PREFIX>_<City>.postman_environment.json` with:
   - `city`, `state`, `origin_lat/lon`, `dest_lat/lon`
   - `driver_merchant_id`, `bap_merchant_id`, `dashboard_merchant_id`
   - `vehicle_variant`, `vehicle_class`, `vehicle_category` (for ride flows)
   - `vehicle_type`, `platform_type` (for FRFS flows)
2. Add city to `dev/feature-migrations/0001-dashboard-access-setup.sql` merchant_access loop
3. No collection changes needed

### Adding a New Test Flow

1. Create `<NN>-<FlowName>.json` in the appropriate directory
2. Add collection prerequest for random number generation
3. Follow step naming: `Verb + Object (Context)` e.g., `Get Booking Status (Trigger Confirm)`
4. Use mock status/override APIs to control external service behavior
5. Always clean up overrides after use

### Adding a New Mock Service

1. Create `Backend/dev/mock-servers/services/<name>.py`
2. Implement `handle(handler, path, body)` using the standard pattern
3. Add route to `ROUTES` in `server.py`
4. Match response types to Haskell `FromJSON` expectations
5. If encrypted requests: register a body decoder via `register_body_decoder()`

### Adding a New Config Table

1. Add to `config.json` under the correct schema
2. If it has encrypted fields: add `ENCRYPT:S"mock-value"` overrides in patches.json
3. If it has external URLs: verify coverage by global_replacements
4. Run `python config_transfer.py discover` to validate

---

## 7. Port Reference

| Service | Port | Type |
|---------|------|------|
| rider-app | 8013 | Haskell |
| beckn-gateway | 8015 | Haskell |
| driver-app | 8016 | Haskell |
| rider-dashboard | 8017 | Haskell |
| provider-dashboard | 8018 | Haskell |
| mock-google | 8019 | Haskell |
| mock-registry | 8020 | Haskell |
| unified-dashboard | 8021 | Haskell |
| mock-server (unified) | 8080 | Python |
| location-tracking-service | 8081 | External |
| mock-sms | 4343 | Haskell |
| mock-fcm | 4545 | Haskell |
| mock-idfy | 6235 | Haskell |
| PostgreSQL primary | 5434 | Infrastructure |
| PostgreSQL replica | 5435 | Infrastructure |
| Redis standalone | 6379 | Infrastructure |
| Redis cluster | 30001-30006 | Infrastructure |
| Kafka | 29092 | Infrastructure |
| Zookeeper | 2181 | Infrastructure |
| OSRM | 5001 | Infrastructure |
| Passetto | 8079 | Infrastructure |
| Nginx | 8085 | Infrastructure |
| Test Dashboard | 7070 | Test tool |
| Context API | 7082 | Test tool |
