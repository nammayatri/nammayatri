# Mock Servers

Unified mock server for NammaYatri merchant service providers.

## Architecture

| Server | Port | Language | Providers |
|--------|------|----------|-----------|
| mock-google | 8019 | Haskell | Maps, Places, Directions, Roads |
| mock-fcm | 4545 | Haskell | Firebase Cloud Messaging |
| mock-sms | 4343 | Haskell | GupShup, Exotel, Karix, MyValueFirst, Pingbix, Twilio |
| mock-idfy | 6235 | Haskell | Document verification (Idfy, HyperVerge) |
| mock-registry | 8020 | Haskell | Beckn registry |
| **mock-server** | **8080** | **Python** | **Everything else** (Juspay, PayTM, Acko, SOS, WhatsApp, etc.) |

## Usage

```bash
# Started automatically by , run-mobility-stack-dev
# Or manually:
python dev/mock-servers/server.py --port 8080
```

## Endpoints (mock-server :8080)

| Path | Service |
|------|---------|
| `/juspay/*` | Juspay Payment/Payout |
| `/paytm/*` | PayTM EDC/Notifications |
| `/acko/*` | Acko Insurance |
| `/erss/*`, `/sos/*` | SOS/ERSS/Trinity |
| `/kapture/*` | Kapture Ticketing |
| `/mmi/*` | MapMyIndia/Mappls |
| `/nextbillion/*` | NextBillion |
| `/gridline/*` | Gridline (Aadhaar) |
| `/nandi/*`, `/gtfs/*` | OTP Transit |
| `/hyperverge/*` | HyperVerge verification |
| `/gullak/*` | Gullak tokenization |
| `/openai/*` | Azure OpenAI LLM |
| `/hdfc/*` | HDFC CBX bulk payouts (see below) |
| `/health` | Health check |
| `POST /mock/s3/put-text` | Write a text object into `Backend/s3/local/test-bucket/<key>` for the local mock bucket |

## HDFC CBX bulk payouts (`/hdfc/*`)

Stateful mock of the bank's token, bulk payment, bulk inquiry and getBatchNo APIs, with the JWS-in-JWE envelope. The real shared-kernel adapter runs against it unchanged: only the URLs in `merchant_service_config` differ. It speaks plain HTTP with no mTLS. The app still builds its mTLS manager from the kit's dummy certificate, but no handshake happens. The services module docstring (`services/hdfc_cbx.py`) is the full reference.

```bash
python3 dev/mock-servers/hdfc-cbx/seed_config.py            # MSIL cities -> mock (old rows backed up OUTSIDE the repo)
python3 dev/mock-servers/hdfc-cbx/seed_config.py --restore  # back to whatever was there (e.g. UAT)
python3 dev/mock-servers/hdfc-cbx/client.py selftest        # drive every API/scenario like the adapter does
python3 dev/mock-servers/hdfc-cbx/make_keys.py --force      # regenerate the TEST-ONLY kit (hdfc-cbx/test-keys); then re-seed and restart the mock
```

`seed_config.py` points each city's `Payout_HdfcCbx` config at the mock, routes its payouts to HdfcCbx (`merchant_service_usage_config`, and the PREPAID_SUBSCRIPTION payout service in `subscription_config`), keeps `payout_order` off KV, and clears the cached config keys. From then on every payout triggered in the city (instant, adhoc or the scheduled sweep) goes to the mock. It only changes config: no drivers or other data are added. Run it again after a config-sync, which replaces the city's config with master's.

After seeding, restart the driver app and the allocator: they read the config and `kv_configs`, and build their HTTP managers, at startup. The scripts need `cryptography`, which the mock-server interpreter in `nix/services/nammayatri.nix` provides.

- **Inquiry, per batch, as HDFC answers it.**
  - The first hit of a check gets `202` "We have accepted your request. Please enquire again after sometime" and the batch is marked processing.
  - The next hit, once `readyAfterSeconds` have passed (default 5; set 15 for the real spacing), gets the actual rows, with each item advanced one step. The next check then starts over.
  - A hit before the answer is ready is told to enquire again.
- **Ladders.**
  - NEFT/RTGS/IMPS: `P → C → E → E/TXSIP → E/TXSETT`, with a UTR (an RRN for IMPS) in `refno`.
  - Intra-bank: `P → C → E`, with an FT number in `bankrefno`. An item is intra-bank when its cdflag is `I` or its IFSC starts with `HDFC`.
- **Rejected at once:** names over 40 characters, a malformed IFSC, and cdflag `I` with a non-HDFC IFSC.
- **Account-number suffixes:**

| Suffix | Outcome |
|---|---|
| `9001` | rejected |
| `9002` | returned after debit (TXREJE) |
| `9003` | deemed settled (TXDSETT) |
| `9004` | stuck at C |
| `9005` | stuck at P |
| `9006` | unknown codstatus |
| `9007` | unknown rbistatus |

- **Control (plain JSON):**
  - `GET /hdfc/mock/state`
  - `POST /hdfc/mock/reset`
  - `POST /hdfc/mock/config`, e.g. `{"autoApprove": false, "stepsPerInquiry": 5}`
  - `POST /hdfc/mock/batch/<batchnum>/approve`
  - `POST /hdfc/mock/item/<custrefno>` with a row to force
  - `POST /hdfc/mock/fault` for one-shot faults, e.g. `{"api":"payment","kind":"5xx","process":true}` (a file that was accepted but answered 500), `timeout`, `duplicate`, `accept-no-batchnum`, `notready`, `ndf`, `norecord`.
- **State** persists to `.state/hdfc_cbx.json`, which is gitignored.
- **No real credentials anywhere in the mock.**
  - The kit's keys and certificates are TEST-ONLY. They're generated into `hdfc-cbx/test-keys/` the first time `seed_config.py` or the mock needs them, and that folder is gitignored, so each machine has its own.
  - The seeded config uses made-up identifiers (`MOCKSCOPE` / `MOCKGROUP` / `9999` / `MOCKUSER`), and the mock accepts any key, secret or api key.
  - `seed_config.py` writes the real config it replaces to `~/.hdfc-cbx-mock/config-backup/` (override with `HDFC_MOCK_BACKUP_DIR`), never inside the repo; `--restore` reads it back from there.

