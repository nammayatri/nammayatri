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
| `/health` | Health check |
