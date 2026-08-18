# Local Devkit Architecture

How the six containers of the mobility devkit fit together, addressed at the level of "what does each port do, who calls whom, and where does the signing/registry work happen."

Companion to [`README.md`](./README.md) (runbook) — this doc is the map.

---

## Two layers

The stack is cleanly split in two:

- **Protocol layer (ONIX adapters)** — signs outgoing messages, verifies incoming signatures against DeDi, validates schemas, routes. Owns Beckn semantics. Contains no business logic.
- **Application layer (sandbox mocks)** — stand-in for the rider-facing app (BAP side) and the ride-hailing platform (BPP side). Where product code would live in a real deployment.

The application layer is dumb on purpose — `sandbox-bap` just logs whatever it receives, `sandbox-bpp` just replays canned fixtures from disk.

---

## Container inventory

Six containers, all attached to a single Docker bridge network `beckn_network`:

| Container | Image | Role | Host port |
|---|---|---|---|
| `beckn-router` | `caddy:alpine` | Reverse proxy — single public entry point | 9000 |
| `redis` | `redis:alpine` | Shared cache for DeDi lookups + payload store | 6379 |
| `onix-bap` | `fidedocker/onix-adapter` | ONIX Beckn adapter, BAP role | 8081 |
| `onix-bpp` | `fidedocker/onix-adapter` | ONIX Beckn adapter, BPP role | 8082 |
| `sandbox-bap` | `fidedocker/sandbox-2.0:latest` | Mock rider app: logs `on_*` payloads | 3001 |
| `sandbox-bpp` | `fidedocker/sandbox-2.0:latest` | Mock provider app: returns canned `on_*` fixtures | 3002 |

Defined in [`install/docker-compose-mobility.yml`](./install/docker-compose-mobility.yml).

---

## ONIX module structure

Each ONIX adapter (BAP and BPP) is a single Go process hosting **two HTTP modules**:

| Module | Path | Direction | Purpose |
|---|---|---|---|
| `receiver` | `/{role}/receiver/` | Inbound | Accepts signed messages from peers, verifies signature via DeDi, forwards to the application layer |
| `caller` | `/{role}/caller/` | Outbound | Accepts unsigned messages from the application layer, signs them, forwards to the peer |

Only the receiver is a public identity — its URL is what gets registered as the DeDi subscriber's `url`. The caller is a purely internal handoff surface (business logic calls it to originate outbound traffic).

Concretely:

- `onix-bap` — `/bap/receiver/` (from BPPs) and `/bap/caller/` (from your rider app)
- `onix-bpp` — `/bpp/receiver/` (from BAPs) and `/bpp/caller/` (from your provider platform)

Adapter configs: [`config/mobility-bap.yaml`](./config/mobility-bap.yaml) and [`config/mobility-bpp.yaml`](./config/mobility-bpp.yaml).

---

## Routing

Four routing YAMLs — one per module — map action names to destination URLs:

| File | Action set | Destination |
|---|---|---|
| [`mobility-routing-BAPCaller.yaml`](./config/mobility-routing-BAPCaller.yaml) | `discover`, `select`, `init`, `confirm`, ... | `http://onix-bpp:8082/bpp/receiver` |
| [`mobility-routing-BAPReceiver.yaml`](./config/mobility-routing-BAPReceiver.yaml) | `on_discover`, `on_select`, ... | `http://sandbox-bap:3001/api/bap-webhook` |
| [`mobility-routing-BPPCaller.yaml`](./config/mobility-routing-BPPCaller.yaml) | `on_discover`, `on_select`, ... | `http://onix-bap:8081/bap/receiver` |
| [`mobility-routing-BPPReceiver.yaml`](./config/mobility-routing-BPPReceiver.yaml) | `discover`, `select`, ... | `http://sandbox-bpp:3002/api/webhook` |

All entries use `targetType: url` (hardcoded container URLs) instead of `targetType: bap`/`bpp` (registry-resolved). See [Why fixed URLs](./README.md#why-fixed-urls-instead-of-registry-resolved-routing) in the README for the reasoning.

---

## Docker networking model

Two networks are in play, and they use different address forms — mixing them up is the most common source of confusion:

- **Host network (your mac)** — reaches container ports via `localhost:<published-port>`. Postman and curl on the mac use this.
- **`beckn_network` bridge (inside Docker)** — containers reach each other by service name (`onix-bap`, `sandbox-bpp`, `redis`, ...). Docker's embedded DNS resolves those; the host does not.

Same container, two names:

| Speaker | How to reach onix-bpp |
|---|---|
| Postman on the mac | `http://localhost:8082` |
| Any container | `http://onix-bpp:8082` |

`localhost` inside a container refers to the container itself, so container-to-container traffic never uses it. That's why routing configs use service names and Postman collection variables use `localhost`.

The `ports: - "8082:8082"` line in the compose file is what publishes 8082 from the container to the host, enabling the `localhost:8082` form.

---

## The Caddy router

`beckn-router:9000` reverse-proxies (per [`install/Caddyfile`](./install/Caddyfile)):

- `/bap/*` → `onix-bap:8081`
- `/bpp/*` → `onix-bpp:8082`

It exists for two reasons:

1. **Single public URL for ngrok tunnelling** — tunnel one port (9000) instead of two.
2. **Stable `bapUri`/`bppUri` in payloads** — the Postman collection sends `http://beckn-router:9000/bap/receiver` as the callback URI. That's a container-network address, so any container on `beckn_network` can resolve and hit it.

For local-only testing where callback URIs don't need to resolve, hitting `localhost:8081`/`localhost:8082` skips the Caddy hop with no functional change.

---

## Signing and DeDi verification

Every message that crosses between adapters is signed. Every receiver verifies the sender's signature via a live DeDi registry lookup:

```
https://fabric.nfh.global/registry/dedi/lookup/<subscriberId>/subscribers.<network>/<keyId>
```

The DeDi response includes the subscriber's public keys and the list of networks it's a member of. If the sender's `context.networkId` isn't in that list, verification fails with `degraded trust` and the request NACKs.

Redis (`redis:6379`) caches these lookups for both adapters to avoid a round trip per message.

**Devkit gotcha**: the shipped sandbox subscribers `bap.example.com` / `bpp.example.com` are DeDi-registered under **`beckn.one/testnet`**, not this repo's own `nfh.global/testnet-mobility`. Override the Postman `networkId` collection variable to `beckn.one/testnet` when driving the flow, or the BPP receiver will reject the BAP's signature.

---

## End-to-end message flow

Full round trip for `select` — the same pattern applies to `init`, `confirm`, `status`, `track`, `update`, `cancel`, `rate`, `support`:

```
[Rider app / Postman]
  (mac, localhost)
        │
        │  POST http://localhost:8081/bap/caller/select
        ▼
┌──────────────────────────────────────────────────────────────────────────┐
│ onix-bap :8081  /bap/caller/select                                       │
│   1. reqpreprocessor middleware extracts txn/msg IDs                     │
│   2. addRoute        (BAPCaller.yaml → onix-bpp:8082/bpp/receiver)       │
│   3. storePayload    (Redis)                                             │
│   4. validateSchema  (v2.0.0 core + extended mobility contexts)          │
│   5. sign            (BAP private key)                                   │
│   6. POST outbound; validateAckSign on the sync response                 │
└──────────────────────────────────────────────────────────────────────────┘
        │
        │  signed POST http://onix-bpp:8082/bpp/receiver/select
        ▼
┌──────────────────────────────────────────────────────────────────────────┐
│ onix-bpp :8082  /bpp/receiver/select                                     │
│   1. validateSign    (DeDi lookup bap.example.com → verify)              │
│   2. addRoute        (BPPReceiver.yaml → sandbox-bpp:3002/api/webhook)   │
│   3. validateSchema                                                      │
│   4. signAck         (ACK signed with BPP key, returned to onix-bap)     │
│   5. forward request to application layer                                │
└──────────────────────────────────────────────────────────────────────────┘
        │
        │  POST http://sandbox-bpp:3002/api/webhook
        ▼
┌──────────────────────────────────────────────────────────────────────────┐
│ sandbox-bpp :3002   (mock provider app)                                  │
│   • Read fixture from mounted volume:                                    │
│       sandbox-payloads/nfh.global/testnet-mobility/response/on_select.json │
│   • POST it back as an async on_select to onix-bpp's caller              │
└──────────────────────────────────────────────────────────────────────────┘
        │
        │  POST http://onix-bpp:8082/bpp/caller/on_select
        ▼
┌──────────────────────────────────────────────────────────────────────────┐
│ onix-bpp :8082  /bpp/caller/on_select                                    │
│   1. addRoute        (BPPCaller.yaml → onix-bap:8081/bap/receiver)       │
│   2. storePayload                                                        │
│   3. validateSchema                                                      │
│   4. sign            (BPP private key)                                   │
│   5. POST outbound                                                       │
└──────────────────────────────────────────────────────────────────────────┘
        │
        │  signed POST http://onix-bap:8081/bap/receiver/on_select
        ▼
┌──────────────────────────────────────────────────────────────────────────┐
│ onix-bap :8081  /bap/receiver/on_select                                  │
│   1. validateSign    (DeDi lookup bpp.example.com → verify)              │
│   2. addRoute        (BAPReceiver.yaml → sandbox-bap:3001/api/bap-webhook)│
│   3. validateSchema                                                      │
│   4. signAck                                                             │
│   5. forward callback to application layer                               │
└──────────────────────────────────────────────────────────────────────────┘
        │
        │  POST http://sandbox-bap:3001/api/bap-webhook
        ▼
┌──────────────────────────────────────────────────────────────────────────┐
│ sandbox-bap :3001   (mock rider app)                                     │
│   • Logs full on_select payload — visible via `docker logs -f sandbox-bap` │
│   • In real life: your rider app renders ride offers, driver ETAs, etc.  │
└──────────────────────────────────────────────────────────────────────────┘
```

Every arrow crossing between adapters is HTTP + Ed25519-signed Beckn Authorization header. Every DeDi lookup goes out to `fabric.nfh.global` (or cache-hits in Redis).

---

## V1 → V2 protocol shift

The devkit's application layer (sandbox mocks, and by extension real merchant clients today) still speaks **V1** — legacy Beckn 2.x with snake_case fields, an `order`-centric message body, and the older action vocabulary. Fabric and every ONIX adapter downstream now expect **V2** — camelCase, a `contract`-centric body typed via JSON-LD, and the renamed action set.

Each ONIX process bridges the two by running the `reqmapper` plugin (config in [`config/mobility-mappings.yaml`](./config/mobility-mappings.yaml)) as a pipeline step. It hand-executes a JSONata transform per `{action, direction}` pair — the `bapMappings` block rewrites V1 → V2 on the caller side (going out to fabric), and the `bppMappings` block rewrites V2 → V1 on the receiver side (going back to the app layer).

### 1. Action rename

V1's `search` / `on_search` are gone on the wire. V2 uses `discover` / `on_discover`, and fabric will reject the old names. The mapping file keeps `search` / `on_search` entries as pass-through (`$`) for backward compatibility, but any V1 client aiming to be V2-native must switch action strings.

| V1 action | V2 action | Notes |
|---|---|---|
| `search` | `discover` | Semantics unchanged; name only |
| `on_search` | `on_discover` | Same |
| `select`, `init`, `confirm`, `status`, `track`, `update`, `cancel`, `rate`, `support` and their `on_*` callbacks | *unchanged* | Only body/envelope shape shifts |

### 2. Context envelope

V1 envelope (snake_case, `domain`, `version: 2.1.0`) becomes a V2 envelope (camelCase, `networkId`, `version: 2.0.0`, plus a new `schemaContext` array pointing at the JSON-LD schemas the body relies on).

| V1 field | V2 field | Transform |
|---|---|---|
| `context.domain` | `context.networkId` | Rename only. Format stays `<network>:<subnet>` (e.g. `nfh.global:testnet-mobility`). All V2 requests fabric routes must carry it — this is what ONIX matches DeDi subscribers against for signature verification |
| `context.bap_id` / `context.bap_uri` | `context.bapId` / `context.bapUri` | Camelcase |
| `context.bpp_id` / `context.bpp_uri` | `context.bppId` / `context.bppUri` | Camelcase |
| `context.transaction_id` / `context.message_id` | `context.transactionId` / `context.messageId` | Camelcase |
| `context.timestamp` / `context.ttl` | `context.timestamp` / `context.ttl` | Unchanged |
| `context.version = "2.1.0"` | `context.version = "2.0.0"` | Reset by V2 spec |
| — (absent) | `context.schemaContext` | New array — JSON-LD `@context` URLs referenced by typed sub-objects in the body |

### 3. Message body model

The deep change. V1 is an `order`-graph of flat Beckn primitives (`items`, `fulfillments`, `providers`, `payments`, `billing`). V2 is a `contract`-graph where domain concepts are typed via `@context` / `@type` pointing at published JSON-LD schemas (`TripRequest`, `RideOption`, `ServiceClass`, `Passenger`, `SettlementTerm`, `Driver`).

| Concern | V1 shape | V2 shape |
|---|---|---|
| Root | `message.order` | `message.contract` |
| Line items | `order.items[]` + `order.fulfillments[]` (linked by `fulfillment_ids`) | `contract.commitments[].resources[]` + `commitments[].offer` (with `resourceIds`) |
| Provider / participants | `order.provider`, `order.billing` | `contract.participants[]` (each with `role` = `PROVIDER`/`BPP`/`CONSUMER` and typed `participantAttributes`) |
| Payment | `order.payments[]` | `contract.settlements[].settlementAttributes` (typed `SettlementTerm`) |
| Ride/vehicle metadata | Free-form `vehicle` block + `tags` under `fulfillments[]` | `resources[].resourceAttributes` typed as `TripRequest` — `origin`/`destination` become GeoJSON `Point` with `[lon, lat]` coordinates, `stops[]` moved inside |
| Fare rules | `tags[descriptor.code=…]` list under item | `resources[].considerations[].considerationAttributes` typed by `FareParameter`, structured amounts |
| Discover intent | `message.intent` (full Beckn object graph) | `message.intent.filters` with `type: "jsonpath"` and the V1 intent **stringified** into `expression`. The V2 → V1 direction `$eval()`s it back on the far side |
| Catalog (on_discover) | `catalogs[].providers[].items[]` + `fulfillments[]` | `catalogs[].providers[].resources[]` + `offers[]`, each offer carrying `offerAttributes` (categories, pricing model, cancellation terms) and linked to a resource via `resourceIds` |

Coordinate format is worth flagging: V1 uses `"gps": "lat, lon"` (comma-separated string, lat first). V2 uses GeoJSON `[lon, lat]` numeric arrays. The mapper swaps order on every stop location.

### Where the shift is enforced

```
   V1 client                Onix (BAP caller)                Fabric                Onix (BPP receiver)             V1 peer
  ─────────                ─────────────────                ──────                 ────────────────────            ───────
   POST /caller/discover
   { order-shape V1 } ──►  reqmapper.bap  V1→V2
                           schema (v2 core+mobility)
                           sign (BAP key)         ────────► V2 wire ────────────►  validateSign (DeDi)
                                                                                    reqmapper.bpp  V2→V1
                                                                                    schema · signAck
                                                                                    forward ──────────► POST /webhook
                                                                                                        { order-shape V1 }
```

The plugin runs *inside* the adapter's request pipeline (per [`config/mobility-bap.yaml`](./config/mobility-bap.yaml) / [`config/mobility-bpp.yaml`](./config/mobility-bpp.yaml)), so from the app-layer's perspective everything remains V1. That's why the sandboxes and any existing V1 rider/provider platform can plug in unmodified — the wire dialect is entirely a concern of the ONIX layer.

The trade-off: `mobility-mappings.yaml` is 2,766 lines of hand-written JSONata across ~11 actions × 2 directions. Drift between deployments has already caused bugs — see [`final-arch.md`](../../final-arch.md) for the auto-generation ask that would replace this file at request time with a plugin-generated transform + a small overrides input for semantic transforms the generator can't infer.

### What the client actually has to change

Only one thing on the app layer: push `discover` / `on_discover` as the `action` string where V1 code used to send `search` / `on_search`. The endpoint path follows suit (`/bap/caller/discover`, `/bpp/caller/on_discover`), and the routing YAMLs are already keyed on the new names.

Everything else in this section — envelope camelCasing, `domain` → `networkId`, `order` → `contract`, GeoJSON coordinates, stringified intent — is handled by the mapper. V1 clients keep sending V1 shapes; ONIX bridges to V2 on the wire.

---

## `discover` exception

`sandbox-bpp` ships without an `on_discover` handler — it returns HTTP 400 for that action. `discover` is included in the routing tables and the Postman collection for completeness, but it can't complete a round trip in this devkit. Start the flow at `select`.

The rest of the lifecycle (`select` → `init` → `confirm` → `status` → `track` → `update` → `cancel` → `rate` → `support`) has fixtures in [`sandbox-payloads/nfh.global/testnet-mobility/response/`](../../sandbox-payloads/nfh.global/testnet-mobility/response/) and completes cleanly.

---

## Where to plug in real code

The devkit-to-production migration path is a set of targeted swaps:

| Change | File | From | To |
|---|---|---|---|
| Real rider app | `mobility-routing-BAPReceiver.yaml` | `http://sandbox-bap:3001/api/bap-webhook` | your app's inbound webhook |
| Real provider platform | `mobility-routing-BPPReceiver.yaml` | `http://sandbox-bpp:3002/api/webhook` | your platform's action handler |
| Your BAP identity | `mobility-bap.yaml` → `keyManager` | shared `bap.example.com` keys | your DeDi-registered subscriberId + keys |
| Your BPP identity | `mobility-bpp.yaml` → `keyManager` | shared `bpp.example.com` keys | your DeDi-registered subscriberId + keys |
| Registry-resolved routing | all four `*-routing-*.yaml` | `targetType: url` + hardcoded container URLs | `targetType: bap` / `bpp` — ONIX resolves peer URLs via DeDi |

Only the receiver URL of each subscriber is public information (it goes in the DeDi record). The caller URL stays private — it's your app's handoff into ONIX and nobody outside your deployment ever calls it.
