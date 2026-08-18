# Fabric (NFH) Migration Plan

Companion to [`Architecture-nfh.md`](./Architecture-nfh.md). Actionable engineering plan for adding Fabric (NFH/onix) as a first-class gateway alongside ONDC and NY.

---

## Design summary

- **Fabric is a gateway variant.** Added to `GatewayAndRegistryService` and `NetworkEnums`. Fabric traffic goes via onix's `/bap/caller/<subscriberId>` endpoint; onix does DeDi lookup + signing + peer-to-peer forwarding.
- **Priority list = mandate, not fallback.** New per-merchant field `gatewayDispatchGroups :: Maybe [[GatewayAndRegistryService]]` on Merchant. `Just [[NY, Fabric]]` fans out to both gateways with the same txn; `Nothing` preserves today's single-call behavior (backward-compat for every existing merchant).
- **Same subscriberId across gateways.** `merchant.bapId` is set to the fabric subscriberId (`9292bap.finomad.eu`); no `context.bap_id` mutation needed.
- **Only one context mutation for Fabric branch:** `context.domain` → `bapConfig.networkId`. Everything else in the request body is preserved (same txn_id, message_id, order/fulfillment IDs).
- **Dedup at destination.** BPP-side Redis dedup catches concurrent duplicate txns; response cached so the second-gateway peer sees a proper signed `on_search` body rather than an error.
- **syncSearch untouched.** Independent latency optimization; fires alongside gateway dispatch as today.
- **Onix handles the V1 ↔ V2 wire shift** via `reqmapper` (mobility-mappings.yaml). Our code emits V1 shape (`context.domain`, snake_case); onix maps to `context.networkId` on the V2 wire.

Reference deployment (mobility-devkit, live testnet):
- Identity: `9292bap.finomad.eu` ↔ `9292bpp.finomad.eu` on `finomad.eu/OMN_Registry`.
- Onix URLs: `https://api.eu.moving.tech/beckn/onix/{bap,bpp}/{caller,receiver}/<subscriberId>`.
- All 10 ride-hailing actions verified end-to-end via `mobility/testnet/mobility-devkit/k8s/testing-prod.md`.

---

## Data model changes

### 1. `GatewayAndRegistryService` enum extension

**File:** `Backend/lib/beckn-spec/src/Domain/Types.hs:38`

```haskell
data GatewayAndRegistryService = ONDC | NY | Fabric
```

Both apps' `Environment.hs::fetchUrlFromList` and both apps' `castNetworkEnums` updated for exhaustive pattern matches. Fabric registry lookup routes to `ondcRegistryUrl` as a placeholder (onix does DeDi lookup; we do no registry hit from our side).

### 2. Dashboard `NetworkEnums`

**File:** `Backend/app/dashboard/CommonAPIs/src/Dashboard/Common/Merchant.hs:906`

```haskell
data NetworkEnums = ONDC | NY | Fabric
```

Both apps' `castNetworkEnums` cover `Common.Fabric = Domain.Types.Fabric`.

### 3. `BecknConfig` YAML additions

**Files:**
- `Backend/app/rider-platform/rider-app/Main/spec/Storage/BecknConfig.yaml`
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/BecknConfig.yaml`

Added:

```yaml
networkId: Maybe Text
```

Used only when Fabric is chosen — mutation source for `context.domain` (e.g. `"finomad.eu/OMN_Registry"`). Optional per merchant; regenerated via `, run-generator`.

### 4. `Merchant` YAML — nested-list dispatch groups

**Files:**
- `Backend/app/rider-platform/rider-app/Main/spec/Storage/Merchant.yaml:91`
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/Merchant.yaml:109`

```yaml
gatewayDispatchGroups: "Maybe [[GatewayAndRegistryService]]"
```

**Semantics:**
- `Nothing` → helper uses head of existing `gatewayAndRegistryPriorityList` (single call — identical to today).
- `Just [[NY, Fabric]]` → fan out NY + Fabric together for one dispatch step (MVP MANDATE mode).
- `Just [[NY, Fabric], [ONDC]]` → outer list reserves fallback semantics for a follow-up PR; MVP uses only the first inner group.

Field is declaration-only in the YAML (`fields:` section) — the generator handles beam type, `toTType`/`fromTType`, and SQL storage automatically. No custom JSON marshaling written.

### 5. Dhall / AppCfg / AppEnv — fabric base URL

**Files:**
- `Backend/dhall-configs/dev/common.dhall` — `let fabricGatewayBaseUrl = "http://localhost:${becknGatewayPort}/v1"` (exported in return record).
- `Backend/dhall-configs/dev/rider-app.dhall` + `Backend/dhall-configs/dev/dynamic-offer-driver-app.dhall` — wire `fabricGatewayBaseUrl = common.fabricGatewayBaseUrl`.
- `Backend/app/rider-platform/rider-app/Main/src/Environment.hs` — `fabricGatewayBaseUrl :: BaseUrl` in both `AppCfg` and `AppEnv`.
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Environment.hs` — same.

**Prod value example:** `https://api.eu.moving.tech/beckn/onix`.

Onix routes on the path segment after `/bap/caller/`:
- `<base>/bap/caller/<subscriberId1>/<action>` → onix module keyed to subscriberId1.
- Same onix pod supports N subscribers via N module blocks in the onix YAML.

Receiver counterpart: `<base>/bap/receiver/<subscriberId>/<action>` (mobility team registers this URL in the DeDi registry per subscriber; we do not construct it in nammayatri code today).

---

## Code changes

### 6. `mutateFabricContext` helper (pure JSON)

**File:** `Backend/lib/beckn-spec/src/BecknV2/OnDemand/Utils/Context.hs`

```haskell
mutateFabricContext :: Text -> A.Value -> A.Value
mutateFabricContext networkId (A.Object o) =
  A.Object $ AKM.adjust setDomain "context" o
  where
    setDomain (A.Object ctx) = A.Object $ AKM.insert "domain" (A.String networkId) ctx
    setDomain v = v
mutateFabricContext _ v = v
```

Only overrides `context.domain`. Body invariant: order/fulfillment IDs, txn_id, message_id preserved.

### 7. `GatewayLookup` module (generic)

**New file:** `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/GatewayLookup.hs`

```haskell
resolveGatewayUrl
  :: (HasFlowEnv m r '["ondcGatewayUrl" ::: BaseUrl, "nyGatewayUrl" ::: BaseUrl, "fabricGatewayBaseUrl" ::: BaseUrl])
  => Merchant -> GatewayAndRegistryService -> m BaseUrl
resolveGatewayUrl merchant = \case
  ONDC   -> asks (.ondcGatewayUrl)
  NY     -> asks (.nyGatewayUrl)
  Fabric -> do
    base <- asks (.fabricGatewayBaseUrl)
    pure $ base {baseUrlPath = baseUrlPath base <> "/bap/caller/" <> T.unpack merchant.bapId}

dispatchToGateways
  :: (MonadFlow m, HasFlowEnv m r '[...])
  => Merchant -> (BaseUrl -> GatewayAndRegistryService -> m ()) -> m ()
dispatchToGateways merchant callFn = do
  let targets = case merchant.gatewayDispatchGroups of
        Just (grp : _) -> grp
        _              -> take 1 merchant.gatewayAndRegistryPriorityList
  forM_ targets $ \gw -> do
    url <- resolveGatewayUrl merchant gw
    callFn url gw
```

Registered in `rider-app.cabal:527`. Symmetric helper **not** required on driver-app: BPP-side `CallBAP.hs` on_* dispatches route via `context.bapUri` from the inbound request, not via gateway lookup.

### 8. Wire dispatchers at search sites

**File:** `Backend/app/rider-platform/rider-app/Main/src/API/UI/Search.hs:313, 328`

Replaced single-URL fire with fan-out:

```haskell
GatewayLookup.dispatchToGateways dSearchRes.merchant $ \url _gw ->
  void $ CallBPP.searchV2 url becknTaxiReqV2 merchantId
```

Zero behavior change for merchants with `gatewayDispatchGroups = Nothing` (all existing merchants) — helper falls back to `take 1` of the existing priority list.

### 9. BPP-side response cache for duplicate txn

**Files:**
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Search.hs:1252` — new sibling helper `searchTxnResponseCacheKey :: Text -> Text -> Text` (namespace `Driver:Search:TxnResponseCache-`).
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/API/Internal/SyncSearch.hs:72-88` — sync-search handler now caches `onSearchReq` after first processing and returns cached body on duplicate.

Key semantics:
- Same 30s TTL as sibling dedup key (mirror arrives within seconds; 30s = 15-30× actual gap; halved memory vs previous 60s TTL).
- On race window (dedup set but response not yet cached), falls back to today's error behavior.
- Peer/fabric telemetry sees a real signed on_search response for both dispatches instead of an error on the second.

### 10. BAP-side on_search dedup (verified, no code change)

**File:** `Backend/app/rider-platform/rider-app/Main/src/API/Beckn/OnSearch.hs:78-84`

Existing `onSearchHandledKey` (30s TTL) silently drops the second-gateway on_search — already handles our fan-out case.

---

## Deferred (next steps)

### 11. `mutateFabricContext` wiring in `dispatchToGateways`

Fabric branch needs to mutate `context.domain` before dispatch. Requires:
- Fetch `bapConfig.networkId` inside the helper (or thread from caller).
- Convert typed request → `Value` → mutate → wire.

Complication: current fan-out invokes the caller's callback with `(url, gateway)` and the caller reuses the typed `CallBPP.searchV2`. To support per-gateway JSON mutation, either:
- Add a new unsigned raw-JSON call (`callBecknAPIUnsigned`) invoked only for the Fabric branch.
- Or restructure the callback to accept a JSON `Value` and dispatch both signed/unsigned paths.

### 12. `callBecknAPIUnsigned`

**Files:** `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/CallBPP.hs` + `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/CallBAP.hs`

Bare POST wrapper for Fabric outbound — onix signs, we skip. Needed for real Fabric traffic where signed body would be invalidated by onix's own signer.

### 13. Webhook receivers

- Rider-app: `POST /api/onixBapWebhook/:action` — receives `on_*` from fabric peers via onix. Skips `SignatureAuth` (onix already verified).
- Driver-app: `POST /api/onixBppWebhook/:action` — receives inbound `search/select/init/...` via onix.
- Handler bodies reused by refactoring existing `API/Beckn/On*.hs` (rider) and `API/Beckn/*.hs` (driver) into pure `handleFoo :: req -> m res` functions wrapped twice (signed + unsigned).

Trust boundary: Istio `AuthorizationPolicy` restricts source to the `onix` namespace. Never publicly exposed.

### 14. Outer-list fallback semantics

MVP uses only the first inner group of `gatewayDispatchGroups`. Full fallback across outer groups (try group 1; if all fail, try group 2) not yet implemented since today's dispatch has no cross-gateway fallback at all (only registry lookup does, via `reorderList`). Adding this would require passing an error handler through every dispatch site.

### 15. Mobility routing YAML updates

**File:** `mobility/testnet/mobility-devkit/k8s/config/mobility-routing-BAPReceiver.yaml`
- Replace mock target with nammayatri rider service DNS + `/api/onixBapWebhook`.

**File:** `mobility/testnet/mobility-devkit/k8s/config/mobility-routing-BPPReceiver.yaml`
- Replace mock target with driver service DNS + `/api/onixBppWebhook`.

Onix module blocks per subscriber (single onix pod, N tenants):
```yaml
- name: bapTxnCaller_9292bap
  path: /bap/caller/9292bap.finomad.eu/
  handler:
    plugins:
      keyManager:
        config:
          subscriberId: 9292bap.finomad.eu
          # per-subscriber keys
```

Per-subscriber DeDi registry entry: `bap_uri = <fabricGatewayBaseUrl>/bap/receiver/<subscriberId>/` (mobility team registers).

### 16. FRFS

Deferred. FRFS traffic for merchants with `[Fabric]` in `gatewayDispatchGroups` silently falls through to the existing FRFS gateway (documented, safe default).

### 17. Metrics

Deferred (with logs). Add when fabric traffic is live:
- `fabric_outbound_total{action}` — counter for outbound fabric POSTs.
- `fabric_webhook_received_total{action}` — counter for inbound webhook hits.
- `fabric_outbound_errors_total{action, reason}` — counter for failed outbound.

---

## What we're deliberately not doing

- **Log-push / mirror model.** Dropped — user pivoted from mirror to real MANDATE dispatch. No `pushLogsToFabricLayer`, no `gatewayLogsEnabled` toggle.
- **`fabricSubscriberId` field in BecknConfig.** Dropped — `merchant.bapId` unified across gateways.
- **`fabricPathKey` field.** Dropped — `merchant.bapId` doubles as the onix path key.
- **Per-merchant `fabricGatewayUrl` in BecknConfig.** Dropped — one Dhall `fabricGatewayBaseUrl` per cluster; path derived per-subscriber.
- **Native V3 wire (drop onix, emit V2 directly).** Out of scope; onix handles V1↔V2 mapping.
- **SignatureAuth changes for non-fabric traffic.** Untouched.
- **Payment / auth flow changes.** Untouched (Stripe already supported at `BecknV2/OnDemand/Enums.hs:305-321`).
- **BecknConfig context version rename.** Onix mapper handles `2.1.0 → 2.0.0` on the wire.
- **Unit tests for `mutateFabricContext`.** Add if requested; not in default scope.

---

## Rollout

Single PR spanning nammayatri + mobility. Ships dormant — no merchant has `gatewayDispatchGroups` populated yet.

1. Merge the nammayatri PR. All existing merchants unaffected (`gatewayDispatchGroups = NULL` → `take 1` head of priority list = today's behavior).
2. Merge mobility routing YAML updates. Onix receivers point at nammayatri webhooks; no traffic yet.
3. Enable a dev/test merchant via SQL:
   ```sql
   UPDATE beckn_config
   SET network_id = 'finomad.eu/OMN_Registry'
   WHERE merchant_id = '<merchant_id>' AND domain = 'MOBILITY';

   UPDATE merchant
   SET bap_id = '9292bap.finomad.eu',
       gateway_dispatch_groups = '[["NY","Fabric"]]'::jsonb
   WHERE id = '<merchant_id>';
   ```
4. Fire a `search` from the dev merchant. Watch:
   - `beckn-onix-bap` pod logs (sign + forward via DeDi).
   - `beckn-onix-bpp` pod logs (verify + forward to webhook).
   - Nammayatri driver-app webhook receiver logs.
   - Round-trip `on_search` back through onix-bpp-caller → onix-bap-receiver → rider-app webhook.
5. Verify full ride lifecycle.
6. Roll to production merchants one at a time.

---

## Rollback

Any of the following, individually or together:

- SQL: `UPDATE merchant SET gateway_dispatch_groups = NULL WHERE id = ...` — instant per-merchant disable. Reverts to head-of-priority-list single call.
- SQL: `UPDATE beckn_config SET network_id = NULL WHERE merchant_id = ...` — clears fabric identity config.
- Revert mobility routing YAML to point at sandbox mock; nammayatri code stays dormant.

No redeploy required for any rollback path.

---

## Sequencing constraints

- Both nammayatri BAP AND BPP must have consistent `gatewayDispatchGroups` for a merchant. If BAP is on Fabric but BPP is still ONDC-only, cross-network calls fail at peer's DeDi verification. Enforced by convention.
- Per-merchant DeDi registration happens on the mobility/NFH side. Verified done for `9292bap.finomad.eu` ↔ `9292bpp.finomad.eu` per `mobility/testnet/mobility-devkit/k8s/testing-prod.md`.
- Onix `reqmapper` mobility-mappings.yaml must cover every action we already use. Verified for all 10 ride-hailing actions in `testing-prod.md`.

---

## Files touched (this PR — nammayatri side)

**Enum + dashboard:**
- `Backend/lib/beckn-spec/src/Domain/Types.hs:38` — `Fabric` variant.
- `Backend/app/dashboard/CommonAPIs/src/Dashboard/Common/Merchant.hs:906` — `NetworkEnums Fabric`.
- `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Dashboard/Merchant.hs:1039-1042` — `castNetworkEnums Common.Fabric`.
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Dashboard/Management/Merchant.hs:4595-4598` — same.

**Storage specs (regenerated via `, run-generator`):**
- `Backend/app/rider-platform/rider-app/Main/spec/Storage/BecknConfig.yaml` — `networkId`.
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/BecknConfig.yaml` — `networkId`.
- `Backend/app/rider-platform/rider-app/Main/spec/Storage/Merchant.yaml:91` — `gatewayDispatchGroups`.
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/Merchant.yaml:109` — same.

**Environment / config:**
- `Backend/dhall-configs/dev/common.dhall` — `fabricGatewayBaseUrl` binding + export.
- `Backend/dhall-configs/dev/rider-app.dhall` + `dynamic-offer-driver-app.dhall` — wire `fabricGatewayBaseUrl = common.fabricGatewayBaseUrl`.
- `Backend/app/rider-platform/rider-app/Main/src/Environment.hs` — `AppCfg`/`AppEnv` field.
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Environment.hs` — same.
- Both `Environment.hs::fetchUrlFromList` — `Fabric` branch (registry placeholder).

**Wire helpers:**
- `Backend/lib/beckn-spec/src/BecknV2/OnDemand/Utils/Context.hs` — `mutateFabricContext`.
- `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/GatewayLookup.hs` — new module.
- `Backend/app/rider-platform/rider-app/Main/rider-app.cabal:527` — module registered.

**Dispatch sites:**
- `Backend/app/rider-platform/rider-app/Main/src/API/UI/Search.hs:104, 313, 328` — helper import + `dispatchToGateways` at both fork sites.

**Dedup + response cache:**
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Search.hs:1252` — `searchTxnResponseCacheKey`.
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/API/Internal/SyncSearch.hs:72-88` — cache-on-first, return-cached-on-duplicate; 60s TTL → 30s.
