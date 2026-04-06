"""
ONDC Beckn Protocol Mock for FRFS (Bus/Metro/Subway)

On receiving search/init/confirm, returns ACK immediately and then
asynchronously calls back the BAP with on_search/on_init/on_confirm.

The callback URL is extracted from context.bap_uri in the incoming request.
"""

import json
import os
import threading
import time
import urllib.request
import urllib.error
import logging
import uuid
import base64
import hashlib
from datetime import datetime, timezone

from status_store import extract_path_ids, deep_merge

log = logging.getLogger("mock.ondc")

ACK = {"message": {"ack": {"status": "ACK"}}}

# Dev signing key (same as all local Beckn services)
_PRIVATE_KEY_B64 = "Lw9M+SHLY+yyTmqPVlbKxgvktZRfuIT8nHyE89Jmf+o="
_SIGNING_TTL = 600  # seconds

# Registry subscriber lookup — loaded lazily from DB on first use
_subscriber_cache = None


def _get_subscriber_for_path(url_path):
    """Find the registry subscriber_id and unique_key_id for a given mock server URL path.
    Queries the registry DB once and caches the result."""
    global _subscriber_cache
    if _subscriber_cache is None or _subscriber_cache.get("__version") != "v4":
        _subscriber_cache = {}
        try:
            import psycopg2
            conn = psycopg2.connect(host="localhost", port=5434, dbname="atlas_dev",
                                    user=os.environ.get("DB_USER", os.environ.get("USER", "atlas")),
                                    password=os.environ.get("DB_PASS", ""))
            conn.autocommit = True
            cur = conn.cursor()
            cur.execute("""
                SELECT subscriber_id, unique_key_id, subscriber_url
                FROM atlas_registry.subscriber
                WHERE (subscriber_url LIKE '%%localhost:8080/ondc%%'
                   OR subscriber_url LIKE '%%localhost:8080/cmrl%%')
                  AND domain = 'PUBLIC_TRANSPORT'
            """)
            for row in cur.fetchall():
                sub_id, key_id, sub_url = row
                from urllib.parse import urlparse
                p = urlparse(sub_url).path.rstrip("/")
                _subscriber_cache[p] = (sub_id, key_id)
            conn.close()
            _subscriber_cache["__version"] = "v4"
            print(f"  [ONDC] Loaded {len(_subscriber_cache)-1} subscriber mappings from registry", flush=True)
        except Exception as e:
            print(f"  [ONDC] ERROR: Cannot load subscribers from registry DB: {e}", flush=True)
            print(f"  [ONDC] Ensure psycopg2 is installed (restart stack for nix rebuild)", flush=True)
            _subscriber_cache = {"__version": "v4"}

    # Try to match the request path against cached subscriber URLs
    # Handler receives path like /ondc/ondc/seller/v1/search — the first /ondc is the route prefix
    check_path = url_path.rstrip("/")
    # Strip the action (last segment): search, init, confirm, status, cancel
    for action in ["/search", "/init", "/confirm", "/status", "/cancel", "/update", "/select"]:
        if check_path.endswith(action):
            check_path = check_path[:-len(action)]
            break

    # Try multiple path variations for matching
    candidates = [check_path]
    # Also try without route prefix: /ondc/ondc/seller/v1 → /ondc/seller/v1
    if check_path.startswith("/ondc"):
        candidates.append(check_path[len("/ondc"):])

    for candidate in candidates:
        if candidate in _subscriber_cache:
            return _subscriber_cache[candidate]

    # Try longest prefix match (most specific path wins)
    best_match = None
    best_len = 0
    for cached_path, ids in _subscriber_cache.items():
        if cached_path.startswith("__"):
            continue
        for candidate in candidates:
            if candidate.startswith(cached_path) and len(cached_path) > best_len:
                best_match = ids
                best_len = len(cached_path)

    return best_match


def handle(handler, path, body):
    """Route ONDC Beckn requests — ACK + async callback."""
    _override_status, extra = handler._get_override("ondc", *extract_path_ids(path))

    # Parse body — mock server passes raw bytes
    parsed_body = {}
    if body:
        try:
            parsed_body = json.loads(body if isinstance(body, str) else body.decode("utf-8", errors="replace"))
        except (json.JSONDecodeError, UnicodeDecodeError):
            pass

    # Determine action from path: /ondc/seller/<provider>/search → "search"
    parts = path.strip("/").split("/")
    action = parts[-1] if parts else ""

    # Extract context from incoming request
    ctx = parsed_body.get("context", {})
    bap_uri = ctx.get("bap_uri", "")
    bap_id = ctx.get("bap_id", "")
    print(f"  [ONDC] {action}: bap_uri={bap_uri}, bap_id={bap_id}, txn={ctx.get('transaction_id','')[:12]}...", flush=True)
    transaction_id = ctx.get("transaction_id", str(uuid.uuid4()))
    message_id = ctx.get("message_id", str(uuid.uuid4()))
    domain = ctx.get("domain", "ONDC:TRV11")

    # Extract provider from path (e.g., /ondc/seller/chalo-bus/search → "chalo-bus")
    provider_id = ""
    if "seller" in parts:
        idx = parts.index("seller")
        if idx + 1 < len(parts) - 1:
            provider_id = parts[idx + 1]

    # Look up the registry subscriber for this BPP URL path to get correct subscriber_id for signing
    subscriber_info = _get_subscriber_for_path(path)
    if subscriber_info:
        bpp_id = subscriber_info[0]  # subscriber_id from registry
        bpp_unique_key = subscriber_info[1]  # unique_key_id from registry
        print(f"  [ONDC] Matched registry subscriber: {bpp_id} (key={bpp_unique_key})", flush=True)
    else:
        bpp_id = provider_id or "mock-ondc-bpp"
        bpp_unique_key = bpp_id
        print(f"  [ONDC] No registry match, using provider_id as bpp_id: {bpp_id}", flush=True)
    bpp_uri = f"http://localhost:8080{path.rsplit('/', 1)[0]}"

    # Build callback context
    cb_ctx = {
        "domain": domain,
        "action": f"on_{action}",
        "bap_id": bap_id,
        "bap_uri": bap_uri,
        "bpp_id": bpp_id,
        "bpp_uri": bpp_uri,
        "transaction_id": transaction_id,
        "message_id": str(uuid.uuid4()),
        "timestamp": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.%fZ"),
        "version": "2.0.0",
        "ttl": "PT30S",
    }

    # Schedule async callback based on action
    if action == "search" and bap_uri:
        msg = parsed_body.get("message", {})
        intent = msg.get("intent", {})
        print(f"  [ONDC] Spawning on_search callback (bpp_id={bpp_id}) to {bap_uri}", flush=True)
        threading.Thread(target=_callback_on_search, args=(bap_uri, cb_ctx, intent, provider_id, bpp_id, bpp_unique_key), daemon=True).start()
    elif action == "init" and bap_uri:
        order = parsed_body.get("message", {}).get("order", {})
        print(f"  [ONDC] Spawning on_init callback (bpp_id={bpp_id}) to {bap_uri}", flush=True)
        threading.Thread(target=_callback_on_init, args=(bap_uri, cb_ctx, order, provider_id, bpp_id, bpp_unique_key), daemon=True).start()
    elif action == "confirm" and bap_uri:
        order = parsed_body.get("message", {}).get("order", {})
        print(f"  [ONDC] Spawning on_confirm callback (bpp_id={bpp_id}) to {bap_uri}", flush=True)
        threading.Thread(target=_callback_on_confirm, args=(bap_uri, cb_ctx, order, provider_id, bpp_id, bpp_unique_key), daemon=True).start()
    elif action == "status" and bap_uri:
        order_id = parsed_body.get("message", {}).get("order_id", "")
        print(f"  [ONDC] Spawning on_status callback (bpp_id={bpp_id}) to {bap_uri}", flush=True)
        threading.Thread(target=_callback_on_status, args=(bap_uri, cb_ctx, order_id, provider_id, bpp_id, bpp_unique_key), daemon=True).start()
    elif not bap_uri:
        print(f"  [ONDC] WARNING: No bap_uri in context, cannot send callback", flush=True)

    resp = dict(ACK)
    if extra:
        deep_merge(resp, extra)
    handler._json(resp)


# ── Async Callbacks ──

def _sign_beckn_request(body_bytes, subscriber_id, unique_key_id):
    """Create Beckn signature Authorization header using ed25519.
    Follows ONDC reference: https://github.com/ONDC-Official/reference-implementations
    """
    now = int(time.time())
    expires = now + _SIGNING_TTL

    # BLAKE2b-512 hash of body
    body_hash = base64.b64encode(hashlib.blake2b(body_bytes, digest_size=64).digest()).decode()

    # Build signing string per ONDC spec
    signing_string = f"(created): {now}\n(expires): {expires}\ndigest: BLAKE-512={body_hash}"

    # Sign with ed25519 — try nacl first, then cryptography
    sig = None
    private_key_bytes = base64.b64decode(_PRIVATE_KEY_B64)

    try:
        import nacl.signing
        # Dev key is 32-byte seed
        signer = nacl.signing.SigningKey(private_key_bytes)
        signed = signer.sign(signing_string.encode("utf-8"))
        sig = base64.b64encode(signed.signature).decode()
    except ImportError:
        try:
            from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey
            pk = Ed25519PrivateKey.from_private_bytes(private_key_bytes)
            sig = base64.b64encode(pk.sign(signing_string.encode("utf-8"))).decode()
        except ImportError:
            print("  [ONDC] WARNING: neither nacl nor cryptography available, sending without signature", flush=True)
            return None, None

    key_id = f"{subscriber_id}|{unique_key_id}|ed25519"
    auth = (
        f'Signature keyId="{key_id}",algorithm="ed25519",'
        f'created="{now}",expires="{expires}",'
        f'headers="(created) (expires) digest",'
        f'signature="{sig}"'
    )
    return auth, body_hash


def _post_callback(bap_uri, path, payload, subscriber_id="mock-ondc-bpp", unique_key_id="mock-ondc-bpp"):
    """POST callback to BAP with Beckn signature auth.
    bap_uri is like http://localhost:8013/frfs/4b17bd06-... — the APIM route expects
    /beckn/frfs/v1/{merchantId}/on_search. Extract merchantId from bap_uri path."""
    from urllib.parse import urlparse
    parsed = urlparse(bap_uri)
    base_url = f"{parsed.scheme}://{parsed.netloc}"
    # Extract merchantId from bap_uri path: /frfs/<merchantId> or /beckn/cab/v1/<merchantId>
    uri_parts = [p for p in parsed.path.strip("/").split("/") if p]
    merchant_id = uri_parts[-1] if uri_parts else ""
    # Use the APIM route: /beckn/frfs/v1/{merchantId}/{action}
    url = f"{base_url}/beckn/frfs/v1/{merchant_id}/{path.lstrip('/')}"
    print(f"  [ONDC] Sending callback POST {url}", flush=True)
    data = json.dumps(payload).encode("utf-8")

    headers = {"Content-Type": "application/json"}
    auth, body_hash = _sign_beckn_request(data, subscriber_id, unique_key_id)
    if auth:
        headers["Authorization"] = auth
        headers["Beckn-Body-Hash"] = body_hash

    req = urllib.request.Request(url, data=data, headers=headers, method="POST")
    try:
        time.sleep(0.5)  # Small delay to simulate async BPP processing
        with urllib.request.urlopen(req, timeout=10) as resp:
            print(f"  [ONDC] Callback {path} → {resp.status}", flush=True)
    except urllib.error.HTTPError as e:
        body = e.read().decode()[:300]
        print(f"  [ONDC] Callback {path} → {e.code}: {body}", flush=True)
    except Exception as e:
        print(f"  [ONDC] Callback {path} FAILED: {e}", flush=True)


def _callback_on_search(bap_uri, ctx, intent, provider_id, bpp_id, bpp_unique_key):
    """Build on_search with catalog containing quotes."""
    fulfillment = intent.get("fulfillment", {})
    stops = fulfillment.get("stops", [])

    start_code = stops[0].get("location", {}).get("descriptor", {}).get("code", "STN_A") if stops else "STN_A"
    end_code = stops[1].get("location", {}).get("descriptor", {}).get("code", "STN_B") if len(stops) > 1 else "STN_B"

    quote_id = str(uuid.uuid4())
    fulfillment_id = str(uuid.uuid4())

    payload = {
        "context": ctx,
        "message": {
            "catalog": {
                "providers": [{
                    "id": provider_id or "mock-provider",
                    "descriptor": {"name": provider_id or "Mock BPP"},
                    "items": [{
                        "id": quote_id,
                        "descriptor": {"code": "SJT", "name": "Single Journey Ticket"},
                        "price": {"value": "10", "currency": "INR"},
                        "quantity": {"maximum": {"count": 6}},
                        "fulfillment_ids": [fulfillment_id],
                        "time": {
                            "range": {
                                "start": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
                                "end": "2099-12-31T23:59:59Z",
                            }
                        },
                    }],
                    "fulfillments": [{
                        "id": fulfillment_id,
                        "type": "ROUTE",
                        "stops": [
                            {"type": "START", "location": {"descriptor": {"code": start_code, "name": f"Station {start_code}"}}},
                            {"type": "END", "location": {"descriptor": {"code": end_code, "name": f"Station {end_code}"}}},
                        ],
                        "vehicle": {"category": "BUS"},
                    }],
                    "payments": [{
                        "id": str(uuid.uuid4()),
                        "collected_by": "BAP",
                        "type": "PRE-ORDER",
                        "tags": [{"descriptor": {"code": "BUYER_FINDER_FEES"}, "list": [{"descriptor": {"code": "BUYER_FINDER_FEES_PERCENTAGE"}, "value": "1"}]}],
                    }],
                }]
            }
        }
    }
    _post_callback(bap_uri, "on_search", payload, bpp_id, bpp_unique_key)


def _callback_on_init(bap_uri, ctx, order, provider_id, bpp_id, bpp_unique_key):
    """Build on_init with booking details."""
    order_id = str(uuid.uuid4())
    items = order.get("items", [{}])
    item_id = items[0].get("id", "") if items else ""
    fulfillment_ids = items[0].get("fulfillment_ids", []) if items else []
    provider = order.get("provider", {})

    payload = {
        "context": ctx,
        "message": {
            "order": {
                "id": order_id,
                "status": "ACTIVE",
                "provider": provider or {"id": provider_id or "mock-provider"},
                "items": items,
                "billing": order.get("billing", {"name": "Test User", "email": "test@test.com", "phone": "9999999999"}),
                "fulfillments": [{
                    "id": fulfillment_ids[0] if fulfillment_ids else str(uuid.uuid4()),
                    "type": "ROUTE",
                    "stops": order.get("fulfillments", [{}])[0].get("stops", []) if order.get("fulfillments") else [],
                }],
                "payments": [{
                    "id": str(uuid.uuid4()),
                    "collected_by": "BAP",
                    "type": "PRE-ORDER",
                    "status": "NOT-PAID",
                    "params": {
                        "amount": items[0].get("price", {}).get("value", "10") if items else "10",
                        "currency": "INR",
                        "bank_code": "MOCK_BANK",
                        "bank_account_number": "1234567890",
                    },
                }],
                "quote": {
                    "price": {"value": items[0].get("price", {}).get("value", "10") if items else "10", "currency": "INR"},
                    "breakup": [{
                        "title": "BASE_FARE",
                        "price": {"value": items[0].get("price", {}).get("value", "10") if items else "10", "currency": "INR"},
                    }],
                },
            }
        }
    }
    _post_callback(bap_uri, "on_init", payload, bpp_id, bpp_unique_key)


def _callback_on_confirm(bap_uri, ctx, order, provider_id, bpp_id, bpp_unique_key):
    """Build on_confirm with confirmed booking."""
    order_id = order.get("id", str(uuid.uuid4()))
    items = order.get("items", [{}])

    payload = {
        "context": ctx,
        "message": {
            "order": {
                "id": order_id,
                "status": "CONFIRMED",
                "provider": order.get("provider", {"id": provider_id or "mock-provider"}),
                "items": items,
                "fulfillments": order.get("fulfillments", []),
                "payments": [{
                    "id": str(uuid.uuid4()),
                    "collected_by": "BAP",
                    "type": "PRE-ORDER",
                    "status": "PAID",
                    "params": {
                        "amount": items[0].get("price", {}).get("value", "10") if items else "10",
                        "currency": "INR",
                    },
                }],
                "quote": order.get("quote", {
                    "price": {"value": "10", "currency": "INR"},
                }),
            }
        }
    }
    _post_callback(bap_uri, "on_confirm", payload, bpp_id, bpp_unique_key)


def _callback_on_status(bap_uri, ctx, order_id, provider_id, bpp_id, bpp_unique_key):
    """Build on_status with current booking status."""
    payload = {
        "context": ctx,
        "message": {
            "order": {
                "id": order_id,
                "status": "CONFIRMED",
                "provider": {"id": provider_id or "mock-provider"},
            }
        }
    }
    _post_callback(bap_uri, "on_status", payload, bpp_id, bpp_unique_key)
