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

from ._env import MOCK_SERVER_PORT

from status_store import extract_path_ids, deep_merge

log = logging.getLogger("mock.ondc")

ACK = {"message": {"ack": {"status": "ACK"}}}

# Dev signing key (same as all local Beckn services)
_PRIVATE_KEY_B64 = "Lw9M+SHLY+yyTmqPVlbKxgvktZRfuIT8nHyE89Jmf+o="
_SIGNING_TTL = 600  # seconds

# BPP signing identity used for every outbound on_* callback.
# Pulled lazily from atlas_registry.subscriber (first PUBLIC_TRANSPORT BPP row)
# and cached. No fallback — if the registry has no row, signature verification
# is going to fail anyway, so failing loudly here is the right behaviour.
_BPP_IDENTITY_CACHE: tuple[str, str] | None = None

def _get_bpp_identity() -> tuple[str, str]:
    """Return (subscriber_id, unique_key_id) for the first PUBLIC_TRANSPORT BPP
    row in atlas_registry.subscriber. Cached after first call."""
    global _BPP_IDENTITY_CACHE
    if _BPP_IDENTITY_CACHE is not None:
        return _BPP_IDENTITY_CACHE
    import psycopg2
    conn = psycopg2.connect(
        host="localhost", port=int(os.environ.get("DB_PRIMARY_PORT", "5434")), dbname="atlas_dev",
        user=os.environ.get("DB_USER", os.environ.get("USER", "atlas")),
        password=os.environ.get("DB_PASS", ""),
    )
    conn.autocommit = True
    cur = conn.cursor()
    cur.execute("""
        SELECT subscriber_id, unique_key_id
        FROM atlas_registry.subscriber
        WHERE type = 'BPP' AND domain = 'PUBLIC_TRANSPORT'
        LIMIT 1
    """)
    row = cur.fetchone()
    cur.close()
    conn.close()
    if not row:
        raise RuntimeError(
            "No PUBLIC_TRANSPORT BPP row in atlas_registry.subscriber."
        )
    _BPP_IDENTITY_CACHE = (row[0], row[1])
    print(f"  [ONDC] Cached BPP identity from registry: {_BPP_IDENTITY_CACHE[0]} "
          f"(key={_BPP_IDENTITY_CACHE[1]})", flush=True)
    return _BPP_IDENTITY_CACHE

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

    # Every callback is signed with a single PUBLIC_TRANSPORT BPP identity
    # pulled from atlas_registry.subscriber (cached after first lookup).
    bpp_id, bpp_unique_key = _get_bpp_identity()
    bpp_uri = f"http://localhost:{MOCK_SERVER_PORT}{path.rsplit('/', 1)[0]}"

    # Build callback context
    cb_ctx = {
        "domain": domain,
        "action": f"on_{action}",
        "bap_id": bap_id,
        "bap_uri": bap_uri,
        "bpp_id": bpp_id,
        "bpp_uri": bpp_uri,
        "transaction_id": transaction_id,
        "message_id": message_id,
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
    elif action == "select" and bap_uri:
        order = parsed_body.get("message", {}).get("order", {})
        print(f"  [ONDC] Spawning on_select callback (bpp_id={bpp_id}) to {bap_uri}", flush=True)
        threading.Thread(target=_callback_on_select, args=(bap_uri, cb_ctx, order, provider_id, bpp_id, bpp_unique_key), daemon=True).start()
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
    vehicle_category = (fulfillment.get("vehicle") or {}).get("category") or "BUS"

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
                        "vehicle": {"category": vehicle_category},
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


def _callback_on_select(bap_uri, ctx, order, provider_id, bpp_id, bpp_unique_key):
    """Build on_select with a firm quote for the selected item(s).

    Per ONDC: select tells the BPP "here are the items I want a quote for";
    on_select returns the priced order with a quote.breakup but no billing /
    payment status yet (those come in on_init / on_confirm).

    Mandatory shape rider-app's FRFS ACL (Beckn/ACL/FRFS/Utils.hs) enforces:
      - each `items[].id` must echo back so the BAP can map the quote.
      - each `items[].quantity.selected.count` must be present (else
        "Item Quantity not found" from mkDCategorySelect).
      - each `items[].descriptor.code` is read for fare category (SJT default).
      - `quote.breakup` must contain one BASE_FARE entry per item with
        `item.id == <that item's id>`, else `baseFareForItem` raises
        "BASE_FARE not found for item <id>".
    """
    req_items = order.get("items") or [{}]
    provider = order.get("provider", {})
    fulfillment_ids_in = (req_items[0].get("fulfillment_ids") or []) if req_items else []
    req_fulfillments = order.get("fulfillments") or []

    # Echo items with mandatory fields filled in. Per-item price defaults to
    # the request's item price (or 10 if absent).
    resp_items = []
    breakup = []
    total = 0
    for it in req_items:
        item_id = it.get("id") or str(uuid.uuid4())
        price_val = (it.get("price") or {}).get("value", "10")
        currency = (it.get("price") or {}).get("currency", "INR")
        qty = (((it.get("quantity") or {}).get("selected") or {}).get("count")) or 1
        try:
            total += float(price_val) * int(qty)
        except (TypeError, ValueError):
            pass
        desc_code = (it.get("descriptor") or {}).get("code") or "SJT"
        resp_items.append({
            "id": item_id,
            "descriptor": {"code": desc_code, "name": it.get("descriptor", {}).get("name", "Single Journey Ticket")},
            "price": {"value": price_val, "currency": currency},
            "quantity": {"selected": {"count": int(qty)}},
            "fulfillment_ids": it.get("fulfillment_ids") or fulfillment_ids_in or [str(uuid.uuid4())],
        })
        # One BASE_FARE breakup row per item, linked by item.id — required by
        # baseFareForItem in Beckn/ACL/FRFS/Utils.hs.
        breakup.append({
            "title": "BASE_FARE",
            "item": {"id": item_id},
            "price": {"value": price_val, "currency": currency},
        })

    payload = {
        "context": ctx,
        "message": {
            "order": {
                "provider": provider or {"id": provider_id or "mock-provider"},
                "items": resp_items,
                "fulfillments": req_fulfillments or [{
                    "id": fulfillment_ids_in[0] if fulfillment_ids_in else str(uuid.uuid4()),
                    "type": "ROUTE",
                    "stops": [],
                }],
                "quote": {
                    "price": {"value": f"{total:.2f}" if total else "10", "currency": "INR"},
                    "breakup": breakup,
                    "ttl": "PT15M",
                },
            }
        }
    }
    _post_callback(bap_uri, "on_select", payload, bpp_id, bpp_unique_key)


def _callback_on_init(bap_uri, ctx, order, provider_id, bpp_id, bpp_unique_key):
    """Build on_init with booking details.

    Same item/breakup linkage rule as on_select: rider-app's FRFS OnInit ACL
    (Beckn/ACL/FRFS/OnInit.hs) calls zipItemsWithPrice → baseFareForItem, which
    requires one `BASE_FARE` breakup row per item with `item.id == <item id>`.
    """
    order_id = str(uuid.uuid4())
    req_items = order.get("items") or [{}]
    fulfillment_ids_in = (req_items[0].get("fulfillment_ids") or []) if req_items else []
    provider = order.get("provider", {})

    resp_items, breakup, total = [], [], 0
    for it in req_items:
        item_id = it.get("id") or str(uuid.uuid4())
        price_val = (it.get("price") or {}).get("value", "10")
        currency = (it.get("price") or {}).get("currency", "INR")
        qty = (((it.get("quantity") or {}).get("selected") or {}).get("count")) or 1
        try:
            total += float(price_val) * int(qty)
        except (TypeError, ValueError):
            pass
        desc_code = (it.get("descriptor") or {}).get("code") or "SJT"
        resp_items.append({
            "id": item_id,
            "descriptor": {"code": desc_code, "name": (it.get("descriptor") or {}).get("name", "Single Journey Ticket")},
            "price": {"value": price_val, "currency": currency},
            "quantity": {"selected": {"count": int(qty)}},
            "fulfillment_ids": it.get("fulfillment_ids") or fulfillment_ids_in or [str(uuid.uuid4())],
        })
        breakup.append({
            "title": "BASE_FARE",
            "item": {"id": item_id},
            "price": {"value": price_val, "currency": currency},
        })

    total_str = f"{total:.2f}" if total else "10"
    payload = {
        "context": ctx,
        "message": {
            "order": {
                "id": order_id,
                "status": "ACTIVE",
                "provider": provider or {"id": provider_id or "mock-provider"},
                "items": resp_items,
                "billing": order.get("billing", {"name": "Test User", "email": "test@test.com", "phone": "9999999999"}),
                "fulfillments": [{
                    "id": fulfillment_ids_in[0] if fulfillment_ids_in else str(uuid.uuid4()),
                    "type": "ROUTE",
                    "stops": order.get("fulfillments", [{}])[0].get("stops", []) if order.get("fulfillments") else [],
                }],
                "payments": [{
                    "id": str(uuid.uuid4()),
                    "collected_by": "BAP",
                    "type": "PRE-ORDER",
                    "status": "NOT-PAID",
                    "params": {
                        "amount": total_str,
                        "currency": "INR",
                        "bank_code": "MOCK_BANK",
                        "bank_account_number": "1234567890",
                    },
                }],
                "quote": {
                    "price": {"value": total_str, "currency": "INR"},
                    "breakup": breakup,
                    "ttl": "PT15M",
                },
            }
        }
    }
    _post_callback(bap_uri, "on_init", payload, bpp_id, bpp_unique_key)


def _callback_on_confirm(bap_uri, ctx, order, provider_id, bpp_id, bpp_unique_key):
    """Build on_confirm with confirmed booking + issued ticket(s).

    Rider-app's OnConfirm ACL (Beckn/ACL/FRFS/OnConfirm.hs:38) calls
    `parseTickets item fulfillments` → `parseTicket` (Utils.hs:156) which
    requires, for each item:
      - item.fulfillment_ids non-empty (Utils.hs:139-140)
      - a matching TICKET (or TRIP) fulfillment whose START stop has
        authorization {token, valid_to, status} — that's the QR code.

    We synthesize a TICKET fulfillment per item, link them via
    item.fulfillment_ids → fulfillment.id, and put a mock QR token in the
    START stop's authorization block.
    """
    from datetime import timedelta
    order_id = order.get("id") or str(uuid.uuid4())
    req_items = order.get("items") or [{}]
    req_stops = (order.get("fulfillments") or [{}])[0].get("stops") or []
    valid_to = (datetime.now(timezone.utc) + timedelta(hours=24)).strftime("%Y-%m-%dT%H:%M:%SZ")

    resp_items, breakup, fulfillments, total = [], [], [], 0
    for it in req_items:
        item_id = it.get("id") or str(uuid.uuid4())
        price_val = (it.get("price") or {}).get("value", "10")
        currency = (it.get("price") or {}).get("currency", "INR")
        qty = (((it.get("quantity") or {}).get("selected") or {}).get("count")) or 1
        try:
            total += float(price_val) * int(qty)
        except (TypeError, ValueError):
            pass
        desc_code = (it.get("descriptor") or {}).get("code") or "SJT"
        # One TICKET fulfillment per item with mock QR / ticket number.
        fid = (it.get("fulfillment_ids") or [str(uuid.uuid4())])[0]
        ticket_number = f"TKT-{uuid.uuid4().hex[:10].upper()}"
        qr_token = f"QR-{uuid.uuid4().hex}"
        # Build stops: copy request stops if present, else minimal START/END.
        if req_stops:
            stops = json.loads(json.dumps(req_stops))  # deep-copy
        else:
            stops = [
                {"type": "START", "location": {"descriptor": {"code": "STN_A", "name": "Station A"}}},
                {"type": "END",   "location": {"descriptor": {"code": "STN_B", "name": "Station B"}}},
            ]
        # Inject authorization on the START stop — that's where parseTicket
        # reads QR data / validTill / status from.
        for s in stops:
            if s.get("type") == "START":
                s["authorization"] = {
                    "token": qr_token,
                    "valid_to": valid_to,
                    "status": "UNCLAIMED",
                    "type": "QR",
                }
                break
        fulfillments.append({
            "id": fid,
            "type": "TICKET",
            "stops": stops,
            "tags": [{
                "descriptor": {"code": "TICKET_INFO"},
                "list": [{"descriptor": {"code": "NUMBER"}, "value": ticket_number}],
            }],
        })
        resp_items.append({
            "id": item_id,
            "descriptor": {"code": desc_code, "name": (it.get("descriptor") or {}).get("name", "Single Journey Ticket")},
            "price": {"value": price_val, "currency": currency},
            "quantity": {"selected": {"count": int(qty)}},
            "fulfillment_ids": [fid],
        })
        breakup.append({
            "title": "BASE_FARE",
            "item": {"id": item_id},
            "price": {"value": price_val, "currency": currency},
        })

    total_str = f"{total:.2f}" if total else "10"
    payload = {
        "context": ctx,
        "message": {
            "order": {
                "id": order_id,
                "status": "CONFIRMED",
                "provider": order.get("provider") or {"id": provider_id or "mock-provider"},
                "items": resp_items,
                "fulfillments": fulfillments,
                "payments": [{
                    "id": str(uuid.uuid4()),
                    "collected_by": "BAP",
                    "type": "PRE-ORDER",
                    "status": "PAID",
                    "params": {
                        "amount": total_str,
                        "currency": "INR",
                    },
                }],
                "quote": {
                    "price": {"value": total_str, "currency": "INR"},
                    "breakup": breakup,
                },
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
