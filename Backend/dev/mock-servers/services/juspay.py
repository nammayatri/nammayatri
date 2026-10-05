"""Juspay payment gateway mock — orders, offers, payouts, mandates.

Also absorbs the retired Haskell mock-payment service (app/mocks/payment):

  POST /payment/external/{merchantShortId}/service/juspay/payment?city=..&serviceType=..
      Body: Lib.Payment PaymentStatusResp (tag == "PaymentStatus"). Builds a
      Juspay-style WebhookReq (enriched from atlas_app.payment_order /
      payment_transaction when the dev DB is reachable) and POSTs it to
      {JUSPAY_WEBHOOK_BASE_URL}/{merchantShortId}/service/juspay/payment with
      the same Basic auth header the Haskell mock used, relaying the response.
  GET /payment/internal/orders/{orderShortId}/status
      Returns Juspay OrderData built from atlas_app.payment_order +
      payment_transaction + refunds + payment_order_offer.

The Haskell service listened on :8091; server.py now binds that port with
juspay as the default service, so old URLs keep working verbatim.

The `response` dict from POST /mock/override is deep-merged into responses.
This lets test collections override any response field:

  POST /mock/override {
    "service": "juspay",
    "extract": "path.2",
    "value": "order-123",
    "match": "/orders",
    "response": {
      "status": "CHARGED",
      "amount": 150.0,
      "refunds": [{"id": "re_1", "amount": 50, "status": "succeeded"}],
      "offers": [{"offer_id": "FLAT50", "offer_code": "FLAT50", "status": "ELIGIBLE"}]
    }
  }

Fields in `response` are merged into the default response — overriding status,
adding refunds/offers blocks, or changing any field the test needs.
"""

import json
import os
import re
import uuid
from datetime import datetime, timezone
from status_store import add_override, deep_merge

from ._env import MOCK_SERVER_PORT

# Where the webhook built by POST /payment/external/... is delivered.
# Same default as the retired Haskell mock's dhall config
# (dhall-configs/dev/mock-payment.dhall: juspayWebhookBaseUrl).
JUSPAY_WEBHOOK_BASE_URL = os.environ.get(
    "JUSPAY_WEBHOOK_BASE_URL", "https://api.sandbox.moving.tech/app")

_INTERNAL_STATUS_RE = re.compile(r"/payment/internal/orders/([^/]+)/status/?$")


def handle(handler, path, body):
    """Route Juspay requests."""
    path_lower = path.lower()
    path_parts = path.strip("/").split("/")

    # ── Haskell mock-payment parity routes (must match before the generic
    #    "orders"/"offer" branches below) ──
    if "/payment/external/" in path and handler.command == "POST":
        return _external_payment(handler, path, body)
    m = _INTERNAL_STATUS_RE.search(path)
    if m and handler.command == "GET":
        return _internal_order_status(handler, m.group(1))

    order_id_from_path = None
    for i, part in enumerate(path_parts):
        if part == "orders" and i + 1 < len(path_parts):
            order_id_from_path = path_parts[i + 1]
            break

    order_id_from_body = None
    order_short_id_from_body = None
    if body:
        try:
            req = json.loads(body)
            order_id_from_body = req.get("order_id") or req.get("orderId")
            order_short_id_from_body = req.get("orderShortId") or req.get("order_short_id")
        except (json.JSONDecodeError, AttributeError):
            pass

    # ── Offers (must match before "order" since path contains /juspay/) ──
    if "offer" in path_lower:
        return _offer(handler, path_lower, body)

    # ── Refund: POST /orders/{orderId}/refunds ──
    # When rider-app calls refund, auto-update the order status to include refund data
    if "refund" in path_lower and order_id_from_path and handler.command == "POST":
        return _refund(handler, order_id_from_path, body)

    # ── Order status: GET /orders/{orderId} ──
    if order_id_from_path and handler.command == "GET":
        return _order_data(handler, order_id_from_path)

    # ── Create order / session: POST ──
    if handler.command == "POST" and ("order" in path_lower or "session" in path_lower):
        oid = order_id_from_body or f"mock-order-{uuid.uuid4().hex[:8]}"
        short_id = order_short_id_from_body or f"mock-short-{uuid.uuid4().hex[:6]}"
        return _create_order(handler, oid, short_id)

    # ── Payout / fulfillment ──
    if "payout" in path_lower or "fulfillment" in path_lower:
        handler._json({"status": "SUCCESS", "fulfillmentId": "mock-fulfill-123"})
        return

    # ── Mandate ──
    if "mandate" in path_lower:
        handler._json({"status": "ACTIVE", "mandate_id": "mock-mandate-123"})
        return

    # ── Fallback: order data if we have an ID ──
    if order_id_from_path:
        return _order_data(handler, order_id_from_path)

    handler._json({"status": "SUCCESS"})


def _refund(handler, order_id, body):
    """Handle POST /orders/{orderId}/refunds.

    Installs a /mock/override rule keyed on path.2 == order_id so that subsequent
    GET /juspay/orders/{order_id} returns the refund in the response. Returns
    AutoRefundResp. Multiple refunds for the same order accumulate because each
    override entry remains in the rule list and check_overrides deep-merges all
    matches; the latest refunds array wins.
    """
    from urllib.parse import unquote_plus
    from status_store import list_overrides

    params = {}
    if body:
        text = body.decode("utf-8") if isinstance(body, bytes) else body
        try:
            params = json.loads(text)
        except (json.JSONDecodeError, ValueError):
            for pair in text.split("&"):
                if "=" in pair:
                    k, v = pair.split("=", 1)
                    params[unquote_plus(k)] = unquote_plus(v)

    amount = float(params.get("amount", 0))
    unique_request_id = params.get("unique_request_id", f"ref-{uuid.uuid4().hex[:8]}")

    # Pull any previously-installed refunds for this order from active overrides
    refunds = []
    existing_status = "CHARGED"
    for o in list_overrides():
        if (o["service"] == "juspay" and o["extract"] == "path.2"
                and o["value"] == str(order_id)):
            resp = o.get("response") or {}
            if resp.get("refunds"):
                refunds = list(resp["refunds"])
            if resp.get("status"):
                existing_status = resp["status"]

    refund_entry = {
        "id": f"rfnd-{uuid.uuid4().hex[:12]}",
        "amount": amount,
        "status": "REFUND_PENDING",
        "error_message": None,
        "error_code": None,
        "initiated_by": "merchant",
        "unique_request_id": unique_request_id,
        "arn": None,
    }
    refunds.append(refund_entry)
    amount_refunded = sum(r.get("amount", 0) for r in refunds)

    add_override(
        "juspay", "path.2", order_id,
        {
            "status": existing_status,
            "refunds": refunds,
            "amount_refunded": amount_refunded,
        },
        match="/orders",
    )

    handler._json({
        "order_id": order_id,
        "merchant_id": "nammayatri",
        "customer_id": "mock-customer",
        "currency": "INR",
        "amount_refunded": amount_refunded,
        "refunds": refunds,
    })


def _offer(handler, path_lower, body):
    """Handle offer_list, offer_apply, offer_notify endpoints."""
    if "offer_list" in path_lower or "list" in path_lower:
        # OfferListResp: {best_offer_combinations: [], offers: []}
        handler._json({
            "best_offer_combinations": [],
            "offers": [],
        })
        return

    if "apply" in path_lower:
        # OfferApplyResp: {offers: []}
        handler._json({"offers": []})
        return

    if "notify" in path_lower:
        # OfferNotifyResp
        handler._json({"code": "SUCCESS", "status": "SUCCESS", "response": "OK"})
        return

    handler._json({"best_offer_combinations": [], "offers": []})


def _create_order(handler, order_id, short_id):
    handler._json({
        "id": short_id,
        "order_id": order_id,
        "status": "NEW",
        "status_id": 10,
        "amount": 0.0,
        "currency": "INR",
        "payment_links": {"web": f"http://localhost:{MOCK_SERVER_PORT}/juspay/pay/{order_id}"},
        "sdk_payload": {
            "requestId": order_id,
            "service": "in.juspay.nammayatri",
            "payload": {
                "clientId": "nammayatri",
                "amount": "0",
                "merchantId": "nammayatri",
                "clientAuthToken": f"mock-auth-{uuid.uuid4().hex[:8]}",
                "clientAuthTokenExpiry": "2027-01-01T00:00:00Z",
                "environment": "sandbox",
                "currency": "INR",
                "firstName": "Test",
                "lastName": "User",
                "customerId": "test-customer",
                "returnUrl": f"http://localhost:{MOCK_SERVER_PORT}/juspay/return",
                "orderId": order_id,
            }
        },
    })


def _order_data(handler, order_id):
    """Return OrderData. The `data` dict from the status store is deep-merged
    into the default response, so test collections can override any field."""
    override_status, extra = handler._get_override("juspay", order_id)
    status = override_status or "NEW"

    status_id_map = {
        "NEW": 10, "PENDING_VBV": 20, "CHARGED": 21,
        "AUTHENTICATION_FAILED": 22, "AUTHORIZATION_FAILED": 23,
        "JUSPAY_DECLINED": 24, "AUTHORIZING": 25, "COD_INITIATED": 26,
        "STARTED": 27, "AUTO_REFUNDED": 28, "CLIENT_AUTH_TOKEN_EXPIRED": 29,
        "CANCELLED": 30, "PARTIAL_CHARGED": 31,
    }
    event_map = {
        "CHARGED": "ORDER_SUCCEEDED",
        "AUTO_REFUNDED": "ORDER_REFUNDED",
        "AUTHENTICATION_FAILED": "ORDER_FAILED",
        "AUTHORIZATION_FAILED": "ORDER_FAILED",
        "JUSPAY_DECLINED": "ORDER_FAILED",
    }
    now = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

    base = {
        "order_id": order_id,
        "txn_uuid": f"mock-txn-{uuid.uuid4().hex[:8]}",
        "txn_id": f"mock-txn-{uuid.uuid4().hex[:8]}",
        "status_id": status_id_map.get(status, 10),
        "status": status,
        "event_name": event_map.get(status),
        "amount": 0.0,
        "currency": "INR",
        "date_created": now,
        "payment_method_type": None,
        "payment_method": None,
        "resp_message": None,
        "resp_code": None,
        "gateway_reference_id": None,
        "payer_vpa": None,
        "bank_error_code": None,
        "bank_error_message": None,
        "mandate": None,
        "upi": None,
        "payment_gateway_response": None,
        "refunds": None,
        "offers": None,
    }

    # Deep-merge extra data from the status store override
    if extra:
        base = deep_merge(base, extra)

    handler._json(base)


# ═══════════════════════════════════════════════════════════════════════════
# Haskell mock-payment parity (app/mocks/payment — retired)
#
# JSON shapes mirror the aeson encodings exactly:
#   WebhookReq / OrderData  — Kernel.External.Payment.Juspay.Types (snake_case
#       fields, Nothing → null; aeson default options do NOT omit nulls)
#   PaymentStatusResp       — Lib.Payment.Domain.Action (generic TaggedObject:
#       {"tag": "PaymentStatus", ...camelCase fields...})
# ═══════════════════════════════════════════════════════════════════════════

# Haskell statusToId (Handler.hs)
_STATUS_ID = {
    "NEW": 10, "PENDING_VBV": 20, "CHARGED": 21,
    "AUTHENTICATION_FAILED": 22, "AUTHORIZATION_FAILED": 23,
    "JUSPAY_DECLINED": 24, "AUTHORIZING": 25, "COD_INITIATED": 26,
    "STARTED": 27, "AUTO_REFUNDED": 28, "CLIENT_AUTH_TOKEN_EXPIRED": 29,
    "CANCELLED": 30, "PARTIAL_CHARGED": 31,
}

# Interface RefundStatus (JSON string) → Juspay RefundStatus (JSON string).
# Mirrors toJuspayRefundStatus + the Juspay ToJSON instance
# (REFUND_PENDING → "PENDING", REFUND_CANCELED → REFUND_FAILURE → "FAILURE", …).
_REFUND_STATUS_TO_JUSPAY = {
    "PENDING": "PENDING",
    "FAILURE": "FAILURE",
    "SUCCESS": "SUCCESS",
    "MANUAL_REVIEW": "MANUAL_REVIEW",
    "REFUND_CANCELED": "FAILURE",
    "REFUND_REQUIRES_ACTION": "PENDING",
}

# refunds.status DB column (Show form) → Juspay RefundStatus JSON string
_DB_REFUND_STATUS_TO_JUSPAY = {
    "REFUND_PENDING": "PENDING",
    "REFUND_FAILURE": "FAILURE",
    "REFUND_SUCCESS": "SUCCESS",
    "MANUAL_REVIEW": "MANUAL_REVIEW",
    "REFUND_CANCELED": "FAILURE",
    "REFUND_REQUIRES_ACTION": "PENDING",
}

# payment_order_offer.status DB column (OfferState Show form) → JSON string
# (custom ToJSON: OFFER_AVAILED → "AVAILED", …)
_DB_OFFER_STATE_TO_JSON = {
    "OFFER_INITIATED": "INITIATED",
    "OFFER_AVAILED": "AVAILED",
    "OFFER_REFUNDED": "REFUNDED",
    "OFFER_FAILED": "FAILED",
}

_PGR_FIELDS = ("resp_code", "rrn", "created", "epg_txn_id", "resp_message",
               "auth_id_code", "payer_vpa", "txn_flow_type")


def _status_to_payment_status(status):
    """Haskell statusToPaymentStatus: CHARGED → ORDER_SUCCEEDED,
    AUTO_REFUNDED → ORDER_REFUNDED, everything else → ORDER_FAILED."""
    if status == "CHARGED":
        return "ORDER_SUCCEEDED"
    if status == "AUTO_REFUNDED":
        return "ORDER_REFUNDED"
    return "ORDER_FAILED"


def _iso_utc(dt=None):
    """aeson-style UTCTime: 2026-10-05T12:34:56.000000Z"""
    if dt is None:
        dt = datetime.now(timezone.utc)
    if dt.tzinfo is not None:
        dt = dt.astimezone(timezone.utc).replace(tzinfo=None)
    return dt.strftime("%Y-%m-%dT%H:%M:%S.") + f"{dt.microsecond:06d}Z"


def _num(v):
    return float(v) if v is not None else None


def _parse_pgr(text_or_obj):
    """Replicates parsePaymentGatewayResponse: decode stored juspay_response
    into Juspay.PaymentGatewayResponse (all-Maybe record → any object parses;
    re-encoding emits all 8 keys, missing → null)."""
    obj = text_or_obj
    if isinstance(obj, (bytes, str)):
        try:
            obj = json.loads(obj)
        except (json.JSONDecodeError, ValueError, UnicodeDecodeError):
            return None
    if not isinstance(obj, dict):
        return None
    return {k: obj.get(k) for k in _PGR_FIELDS}


def _split_settlement_to_juspay(obj):
    """Interface SplitSettlementResponse (camelCase, as stored on
    payment_transaction.split_settlement_response) → Juspay snake_case."""
    if isinstance(obj, (bytes, str)):
        try:
            obj = json.loads(obj)
        except (json.JSONDecodeError, ValueError, UnicodeDecodeError):
            return None
    if not isinstance(obj, dict):
        return None
    details = obj.get("splitDetails")
    out_details = None
    if isinstance(details, list):
        out_details = [{
            "sub_vendor_id": d.get("subVendorId"),
            "amount": d.get("amount"),
            "merchant_commission": d.get("merchantCommission"),
            "gateway_sub_account_id": d.get("gatewaySubAccountId"),
            "epg_txn_id": d.get("epgTxnId"),
            "unique_split_id": d.get("uniqueSplitId"),
        } for d in details if isinstance(d, dict)]
    return {"split_details": out_details, "split_applied": obj.get("splitApplied")}


def _card_to_juspay(card):
    """Interface CardInfo (camelCase) → Juspay CardInfo (snake_case)."""
    if not isinstance(card, dict):
        return None
    return {
        "card_type": card.get("cardType"),
        "last_four_digits": card.get("lastFourDigits"),
        "name_on_card": card.get("nameOnCard"),
        "card_brand": card.get("cardBrand"),
        "card_isin": card.get("cardIsin"),
        "card_issuer": card.get("cardIssuer"),
    }


def _refund_to_juspay(r):
    """Interface RefundsData (camelCase) → Juspay RefundsData (snake_case)."""
    return {
        "id": r.get("idAssignedByServiceProvider"),
        "amount": _num(r.get("amount")) or 0.0,
        "status": _REFUND_STATUS_TO_JUSPAY.get(r.get("status"), "PENDING"),
        "error_message": r.get("errorMessage"),
        "error_code": r.get("errorCode"),
        "initiated_by": r.get("initiatedBy"),
        "unique_request_id": r.get("requestId"),
        "arn": r.get("arn"),
    }


def _offer_to_juspay(o):
    """Interface Offer (camelCase) → Juspay Offer (snake_case).
    The OfferState JSON string is identical on both sides — pass through."""
    return {
        "offer_id": o.get("offerId"),
        "offer_code": o.get("offerCode"),
        "status": o.get("status"),
    }


def _pg_conn():
    """Connect to the dev Postgres the Haskell mock read (atlas_app schema).
    Same env knobs as server.py's /mock/sql endpoints."""
    import psycopg2
    kwargs = {
        "dbname": os.environ.get("MOCK_SQL_DB", "atlas_dev"),
        "host": os.environ.get("MOCK_SQL_HOST", "localhost"),
        "port": int(os.environ.get("MOCK_SQL_PORT", os.environ.get("DB_PRIMARY_PORT", "5434"))),
        "user": os.environ.get("MOCK_SQL_USER", "atlas_superuser"),
    }
    if os.environ.get("MOCK_SQL_PASSWORD"):
        kwargs["password"] = os.environ["MOCK_SQL_PASSWORD"]
    return psycopg2.connect(**kwargs)


def _fetch_order_and_txn(cur, order_short_id):
    """payment_order by short_id + its newest 'new' transaction
    (txn_uuid IS NULL, mirroring findNewTransactionByOrderId)."""
    cur.execute(
        "SELECT id, created_at, status, amount, bank_error_code,"
        "       bank_error_message, effect_amount"
        "  FROM atlas_app.payment_order WHERE short_id = %s LIMIT 1",
        (order_short_id,))
    row = cur.fetchone()
    if row is None:
        return None, None
    order = dict(zip(
        ("id", "created_at", "status", "amount", "bank_error_code",
         "bank_error_message", "effect_amount"), row))
    cur.execute(
        "SELECT txn_id, txn_uuid, payment_method_type, payment_method,"
        "       resp_message, resp_code, gateway_reference_id,"
        "       juspay_response, split_settlement_response"
        "  FROM atlas_app.payment_transaction"
        " WHERE order_id = %s AND txn_uuid IS NULL"
        " ORDER BY created_at DESC LIMIT 1",
        (order["id"],))
    trow = cur.fetchone()
    txn = None
    if trow is not None:
        txn = dict(zip(
            ("txn_id", "txn_uuid", "payment_method_type", "payment_method",
             "resp_message", "resp_code", "gateway_reference_id",
             "juspay_response", "split_settlement_response"), trow))
    return order, txn


def _external_payment(handler, path, body):
    """POST /payment/external/{merchantShortId}/service/juspay/payment

    Port of Handler.externalPaymentHandler: parse PaymentStatusResp, enrich
    from the payment DB (best-effort), build the Juspay WebhookReq, POST it to
    the configured webhook target, and relay the response."""
    from urllib.parse import urlparse as _urlparse, parse_qs as _parse_qs
    import urllib.request
    import urllib.error

    parts = path.strip("/").split("/")
    try:
        merchant_short_id = parts[parts.index("external") + 1]
    except (ValueError, IndexError):
        return handler._json(
            {"errorCode": "INTERNAL_ERROR",
             "errorMessage": "merchantShortId missing in path"}, status=500)

    qs = _parse_qs(_urlparse(handler.path).query)
    city = (qs.get("city") or [None])[0]
    service_type = (qs.get("serviceType") or [None])[0]

    try:
        req = json.loads(body) if body else {}
    except (json.JSONDecodeError, ValueError):
        return handler._json(
            {"errorCode": "INVALID_REQUEST", "errorMessage": "invalid JSON"},
            status=400)

    if req.get("tag") != "PaymentStatus":
        # Haskell: throwError $ InternalError "Expected PaymentStatus constructor"
        return handler._json(
            {"errorCode": "INTERNAL_ERROR",
             "errorMessage": "Expected PaymentStatus constructor"}, status=500)

    order_short_id = req.get("orderShortId")
    status = req.get("status")
    refunds_in = req.get("refunds") or []

    # ── DB enrichment (best-effort — the Haskell mock read the same rows) ──
    order = txn = None
    try:
        conn = _pg_conn()
        try:
            cur = conn.cursor()
            order, txn = _fetch_order_and_txn(cur, order_short_id)
            cur.close()
        finally:
            conn.close()
    except Exception:
        pass  # DB unreachable → webhook still built from the request body

    amount_refunded = sum(
        _num(r.get("amount")) or 0.0
        for r in refunds_in if r.get("status") == "SUCCESS")

    order_data = {
        "id": None,
        "order_id": order_short_id,
        "txn_uuid": req.get("txnUUID"),
        "txn_id": req.get("txnId"),
        "status_id": _STATUS_ID.get(status),
        "event_name": _status_to_payment_status(status),
        "status": status,
        "payment_method_type": req.get("paymentMethodType"),
        "payment_method": (txn or {}).get("payment_method"),
        "payment_gateway_response": _parse_pgr((txn or {}).get("juspay_response")),
        "resp_message": (txn or {}).get("resp_message"),
        "resp_code": (txn or {}).get("resp_code"),
        "gateway_reference_id": (txn or {}).get("gateway_reference_id"),
        "amount": _num(req.get("amount")) or 0.0,
        "currency": "INR",
        "date_created": _iso_utc(order["created_at"]) if order else None,
        "mandate": None,
        "payer_vpa": req.get("payerVpa"),
        "bank_error_code": req.get("bankErrorCode"),
        "bank_error_message": req.get("bankErrorMessage"),
        "upi": None,
        "card": _card_to_juspay(req.get("card")),
        "metadata": None,
        "additional_info": None,
        "links": None,
        "amount_refunded": amount_refunded,
        "refunds": [_refund_to_juspay(r) for r in refunds_in],
        "split_settlement_response": _split_settlement_to_juspay(
            (txn or {}).get("split_settlement_response")),
        "effective_amount": _num(req.get("effectAmount")),
        "offers": ([_offer_to_juspay(o) for o in req["offers"]]
                   if req.get("offers") is not None else None),
        "txn_detail": None,
        "loyalty_info": None,
        "txn_list": None,
    }

    webhook_payload = {
        "id": "evt_" + str(uuid.uuid4()),
        "date_created": _iso_utc(),
        "event_name": _status_to_payment_status(status),
        "content": {
            "order": order_data,
            "mandate": None,
            "notification": None,
            "txn": None,
        },
    }

    webhook_url = (f"{JUSPAY_WEBHOOK_BASE_URL.rstrip('/')}/"
                   f"{merchant_short_id}/service/juspay/payment")
    sep = "?"
    if city:
        webhook_url += f"?city={city}"
        sep = "&"
    if service_type:
        # Haskell wraps serviceType in %22 quotes (JSON-encoded query param)
        webhook_url += f"{sep}serviceType=%22{service_type}%22"

    wreq = urllib.request.Request(
        webhook_url,
        data=json.dumps(webhook_payload).encode("utf-8"),
        headers={
            "Content-Type": "application/json",
            "Authorization": "Basic Y3VtdGE6Y3VtdGFAMTIz",
        },
        method="POST",
    )
    try:
        with urllib.request.urlopen(wreq, timeout=60) as resp:
            resp_body = resp.read()
        try:
            return handler._json(json.loads(resp_body))
        except (json.JSONDecodeError, ValueError):
            # Haskell falls back to an Ack when the response isn't JSON
            return handler._json({"message": {"ack": {"status": "ACK"}}})
    except urllib.error.HTTPError as e:
        # Relay the webhook target's error response verbatim
        err_body = e.read()
        try:
            return handler._json(json.loads(err_body), status=e.code)
        except (json.JSONDecodeError, ValueError):
            return handler._raw(e.code, "application/json",
                                err_body or b'{"error": "webhook call failed"}')
    except Exception as e:
        return handler._json(
            {"errorCode": "INTERNAL_ERROR",
             "errorMessage": f"webhook call failed: {e}"}, status=500)


def _internal_order_status(handler, order_short_id):
    """GET /payment/internal/orders/{orderShortId}/status

    Port of Handler.internalOrderStatusHandler: build Juspay OrderData from
    atlas_app.payment_order + payment_transaction + refunds +
    payment_order_offer."""
    try:
        conn = _pg_conn()
    except Exception as e:
        return handler._json(
            {"errorCode": "INTERNAL_ERROR",
             "errorMessage": f"payment DB unreachable: {e}"}, status=500)
    try:
        cur = conn.cursor()
        order, txn = _fetch_order_and_txn(cur, order_short_id)
        if order is None:
            return handler._json(
                {"errorCode": "INTERNAL_ERROR",
                 "errorMessage": f"Order not found: {order_short_id}"},
                status=500)

        # refunds are keyed by the order SHORT id (HQRefunds.findAllByOrderId)
        cur.execute(
            "SELECT id_assigned_by_service_provider, refund_amount, status,"
            "       error_message, error_code, initiated_by, short_id, arn"
            "  FROM atlas_app.refunds WHERE order_id = %s",
            (order_short_id,))
        refund_rows = cur.fetchall()

        cur.execute(
            "SELECT offer_id, offer_code, status"
            "  FROM atlas_app.payment_order_offer WHERE payment_order_id = %s",
            (order["id"],))
        offer_rows = cur.fetchall()
        cur.close()
    except Exception as e:
        return handler._json(
            {"errorCode": "INTERNAL_ERROR",
             "errorMessage": f"payment DB error: {e}"}, status=500)
    finally:
        conn.close()

    refunds = [{
        "id": r[0],
        "amount": _num(r[1]) or 0.0,
        "status": _DB_REFUND_STATUS_TO_JUSPAY.get(r[2], "PENDING"),
        "error_message": r[3],
        "error_code": r[4],
        "initiated_by": r[5],
        "unique_request_id": r[6],
        "arn": r[7],
    } for r in refund_rows]
    total_refunded = sum(
        _num(r[1]) or 0.0 for r in refund_rows if r[2] == "REFUND_SUCCESS")

    offers = [{
        "offer_id": o[0],
        "offer_code": o[1],
        "status": _DB_OFFER_STATE_TO_JSON.get(o[2], o[2]),
    } for o in offer_rows]

    status = order["status"]
    base = {
        "id": None,
        "order_id": order_short_id,
        "txn_uuid": (txn or {}).get("txn_uuid"),
        "txn_id": (txn or {}).get("txn_id"),
        "status_id": _STATUS_ID.get(status),
        "event_name": _status_to_payment_status(status),
        "status": status,
        "payment_method_type": (txn or {}).get("payment_method_type"),
        "payment_method": (txn or {}).get("payment_method"),
        "payment_gateway_response": _parse_pgr((txn or {}).get("juspay_response")),
        "resp_message": (txn or {}).get("resp_message"),
        "resp_code": (txn or {}).get("resp_code"),
        "gateway_reference_id": (txn or {}).get("gateway_reference_id"),
        "amount": _num(order["amount"]) or 0.0,
        "currency": "INR",
        "date_created": _iso_utc(order["created_at"]) if order.get("created_at") else None,
        "mandate": None,
        # Haskell quirk preserved: payer_vpa is filled from paymentMethod
        "payer_vpa": (txn or {}).get("payment_method"),
        "bank_error_code": order.get("bank_error_code"),
        "bank_error_message": order.get("bank_error_message"),
        "upi": None,
        "card": None,
        "metadata": None,
        "additional_info": None,
        "links": None,
        "amount_refunded": total_refunded,
        "refunds": refunds,
        "split_settlement_response": _split_settlement_to_juspay(
            (txn or {}).get("split_settlement_response")),
        "effective_amount": _num(order.get("effect_amount")),
        "offers": offers,
        "txn_detail": None,
        "loyalty_info": None,
        "txn_list": None,
    }

    # Allow /mock/override tampering, same as the plain order-status route
    override_status, extra = handler._get_override("juspay", order_short_id)
    if override_status:
        base["status"] = override_status
        base["status_id"] = _STATUS_ID.get(override_status)
        base["event_name"] = _status_to_payment_status(override_status)
    if extra:
        base = deep_merge(base, extra)

    handler._json(base)
