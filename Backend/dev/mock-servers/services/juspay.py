"""Juspay payment gateway mock — orders, offers, payouts, mandates.

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
import uuid
from datetime import datetime, timezone
from status_store import add_override, deep_merge


def handle(handler, path, body):
    """Route Juspay requests."""
    path_lower = path.lower()
    path_parts = path.strip("/").split("/")

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
        "payment_links": {"web": f"http://localhost:8080/juspay/pay/{order_id}"},
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
                "returnUrl": "http://localhost:8080/juspay/return",
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
        "CANCELLED": 30,
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
