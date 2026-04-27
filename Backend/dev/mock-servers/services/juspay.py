"""Juspay payment gateway mock — orders, offers, payouts, mandates.

The `data` dict from POST /mock/status is deep-merged into responses.
This lets test collections override any response field:

  POST /mock/status {
    "service": "juspay",
    "id": ["order-123"],
    "status": "CHARGED",
    "data": {
      "amount": 150.0,
      "refunds": [{"id": "re_1", "amount": 50, "status": "succeeded"}],
      "offers": [{"offer_id": "FLAT50", "offer_code": "FLAT50", "status": "ELIGIBLE"}]
    }
  }

Fields in `data` are merged into the default response — overriding status, adding
refunds/offers blocks, or changing any field the test needs.
"""

import json
import uuid
from datetime import datetime, timezone
from status_store import deep_merge


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

    # ── Loyalty: POST /loyalty/programs (must match before "order" check) ──
    if "loyalty" in path_lower:
        return _loyalty_info(handler, body)

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
        from status_store import set_status
        oid = order_id_from_body or f"mock-order-{uuid.uuid4().hex[:8]}"
        short_id = order_short_id_from_body or f"mock-short-{uuid.uuid4().hex[:6]}"
        ids = list({oid, short_id})
        set_status("juspay", ids, "NEW")
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
    Auto-updates the order's status store to include refund data with REFUND_PENDING.
    Subsequent GET /orders/{orderId} will return the refund in the response.
    Returns AutoRefundResp.
    """
    from status_store import set_status, get_status
    from urllib.parse import unquote_plus

    # Parse form-urlencoded body (Juspay SDK sends form data)
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

    # Update the existing order entry to include refund data
    existing = get_status("juspay", order_id)
    existing_data = existing.get("data", {}) if existing else {}

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

    # Merge refund into existing data
    refunds = existing_data.get("refunds", []) or []
    refunds.append(refund_entry)
    existing_data["refunds"] = refunds
    existing_data["amount_refunded"] = sum(r.get("amount", 0) for r in refunds)

    set_status("juspay", order_id, existing.get("status", "CHARGED") if existing else "CHARGED", existing_data)

    # Return AutoRefundResp
    handler._json({
        "order_id": order_id,
        "merchant_id": "nammayatri",
        "customer_id": "mock-customer",
        "currency": "INR",
        "amount_refunded": existing_data["amount_refunded"],
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
        "id": order_id,
        "order_id": short_id,
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


_DEFAULT_LOYALTY_INFO = {
    "customer_id": "mock-customer",
    "programs": [
        {
            "id": "019d9617-abeb-7a92-ac58-0d58052508c4",
            "code": "LOYALTYOS7",
            "burn": {
                "options": [
                    {
                        "id": "019d9be5-75cc-7fe1-949d-9edbc5d2374d",
                        "type": "ORDER_DISCOUNT",
                        "status": "ELIGIBLE",
                        "increment": "1.00",
                        "partial_allowed": False,
                        "applicable": {
                            "label": "Redeem Points",
                            "max_points": "10000.00",
                            "max_value": "10000.00",
                            "min_points": "10000.00",
                            "rate": {"points": "1", "value": "1"}
                        }
                    }
                ]
            },
            "earn": {
                "status": "ELIGIBLE",
                "applicable": {"label": "Earn 100 points from this order", "points": "100.00"}
            },
            "campaigns": [],
            "membership": {"enroll_on": "AUTO", "enrolled_date": "2026-04-16T11:42:19Z", "status": "ACTIVE"},
            "topup": {"status": "ELIGIBLE"},
            "ui_label": {
                "asset": {"code": "POINTS", "name": "Points"},
                "description": "Earn and redeem points at 1:1 ratio",
                "title": "Loyalty Points Program",
                "tnc": "Points are valid for 12 months from date of credit",
                "tnc_url": "https://example.com/tnc"
            },
            "wallet": {
                "available_points": "15872.00",
                "lifetime_earned": "23120.00",
                "lifetime_redeemed": "8348.00",
                "expiring": {
                    "points": "200.00",
                    "schedule": [
                        {"date": "2027-04-20T13:18:51+00:00", "points": "100.00"},
                        {"date": "2027-04-20T13:20:26+00:00", "points": "100.00"}
                    ]
                },
                "pending_release": {"points": "-1100.00", "schedule": []},
                "pockets": [
                    {"label": "REWARD Points", "available_points": "1400.00",  "lifetime_earned": "4400.00",  "lifetime_redeemed": "1000.00"},
                    {"label": "TOPUP Points",  "available_points": "11372.00", "lifetime_earned": "18720.00", "lifetime_redeemed": "7348.00"}
                ]
            }
        }
    ]
}


def _loyalty_info(handler, body):
    """Handle POST /loyalty/programs.
    Returns the default LoyaltyInfoResponse, deep-merged with any override data
    set via /mock/status with id="loyalty" or service-level extract rules."""
    customer_id = None
    if body:
        try:
            req = json.loads(body)
            customer_id = req.get("customer_id") or req.get("customerId")
        except (json.JSONDecodeError, AttributeError):
            pass

    _, extra = handler._get_override("juspay", "loyalty", customer_id)

    resp = json.loads(json.dumps(_DEFAULT_LOYALTY_INFO))  # deep copy
    if customer_id:
        resp["customer_id"] = customer_id
    if extra:
        resp = deep_merge(resp, extra)
    handler._json(resp)
