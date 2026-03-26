#!/usr/bin/env python3
"""
Mock Stripe Server

Simulates Stripe API endpoints for local testing.
All endpoints accept form-urlencoded (like real Stripe) and return JSON.

Supports configurable behaviors:
  - pm_card_visa           → succeeds immediately (requires_capture)
  - pm_card_authRequired   → returns requires_action (3DS)
  - pm_card_declined       → returns card_declined error
  - pm_card_insufficient   → returns insufficient_funds error

Endpoints implemented (matching shared-kernel Stripe/Flow.hs):
  POST /v1/customers                          → create customer
  GET  /v1/customers/:id                      → get customer
  POST /v1/payment_intents                    → create payment intent
  GET  /v1/payment_intents/:id                → get payment intent
  POST /v1/payment_intents/:id/confirm        → confirm payment intent
  POST /v1/payment_intents/:id/capture        → capture payment intent
  POST /v1/payment_intents/:id/cancel         → cancel payment intent
  POST /v1/setup_intents                      → create setup intent
  POST /v1/ephemeral_keys                     → create ephemeral keys
  GET  /v1/customers/:id/payment_methods      → list payment methods
  POST /v1/payment_methods/:id/detach         → detach payment method
  POST /v1/refunds                            → create refund
  GET  /v1/refunds/:id                        → get refund
  POST /v1/accounts                           → create connect account
  POST /v1/account_links                      → create account link
  GET  /v1/accounts/:id                       → get account

Usage:
  python server.py              # Start on port 9091
  python server.py --port 9092  # Custom port
"""

import json
import uuid
import time
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse, parse_qs, unquote_plus
import sys

PORT = 7081

# ---------------------------------------------------------------------------
# In-memory state
# ---------------------------------------------------------------------------
customers = {}
payment_intents = {}
setup_intents = {}
refunds = {}
accounts = {}
payment_methods = {
    "pm_card_visa": {"id": "pm_card_visa", "type": "card", "card": {"brand": "visa", "last4": "4242", "exp_month": 12, "exp_year": 2030}},
    "pm_card_mastercard": {"id": "pm_card_mastercard", "type": "card", "card": {"brand": "mastercard", "last4": "5556", "exp_month": 6, "exp_year": 2029}},
    "pm_card_authRequired": {"id": "pm_card_authRequired", "type": "card", "card": {"brand": "visa", "last4": "3220", "exp_month": 12, "exp_year": 2030}},
    "pm_card_declined": {"id": "pm_card_declined", "type": "card", "card": {"brand": "visa", "last4": "0002", "exp_month": 12, "exp_year": 2030}},
    "pm_card_insufficient": {"id": "pm_card_insufficient", "type": "card", "card": {"brand": "visa", "last4": "9995", "exp_month": 12, "exp_year": 2030}},
}


def gen_id(prefix):
    return f"{prefix}_{uuid.uuid4().hex[:24]}"


def now_ts():
    return int(time.time())


# ---------------------------------------------------------------------------
# Mock Response Builders
# ---------------------------------------------------------------------------
def mk_customer(params):
    cid = gen_id("cus")
    cust = {
        "id": cid, "object": "customer",
        "email": params.get("email", ""),
        "name": params.get("name", ""),
        "phone": params.get("phone", ""),
        "created": now_ts(),
    }
    customers[cid] = cust
    return cust


def mk_payment_intent(params):
    pi_id = gen_id("pi")
    pm_id = params.get("payment_method", "")
    amount = int(params.get("amount", 0))
    currency = params.get("currency", "usd")
    capture_method = params.get("capture_method", "manual")

    # Determine status based on payment method
    status = "requires_capture"  # default for manual capture
    error = None

    if pm_id == "pm_card_authRequired":
        status = "requires_action"
    elif pm_id == "pm_card_declined":
        error = {"type": "card_error", "code": "card_declined", "message": "Your card was declined."}
    elif pm_id == "pm_card_insufficient":
        error = {"type": "card_error", "code": "insufficient_funds", "message": "Your card has insufficient funds."}

    if error:
        return {"error": error}, 402

    pi = {
        "id": pi_id, "object": "payment_intent",
        "amount": amount, "currency": currency,
        "status": status,
        "capture_method": capture_method,
        "payment_method": pm_id,
        "client_secret": f"{pi_id}_secret_{uuid.uuid4().hex[:12]}",
        "customer": params.get("customer", ""),
        "application_fee_amount": int(params.get("application_fee_amount", 0)) if params.get("application_fee_amount") else None,
        "transfer_data": {"destination": params.get("transfer_data[destination]", "")} if params.get("transfer_data[destination]") else None,
        "on_behalf_of": params.get("on_behalf_of", None),
        "metadata": {},
        "created": now_ts(),
    }

    # Parse metadata
    for k, v in params.items():
        if k.startswith("metadata["):
            meta_key = k[9:-1]
            pi["metadata"][meta_key] = v

    payment_intents[pi_id] = pi
    return pi, 200


def mk_capture(pi_id, params):
    pi = payment_intents.get(pi_id)
    if not pi:
        return {"error": {"type": "invalid_request_error", "message": f"No such payment_intent: {pi_id}"}}, 404
    if pi["status"] != "requires_capture":
        return {"error": {"type": "invalid_request_error", "message": f"Cannot capture intent with status: {pi['status']}"}}, 400
    pi["status"] = "succeeded"
    if params.get("amount_to_capture"):
        pi["amount_capturable"] = 0
        pi["amount_received"] = int(params["amount_to_capture"])
    return pi, 200


def mk_cancel(pi_id):
    pi = payment_intents.get(pi_id)
    if not pi:
        return {"error": {"type": "invalid_request_error", "message": f"No such payment_intent: {pi_id}"}}, 404
    pi["status"] = "canceled"
    return pi, 200


def mk_confirm(pi_id, params):
    pi = payment_intents.get(pi_id)
    if not pi:
        return {"error": {"type": "invalid_request_error", "message": f"No such payment_intent: {pi_id}"}}, 404
    pm_id = params.get("payment_method", pi.get("payment_method", ""))
    if pm_id == "pm_card_authRequired":
        pi["status"] = "requires_action"
    elif pm_id in ("pm_card_declined", "pm_card_insufficient"):
        return {"error": {"type": "card_error", "code": "card_declined", "message": "Declined"}}, 402
    else:
        pi["status"] = "requires_capture"
    return pi, 200


def mk_refund(params):
    ref_id = gen_id("re")
    ref = {
        "id": ref_id, "object": "refund",
        "payment_intent": params.get("payment_intent", ""),
        "amount": int(params.get("amount", 0)) if params.get("amount") else None,
        "status": "succeeded",
        "currency": "usd",
        "created": now_ts(),
    }
    refunds[ref_id] = ref
    return ref, 200


def mk_setup_intent(params):
    si_id = gen_id("seti")
    si = {
        "id": si_id, "object": "setup_intent",
        "client_secret": f"{si_id}_secret_{uuid.uuid4().hex[:12]}",
        "status": "requires_payment_method",
        "customer": params.get("customer", ""),
        "created": now_ts(),
    }
    setup_intents[si_id] = si
    return si, 200


def mk_ephemeral_key(params):
    return {
        "id": gen_id("ephkey"),
        "object": "ephemeral_key",
        "secret": f"ek_test_{uuid.uuid4().hex[:24]}",
        "created": now_ts(),
        "expires": now_ts() + 3600,
    }, 200


def mk_account(params):
    acc_id = gen_id("acct")
    acc = {
        "id": acc_id, "object": "account",
        "charges_enabled": False,
        "details_submitted": False,
        "country": params.get("country", "US"),
        "type": params.get("type", "express"),
        "created": now_ts(),
    }
    accounts[acc_id] = acc
    return acc, 200


# ---------------------------------------------------------------------------
# Handler
# ---------------------------------------------------------------------------
class MockStripeHandler(BaseHTTPRequestHandler):
    def log_message(self, format, *args):
        print(f"  \033[96m[Stripe Mock]\033[0m {args[0]}")

    def _parse_form(self):
        content_len = int(self.headers.get("Content-Length", 0))
        body = self.rfile.read(content_len).decode("utf-8") if content_len else ""
        params = {}
        for pair in body.split("&"):
            if "=" in pair:
                k, v = pair.split("=", 1)
                params[unquote_plus(k)] = unquote_plus(v)
        return params

    def _send_json(self, data, status=200):
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(json.dumps(data).encode())

    def _route(self, method, path, params=None):
        parts = [p for p in path.split("/") if p]
        # Strip v1 prefix
        if parts and parts[0] == "v1":
            parts = parts[1:]

        if not parts:
            return self._send_json({"error": "not found"}, 404)

        resource = parts[0]
        params = params or {}

        # POST /v1/customers
        if method == "POST" and resource == "customers" and len(parts) == 1:
            return self._send_json(mk_customer(params))

        # GET /v1/customers/:id
        if method == "GET" and resource == "customers" and len(parts) == 2:
            cid = parts[1]
            cust = customers.get(cid, {"id": cid, "object": "customer", "email": "", "name": "", "created": now_ts()})
            return self._send_json(cust)

        # GET /v1/customers/:id/payment_methods
        if method == "GET" and resource == "customers" and len(parts) >= 3 and parts[2] == "payment_methods":
            return self._send_json({"data": list(payment_methods.values()), "has_more": False})

        # POST /v1/payment_intents
        if method == "POST" and resource == "payment_intents" and len(parts) == 1:
            resp, status = mk_payment_intent(params)
            return self._send_json(resp, status)

        # GET /v1/payment_intents/:id
        if method == "GET" and resource == "payment_intents" and len(parts) == 2:
            pi = payment_intents.get(parts[1])
            if not pi:
                return self._send_json({"error": {"message": "not found"}}, 404)
            return self._send_json(pi)

        # POST /v1/payment_intents/:id/capture
        if method == "POST" and resource == "payment_intents" and len(parts) == 3 and parts[2] == "capture":
            resp, status = mk_capture(parts[1], params)
            return self._send_json(resp, status)

        # POST /v1/payment_intents/:id/cancel
        if method == "POST" and resource == "payment_intents" and len(parts) == 3 and parts[2] == "cancel":
            resp, status = mk_cancel(parts[1])
            return self._send_json(resp, status)

        # POST /v1/payment_intents/:id/confirm
        if method == "POST" and resource == "payment_intents" and len(parts) == 3 and parts[2] == "confirm":
            resp, status = mk_confirm(parts[1], params)
            return self._send_json(resp, status)

        # POST /v1/setup_intents
        if method == "POST" and resource == "setup_intents" and len(parts) == 1:
            resp, status = mk_setup_intent(params)
            return self._send_json(resp, status)

        # POST /v1/ephemeral_keys
        if method == "POST" and resource == "ephemeral_keys" and len(parts) == 1:
            resp, status = mk_ephemeral_key(params)
            return self._send_json(resp, status)

        # POST /v1/payment_methods/:id/detach
        if method == "POST" and resource == "payment_methods" and len(parts) == 3 and parts[2] == "detach":
            pm = payment_methods.pop(parts[1], None)
            return self._send_json(pm or {"id": parts[1], "deleted": True})

        # POST /v1/refunds
        if method == "POST" and resource == "refunds" and len(parts) == 1:
            resp, status = mk_refund(params)
            return self._send_json(resp, status)

        # GET /v1/refunds/:id
        if method == "GET" and resource == "refunds" and len(parts) == 2:
            ref = refunds.get(parts[1])
            if not ref:
                return self._send_json({"error": {"message": "not found"}}, 404)
            return self._send_json(ref)

        # POST /v1/accounts
        if method == "POST" and resource == "accounts" and len(parts) == 1:
            resp, status = mk_account(params)
            return self._send_json(resp, status)

        # GET /v1/accounts/:id
        if method == "GET" and resource == "accounts" and len(parts) == 2:
            acc = accounts.get(parts[1], {"id": parts[1], "charges_enabled": True, "details_submitted": True})
            return self._send_json(acc)

        # POST /v1/customers/:id (update customer)
        if method == "POST" and resource == "customers" and len(parts) == 2:
            cid = parts[1]
            cust = customers.get(cid, {"id": cid, "object": "customer", "created": now_ts()})
            cust.update({k: v for k, v in params.items() if v})
            customers[cid] = cust
            return self._send_json(cust)

        # GET /v1/customers/:id/cards
        if method == "GET" and resource == "customers" and len(parts) >= 3 and parts[2] == "cards":
            if len(parts) == 3:
                return self._send_json({"data": list(payment_methods.values()), "has_more": False})
            # GET /v1/customers/:id/cards/:card_id
            card = payment_methods.get(parts[3], {"id": parts[3], "brand": "unknown", "last4": "0000", "exp_month": 1, "exp_year": 2030})
            return self._send_json(card)

        # POST /v1/customers/:id/sources (create card)
        if method == "POST" and resource == "customers" and len(parts) >= 3 and parts[2] == "sources":
            card_id = gen_id("card")
            card = {
                "id": card_id, "object": "card",
                "brand": "visa", "last4": params.get("source[number]", "0000")[-4:],
                "exp_month": int(params.get("source[exp_month]", 12)),
                "exp_year": int(params.get("source[exp_year]", 2030)),
                "customer": parts[1],
            }
            payment_methods[card_id] = card
            return self._send_json(card)

        # DELETE /v1/customers/:id/sources/:card_id (delete card)
        if method == "DELETE" and resource == "customers" and len(parts) >= 4 and parts[2] == "sources":
            pm = payment_methods.pop(parts[3], None)
            return self._send_json({"id": parts[3], "deleted": True})

        # POST /v1/payment_intents/:id/increment_authorization
        if method == "POST" and resource == "payment_intents" and len(parts) == 3 and parts[2] == "increment_authorization":
            pi = payment_intents.get(parts[1])
            if not pi:
                return self._send_json({"error": {"message": "not found"}}, 404)
            new_amount = int(params.get("amount", pi["amount"]))
            pi["amount"] = new_amount
            return self._send_json(pi)

        # POST /v1/payment_methods (clone payment method)
        if method == "POST" and resource == "payment_methods" and len(parts) == 1:
            src_pm = params.get("payment_method", "")
            cloned_id = gen_id("pm")
            src = payment_methods.get(src_pm, {"type": "card", "card": {"brand": "visa", "last4": "4242", "exp_month": 12, "exp_year": 2030}})
            cloned = {"id": cloned_id, "type": src.get("type", "card"), "card": src.get("card", {})}
            payment_methods[cloned_id] = cloned
            return self._send_json(cloned)

        # POST /v1/refunds/:id/cancel
        if method == "POST" and resource == "refunds" and len(parts) == 3 and parts[2] == "cancel":
            ref = refunds.get(parts[1])
            if not ref:
                return self._send_json({"error": {"message": "not found"}}, 404)
            ref["status"] = "canceled"
            return self._send_json(ref)

        # POST /v1/account_links
        if method == "POST" and resource == "account_links" and len(parts) == 1:
            return self._send_json({
                "object": "account_link",
                "url": f"https://connect.stripe.com/setup/mock/{uuid.uuid4().hex[:8]}",
                "expires_at": now_ts() + 3600,
                "created": now_ts(),
            })

        self._send_json({"error": {"message": f"Unknown route: {method} /{'/'.join(parts)}"}}, 404)

    def do_GET(self):
        parsed = urlparse(self.path)
        self._route("GET", parsed.path)

    def do_POST(self):
        params = self._parse_form()
        parsed = urlparse(self.path)
        self._route("POST", parsed.path, params)

    def do_DELETE(self):
        parsed = urlparse(self.path)
        self._route("DELETE", parsed.path)

    def do_OPTIONS(self):
        self.send_response(200)
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
        self.send_header("Access-Control-Allow-Headers", "*")
        self.end_headers()


def main():
    port = PORT
    for i, arg in enumerate(sys.argv):
        if arg == "--port" and i + 1 < len(sys.argv):
            port = int(sys.argv[i + 1])

    server = HTTPServer(("0.0.0.0", port), MockStripeHandler)
    print(f"\n  \033[92m🔒 Mock Stripe Server running on http://localhost:{port}\033[0m")
    print(f"  Configured payment methods:")
    print(f"    pm_card_visa          → Success (requires_capture)")
    print(f"    pm_card_mastercard    → Success (requires_capture)")
    print(f"    pm_card_authRequired  → Requires 3DS (requires_action)")
    print(f"    pm_card_declined      → Card declined (402)")
    print(f"    pm_card_insufficient  → Insufficient funds (402)")
    print(f"\n  Press Ctrl+C to stop\n")

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutdown.")
        server.server_close()


if __name__ == "__main__":
    main()
