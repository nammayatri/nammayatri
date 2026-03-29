#!/usr/bin/env python3
"""
Mock Stripe Server

Simulates Stripe API endpoints for local testing.
All endpoints accept form-urlencoded (like real Stripe) and return JSON.

Response shapes match shared-kernel Stripe types exactly:
  - PaymentIntentObject: id, amount, client_secret, latest_charge, status,
    customer_id, description, payment_method, receipt_email,
    application_fee_amount, on_behalf_of, use_stripe_sdk
  - CustomerObject: id, email, name, default_source, phone
  - RefundObject: id, object, amount, status, currency, created, payment_intent, ...
  - SetupIntentObject: id, client_secret, latest_charge, status, customer, ...
  - AccountResp: id, object, charges_enabled, details_submitted
  - CardObject: id, name, brand, country, customer, cvc_check, exp_month, exp_year, funding, fingerprint, last4
  - StripeErrorResp: {error: {type, code, message}}

Supports configurable behaviors:
  - pm_card_visa           -> succeeds immediately (requires_capture)
  - pm_card_mastercard     -> succeeds immediately (requires_capture)
  - pm_card_authRequired   -> returns requires_action (3DS)
  - pm_card_declined       -> returns card_declined error
  - pm_card_insufficient   -> returns insufficient_funds error
  - pm_card_noIncremental  -> succeeds for auth, but fails incremental authorization (issuer unsupported)

Endpoints implemented (matching shared-kernel Stripe/Flow.hs):
  POST /v1/customers                          -> create customer
  GET  /v1/customers/:id                      -> get customer
  POST /v1/customers/:id                      -> update customer
  POST /v1/payment_intents                    -> create payment intent
  GET  /v1/payment_intents/:id                -> get payment intent
  POST /v1/payment_intents/:id/confirm        -> confirm payment intent
  POST /v1/payment_intents/:id/capture        -> capture payment intent
  POST /v1/payment_intents/:id/cancel         -> cancel payment intent
  POST /v1/payment_intents/:id/increment_authorization -> update amount
  POST /v1/setup_intents                      -> create setup intent
  POST /v1/ephemeral_keys                     -> create ephemeral keys
  GET  /v1/customers/:id/payment_methods      -> list payment methods
  GET  /v1/customers/:id/cards                -> list cards (CardObject shape)
  GET  /v1/customers/:id/cards/:card_id       -> get card
  POST /v1/customers/:id/sources              -> create card
  DELETE /v1/customers/:id/sources/:card_id   -> delete card
  POST /v1/payment_methods/:id/detach         -> detach payment method
  POST /v1/payment_methods                    -> clone payment method
  POST /v1/refunds                            -> create refund
  GET  /v1/refunds/:id                        -> get refund
  POST /v1/refunds/:id/cancel                 -> cancel refund
  POST /v1/accounts                           -> create connect account
  POST /v1/account_links                      -> create account link
  GET  /v1/accounts/:id                       -> get account

Usage:
  python server.py              # Start on port 7081
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

# Seeded payment methods (type=card, card sub-object for /payment_methods endpoints)
payment_methods_store = {
    "pm_card_visa": {"id": "pm_card_visa", "type": "card", "card": {"brand": "visa", "last4": "4242", "exp_month": 12, "exp_year": 2030, "country": None}},
    "pm_card_mastercard": {"id": "pm_card_mastercard", "type": "card", "card": {"brand": "mastercard", "last4": "5556", "exp_month": 6, "exp_year": 2029, "country": None}},
    "pm_card_authRequired": {"id": "pm_card_authRequired", "type": "card", "card": {"brand": "visa", "last4": "3220", "exp_month": 12, "exp_year": 2030, "country": None}},
    "pm_card_declined": {"id": "pm_card_declined", "type": "card", "card": {"brand": "visa", "last4": "0002", "exp_month": 12, "exp_year": 2030, "country": None}},
    "pm_card_insufficient": {"id": "pm_card_insufficient", "type": "card", "card": {"brand": "visa", "last4": "9995", "exp_month": 12, "exp_year": 2030, "country": None}},
    "pm_card_captureFail": {"id": "pm_card_captureFail", "type": "card", "card": {"brand": "visa", "last4": "1881", "exp_month": 3, "exp_year": 2028, "country": None}},
    "pm_card_noIncremental": {"id": "pm_card_noIncremental", "type": "card", "card": {"brand": "visa", "last4": "7777", "exp_month": 9, "exp_year": 2029, "country": None}},
}


def gen_id(prefix):
    return f"{prefix}_{uuid.uuid4().hex[:24]}"


def now_ts():
    return int(time.time())


def stripe_error(error_type, code=None, message=None, http_status=400):
    """Build a Stripe error response matching StripeErrorResp { error :: StripeErrorBody { type, code, message } }"""
    body = {"type": error_type}
    if code:
        body["code"] = code
    if message:
        body["message"] = message
    return {"error": body}, http_status


# ---------------------------------------------------------------------------
# Response Builders — shapes match shared-kernel Haskell types exactly
# ---------------------------------------------------------------------------

def mk_payment_intent_resp(pi):
    """Build PaymentIntentObject matching shared-kernel type:
    { id, amount, client_secret, latest_charge, status, customer_id,
      description, payment_method, receipt_email, application_fee_amount,
      on_behalf_of, use_stripe_sdk }
    """
    return {
        "id": pi["id"],
        "amount": pi.get("amount"),
        "client_secret": pi.get("client_secret", ""),
        "latest_charge": pi.get("latest_charge"),
        "status": pi["status"],
        "customer_id": pi.get("customer"),  # Haskell field is customer_id
        "description": pi.get("description"),
        "payment_method": pi.get("payment_method"),
        "receipt_email": pi.get("receipt_email"),
        "application_fee_amount": pi.get("application_fee_amount"),
        "on_behalf_of": pi.get("on_behalf_of"),
        "use_stripe_sdk": pi.get("use_stripe_sdk"),
    }


def mk_customer(params):
    """CustomerObject: { id, email, name, default_source, phone }"""
    cid = gen_id("cus")
    cust = {
        "id": cid,
        "email": params.get("email") or None,
        "name": params.get("name") or None,
        "default_source": None,
        "phone": params.get("phone") or None,
    }
    customers[cid] = cust
    return cust


def mk_payment_intent(params):
    pm_id = params.get("payment_method", "")
    amount = int(params.get("amount", 0))
    currency = params.get("currency", "usd")
    capture_method = params.get("capture_method", "manual")

    # Determine behavior based on payment method
    if pm_id == "pm_card_declined":
        return stripe_error("card_error", "card_declined", "Your card was declined.", 402)
    if pm_id == "pm_card_insufficient":
        return stripe_error("card_error", "insufficient_funds", "Your card has insufficient funds.", 402)

    # With setup_future_usage=off_session, 3DS cards that were set up with off_session
    # get SCA exemption. Without it, they require action.
    sfu = params.get("setup_future_usage", "")
    if pm_id == "pm_card_authRequired" and sfu != "off_session":
        status = "requires_action"
    else:
        status = "requires_capture"

    pi_id = gen_id("pi")
    pi = {
        "id": pi_id,
        "amount": amount,
        "currency": currency,
        "status": status,
        "capture_method": capture_method,
        "payment_method": pm_id,
        "client_secret": f"{pi_id}_secret_{uuid.uuid4().hex[:12]}",
        "customer": params.get("customer", ""),
        "application_fee_amount": int(params.get("application_fee_amount", 0)) if params.get("application_fee_amount") else 0,
        "on_behalf_of": params.get("on_behalf_of") or None,
        "receipt_email": params.get("receipt_email") or None,
        "description": params.get("description") or None,
        "latest_charge": None,
        "use_stripe_sdk": None,
        "metadata": {},
        "transfer_data": {"destination": params.get("transfer_data[destination]", "")} if params.get("transfer_data[destination]") else None,
    }

    # Parse metadata
    for k, v in params.items():
        if k.startswith("metadata["):
            pi["metadata"][k[9:-1]] = v

    payment_intents[pi_id] = pi
    return mk_payment_intent_resp(pi), 200


def mk_capture(pi_id, params):
    pi = payment_intents.get(pi_id)
    if not pi:
        return stripe_error("invalid_request_error", None, f"No such payment_intent: '{pi_id}'", 404)
    if pi["status"] not in ("requires_capture",):
        return stripe_error("invalid_request_error", None,
                            f"This PaymentIntent's status is {pi['status']}, which is not a capturable status.", 400)

    # Simulate capture failure for pm_card_captureFail
    if pi.get("payment_method") == "pm_card_captureFail":
        pi["status"] = "requires_capture"  # stays uncaptured
        pi["latest_charge"] = gen_id("ch")
        return stripe_error("card_error", "card_declined",
                            "The card was declined when attempting to capture the payment. "
                            "The customer's bank refused the charge.", 402)

    pi["status"] = "succeeded"
    pi["latest_charge"] = gen_id("ch")
    if params.get("amount_to_capture"):
        pi["amount"] = int(params["amount_to_capture"])
    return mk_payment_intent_resp(pi), 200


def mk_cancel(pi_id):
    pi = payment_intents.get(pi_id)
    if not pi:
        return stripe_error("invalid_request_error", None, f"No such payment_intent: '{pi_id}'", 404)
    if pi["status"] in ("succeeded", "canceled"):
        return stripe_error("invalid_request_error", None,
                            f"You cannot cancel this PaymentIntent because it has a status of {pi['status']}.", 400)
    pi["status"] = "canceled"
    return mk_payment_intent_resp(pi), 200


def mk_confirm(pi_id, params):
    pi = payment_intents.get(pi_id)
    if not pi:
        return stripe_error("invalid_request_error", None, f"No such payment_intent: '{pi_id}'", 404)
    pm_id = params.get("payment_method", pi.get("payment_method", ""))
    if pm_id == "pm_card_authRequired":
        pi["status"] = "requires_action"
    elif pm_id in ("pm_card_declined", "pm_card_insufficient"):
        return stripe_error("card_error", "card_declined", "Your card was declined.", 402)
    else:
        pi["status"] = "requires_capture" if pi.get("capture_method") == "manual" else "succeeded"
    return mk_payment_intent_resp(pi), 200


def mk_increment_authorization(pi_id, params):
    """Stripe increment_authorization behavior:
    - Only works on PIs in requires_capture status
    - Fails for cards that don't support incremental auth (issuer/network level)
    - Fails with amount_too_large if card limit exceeded
    - On success, updates amount and returns PI object
    - On failure, returns error and PI amount is unchanged

    Failure reasons (per Stripe docs):
    - payment_intent_unexpected_state: PI not in requires_capture
    - amount_too_large: exceeds card/issuer limit
    - card_declined: issuer doesn't support incremental auth for this card
    - charge_exceeds_transaction_limit: network-level limit
    """
    pi = payment_intents.get(pi_id)
    if not pi:
        return stripe_error("invalid_request_error", None, f"No such payment_intent: '{pi_id}'", 404)
    if pi["status"] != "requires_capture":
        return stripe_error("invalid_request_error", "payment_intent_unexpected_state",
            f"This PaymentIntent's status is {pi['status']}. Incremental authorization requires status requires_capture.", 400)
    # Cards that don't support incremental authorization
    pm_id = pi.get("payment_method", "")
    no_incremental_cards = ("pm_card_noIncremental", "pm_card_declined", "pm_card_insufficient")
    if pm_id in no_incremental_cards:
        return stripe_error("card_error", "card_declined",
            "The card issuer does not support incremental authorization.", 402)
    new_amount = int(params.get("amount", pi["amount"]))
    # Amount limit check
    if new_amount > 100000:
        return stripe_error("card_error", "amount_too_large",
            "The charge amount exceeds the maximum amount allowed for the card.", 402)
    pi["amount"] = new_amount
    if params.get("application_fee_amount"):
        pi["application_fee_amount"] = int(params["application_fee_amount"])
    return mk_payment_intent_resp(pi), 200


def mk_refund(params):
    """RefundObject: { id, object, amount, balance_transaction, charge, created, currency,
       metadata, payment_intent, reason, receipt_number, source_transfer_reversal,
       status, failure_balance_transaction, failure_reason, transfer_reversal }"""
    pi_id = params.get("payment_intent", "")
    pi = payment_intents.get(pi_id)
    # Use PI amount if no explicit amount
    amount = int(params.get("amount", 0))
    if amount == 0 and pi:
        amount = pi.get("amount", 0)
    currency = pi.get("currency", "usd") if pi else "usd"

    ref_id = gen_id("re")
    ref = {
        "id": ref_id,
        "object": "refund",
        "amount": amount,  # Always an int, never null (Haskell expects Int, not Maybe Int)
        "balance_transaction": None,
        "charge": pi.get("latest_charge") if pi else None,
        "created": now_ts(),
        "currency": currency.upper(),  # Haskell Currency enum expects uppercase
        "metadata": None,
        "payment_intent": pi_id or None,
        "reason": None,
        "receipt_number": None,
        "source_transfer_reversal": None,
        "status": "succeeded",
        "failure_balance_transaction": None,
        "failure_reason": None,
        "transfer_reversal": None,
    }
    refunds[ref_id] = ref
    return ref, 200


def mk_setup_intent(params):
    """SetupIntentObject: { id, client_secret, latest_charge, status, confirm, customer, description, payment_method }"""
    si_id = gen_id("seti")
    si = {
        "id": si_id,
        "client_secret": f"{si_id}_secret_{uuid.uuid4().hex[:12]}",
        "latest_charge": None,
        "status": "requires_payment_method",
        "confirm": None,
        "customer": params.get("customer") or None,
        "description": params.get("description") or None,
        "payment_method": params.get("payment_method") or None,
    }
    setup_intents[si_id] = si
    return si, 200


def mk_ephemeral_key(params):
    """EphemeralKeysResp: { secret }"""
    return {
        "id": gen_id("ephkey"),
        "object": "ephemeral_key",
        "secret": f"ek_test_{uuid.uuid4().hex[:24]}",
        "created": now_ts(),
        "expires": now_ts() + 3600,
    }, 200


def mk_account(params):
    """AccountResp: { id, object, charges_enabled, details_submitted }"""
    acc_id = gen_id("acct")
    acc = {
        "id": acc_id,
        "object": "account",
        "charges_enabled": False,
        "details_submitted": False,
    }
    accounts[acc_id] = acc
    return acc, 200


def mk_card_object(card_data, customer_id=None):
    """CardObject: { id, name, brand, country, customer, cvc_check, exp_month, exp_year, funding, fingerprint, last4 }"""
    return {
        "id": card_data.get("id", gen_id("card")),
        "name": None,
        "brand": card_data.get("brand", "unknown"),
        "country": card_data.get("country"),
        "customer": customer_id,
        "cvc_check": None,
        "exp_month": card_data.get("exp_month", 12),
        "exp_year": card_data.get("exp_year", 2030),
        "funding": None,
        "fingerprint": None,
        "last4": card_data.get("last4", "0000"),
    }


def pm_to_card_object(pm, customer_id=None):
    """Convert a payment_method dict to CardObject shape (flat, not nested under card)."""
    card = pm.get("card", {})
    return mk_card_object({
        "id": pm.get("id", gen_id("card")),
        "brand": card.get("brand", "unknown"),
        "country": card.get("country"),
        "exp_month": card.get("exp_month", 12),
        "exp_year": card.get("exp_year", 2030),
        "last4": card.get("last4", "0000"),
    }, customer_id)


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

    def _not_found(self, resource_type, resource_id):
        return stripe_error("invalid_request_error", "resource_missing",
                            f"No such {resource_type}: '{resource_id}'", 404)

    def _route(self, method, path, params=None):
        parts = [p for p in path.split("/") if p]
        if parts and parts[0] == "v1":
            parts = parts[1:]
        if not parts:
            return self._send_json(*stripe_error("invalid_request_error", None, "Not found", 404))

        resource = parts[0]
        params = params or {}

        # ---- Customers ----
        if resource == "customers":
            if method == "POST" and len(parts) == 1:
                return self._send_json(mk_customer(params))
            if method == "GET" and len(parts) == 2:
                cid = parts[1]
                cust = customers.get(cid, {"id": cid, "email": None, "name": None, "default_source": None, "phone": None})
                return self._send_json(cust)
            if method == "POST" and len(parts) == 2:
                cid = parts[1]
                cust = customers.get(cid, {"id": cid, "email": None, "name": None, "default_source": None, "phone": None})
                cust.update({k: v for k, v in params.items() if v})
                customers[cid] = cust
                return self._send_json(cust)
            # GET /customers/:id/payment_methods -> PaymentMethodList { data, has_more }
            if method == "GET" and len(parts) >= 3 and parts[2] == "payment_methods":
                pms = [{"id": pm["id"], "card": pm["card"]} for pm in payment_methods_store.values()]
                return self._send_json({"data": pms, "has_more": False})
            # GET /customers/:id/cards -> CardList { data :: [CardObject], has_more }
            if method == "GET" and len(parts) >= 3 and parts[2] == "cards":
                if len(parts) == 3:
                    cards = [pm_to_card_object(pm, parts[1]) for pm in payment_methods_store.values()]
                    return self._send_json({"data": cards, "has_more": False})
                # GET /customers/:id/cards/:card_id
                pm = payment_methods_store.get(parts[3])
                if pm:
                    return self._send_json(pm_to_card_object(pm, parts[1]))
                return self._send_json(mk_card_object({"id": parts[3]}, parts[1]))
            # POST /customers/:id/sources (create card) -> CardObject
            if method == "POST" and len(parts) >= 3 and parts[2] == "sources":
                card_id = gen_id("card")
                card = mk_card_object({
                    "id": card_id,
                    "brand": "visa",
                    "last4": params.get("source[number]", "0000")[-4:],
                    "exp_month": int(params.get("source[exp_month]", 12)),
                    "exp_year": int(params.get("source[exp_year]", 2030)),
                }, parts[1])
                payment_methods_store[card_id] = {"id": card_id, "type": "card", "card": {
                    "brand": card["brand"], "last4": card["last4"],
                    "exp_month": card["exp_month"], "exp_year": card["exp_year"], "country": None
                }}
                return self._send_json(card)
            # DELETE /customers/:id/sources/:card_id -> DeleteCardResp { id, deleted }
            if method == "DELETE" and len(parts) >= 4 and parts[2] == "sources":
                payment_methods_store.pop(parts[3], None)
                return self._send_json({"id": parts[3], "deleted": True})

        # ---- Payment Intents ----
        if resource == "payment_intents":
            if method == "POST" and len(parts) == 1:
                resp, status = mk_payment_intent(params)
                return self._send_json(resp, status)
            if method == "GET" and len(parts) == 2:
                pi = payment_intents.get(parts[1])
                if not pi:
                    return self._send_json(*self._not_found("payment_intent", parts[1]))
                return self._send_json(mk_payment_intent_resp(pi))
            if method == "POST" and len(parts) == 3:
                if parts[2] == "capture":
                    resp, status = mk_capture(parts[1], params)
                    return self._send_json(resp, status)
                if parts[2] == "cancel":
                    resp, status = mk_cancel(parts[1])
                    return self._send_json(resp, status)
                if parts[2] == "confirm":
                    resp, status = mk_confirm(parts[1], params)
                    return self._send_json(resp, status)
                if parts[2] == "increment_authorization":
                    resp, status = mk_increment_authorization(parts[1], params)
                    return self._send_json(resp, status)

        # ---- Setup Intents ----
        if resource == "setup_intents" and method == "POST" and len(parts) == 1:
            resp, status = mk_setup_intent(params)
            return self._send_json(resp, status)

        # ---- Ephemeral Keys ----
        if resource == "ephemeral_keys" and method == "POST" and len(parts) == 1:
            resp, status = mk_ephemeral_key(params)
            return self._send_json(resp, status)

        # ---- Payment Methods ----
        if resource == "payment_methods":
            # POST /payment_methods/:id/detach -> PaymentMethod { id, card }
            if method == "POST" and len(parts) == 3 and parts[2] == "detach":
                pm = payment_methods_store.pop(parts[1], None)
                if pm:
                    return self._send_json({"id": pm["id"], "card": pm["card"]})
                # Return a valid PaymentMethod shape even for unknown PMs
                return self._send_json({"id": parts[1], "card": {"brand": "unknown", "last4": "0000", "exp_month": 1, "exp_year": 2030, "country": None}})
            # POST /payment_methods (clone) -> PaymentMethod { id, card }
            if method == "POST" and len(parts) == 1:
                src_pm = params.get("payment_method", "")
                src = payment_methods_store.get(src_pm, {"type": "card", "card": {"brand": "visa", "last4": "4242", "exp_month": 12, "exp_year": 2030, "country": None}})
                cloned_id = gen_id("pm")
                cloned = {"id": cloned_id, "card": src.get("card", {"brand": "visa", "last4": "4242", "exp_month": 12, "exp_year": 2030, "country": None})}
                payment_methods_store[cloned_id] = {"id": cloned_id, "type": "card", "card": cloned["card"]}
                return self._send_json(cloned)

        # ---- Refunds ----
        if resource == "refunds":
            if method == "POST" and len(parts) == 1:
                resp, status = mk_refund(params)
                return self._send_json(resp, status)
            if method == "GET" and len(parts) == 2:
                ref = refunds.get(parts[1])
                if not ref:
                    return self._send_json(*self._not_found("refund", parts[1]))
                return self._send_json(ref)
            if method == "POST" and len(parts) == 3 and parts[2] == "cancel":
                ref = refunds.get(parts[1])
                if not ref:
                    return self._send_json(*self._not_found("refund", parts[1]))
                ref["status"] = "canceled"
                return self._send_json(ref)

        # ---- Accounts ----
        if resource == "accounts":
            if method == "POST" and len(parts) == 1:
                resp, status = mk_account(params)
                return self._send_json(resp, status)
            if method == "GET" and len(parts) == 2:
                acc = accounts.get(parts[1],
                                   {"id": parts[1], "object": "account", "charges_enabled": True, "details_submitted": True})
                return self._send_json(acc)

        # ---- Account Links ----
        if resource == "account_links" and method == "POST" and len(parts) == 1:
            return self._send_json({
                "object": "account_link",
                "url": f"https://connect.stripe.com/setup/mock/{uuid.uuid4().hex[:8]}",
                "expires_at": now_ts() + 3600,
                "created": now_ts(),
            })

        self._send_json(*stripe_error("invalid_request_error", None, f"Unknown route: {method} /{'/'.join(parts)}", 404))

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
    print(f"\n  \033[92mMock Stripe Server running on http://localhost:{port}\033[0m")
    print(f"  Configured payment methods:")
    print(f"    pm_card_visa          -> Success (requires_capture)")
    print(f"    pm_card_mastercard    -> Success (requires_capture)")
    print(f"    pm_card_authRequired  -> Requires 3DS (requires_action)")
    print(f"    pm_card_declined      -> Card declined (402)")
    print(f"    pm_card_insufficient  -> Insufficient funds (402)")
    print(f"    pm_card_noIncremental -> Auth OK, incremental auth fails (issuer unsupported)")
    print(f"\n  Press Ctrl+C to stop\n")

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutdown.")
        server.server_close()


if __name__ == "__main__":
    main()
