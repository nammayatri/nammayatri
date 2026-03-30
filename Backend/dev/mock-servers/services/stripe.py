"""Full Stripe API mock — customers, payment intents, setup intents, refunds, accounts.

Supports form-urlencoded bodies (real Stripe SDK format) and JSON.

Response shapes match shared-kernel Stripe types exactly:
  - PaymentIntentObject: id, amount, client_secret, latest_charge, status,
    customer_id, description, payment_method, receipt_email,
    application_fee_amount, on_behalf_of, use_stripe_sdk
  - CustomerObject: id, email, name, default_source, phone
  - RefundObject: id, object, amount, status, currency, created, payment_intent, ...
  - SetupIntentObject: id, client_secret, latest_charge, status, customer, ...
  - AccountResp: id, object, charges_enabled, details_submitted
  - CardObject: id, name, brand, country, customer, cvc_check, exp_month, exp_year,
    funding, fingerprint, last4
  - StripeErrorResp: {error: {type, code, message}}

Test card behaviors:
  pm_card_visa           → requires_capture (success)
  pm_card_mastercard     → requires_capture (success)
  pm_card_authRequired   → requires_action (3DS)
  pm_card_declined       → card_declined (402)
  pm_card_insufficient   → insufficient_funds (402)
  pm_card_captureFail    → capture returns card_declined
  pm_card_noIncremental  → auth OK, incremental auth fails (issuer unsupported)
"""

import json
import uuid
import time
from urllib.parse import unquote_plus


# ── In-memory state ──

customers = {}
payment_intents = {}
setup_intents = {}
refunds = {}
accounts = {}
payment_methods = {
    "pm_card_visa": {"id": "pm_card_visa", "type": "card", "card": {"brand": "visa", "last4": "4242", "exp_month": 12, "exp_year": 2030, "country": None}},
    "pm_card_mastercard": {"id": "pm_card_mastercard", "type": "card", "card": {"brand": "mastercard", "last4": "5556", "exp_month": 6, "exp_year": 2029, "country": None}},
    "pm_card_authRequired": {"id": "pm_card_authRequired", "type": "card", "card": {"brand": "visa", "last4": "3220", "exp_month": 12, "exp_year": 2030, "country": None}},
    "pm_card_declined": {"id": "pm_card_declined", "type": "card", "card": {"brand": "visa", "last4": "0002", "exp_month": 12, "exp_year": 2030, "country": None}},
    "pm_card_insufficient": {"id": "pm_card_insufficient", "type": "card", "card": {"brand": "visa", "last4": "9995", "exp_month": 12, "exp_year": 2030, "country": None}},
    "pm_card_captureFail": {"id": "pm_card_captureFail", "type": "card", "card": {"brand": "visa", "last4": "1881", "exp_month": 3, "exp_year": 2028, "country": None}},
    "pm_card_noIncremental": {"id": "pm_card_noIncremental", "type": "card", "card": {"brand": "visa", "last4": "7777", "exp_month": 9, "exp_year": 2029, "country": None}},
}


def _gen_id(prefix):
    return f"{prefix}_{uuid.uuid4().hex[:24]}"


def _now():
    return int(time.time())


def _parse_form(body):
    """Parse form-urlencoded body (Stripe SDK sends this format)."""
    if not body:
        return {}
    text = body.decode("utf-8") if isinstance(body, bytes) else body
    try:
        return json.loads(text)
    except (json.JSONDecodeError, ValueError):
        pass
    params = {}
    for pair in text.split("&"):
        if "=" in pair:
            k, v = pair.split("=", 1)
            params[unquote_plus(k)] = unquote_plus(v)
    return params


def _stripe_error(error_type, code=None, message=None, http_status=400):
    body = {"type": error_type}
    if code:
        body["code"] = code
    if message:
        body["message"] = message
    return {"error": body}, http_status


# ── Response shape builders (match shared-kernel Haskell types) ──


def _mk_pi_resp(pi):
    """PaymentIntentObject shape."""
    return {
        "id": pi["id"],
        "amount": pi.get("amount"),
        "client_secret": pi.get("client_secret", ""),
        "latest_charge": pi.get("latest_charge"),
        "status": pi["status"],
        "customer_id": pi.get("customer"),
        "description": pi.get("description"),
        "payment_method": pi.get("payment_method"),
        "receipt_email": pi.get("receipt_email"),
        "application_fee_amount": pi.get("application_fee_amount"),
        "on_behalf_of": pi.get("on_behalf_of"),
        "use_stripe_sdk": pi.get("use_stripe_sdk"),
    }


def _mk_card_object(card_data, customer_id=None):
    """CardObject shape (flat, not nested under card)."""
    return {
        "id": card_data.get("id", _gen_id("card")),
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


def _pm_to_card_object(pm, customer_id=None):
    """Convert a payment_method dict to CardObject shape."""
    card = pm.get("card", {})
    return _mk_card_object({
        "id": pm.get("id", _gen_id("card")),
        "brand": card.get("brand", "unknown"),
        "country": card.get("country"),
        "exp_month": card.get("exp_month", 12),
        "exp_year": card.get("exp_year", 2030),
        "last4": card.get("last4", "0000"),
    }, customer_id)


# ── Builders ──

def _mk_customer(params):
    cid = _gen_id("cus")
    cust = {
        "id": cid,
        "email": params.get("email") or None,
        "name": params.get("name") or None,
        "default_source": None,
        "phone": params.get("phone") or None,
    }
    customers[cid] = cust
    return cust, 200


def _mk_payment_intent(params):
    pm_id = params.get("payment_method", "")
    amount = int(params.get("amount", 0))
    currency = params.get("currency", "usd")
    capture_method = params.get("capture_method", "manual")

    if pm_id == "pm_card_declined":
        return _stripe_error("card_error", "card_declined", "Your card was declined.", 402)
    if pm_id == "pm_card_insufficient":
        return _stripe_error("card_error", "insufficient_funds", "Your card has insufficient funds.", 402)

    sfu = params.get("setup_future_usage", "")
    if pm_id == "pm_card_authRequired" and sfu != "off_session":
        status = "requires_action"
    else:
        status = "requires_capture"

    pi_id = _gen_id("pi")
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
    for k, v in params.items():
        if k.startswith("metadata["):
            pi["metadata"][k[9:-1]] = v

    payment_intents[pi_id] = pi
    return _mk_pi_resp(pi), 200


def _mk_capture(pi_id, params):
    pi = payment_intents.get(pi_id)
    if not pi:
        return _stripe_error("invalid_request_error", None, f"No such payment_intent: '{pi_id}'", 404)
    if pi["status"] != "requires_capture":
        return _stripe_error("invalid_request_error", None,
                             f"This PaymentIntent's status is {pi['status']}, which is not a capturable status.", 400)

    if pi.get("payment_method") == "pm_card_captureFail":
        pi["latest_charge"] = _gen_id("ch")
        return _stripe_error("card_error", "card_declined",
                             "The card was declined when attempting to capture the payment. "
                             "The customer's bank refused the charge.", 402)

    pi["status"] = "succeeded"
    pi["latest_charge"] = _gen_id("ch")
    if params.get("amount_to_capture"):
        pi["amount"] = int(params["amount_to_capture"])
    return _mk_pi_resp(pi), 200


def _mk_cancel(pi_id):
    pi = payment_intents.get(pi_id)
    if not pi:
        return _stripe_error("invalid_request_error", None, f"No such payment_intent: '{pi_id}'", 404)
    if pi["status"] in ("succeeded", "canceled"):
        return _stripe_error("invalid_request_error", None,
                             f"You cannot cancel this PaymentIntent because it has a status of {pi['status']}.", 400)
    pi["status"] = "canceled"
    return _mk_pi_resp(pi), 200


def _mk_confirm(pi_id, params):
    pi = payment_intents.get(pi_id)
    if not pi:
        return _stripe_error("invalid_request_error", None, f"No such payment_intent: '{pi_id}'", 404)
    pm_id = params.get("payment_method", pi.get("payment_method", ""))
    if pm_id == "pm_card_authRequired":
        pi["status"] = "requires_action"
    elif pm_id in ("pm_card_declined", "pm_card_insufficient"):
        return _stripe_error("card_error", "card_declined", "Your card was declined.", 402)
    else:
        pi["status"] = "requires_capture" if pi.get("capture_method") == "manual" else "succeeded"
    return _mk_pi_resp(pi), 200


def _mk_increment_authorization(pi_id, params):
    pi = payment_intents.get(pi_id)
    if not pi:
        return _stripe_error("invalid_request_error", None, f"No such payment_intent: '{pi_id}'", 404)
    if pi["status"] != "requires_capture":
        return _stripe_error("invalid_request_error", "payment_intent_unexpected_state",
                             f"This PaymentIntent's status is {pi['status']}. "
                             "Incremental authorization requires status requires_capture.", 400)
    pm_id = pi.get("payment_method", "")
    if pm_id in ("pm_card_noIncremental", "pm_card_declined", "pm_card_insufficient"):
        return _stripe_error("card_error", "card_declined",
                             "The card issuer does not support incremental authorization.", 402)
    new_amount = int(params.get("amount", pi["amount"]))
    if new_amount > 100000:
        return _stripe_error("card_error", "amount_too_large",
                             "The charge amount exceeds the maximum amount allowed for the card.", 402)
    pi["amount"] = new_amount
    if params.get("application_fee_amount"):
        pi["application_fee_amount"] = int(params["application_fee_amount"])
    return _mk_pi_resp(pi), 200


def _mk_refund(params):
    pi_id = params.get("payment_intent", "")
    pi = payment_intents.get(pi_id)
    amount = int(params.get("amount", 0))
    if amount == 0 and pi:
        amount = pi.get("amount", 0)
    currency = pi.get("currency", "usd") if pi else "usd"

    ref_id = _gen_id("re")
    ref = {
        "id": ref_id,
        "object": "refund",
        "amount": amount,
        "balance_transaction": None,
        "charge": pi.get("latest_charge") if pi else None,
        "created": _now(),
        "currency": currency.upper(),
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


def _mk_setup_intent(params):
    si_id = _gen_id("seti")
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


def _mk_ephemeral_key(params):
    return {
        "id": _gen_id("ephkey"),
        "object": "ephemeral_key",
        "secret": f"ek_test_{uuid.uuid4().hex[:24]}",
        "created": _now(),
        "expires": _now() + 3600,
    }, 200


def _mk_account(params):
    acc_id = _gen_id("acct")
    acc = {
        "id": acc_id,
        "object": "account",
        "charges_enabled": False,
        "details_submitted": False,
    }
    accounts[acc_id] = acc
    return acc, 200


# ── Router ──

def handle(handler, path, body):
    """Route Stripe requests. Path arrives as /stripe/v1/... — strip /stripe prefix."""
    method = handler.command
    params = _parse_form(body)

    stripped = path
    for prefix in ("/stripe/", "/stripe"):
        if stripped.startswith(prefix):
            stripped = stripped[len(prefix):]
            break

    parts = [p for p in stripped.split("/") if p]
    if parts and parts[0] == "v1":
        parts = parts[1:]

    if not parts:
        handler._json({"status": "ok", "service": "stripe-mock"})
        return

    resource = parts[0]

    # ── Customers ──
    if resource == "customers":
        if method == "POST" and len(parts) == 1:
            resp, st = _mk_customer(params)
            return handler._json(resp, st)
        if method == "GET" and len(parts) == 2:
            cid = parts[1]
            cust = customers.get(cid, {"id": cid, "email": None, "name": None, "default_source": None, "phone": None})
            return handler._json(cust)
        if method == "POST" and len(parts) == 2:
            cid = parts[1]
            cust = customers.get(cid, {"id": cid, "email": None, "name": None, "default_source": None, "phone": None})
            cust.update({k: v for k, v in params.items() if v})
            customers[cid] = cust
            return handler._json(cust)
        if method == "GET" and len(parts) >= 3 and parts[2] == "payment_methods":
            pms = [{"id": pm["id"], "card": pm["card"]} for pm in payment_methods.values()]
            return handler._json({"data": pms, "has_more": False})
        if method == "GET" and len(parts) >= 3 and parts[2] == "cards":
            if len(parts) == 3:
                cards = [_pm_to_card_object(pm, parts[1]) for pm in payment_methods.values()]
                return handler._json({"data": cards, "has_more": False})
            pm = payment_methods.get(parts[3])
            if pm:
                return handler._json(_pm_to_card_object(pm, parts[1]))
            return handler._json(_mk_card_object({"id": parts[3]}, parts[1]))
        if method == "POST" and len(parts) >= 3 and parts[2] == "sources":
            card_id = _gen_id("card")
            card = _mk_card_object({
                "id": card_id,
                "brand": "visa",
                "last4": params.get("source[number]", "0000")[-4:],
                "exp_month": int(params.get("source[exp_month]", 12)),
                "exp_year": int(params.get("source[exp_year]", 2030)),
            }, parts[1])
            payment_methods[card_id] = {"id": card_id, "type": "card", "card": {
                "brand": card["brand"], "last4": card["last4"],
                "exp_month": card["exp_month"], "exp_year": card["exp_year"], "country": None
            }}
            return handler._json(card)
        if method == "DELETE" and len(parts) >= 4 and parts[2] == "sources":
            payment_methods.pop(parts[3], None)
            return handler._json({"id": parts[3], "deleted": True})

    # ── Payment Intents ──
    if resource == "payment_intents":
        if method == "POST" and len(parts) == 1:
            resp, st = _mk_payment_intent(params)
            return handler._json(resp, st)
        if method == "GET" and len(parts) == 2:
            override_status, extra = handler._get_override("stripe", parts[1])
            pi = payment_intents.get(parts[1])
            if not pi:
                if override_status:
                    return handler._json({"id": parts[1], "object": "payment_intent", "status": override_status, **extra})
                return handler._json({"error": {"message": f"No such payment_intent: '{parts[1]}'"}}, 404)
            if override_status:
                pi["status"] = override_status
            return handler._json(_mk_pi_resp(pi))
        if method == "POST" and len(parts) == 3:
            if parts[2] == "capture":
                resp, st = _mk_capture(parts[1], params)
                return handler._json(resp, st)
            if parts[2] == "cancel":
                resp, st = _mk_cancel(parts[1])
                return handler._json(resp, st)
            if parts[2] == "confirm":
                resp, st = _mk_confirm(parts[1], params)
                return handler._json(resp, st)
            if parts[2] == "increment_authorization":
                resp, st = _mk_increment_authorization(parts[1], params)
                return handler._json(resp, st)

    # ── Setup Intents ──
    if resource == "setup_intents" and method == "POST" and len(parts) == 1:
        resp, st = _mk_setup_intent(params)
        return handler._json(resp, st)

    # ── Ephemeral Keys ──
    if resource == "ephemeral_keys" and method == "POST" and len(parts) == 1:
        resp, st = _mk_ephemeral_key(params)
        return handler._json(resp, st)

    # ── Payment Methods ──
    if resource == "payment_methods":
        if method == "POST" and len(parts) == 3 and parts[2] == "detach":
            pm = payment_methods.pop(parts[1], None)
            if pm:
                return handler._json({"id": pm["id"], "card": pm["card"]})
            return handler._json({"id": parts[1], "card": {"brand": "unknown", "last4": "0000", "exp_month": 1, "exp_year": 2030, "country": None}})
        if method == "POST" and len(parts) == 1:
            src_pm = params.get("payment_method", "")
            src = payment_methods.get(src_pm, {"type": "card", "card": {"brand": "visa", "last4": "4242", "exp_month": 12, "exp_year": 2030, "country": None}})
            cloned_id = _gen_id("pm")
            cloned = {"id": cloned_id, "card": src.get("card", {"brand": "visa", "last4": "4242", "exp_month": 12, "exp_year": 2030, "country": None})}
            payment_methods[cloned_id] = {"id": cloned_id, "type": "card", "card": cloned["card"]}
            return handler._json(cloned)

    # ── Refunds ──
    if resource == "refunds":
        if method == "POST" and len(parts) == 1:
            resp, st = _mk_refund(params)
            return handler._json(resp, st)
        if method == "GET" and len(parts) == 2:
            ref = refunds.get(parts[1])
            if not ref:
                return handler._json({"error": {"message": f"No such refund: '{parts[1]}'"}}, 404)
            return handler._json(ref)
        if method == "POST" and len(parts) == 3 and parts[2] == "cancel":
            ref = refunds.get(parts[1])
            if not ref:
                return handler._json({"error": {"message": f"No such refund: '{parts[1]}'"}}, 404)
            ref["status"] = "canceled"
            return handler._json(ref)

    # ── Accounts ──
    if resource == "accounts":
        if method == "POST" and len(parts) == 1:
            resp, st = _mk_account(params)
            return handler._json(resp, st)
        if method == "GET" and len(parts) == 2:
            acc = accounts.get(parts[1],
                               {"id": parts[1], "object": "account", "charges_enabled": True, "details_submitted": True})
            return handler._json(acc)

    # ── Account Links ──
    if resource == "account_links" and method == "POST":
        return handler._json({
            "object": "account_link",
            "url": f"https://connect.stripe.com/setup/mock/{uuid.uuid4().hex[:8]}",
            "expires_at": _now() + 3600,
            "created": _now(),
        })

    handler._json({"error": {"message": f"Unknown route: {method} /{'/'.join(parts)}"}}, 404)
