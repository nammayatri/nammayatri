#!/usr/bin/env python3
"""Can a real driver actually open a real payment page?

Run it ON the VPS:

    python3 probe-subscription-live.py

`probe-subscription-flow.py` proves everything the webhook touches, by signing
its own events. It deliberately cannot prove the other half — that a driver's
token buys him a Chargily checkout — because that half needs a real token and a
real key, and until 2026-08-26 there was no real key.

This is that half, and it is the only thing between "the code is right" and "a
driver can pay":

    1. a driver's token is accepted, and the server works out *whose* it is
    2. that produces a checkout at Chargily
    3. the URL it hands back is a page that actually loads
    4. our own row records the right driver, so the webhook can find him

── What it cannot do ───────────────────────────────────────────────────────
Enter a card. Step 4 of a payment is a human on a phone, and the last mile is
proven by paying 3 000 test dinars on a real handset, once.

── It cleans up after itself ───────────────────────────────────────────────
The pending row is deleted. Left behind, it would show in that driver's own
*Mes reçus* as "En attente de confirmation" for ever — a payment he never
started. The checkout itself stays at Chargily, in test mode, costing nothing.
"""
import json
import re
import subprocess
import sys
import urllib.error
import urllib.request

BASE = "https://api.169-58-139-65.sslip.io"

passed = 0
failed = 0


def check(name, ok, detail=""):
    global passed, failed
    if ok:
        passed += 1
        print(f"  PASS  {name}")
    else:
        failed += 1
        print(f"  FAIL  {name}   {detail}")


def psql(sql):
    r = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres",
         "-d", "atlas_dev", "-tAc", sql],
        capture_output=True, text=True, timeout=60)
    return r.stdout.strip()


def request(path, method="GET", token=None):
    q = urllib.request.Request(BASE + path, method=method)
    if token:
        q.add_header("token", token)
    q.add_header("content-type", "application/json")
    try:
        with urllib.request.urlopen(q, timeout=30) as r:
            return r.status, r.read().decode("utf-8", "replace")
    except urllib.error.HTTPError as e:
        return e.code, e.read().decode("utf-8", "replace")
    except Exception as e:                                   # noqa: BLE001
        return 0, str(e)


# ── a driver who is really signed in ────────────────────────────────────────
print("== a real driver")
row = psql(
    "SELECT rt.token || '|' || rt.entity_id || '|' || coalesce(p.first_name,'?') "
    "  FROM atlas_driver_offer_bpp.registration_token rt "
    "  JOIN atlas_driver_offer_bpp.person p ON p.id = rt.entity_id "
    " WHERE p.role = 'DRIVER' AND rt.verified "
    " ORDER BY rt.created_at DESC LIMIT 1")
if not row or "|" not in row:
    print("   no verified driver token in the database — run ./setup.sh drivers")
    sys.exit(2)

token, driver_id, name = row.split("|", 2)
print(f"   {name}  {driver_id}")

status_code, status_body = request("/subscription/status", token=token)
check("his token is accepted", status_code == 200, f"HTTP {status_code} {status_body[:90]}")
try:
    state = json.loads(status_body)
except ValueError:
    state = {}
print(f"   état: {state.get('state')}  jours: {state.get('daysLeft')}  prix: {state.get('price')}")

# The id is never sent by the client, so this is also the proof that the server
# resolved it from the token against the driver backend.
check("the server has a state for him", state.get("state") in ("never", "active", "lapsed"),
      status_body[:90])

# ── the checkout ────────────────────────────────────────────────────────────
print("\n== the payment page")
code, body = request("/subscription/checkout?method=cib", "POST", token=token)
check("a checkout is created", code == 200, f"HTTP {code} {body[:160]}")
if code != 200:
    print("\n   Chargily refused. If this says 'gateway refused', the secret key")
    print("   is wrong or missing; if 'payments not configured', it is unset.")
    sys.exit(1)

made = json.loads(body)
checkout_id = made.get("checkoutId", "")
url = made.get("checkoutUrl", "")
print(f"   {checkout_id}")
print(f"   {url}")

check("it names an amount", made.get("amount") == state.get("price"),
      f"{made.get('amount')} vs {state.get('price')}")
check("the URL is Chargily's", "chargily" in url, url[:80])

# ── our own row, which is how the webhook will find him ─────────────────────
owner = psql(f"SELECT driver_id FROM movin.subscription_payment "
             f"WHERE checkout_id = '{checkout_id}'")
check("we recorded whose payment it is", owner == driver_id, f"got {owner!r}")
st = psql(f"SELECT status FROM movin.subscription_payment WHERE checkout_id = '{checkout_id}'")
check("and it is pending, not paid", st == "pending", f"status = {st!r}")

# ── does the page load ──────────────────────────────────────────────────────
# curl, not urllib: Chargily is behind Cloudflare, which refuses Python-urllib
# with 403 error 1010 -- a refusal to ask, not a broken URL. See the flow probe.
r = subprocess.run(
    ["curl", "-s", "-o", "/dev/null", "-w", "%{http_code}", "-L",
     "--max-time", "25", url],
    capture_output=True, text=True, timeout=40)
check("the payment page loads", r.stdout.strip() == "200", f"HTTP {r.stdout.strip()}")

# ── put it back ─────────────────────────────────────────────────────────────
psql(f"DELETE FROM movin.subscription_payment WHERE checkout_id = '{checkout_id}'")
print(f"\n   cleaned up; removed the pending row for {checkout_id}")

if failed == 0:
    print("\n   The last mile is a human: open that URL on a phone, pay with a")
    print("   Chargily test card, and the webhook should extend this driver by")
    print("   30 days within seconds.")

print(f"\n{passed} passed, {failed} failed")
sys.exit(1 if failed else 0)
