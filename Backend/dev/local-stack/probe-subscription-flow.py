#!/usr/bin/env python3
"""Does the driver subscription actually hold money, and hold it exactly once?

Run it ON the VPS:

    python3 probe-subscription-flow.py

Everything here goes through the public edge rather than straight at :8030, so
the nginx block is under test too -- a `location /subscription/` that buffered
or rewrote the body would make every webhook look forged, and that failure is
invisible from inside the container.

── The check this exists for ───────────────────────────────────────────────
Chargily retries webhooks. Without a replay guard, one retry is a free month.
So the important test is not that a payment extends a subscription -- it is
that the *same payment delivered twice* extends it once. Test 6 sends the
identical signed bytes a second time and asserts `paid_until` did not move.

── Why it can prove all of this without a live gateway ─────────────────────
The webhook is the only thing that ever extends a subscription, and a webhook
is just signed bytes. The probe signs them with the same secret the deployed
container is running, so every path from the signature check to the row lock is
exercised for real. The one thing it cannot fake is Chargily accepting our key,
and test 12 asks Chargily that directly rather than guessing.

Everything it writes is namespaced `probe_` and removed at the end, including
the driver's subscription row, which is restored to exactly what it was.
"""
import hashlib
import hmac
import json
import subprocess
import sys
import time
import urllib.error
import urllib.request
import uuid

BASE = "https://api.169-58-139-65.sslip.io"
# The shim's own health, which the public URL cannot answer: the edge has
# `location = /healthz { return 200 '{"ok":true}'; }` and never proxies it, so
# asking the public host tells you nginx is alive and nothing about the shim.
# The first run of this probe failed here and the deployment was fine.
SHIM = "http://127.0.0.1:8030"
PRICE = 3000
DAYS = 30

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
    if r.returncode != 0:
        print("psql failed:", r.stderr.strip(), file=sys.stderr)
    return r.stdout.strip()


def shim_env(name):
    """Read what the container is really running, not what the file says."""
    r = subprocess.run(["docker", "exec", "ny-maps-shim", "printenv", name],
                       capture_output=True, text=True, timeout=30)
    return r.stdout.strip()


def request(path, method="GET", body=None, headers=None, base=BASE):
    q = urllib.request.Request(base + path, method=method, data=body)
    for k, v in (headers or {}).items():
        q.add_header(k, v)
    try:
        with urllib.request.urlopen(q, timeout=25) as r:
            return r.status, r.read().decode("utf-8", "replace")
    except urllib.error.HTTPError as e:
        return e.code, e.read().decode("utf-8", "replace")
    except Exception as e:                                  # noqa: BLE001
        return 0, str(e)


def event(checkout_id, kind="checkout.paid", driver_id=None):
    """A Chargily event, byte-for-byte as one arrives."""
    data = {
        "id": checkout_id,
        "entity": "checkout",
        "status": "paid" if kind == "checkout.paid" else kind.split(".")[-1],
        "amount": PRICE,
        "currency": "dzd",
        "metadata": ([{"key": "driver_id", "value": driver_id}] if driver_id else None),
    }
    return json.dumps({
        "id": "evt_" + uuid.uuid4().hex[:16],
        "entity": "event",
        "type": kind,
        "livemode": False,
        "data": data,
        "created_at": int(time.time()),
        "updated_at": int(time.time()),
    }).encode("utf-8")


def post_webhook(raw, secret, mangle=False):
    """Sign the exact bytes. Re-serialising them is the classic way to fail."""
    sig = hmac.new(secret.encode("utf-8"), raw, hashlib.sha256).hexdigest()
    if mangle:
        sig = sig[:-1] + ("0" if sig[-1] != "0" else "1")
    return request("/subscription/webhook", "POST", raw,
                   {"content-type": "application/json", "signature": sig})


def paid_until(driver_id):
    return psql(f"SELECT coalesce(paid_until::text,'') FROM movin.subscription "
                f"WHERE driver_id = '{driver_id}'")


# ── setup ───────────────────────────────────────────────────────────────────
print("== what is deployed")
secret = shim_env("CHARGILY_SECRET_KEY")
public_url = shim_env("PUBLIC_URL")
base = shim_env("CHARGILY_BASE")
print(f"   CHARGILY_BASE       {base or '(unset)'}")
print(f"   PUBLIC_URL          {public_url or '(unset)'}")
print(f"   secret key          {'set, ' + secret[:8] + '...' if secret else 'NOT SET'}")
if not secret:
    print("\n   No secret in the container: the webhook cannot be signed and nothing")
    print("   below would mean anything. Set CHARGILY_SECRET_KEY in .env and")
    print("   `docker compose up -d maps-shim`.")
    sys.exit(2)

if psql("SELECT to_regclass('movin.subscription_payment')") == "":
    print("\n   movin schema is missing. Apply driver-subscription.sql first.")
    sys.exit(2)

driver = psql("SELECT id FROM atlas_driver_offer_bpp.person "
              "WHERE role = 'DRIVER' ORDER BY id LIMIT 1")
if not driver:
    print("   no drivers in the database")
    sys.exit(2)
print(f"   subject             {driver}")

# Snapshot, so the fleet is exactly as it was afterwards.
before = paid_until(driver)
print(f"   his paid_until now  {before or '(no row)'}")


def cleanup():
    psql(f"DELETE FROM movin.subscription_payment WHERE checkout_id LIKE 'probe\\_%'")
    if before:
        psql(f"UPDATE movin.subscription SET paid_until = '{before}' "
             f"WHERE driver_id = '{driver}'")
    else:
        psql(f"DELETE FROM movin.subscription WHERE driver_id = '{driver}'")


cleanup()          # in case a previous run died halfway
print()

try:
    # ── 1-2. the routes are there and they are shut ─────────────────────────
    print("== the door")
    code, body = request("/healthz", base=SHIM)
    try:
        health = json.loads(body)
    except ValueError:
        health = {}
    check("the shim reports payments configured", health.get("payments") is True, body[:120])

    code, body = request("/subscription/status")
    check("status without a token is refused", code == 401, f"got {code} {body[:80]}")

    code, body = request("/subscription/checkout", "POST", b"")
    check("checkout without a token is refused", code == 401, f"got {code} {body[:80]}")

    # ── 3-4. the signature is the whole of the webhook's security ───────────
    print("\n== the signature")
    raw = event("probe_unsigned", driver_id=driver)
    code, body = request("/subscription/webhook", "POST", raw,
                         {"content-type": "application/json"})
    check("webhook with no signature is refused", code == 403, f"got {code} {body[:80]}")

    code, body = post_webhook(raw, secret, mangle=True)
    check("webhook with a wrong signature is refused", code == 403, f"got {code} {body[:80]}")

    code, body = post_webhook(raw, "not-the-secret-at-all")
    check("webhook signed with another key is refused", code == 403, f"got {code} {body[:80]}")

    check("none of that wrote anything",
          paid_until(driver) == before,
          f"paid_until moved to {paid_until(driver)}")

    # ── 5. a payment ────────────────────────────────────────────────────────
    print("\n== one payment")
    c1 = "probe_" + uuid.uuid4().hex[:20]
    psql(f"INSERT INTO movin.subscription_payment "
         f"(checkout_id, driver_id, amount, currency, months, status) "
         f"VALUES ('{c1}', '{driver}', {PRICE}, 'dzd', 1, 'pending')")
    raw1 = event(c1, driver_id=driver)
    code, body = post_webhook(raw1, secret)
    check("a signed checkout.paid is accepted", code == 200, f"got {code} {body[:120]}")

    after1 = paid_until(driver)
    days = psql(f"SELECT round(extract(epoch FROM (paid_until - now()))/86400)::int "
                f"FROM movin.subscription WHERE driver_id = '{driver}'")
    check(f"it bought {DAYS} days", days == str(DAYS), f"got {days} days ({after1})")

    inv = psql(f"SELECT coalesce(invoice_no::text,'') FROM movin.subscription_payment "
               f"WHERE checkout_id = '{c1}'")
    check("a receipt number was drawn", inv.isdigit(), f"invoice_no = {inv!r}")

    covered = psql(f"SELECT round(extract(epoch FROM (covers_until - covers_from))/86400)::int "
                   f"FROM movin.subscription_payment WHERE checkout_id = '{c1}'")
    check(f"the receipt says it covers {DAYS} days", covered == str(DAYS),
          f"covers {covered} days -- the ON CONFLICT ... RETURNING trap")

    # ── 6. THE ONE THAT MATTERS ─────────────────────────────────────────────
    print("\n== the same payment, delivered again (their retry)")
    code, body = post_webhook(raw1, secret)
    check("a replay is accepted rather than errored", code == 200, f"got {code}")
    check("a replay says so", "alreadyApplied" in body, body[:120])
    check("A REPLAY DOES NOT BUY A SECOND MONTH", paid_until(driver) == after1,
          f"{after1} -> {paid_until(driver)}  <-- free month")

    n = psql(f"SELECT count(*) FROM movin.subscription_payment "
             f"WHERE checkout_id = '{c1}' AND applied_at IS NOT NULL")
    check("and it is still one payment", n == "1", f"{n} rows applied")

    # ── 7. paying again while still active stacks ───────────────────────────
    print("\n== a second month, bought early")
    c2 = "probe_" + uuid.uuid4().hex[:20]
    psql(f"INSERT INTO movin.subscription_payment "
         f"(checkout_id, driver_id, amount, currency, months, status) "
         f"VALUES ('{c2}', '{driver}', {PRICE}, 'dzd', 1, 'pending')")
    code, body = post_webhook(event(c2, driver_id=driver), secret)
    days2 = psql(f"SELECT round(extract(epoch FROM (paid_until - now()))/86400)::int "
                 f"FROM movin.subscription WHERE driver_id = '{driver}'")
    check(f"it stacks onto what is left, not from today ({2 * DAYS} days)",
          days2 == str(2 * DAYS), f"got {days2} days")

    # ── 8. paying after lapsing starts from today, not from the old date ────
    print("\n== a month bought after lapsing")
    psql(f"UPDATE movin.subscription SET paid_until = now() - interval '60 days' "
         f"WHERE driver_id = '{driver}'")
    c3 = "probe_" + uuid.uuid4().hex[:20]
    psql(f"INSERT INTO movin.subscription_payment "
         f"(checkout_id, driver_id, amount, currency, months, status) "
         f"VALUES ('{c3}', '{driver}', {PRICE}, 'dzd', 1, 'pending')")
    post_webhook(event(c3, driver_id=driver), secret)
    days3 = psql(f"SELECT round(extract(epoch FROM (paid_until - now()))/86400)::int "
                 f"FROM movin.subscription WHERE driver_id = '{driver}'")
    check(f"it restarts from today ({DAYS} days), not from the old expiry",
          days3 == str(DAYS), f"got {days3} days -- he paid for a month that already ended")

    # ── 9. a failure must not extend anything ───────────────────────────────
    print("\n== a failed payment")
    held = paid_until(driver)
    c4 = "probe_" + uuid.uuid4().hex[:20]
    psql(f"INSERT INTO movin.subscription_payment "
         f"(checkout_id, driver_id, amount, currency, months, status) "
         f"VALUES ('{c4}', '{driver}', {PRICE}, 'dzd', 1, 'pending')")
    code, body = post_webhook(event(c4, "checkout.failed", driver), secret)
    check("checkout.failed is accepted", code == 200, f"got {code} {body[:80]}")
    check("and buys nothing", paid_until(driver) == held,
          f"{held} -> {paid_until(driver)}")
    st = psql(f"SELECT status FROM movin.subscription_payment WHERE checkout_id = '{c4}'")
    check("but is recorded as failed", st == "failed", f"status = {st!r}")

    # ── 10. a payment we never recorded, rebuilt from their metadata ────────
    print("\n== a paid checkout we have no row for")
    held = paid_until(driver)
    c5 = "probe_" + uuid.uuid4().hex[:20]          # deliberately not inserted
    code, body = post_webhook(event(c5, driver_id=driver), secret)
    check("it is attributed from the metadata rather than lost", code == 200, f"got {code}")
    owner = psql(f"SELECT driver_id FROM movin.subscription_payment WHERE checkout_id = '{c5}'")
    check("and the row is rebuilt", owner == driver, f"owner = {owner!r}")

    print("\n== a paid checkout nobody can attribute")
    held = paid_until(driver)
    code, body = post_webhook(event("probe_" + uuid.uuid4().hex[:20]), secret)
    check("is accepted, not retried forever", code == 200, f"got {code}")
    check("and extends nobody", paid_until(driver) == held, "somebody got a free month")

    # ── 11. where his browser lands ─────────────────────────────────────────
    print("\n== the page Chargily sends him back to")
    code, body = request("/subscription/done?state=success")
    check("success is a page, not a 404", code == 200 and "Paiement" in body, f"got {code}")
    check("it offers a way back into the app", "movin://" in body, body[:120])
    code, body = request("/subscription/done?state=failure")
    check("failure says no money was taken", "débité" in body, body[:160])

    # ── 12. the one thing only Chargily can answer ──────────────────────────
    print("\n== is that key one Chargily knows?")
    # curl, not urllib, and that is not a style choice. Chargily sits behind
    # Cloudflare, which refuses `Python-urllib/3.x` with **HTTP 403, error code
    # 1010** -- "banned based on your browser's signature". That is Cloudflare
    # declining to ask, not Chargily declining the key, and reading it as a
    # verdict on the key is how this check lies. curl gets through.
    r = subprocess.run(
        ["curl", "-s", "-o", "/dev/stdout", "-w", "\n%{http_code}",
         "-H", "Authorization: Bearer " + secret,
         "--max-time", "25", base.rstrip("/") + "/balance"],
        capture_output=True, text=True, timeout=40)
    out = r.stdout.rsplit("\n", 1)
    live = (int(out[-1]) if out[-1].strip().isdigit() else 0, out[0][:160])
    if live[0] == 403 and "1010" in live[1]:
        print("  ----  Cloudflare refused the probe itself (1010), so this says")
        print("        nothing about the key. Retry with curl by hand.")
    elif live[0] == 200:
        check("the gateway accepts our key", True)
        print(f"        balance: {live[1]}")
    else:
        print(f"  ----  the gateway does NOT accept this key: HTTP {live[0]} {live[1]}")
        print("        Everything above still holds -- the webhook is ours and it is")
        print("        proven. This is the half that needs the real secret key")
        print("        (test_sk_...). A public key (test_pk_...) cannot create a")
        print("        checkout: their own reference authenticates with the secret.")

finally:
    cleanup()
    print(f"\n   cleaned up; {driver} restored to {before or '(no row)'}")

print(f"\n{passed} passed, {failed} failed")
sys.exit(1 if failed else 0)
